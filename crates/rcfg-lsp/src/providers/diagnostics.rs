use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use rcfg_lang::{
    Diagnostic as RcfgDiagnostic, Item, Severity, SymbolTable, analyze_schema_files,
    analyze_values, analyze_values_from_path_report_with_context_and_root,
    parse_schema_with_diagnostics, parse_values_with_diagnostics,
};
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Url,
};

use crate::document::{
    AliasBinding, DocumentKind, DocumentState, ProjectAnalysis, ProjectSnapshot, SchemaFileSnapshot,
};
use crate::position::span_to_lsp_range;
use crate::project::{
    ValuesSchemaResolution, find_nearest_manifest, load_manifest_graph, manifest_schema_paths,
    resolve_values_schema,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnalysisTrigger {
    DidOpen,
    DidChange,
    DidSave,
    DidClose,
}

#[derive(Debug, Default)]
pub struct DiagnosticsResult {
    pub diagnostics_by_uri: HashMap<Url, Vec<Diagnostic>>,
    pub project: Option<(String, ProjectSnapshot)>,
    pub parse_only_notice: Option<String>,
}

#[derive(Debug, Clone)]
struct SchemaSource {
    uri: Url,
    path: PathBuf,
    text: String,
}

#[derive(Debug)]
struct SchemaAnalysis {
    diagnostics_by_uri: HashMap<Url, Vec<Diagnostic>>,
    analysis: ProjectAnalysis,
    uri_base_offsets: HashMap<Url, usize>,
    doc_indexes: HashMap<Url, String>,
}

pub fn analyze_document(
    uri: &Url,
    kind: DocumentKind,
    trigger: AnalysisTrigger,
    open_documents: &[DocumentState],
) -> DiagnosticsResult {
    match kind {
        DocumentKind::Schema => analyze_schema_document(uri, open_documents),
        DocumentKind::Values => analyze_values_document(uri, trigger, open_documents),
        DocumentKind::Unknown => DiagnosticsResult::default(),
    }
}

fn analyze_schema_document(uri: &Url, open_documents: &[DocumentState]) -> DiagnosticsResult {
    let Some(path) = uri.to_file_path().ok() else {
        return DiagnosticsResult::default();
    };

    let (project_key, sources) = collect_schema_sources_for_path(&path, open_documents);
    let Some(analysis) = analyze_schema_sources(&sources) else {
        return DiagnosticsResult::default();
    };

    let schema_docs = sources
        .iter()
        .map(|item| (item.uri.clone(), item.text.clone()))
        .collect::<HashMap<_, _>>();

    let project = ProjectSnapshot {
        project_key: project_key.clone(),
        schema_docs,
        uri_base_offsets: analysis.uri_base_offsets,
        analysis: Some(analysis.analysis),
        doc_indexes: analysis.doc_indexes,
    };

    DiagnosticsResult {
        diagnostics_by_uri: analysis.diagnostics_by_uri,
        project: Some((project_key, project)),
        parse_only_notice: None,
    }
}

fn analyze_values_document(
    uri: &Url,
    trigger: AnalysisTrigger,
    open_documents: &[DocumentState],
) -> DiagnosticsResult {
    let Some(values_doc) = open_documents.iter().find(|doc| &doc.uri == uri) else {
        return DiagnosticsResult::default();
    };

    let values_path = uri.to_file_path().ok();
    let open_schema_paths = open_documents
        .iter()
        .filter(|doc| doc.kind == DocumentKind::Schema)
        .filter_map(|doc| doc.uri.to_file_path().ok())
        .collect::<Vec<_>>();

    let resolution = values_path
        .as_ref()
        .map(|path| resolve_values_schema(path, &open_schema_paths))
        .unwrap_or_else(|| {
            if open_schema_paths.is_empty() {
                ValuesSchemaResolution::ParseOnly
            } else {
                ValuesSchemaResolution::OpenSchemaFallback {
                    schemas: open_schema_paths.clone(),
                }
            }
        });

    let mut diagnostics_by_uri = HashMap::<Url, Vec<Diagnostic>>::new();
    let mut parse_only_notice = None;
    let mut project = None;

    let schema_sources = match &resolution {
        ValuesSchemaResolution::Manifest { schemas, .. } => {
            collect_schema_sources_from_paths(schemas, open_documents)
        }
        ValuesSchemaResolution::NearbySchema { schema } => {
            collect_schema_sources_from_paths(std::slice::from_ref(schema), open_documents)
        }
        ValuesSchemaResolution::OpenSchemaFallback { schemas } => {
            collect_schema_sources_from_paths(schemas, open_documents)
        }
        ValuesSchemaResolution::ParseOnly => Vec::new(),
    };

    if schema_sources.is_empty() {
        let (values_file, parse_diags) = parse_values_with_diagnostics(&values_doc.text);
        let _ = values_file;
        append_local_values_diagnostics(
            uri,
            &values_doc.text,
            &parse_diags,
            &mut diagnostics_by_uri,
        );

        parse_only_notice = Some(
            "No schema context found for values file; only parse diagnostics are available"
                .to_string(),
        );

        return DiagnosticsResult {
            diagnostics_by_uri,
            project,
            parse_only_notice,
        };
    }

    let Some(schema_analysis) = analyze_schema_sources(&schema_sources) else {
        return DiagnosticsResult {
            diagnostics_by_uri,
            project,
            parse_only_notice,
        };
    };

    let symbols = schema_analysis.analysis.symbols.clone();

    if let Some(path) = values_path.as_ref()
        && matches!(trigger, AnalysisTrigger::DidSave)
    {
        let semantic_diags = match &resolution {
            ValuesSchemaResolution::Manifest { manifest, .. } => {
                let root = manifest.parent().unwrap_or_else(|| Path::new("."));
                analyze_values_from_path_report_with_context_and_root(
                    path,
                    &symbols,
                    &HashMap::new(),
                    root,
                )
                .diagnostics
            }
            ValuesSchemaResolution::NearbySchema { .. }
            | ValuesSchemaResolution::OpenSchemaFallback { .. }
            | ValuesSchemaResolution::ParseOnly => {
                analyze_values_from_path_report_with_context_and_root(
                    path,
                    &symbols,
                    &HashMap::new(),
                    path.parent().unwrap_or_else(|| Path::new(".")),
                )
                .diagnostics
            }
        };

        append_include_aware_values_diagnostics(
            uri,
            &values_doc.text,
            &semantic_diags,
            open_documents,
            &mut diagnostics_by_uri,
        );
    } else {
        let (values_file, parse_diags) = parse_values_with_diagnostics(&values_doc.text);
        append_local_values_diagnostics(
            uri,
            &values_doc.text,
            &parse_diags,
            &mut diagnostics_by_uri,
        );
        let semantic_diags = analyze_values(&values_file, &symbols);
        append_local_values_diagnostics(
            uri,
            &values_doc.text,
            &semantic_diags,
            &mut diagnostics_by_uri,
        );
    }

    let key = values_path
        .as_ref()
        .and_then(|path| find_nearest_manifest(path))
        .map(|path| format!("manifest:{}", path.display()))
        .unwrap_or_else(|| format!("values:{}", uri));

    let schema_docs = schema_sources
        .iter()
        .map(|item| (item.uri.clone(), item.text.clone()))
        .collect::<HashMap<_, _>>();

    project = Some((
        key.clone(),
        ProjectSnapshot {
            project_key: key,
            schema_docs,
            uri_base_offsets: schema_analysis.uri_base_offsets,
            analysis: Some(schema_analysis.analysis),
            doc_indexes: schema_analysis.doc_indexes,
        },
    ));

    DiagnosticsResult {
        diagnostics_by_uri,
        project,
        parse_only_notice,
    }
}

fn collect_schema_sources_for_path(
    path: &Path,
    open_documents: &[DocumentState],
) -> (String, Vec<SchemaSource>) {
    let open_schema_overlays = open_documents
        .iter()
        .filter(|doc| doc.kind == DocumentKind::Schema)
        .filter_map(|doc| {
            doc.uri
                .to_file_path()
                .ok()
                .map(|path| (path, doc.text.clone(), doc.uri.clone()))
        })
        .collect::<Vec<_>>();

    if let Some(manifest) = find_nearest_manifest(path)
        && let Ok(graph) = load_manifest_graph(&manifest)
    {
        let project_key = format!("manifest:{}", manifest.display());
        let baseline = manifest_schema_paths(&graph);

        let mut resolved = HashMap::<PathBuf, SchemaSource>::new();
        for schema_path in baseline {
            if let Some((_, text, uri)) = open_schema_overlays
                .iter()
                .find(|(candidate, _, _)| same_path(candidate, &schema_path))
            {
                resolved.insert(
                    schema_path.clone(),
                    SchemaSource {
                        uri: uri.clone(),
                        path: schema_path,
                        text: text.clone(),
                    },
                );
                continue;
            }

            if let Ok(text) = fs::read_to_string(&schema_path)
                && let Ok(uri) = Url::from_file_path(&schema_path)
            {
                resolved.insert(
                    schema_path.clone(),
                    SchemaSource {
                        uri,
                        path: schema_path,
                        text,
                    },
                );
            }
        }

        for (open_path, text, open_uri) in &open_schema_overlays {
            let same_project = find_nearest_manifest(open_path)
                .as_ref()
                .is_some_and(|candidate| same_path(candidate, &manifest));
            if !same_project {
                continue;
            }

            resolved
                .entry(open_path.clone())
                .or_insert_with(|| SchemaSource {
                    uri: open_uri.clone(),
                    path: open_path.clone(),
                    text: text.clone(),
                });
        }

        let mut sources = resolved.into_values().collect::<Vec<_>>();
        sources.sort_by(|left, right| left.path.cmp(&right.path));
        return (project_key, sources);
    }

    let mut sources = collect_schema_sources_from_paths(&[path.to_path_buf()], open_documents);

    for (open_path, text, open_uri) in &open_schema_overlays {
        let no_manifest = find_nearest_manifest(open_path).is_none();
        if no_manifest
            && !sources
                .iter()
                .any(|source| same_path(&source.path, open_path))
        {
            sources.push(SchemaSource {
                uri: open_uri.clone(),
                path: open_path.clone(),
                text: text.clone(),
            });
        }
    }

    sources.sort_by(|left, right| left.path.cmp(&right.path));
    let key = path
        .parent()
        .map(|parent| format!("dir:{}", parent.display()))
        .unwrap_or_else(|| format!("uri:{}", path.display()));
    (key, sources)
}

fn collect_schema_sources_from_paths(
    paths: &[PathBuf],
    open_documents: &[DocumentState],
) -> Vec<SchemaSource> {
    let overlays = open_documents
        .iter()
        .filter(|doc| doc.kind == DocumentKind::Schema)
        .filter_map(|doc| {
            doc.uri
                .to_file_path()
                .ok()
                .map(|path| (path, doc.text.clone(), doc.uri.clone()))
        })
        .collect::<Vec<_>>();

    let mut out = Vec::new();
    for path in paths {
        if let Some((_, text, uri)) = overlays
            .iter()
            .find(|(candidate, _, _)| same_path(candidate, path))
        {
            out.push(SchemaSource {
                uri: uri.clone(),
                path: path.clone(),
                text: text.clone(),
            });
            continue;
        }

        if let Ok(text) = fs::read_to_string(path)
            && let Ok(uri) = Url::from_file_path(path)
        {
            out.push(SchemaSource {
                uri,
                path: path.clone(),
                text,
            });
        }
    }

    out.sort_by(|left, right| left.path.cmp(&right.path));
    out.dedup_by(|left, right| same_path(&left.path, &right.path));
    out
}

fn analyze_schema_sources(sources: &[SchemaSource]) -> Option<SchemaAnalysis> {
    if sources.is_empty() {
        return None;
    }

    let mut files = Vec::new();
    let mut all_diags = Vec::new();
    let mut schema_files = Vec::new();
    let mut uri_base_offsets = HashMap::new();
    let mut doc_indexes = HashMap::new();

    let mut base_offset = 0usize;
    for source in sources {
        let prefixed = format!("{}{}", " ".repeat(base_offset), source.text);
        let (file, parse_diags) = parse_schema_with_diagnostics(&prefixed);

        files.push(file);
        all_diags.extend(parse_diags);

        schema_files.push(SchemaFileSnapshot {
            uri: source.uri.clone(),
            text: source.text.clone(),
            base_offset,
        });
        uri_base_offsets.insert(source.uri.clone(), base_offset);
        doc_indexes.insert(source.uri.clone(), source.text.clone());

        base_offset += source.text.len() + 1;
    }

    let semantic = analyze_schema_files(&files);
    all_diags.extend(semantic.diagnostics.clone());

    let mut diagnostics_by_uri = HashMap::<Url, Vec<Diagnostic>>::new();
    for diag in &all_diags {
        let (uri, text, local_diag) = map_schema_diag_to_local(diag, &schema_files);
        let lsp = rcfg_diag_to_lsp(&local_diag, text, &schema_files);
        diagnostics_by_uri.entry(uri).or_default().push(lsp);
    }

    for diagnostics in diagnostics_by_uri.values_mut() {
        dedup_lsp_diagnostics(diagnostics);
    }

    let analysis = ProjectAnalysis {
        symbols: semantic.symbols.clone(),
        diagnostics: semantic.diagnostics,
        position_index: semantic.symbols.build_position_index(),
        schema_files,
        alias_bindings: collect_alias_bindings(sources, &semantic.symbols),
    };

    Some(SchemaAnalysis {
        diagnostics_by_uri,
        analysis,
        uri_base_offsets,
        doc_indexes,
    })
}

fn collect_alias_bindings(sources: &[SchemaSource], symbols: &SymbolTable) -> Vec<AliasBinding> {
    let mut bindings = Vec::new();

    for source in sources {
        let (file, _) = parse_schema_with_diagnostics(&source.text);
        let mut scope = Vec::new();
        collect_alias_bindings_in_items(
            &file.items,
            &mut scope,
            &source.uri,
            symbols,
            &mut bindings,
        );
    }

    bindings.sort_by(|left, right| {
        left.uri
            .as_str()
            .cmp(right.uri.as_str())
            .then(left.alias_span.start.cmp(&right.alias_span.start))
            .then(left.alias.cmp(&right.alias))
    });
    bindings.dedup_by(|left, right| {
        left.uri == right.uri
            && left.alias_span == right.alias_span
            && left.alias == right.alias
            && left.target_path == right.target_path
    });

    bindings
}

fn collect_alias_bindings_in_items(
    items: &[Item],
    scope: &mut Vec<String>,
    uri: &Url,
    symbols: &SymbolTable,
    out: &mut Vec<AliasBinding>,
) {
    for item in items {
        match item {
            Item::Use(use_stmt) => {
                let Some(alias) = &use_stmt.alias else {
                    continue;
                };

                let raw_path = use_stmt.path.to_string();
                let Some(target_path) = resolve_alias_target_path(symbols, scope, &raw_path) else {
                    continue;
                };

                out.push(AliasBinding {
                    uri: uri.clone(),
                    alias: alias.value.clone(),
                    raw_path,
                    alias_span: alias.span,
                    target_path,
                });
            }
            Item::Mod(mod_decl) => {
                scope.push(mod_decl.name.value.clone());
                collect_alias_bindings_in_items(&mod_decl.items, scope, uri, symbols, out);
                let _ = scope.pop();
            }
            Item::When(when_block) => {
                collect_alias_bindings_in_items(&when_block.items, scope, uri, symbols, out);
            }
            Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_alias_bindings_in_items(&case.items, scope, uri, symbols, out);
                }
            }
            _ => {}
        }
    }
}

fn resolve_alias_target_path(
    symbols: &SymbolTable,
    scope: &[String],
    raw_path: &str,
) -> Option<String> {
    let mut matches = build_candidate_paths(scope, raw_path)
        .into_iter()
        .filter(|candidate| symbol_path_exists(symbols, candidate))
        .collect::<Vec<_>>();

    matches.sort();
    matches.dedup();

    if matches.len() == 1 {
        return matches.pop();
    }

    if let Some(exact) = matches
        .iter()
        .find(|candidate| candidate.as_str() == raw_path)
    {
        return Some(exact.clone());
    }

    if !matches.is_empty() {
        return None;
    }

    if symbol_path_exists(symbols, raw_path) {
        return Some(raw_path.to_string());
    }

    let suffix = format!("::{raw_path}");
    let mut suffix_matches = collect_known_symbol_paths(symbols)
        .into_iter()
        .filter(|candidate| candidate.ends_with(&suffix))
        .collect::<Vec<_>>();
    suffix_matches.sort();
    suffix_matches.dedup();

    if suffix_matches.len() == 1 {
        return suffix_matches.pop();
    }

    None
}

fn build_candidate_paths(scope: &[String], raw_path: &str) -> Vec<String> {
    let mut candidates = Vec::new();

    for level in (0..=scope.len()).rev() {
        let prefix = &scope[..level];
        if prefix.is_empty() {
            candidates.push(raw_path.to_string());
        } else {
            candidates.push(format!("{}::{raw_path}", prefix.join("::")));
        }
    }

    candidates
}

fn symbol_path_exists(symbols: &SymbolTable, path: &str) -> bool {
    symbols.get(path).is_some()
        || symbols.option_span(path).is_some()
        || symbols.symbol_span(path).is_some()
        || symbols.enum_variant_span(path).is_some()
        || symbols.enum_owner_of_variant(path).is_some()
}

fn collect_known_symbol_paths(symbols: &SymbolTable) -> Vec<String> {
    let mut paths = symbols
        .iter()
        .map(|(path, _)| path.to_string())
        .collect::<Vec<_>>();

    for occurrence in symbols.build_position_index().occurrences() {
        if occurrence.role == rcfg_lang::SymbolOccurrenceRole::Definition
            && symbols.enum_owner_of_variant(&occurrence.path).is_some()
        {
            paths.push(occurrence.path.clone());
        }
    }

    paths.sort();
    paths.dedup();
    paths
}

fn map_schema_diag_to_local<'a>(
    diag: &RcfgDiagnostic,
    schema_files: &'a [SchemaFileSnapshot],
) -> (Url, &'a str, RcfgDiagnostic) {
    let fallback = schema_files
        .first()
        .expect("schema_files should not be empty");

    let schema = schema_files
        .iter()
        .find(|file| file.contains_global_offset(diag.span.start))
        .unwrap_or(fallback);

    let mut local_diag = diag.clone();
    local_diag.span.start = schema.global_to_local_offset(diag.span.start);
    local_diag.span.end = schema.global_to_local_offset(diag.span.end);

    for related in &mut local_diag.related {
        if let Some(related_schema) = schema_files
            .iter()
            .find(|file| file.contains_global_offset(related.span.start))
        {
            related.span.start = related_schema.global_to_local_offset(related.span.start);
            related.span.end = related_schema.global_to_local_offset(related.span.end);
        }
    }

    (schema.uri.clone(), schema.text.as_str(), local_diag)
}

fn append_local_values_diagnostics(
    uri: &Url,
    text: &str,
    diagnostics: &[RcfgDiagnostic],
    out: &mut HashMap<Url, Vec<Diagnostic>>,
) {
    for diag in diagnostics {
        let lsp = rcfg_diag_to_lsp(diag, text, &[]);
        out.entry(uri.clone()).or_default().push(lsp);
    }

    if let Some(items) = out.get_mut(uri) {
        dedup_lsp_diagnostics(items);
    }
}

fn append_include_aware_values_diagnostics(
    current_uri: &Url,
    current_text: &str,
    diagnostics: &[RcfgDiagnostic],
    open_documents: &[DocumentState],
    out: &mut HashMap<Url, Vec<Diagnostic>>,
) {
    for diag in diagnostics {
        let (uri, text) = if let Some(source) = &diag.source {
            let source_path = PathBuf::from(source);
            let uri = Url::from_file_path(&source_path)
                .ok()
                .unwrap_or_else(|| current_uri.clone());
            let text = open_documents
                .iter()
                .find(|doc| doc.uri == uri)
                .map(|doc| doc.text.clone())
                .or_else(|| fs::read_to_string(&source_path).ok())
                .unwrap_or_else(|| current_text.to_string());
            (uri, text)
        } else {
            (current_uri.clone(), current_text.to_string())
        };

        let lsp = rcfg_diag_to_lsp(diag, &text, &[]);
        out.entry(uri).or_default().push(lsp);
    }

    for diagnostics in out.values_mut() {
        dedup_lsp_diagnostics(diagnostics);
    }
}

fn rcfg_diag_to_lsp(
    diag: &RcfgDiagnostic,
    source_text: &str,
    schema_files: &[SchemaFileSnapshot],
) -> Diagnostic {
    let mut message = diag.message.clone();
    if let Some(note) = diag.note.as_ref().filter(|text| !text.trim().is_empty()) {
        message.push_str("\n\n");
        message.push_str(note);
    }
    if !diag.include_chain.is_empty() {
        message.push_str("\n\ninclude chain: ");
        message.push_str(&diag.include_chain.join(" -> "));
    }

    let related_information = if diag.related.is_empty() {
        None
    } else {
        let related = diag
            .related
            .iter()
            .map(|item| {
                let location = if let Some(path) = item.path.as_ref() {
                    let maybe_path = PathBuf::from(path);
                    if maybe_path.is_file() {
                        Url::from_file_path(maybe_path).ok().map(|uri| Location {
                            uri,
                            range: span_to_lsp_range(source_text, item.span),
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
                .or_else(|| {
                    schema_files
                        .iter()
                        .find(|file| file.contains_global_offset(item.span.start))
                        .map(|file| Location {
                            uri: file.uri.clone(),
                            range: span_to_lsp_range(
                                file.text.as_str(),
                                rcfg_lang::Span::new(
                                    file.global_to_local_offset(item.span.start),
                                    file.global_to_local_offset(item.span.end),
                                ),
                            ),
                        })
                })
                .unwrap_or_else(|| Location {
                    uri: Url::parse("untitled:rcfg").expect("valid fallback URI"),
                    range: span_to_lsp_range(source_text, item.span),
                });

                DiagnosticRelatedInformation {
                    location,
                    message: item.message.clone(),
                }
            })
            .collect::<Vec<_>>();
        Some(related)
    };

    Diagnostic {
        range: span_to_lsp_range(source_text, diag.span),
        severity: Some(match diag.severity {
            Severity::Error => DiagnosticSeverity::ERROR,
            Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: Some(NumberOrString::String(diag.code.clone())),
        source: Some("rcfg-lang".to_string()),
        message,
        related_information,
        ..Diagnostic::default()
    }
}

fn dedup_lsp_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    let mut seen = HashSet::new();
    diagnostics.retain(|diag| {
        let key = format!(
            "{:?}:{:?}:{:?}:{:?}:{:?}:{:?}",
            diag.severity, diag.code, diag.range.start, diag.range.end, diag.source, diag.message
        );
        seen.insert(key)
    });
}

fn same_path(left: &Path, right: &Path) -> bool {
    let left = fs::canonicalize(left).unwrap_or_else(|_| left.to_path_buf());
    let right = fs::canonicalize(right).unwrap_or_else(|_| right.to_path_buf());
    left == right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn collects_alias_binding_with_scoped_resolution() {
        let source = SchemaSource {
            uri: Url::from_file_path("/tmp/alias_scope_schema.rcfg").expect("uri"),
            path: PathBuf::from("/tmp/alias_scope_schema.rcfg"),
            text: r#"
mod app {
  option enabled: bool = true;
  use enabled as enabled_alias;

  when enabled_alias {
    require!(enabled_alias == true);
  }
}
"#
            .to_string(),
        };

        let analysis = analyze_schema_sources(&[source]).expect("analysis");
        let binding = analysis
            .analysis
            .alias_bindings
            .iter()
            .find(|binding| binding.alias == "enabled_alias")
            .expect("alias binding should exist");

        assert_eq!(binding.raw_path, "enabled");
        assert_eq!(binding.target_path, "app::enabled");
        assert!(binding.contains_local_offset(binding.alias_span.start));
    }
}
