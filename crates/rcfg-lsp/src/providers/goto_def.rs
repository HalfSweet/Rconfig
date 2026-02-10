use rcfg_lang::{SymbolKind, ast::ValuesStmt, parse_values_with_diagnostics};
use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Url};

use crate::document::{DocumentKind, ProjectSnapshot};
use crate::position::{lsp_position_to_offset, span_to_lsp_range};

pub fn provide(
    project: &ProjectSnapshot,
    uri: &Url,
    position: Position,
    source_text: &str,
    kind: DocumentKind,
) -> Option<GotoDefinitionResponse> {
    let analysis = project.analysis.as_ref()?;
    let local_offset = lsp_position_to_offset(source_text, position);

    if kind == DocumentKind::Values {
        return provide_values_definition(project, source_text, local_offset);
    }

    if let Some(binding) = analysis
        .alias_bindings
        .iter()
        .find(|binding| binding.uri == *uri && binding.contains_local_offset(local_offset))
        && let Some(location) = definition_location(project, &binding.target_path)
    {
        return Some(GotoDefinitionResponse::Scalar(location));
    }

    let base_offset = project.uri_base_offsets.get(uri).copied().unwrap_or(0);
    let global_offset = local_offset + base_offset;

    let occurrence = analysis
        .position_index
        .find_symbol_at_offset(global_offset)?;

    if let Some(location) = definition_location(project, &occurrence.path) {
        return Some(GotoDefinitionResponse::Scalar(location));
    }

    None
}

fn provide_values_definition(
    project: &ProjectSnapshot,
    source_text: &str,
    local_offset: usize,
) -> Option<GotoDefinitionResponse> {
    let analysis = project.analysis.as_ref()?;
    let (values, _) = parse_values_with_diagnostics(source_text);

    for stmt in values.stmts {
        let ValuesStmt::Assign(assign) = stmt else {
            continue;
        };
        if !span_contains_with_cursor_tail(assign.path.span, local_offset) {
            continue;
        }

        let raw_path = assign.path.to_string();
        let mut matches = analysis
            .symbols
            .iter()
            .filter_map(|(path, info)| {
                if !matches!(info.kind, SymbolKind::Option) {
                    return None;
                }
                if path == &raw_path || path.ends_with(&format!("::{raw_path}")) {
                    return Some(path.to_string());
                }
                None
            })
            .collect::<Vec<_>>();
        matches.sort();
        matches.dedup();
        if matches.len() != 1 {
            return None;
        }

        if let Some(location) = definition_location(project, &matches[0]) {
            return Some(GotoDefinitionResponse::Scalar(location));
        }
    }

    None
}

fn span_contains_with_cursor_tail(span: rcfg_lang::Span, offset: usize) -> bool {
    if span.start == span.end {
        offset == span.start
    } else {
        (offset >= span.start && offset < span.end) || offset == span.end
    }
}

fn definition_location(project: &ProjectSnapshot, path: &str) -> Option<Location> {
    let analysis = project.analysis.as_ref()?;

    let span = analysis
        .symbols
        .symbol_span(path)
        .or_else(|| analysis.symbols.option_span(path))
        .or_else(|| analysis.symbols.enum_variant_span(path))?;

    for file in &analysis.schema_files {
        if !file.contains_global_offset(span.start) {
            continue;
        }

        let local_span = rcfg_lang::Span::new(
            file.global_to_local_offset(span.start),
            file.global_to_local_offset(span.end),
        );

        return Some(Location {
            uri: file.uri.clone(),
            range: span_to_lsp_range(file.text.as_str(), local_span),
        });
    }

    None
}
