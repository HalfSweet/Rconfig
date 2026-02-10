use super::*;

mod type_checker;
use self::type_checker::TypeChecker;

fn analyze_schema_items(items: &[Item]) -> SemanticReport {
    let mut collector = SymbolCollector::default();
    let mut root_scope = Vec::new();
    collector.collect_items(items, &mut root_scope);

    let mut diagnostics = collector.diagnostics;
    let mut symbols = collector.symbols;
    symbols.set_schema_items(items.to_vec());

    let mut checker = TypeChecker::new(&symbols);
    let mut scope = Vec::new();
    checker.check_items(items, &mut scope);
    diagnostics.extend(checker.diagnostics);

    SemanticReport {
        symbols,
        diagnostics,
    }
}

pub fn analyze_schema(file: &File) -> SemanticReport {
    analyze_schema_items(&file.items)
}

pub fn analyze_schema_strict(file: &File) -> SemanticReport {
    let mut report = analyze_schema(file);
    promote_strict_diagnostics(&mut report.diagnostics);
    report
}

pub fn analyze_schema_files(files: &[File]) -> SemanticReport {
    let items = files
        .iter()
        .flat_map(|file| file.items.iter().cloned())
        .collect::<Vec<_>>();
    analyze_schema_items(&items)
}

pub fn analyze_schema_files_strict(files: &[File]) -> SemanticReport {
    let mut report = analyze_schema_files(files);
    promote_strict_diagnostics(&mut report.diagnostics);
    report
}

pub fn analyze_values(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    analyze_values_with_context(values, symbols, &HashMap::new())
}

pub fn analyze_values_strict(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    let mut diagnostics = analyze_values_with_context(values, symbols, &HashMap::new());
    promote_strict_diagnostics(&mut diagnostics);
    diagnostics
}

pub fn analyze_values_with_context(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> Vec<Diagnostic> {
    analyze_values_with_context_and_stmt_indexes(values, symbols, context).0
}

pub fn analyze_values_with_context_strict(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> Vec<Diagnostic> {
    let mut diagnostics = analyze_values_with_context(values, symbols, context);
    promote_strict_diagnostics(&mut diagnostics);
    diagnostics
}

fn analyze_values_with_context_and_stmt_indexes(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> (Vec<Diagnostic>, Vec<Option<usize>>) {
    let mut checker = ValuesChecker::new(symbols);
    checker.ingest_context(context);
    checker.check(values);
    let post_diags = checker.post_check_diagnostics();

    let mut diagnostics = checker.diagnostics;
    let mut stmt_indexes = checker.diagnostic_stmt_indexes;
    for (diagnostic, stmt_index) in post_diags {
        diagnostics.push(diagnostic);
        stmt_indexes.push(stmt_index);
    }

    (diagnostics, stmt_indexes)
}

pub fn analyze_values_from_path_report(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> ValuesAnalysisReport {
    analyze_values_from_path_report_with_context(entry, symbols, &HashMap::new())
}

pub fn analyze_values_from_path_report_with_context(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ValuesAnalysisReport {
    let (values, stmt_origins, mut diagnostics) = expand_values_includes_with_origins(entry);
    let mut diagnostic_stmt_indexes = vec![None; diagnostics.len()];
    let (mut semantic, mut semantic_stmt_indexes) =
        analyze_values_with_context_and_stmt_indexes(&values, symbols, context);
    diagnostics.append(&mut semantic);
    diagnostic_stmt_indexes.append(&mut semantic_stmt_indexes);
    let resolved = resolve_values_with_context(&values, symbols, context);
    ValuesAnalysisReport {
        values,
        resolved,
        stmt_origins,
        diagnostics,
        diagnostic_stmt_indexes,
    }
}

pub fn analyze_values_from_path_report_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> ValuesAnalysisReport {
    let mut report = analyze_values_from_path_report_with_context(entry, symbols, &HashMap::new());
    promote_strict_diagnostics(&mut report.diagnostics);
    report
}

pub fn analyze_values_from_path_report_with_context_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ValuesAnalysisReport {
    let mut report = analyze_values_from_path_report_with_context(entry, symbols, context);
    promote_strict_diagnostics(&mut report.diagnostics);
    report
}

pub fn analyze_values_from_path(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> (ValuesFile, Vec<Diagnostic>) {
    let report = analyze_values_from_path_report(entry, symbols);
    (report.values, report.diagnostics)
}

pub fn analyze_values_from_path_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> (ValuesFile, Vec<Diagnostic>) {
    let report = analyze_values_from_path_report_strict(entry, symbols);
    (report.values, report.diagnostics)
}

pub fn resolve_values(values: &ValuesFile, symbols: &SymbolTable) -> ResolvedConfig {
    resolve_values_with_context(values, symbols, &HashMap::new())
}

pub fn resolve_values_with_context(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ResolvedConfig {
    let mut checker = ValuesChecker::new(symbols);
    checker.ingest_context(context);
    checker.check(values);
    let _ = checker.post_check_diagnostics();
    checker.resolved_config().unwrap_or_else(|| {
        build_resolved_config(
            symbols,
            &RuntimeState {
                active: BTreeSet::new(),
                values: BTreeMap::new(),
                sources: BTreeMap::new(),
                ctx_references: BTreeSet::new(),
            },
        )
    })
}

pub fn expand_values_includes_from_path(entry: &FsPath) -> (ValuesFile, Vec<Diagnostic>) {
    let (values, _, diagnostics) = expand_values_includes_with_origins(entry);
    (values, diagnostics)
}

pub fn expand_values_includes_with_origins(
    entry: &FsPath,
) -> (ValuesFile, Vec<ValuesStmtOrigin>, Vec<Diagnostic>) {
    let mut expander = IncludeExpander::default();
    let mut stmts = Vec::new();
    expander.expand_file(entry, &mut stmts);
    let mut values_stmts = Vec::with_capacity(stmts.len());
    let mut origins = Vec::with_capacity(stmts.len());
    for expanded in stmts {
        values_stmts.push(expanded.stmt);
        origins.push(expanded.origin);
    }
    (
        ValuesFile {
            stmts: values_stmts,
        },
        origins,
        expander.diagnostics,
    )
}

#[derive(Debug, Clone, Copy)]
struct DeclInfo {
    kind: SymbolKind,
    span: Span,
}

#[derive(Default)]
struct SymbolCollector {
    symbols: SymbolTable,
    diagnostics: Vec<Diagnostic>,
    scope_members: HashMap<String, HashMap<String, DeclInfo>>,
    mod_metadata_spans: HashMap<String, Span>,
}

impl SymbolCollector {
    fn collect_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        self.collect_items_in_context(items, scope, false);
    }

    fn collect_items_in_context(
        &mut self,
        items: &[Item],
        scope: &mut Vec<String>,
        in_conditional: bool,
    ) {
        for item in items {
            match item {
                Item::Use(_) | Item::Require(_) | Item::Constraint(_) => {}
                Item::Option(option) => {
                    let full_path = build_full_path(scope, &option.name.value);
                    if self.declare(
                        scope,
                        &option.name.value,
                        SymbolKind::Option,
                        option.name.span,
                    ) {
                        self.symbols.insert_option_type(
                            full_path.clone(),
                            option_type_to_value_type(&option.ty),
                        );
                        self.symbols
                            .insert_option_span(full_path.clone(), option.name.span);
                        self.symbols.insert_option_secret(
                            full_path.clone(),
                            option_has_secret_attr(option),
                        );
                        self.symbols
                            .insert_option_always_active(full_path.clone(), !in_conditional);
                        if let Some(default) = option.default.clone() {
                            self.symbols
                                .insert_option_default(full_path.clone(), default);
                        }
                        if let Some(range) = extract_range_attr(option) {
                            self.symbols.insert_option_range(full_path, range);
                        }
                    }
                }
                Item::Enum(enum_decl) => {
                    let enum_path = build_full_path(scope, &enum_decl.name.value);
                    if self.declare(
                        scope,
                        &enum_decl.name.value,
                        SymbolKind::Enum,
                        enum_decl.name.span,
                    ) {
                        for variant in &enum_decl.variants {
                            let variant_path = format!("{}::{}", enum_path, variant.name.value);
                            if self
                                .symbols
                                .insert_enum_variant(
                                    variant_path.clone(),
                                    enum_path.clone(),
                                    variant.name.span,
                                )
                                .is_some()
                            {
                                self.diagnostics.push(Diagnostic::error(
                                    "E_SYMBOL_REDEFINED",
                                    format!("enum variant `{}` is redefined", variant_path),
                                    variant.name.span,
                                ));
                            }
                        }
                    }
                }
                Item::Mod(module) => {
                    let module_path = build_full_path(scope, &module.name.value);
                    if module_has_metadata(module) {
                        if let Some(previous_span) = self.mod_metadata_spans.get(&module_path) {
                            self.diagnostics.push(Diagnostic::warning(
                                "W_DUPLICATE_MOD_METADATA",
                                format!(
                                    "module `{}` metadata is repeated; first metadata wins",
                                    module_path
                                ),
                                module.span.join(*previous_span),
                            ));
                        } else {
                            self.mod_metadata_spans
                                .insert(module_path.clone(), module.span);
                        }
                    }

                    self.declare(scope, &module.name.value, SymbolKind::Mod, module.name.span);
                    scope.push(module.name.value.clone());
                    self.collect_items_in_context(&module.items, scope, in_conditional);
                    scope.pop();
                }
                Item::When(when_block) => {
                    self.collect_items_in_context(&when_block.items, scope, true);
                }
                Item::Match(match_block) => {
                    for case in &match_block.cases {
                        self.collect_items_in_context(&case.items, scope, true);
                    }
                }
            }
        }
    }

    fn declare(&mut self, scope: &[String], name: &str, kind: SymbolKind, span: Span) -> bool {
        let scope_key = scope.join("::");
        let full_path = if scope_key.is_empty() {
            name.to_string()
        } else {
            format!("{}::{}", scope_key, name)
        };

        let members = self.scope_members.entry(scope_key).or_default();
        if let Some(existing) = members.get(name) {
            if existing.kind == kind {
                if kind == SymbolKind::Mod {
                    return false;
                }
                self.diagnostics.push(Diagnostic::error(
                    "E_SYMBOL_REDEFINED",
                    format!("symbol `{}` is redefined", full_path),
                    span,
                ));
                return false;
            }

            self.diagnostics.push(Diagnostic::error(
                "E_SYMBOL_KIND_CONFLICT",
                format!(
                    "symbol `{}` conflicts with previously declared kind",
                    full_path
                ),
                span.join(existing.span),
            ));
            return false;
        }

        members.insert(name.to_string(), DeclInfo { kind, span });
        self.symbols.insert_symbol(SymbolInfo {
            kind,
            path: full_path,
        });
        true
    }
}

#[derive(Debug)]
pub(super) enum ResolvePathResult {
    Resolved(ValueType),
    NotFound,
    Ambiguous(Vec<String>),
}

#[derive(Debug)]
pub(super) enum ResolveOptionPathResult {
    Resolved(String, ValueType),
    NotFound,
    Ambiguous(Vec<String>),
}

#[derive(Debug)]
pub(super) enum ResolveEnumVariantPathResult {
    Resolved(String, String),
    NotFound,
    Ambiguous(Vec<String>),
}
