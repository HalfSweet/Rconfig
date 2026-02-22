use super::*;

mod collector;
mod references;
mod type_checker;
use self::collector::SymbolCollector;
use self::references::ReferenceCollector;
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

    let mut references = ReferenceCollector::new(&mut symbols);
    let mut reference_scope = Vec::new();
    references.collect_items(items, &mut reference_scope);

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
    let context = HashMap::new();
    analyze_values_with_options(values, symbols, AnalyzeValuesOptions::new(&context))
}

pub fn analyze_values_strict(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    let context = HashMap::new();
    analyze_values_with_options(
        values,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context: &context,
            include_root: None,
        },
    )
}

pub fn analyze_values_with_options(
    values: &ValuesFile,
    symbols: &SymbolTable,
    options: AnalyzeValuesOptions<'_>,
) -> Vec<Diagnostic> {
    let mut diagnostics = if let Some(include_root) = options.include_root {
        let (expanded_values, mut include_diags) =
            expand_values_includes_from_memory_with_root(values, include_root);
        let (mut semantic, _) =
            analyze_values_with_context_and_stmt_indexes(&expanded_values, symbols, options.context);
        include_diags.append(&mut semantic);
        include_diags
    } else {
        analyze_values_with_context_and_stmt_indexes(values, symbols, options.context).0
    };

    if options.strict {
        promote_strict_diagnostics(&mut diagnostics);
    }

    diagnostics
}

pub fn analyze_values_with_context(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> Vec<Diagnostic> {
    analyze_values_with_options(values, symbols, AnalyzeValuesOptions::new(context))
}

pub fn analyze_values_with_context_and_root(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    include_root: &FsPath,
) -> Vec<Diagnostic> {
    analyze_values_with_options(
        values,
        symbols,
        AnalyzeValuesOptions {
            strict: false,
            context,
            include_root: Some(include_root),
        },
    )
}

pub fn analyze_values_with_context_strict(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> Vec<Diagnostic> {
    analyze_values_with_options(
        values,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context,
            include_root: None,
        },
    )
}

pub fn analyze_values_with_context_and_root_strict(
    values: &ValuesFile,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    include_root: &FsPath,
) -> Vec<Diagnostic> {
    analyze_values_with_options(
        values,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context,
            include_root: Some(include_root),
        },
    )
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
    let context = HashMap::new();
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions::new(&context),
    )
}

pub fn analyze_values_from_path_report_with_options(
    entry: &FsPath,
    symbols: &SymbolTable,
    options: AnalyzeValuesOptions<'_>,
) -> ValuesAnalysisReport {
    let (values, stmt_origins, mut diagnostics) = if let Some(include_root) = options.include_root {
        expand_values_includes_with_origins_with_root(entry, include_root)
    } else {
        expand_values_includes_with_origins(entry)
    };
    let mut diagnostic_stmt_indexes = vec![None; diagnostics.len()];
    let (mut semantic, mut semantic_stmt_indexes) =
        analyze_values_with_context_and_stmt_indexes(&values, symbols, options.context);
    diagnostics.append(&mut semantic);
    diagnostic_stmt_indexes.append(&mut semantic_stmt_indexes);

    if options.strict {
        promote_strict_diagnostics(&mut diagnostics);
    }

    let resolved = resolve_values_with_context(&values, symbols, options.context);
    ValuesAnalysisReport {
        values,
        resolved,
        stmt_origins,
        diagnostics,
        diagnostic_stmt_indexes,
    }
}

pub fn analyze_values_from_path_report_with_context(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ValuesAnalysisReport {
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions::new(context),
    )
}

pub fn analyze_values_from_path_report_with_context_and_root(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    include_root: &FsPath,
) -> ValuesAnalysisReport {
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions {
            strict: false,
            context,
            include_root: Some(include_root),
        },
    )
}

pub fn analyze_values_from_path_report_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> ValuesAnalysisReport {
    let context = HashMap::new();
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context: &context,
            include_root: None,
        },
    )
}

pub fn analyze_values_from_path_report_with_context_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ValuesAnalysisReport {
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context,
            include_root: None,
        },
    )
}

pub fn analyze_values_from_path_report_with_context_and_root_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    include_root: &FsPath,
) -> ValuesAnalysisReport {
    analyze_values_from_path_report_with_options(
        entry,
        symbols,
        AnalyzeValuesOptions {
            strict: true,
            context,
            include_root: Some(include_root),
        },
    )
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

#[doc(hidden)]
pub fn expand_values_includes_from_path(entry: &FsPath) -> (ValuesFile, Vec<Diagnostic>) {
    let (values, _, diagnostics) = expand_values_includes_with_origins(entry);
    (values, diagnostics)
}

#[doc(hidden)]
pub fn expand_values_includes_from_path_with_root(
    entry: &FsPath,
    include_root: &FsPath,
) -> (ValuesFile, Vec<Diagnostic>) {
    let (values, _, diagnostics) =
        expand_values_includes_with_origins_with_root(entry, include_root);
    (values, diagnostics)
}

#[doc(hidden)]
pub fn expand_values_includes_with_origins(
    entry: &FsPath,
) -> (ValuesFile, Vec<ValuesStmtOrigin>, Vec<Diagnostic>) {
    expand_values_includes_with_origins_internal(entry, None)
}

#[doc(hidden)]
pub fn expand_values_includes_with_origins_with_root(
    entry: &FsPath,
    include_root: &FsPath,
) -> (ValuesFile, Vec<ValuesStmtOrigin>, Vec<Diagnostic>) {
    expand_values_includes_with_origins_internal(entry, Some(include_root))
}

fn canonical_include_root(root: &FsPath) -> PathBuf {
    fs::canonicalize(root).unwrap_or_else(|_| root.to_path_buf())
}

fn resolve_memory_include_target(root_dir: &FsPath, include_path: &str) -> PathBuf {
    if include_path == "@root" {
        return root_dir.to_path_buf();
    }

    if let Some(stripped) = include_path.strip_prefix("@root/") {
        return root_dir.join(stripped);
    }

    root_dir.join(include_path)
}

fn expand_values_includes_from_memory_with_root(
    values: &ValuesFile,
    include_root: &FsPath,
) -> (ValuesFile, Vec<Diagnostic>) {
    let root_dir = canonical_include_root(include_root);
    let mut expander = IncludeExpander::with_root(root_dir.clone());
    let mut expanded_stmts = Vec::new();

    for stmt in values.stmts.iter().cloned() {
        match stmt {
            ValuesStmt::Include(include) => {
                let target = resolve_memory_include_target(&root_dir, &include.path.value);
                expander.expand_file(&target, &mut expanded_stmts);
            }
            other => {
                expanded_stmts.push(ExpandedValuesStmt {
                    stmt: other,
                    origin: ValuesStmtOrigin {
                        source: "<memory>".to_string(),
                        include_chain: vec!["<memory>".to_string()],
                    },
                });
            }
        }
    }

    let values = ValuesFile {
        stmts: expanded_stmts
            .into_iter()
            .map(|expanded| expanded.stmt)
            .collect::<Vec<_>>(),
    };

    (values, expander.diagnostics)
}

fn expand_values_includes_with_origins_internal(
    entry: &FsPath,
    include_root: Option<&FsPath>,
) -> (ValuesFile, Vec<ValuesStmtOrigin>, Vec<Diagnostic>) {
    let root_dir = if let Some(root) = include_root {
        canonical_include_root(root)
    } else if let Some(parent) = entry.parent() {
        canonical_include_root(parent)
    } else {
        std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    };

    let mut expander = IncludeExpander::with_root(root_dir);
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
