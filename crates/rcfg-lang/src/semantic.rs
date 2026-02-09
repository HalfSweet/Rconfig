use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path as FsPath, PathBuf};

use crate::ast::{
    AttrKind, BinaryOp, ConstValue, ConstraintItem, Expr, File, InSetElem, IntRange, Item,
    MatchBlock, MatchPat, OptionDecl, Path, Type, UnaryOp, ValueExpr, ValuesFile, ValuesStmt,
};
use crate::error::{Diagnostic, DiagnosticArgValue, Severity};
use crate::parser::parse_values_with_diagnostics;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Mod,
    Enum,
    Option,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolInfo {
    pub kind: SymbolKind,
    pub path: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType {
    Bool,
    Int(IntType),
    UntypedInt,
    String,
    Enum(String),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
    U8,
    U16,
    U32,
    I32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedValue {
    Bool(bool),
    Int(i128),
    String(String),
    EnumVariant(String),
}

impl ResolvedValue {
    fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    fn as_int(&self) -> Option<i128> {
        if let Self::Int(value) = self {
            Some(*value)
        } else {
            None
        }
    }

    fn as_string(&self) -> Option<&str> {
        if let Self::String(value) = self {
            Some(value)
        } else {
            None
        }
    }

    fn as_enum_variant(&self) -> Option<&str> {
        if let Self::EnumVariant(value) = self {
            Some(value)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueSource {
    User,
    Default,
    Context,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedOption {
    pub path: String,
    pub active: bool,
    pub value: Option<ResolvedValue>,
    pub source: Option<ValueSource>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ResolvedConfig {
    pub options: Vec<ResolvedOption>,
}

#[derive(Debug, Clone)]
struct RuntimeState {
    active: HashSet<String>,
    values: HashMap<String, ResolvedValue>,
    sources: HashMap<String, ValueSource>,
    ctx_references: HashSet<String>,
}

impl RuntimeState {
    fn is_active(&self, path: &str) -> bool {
        self.active.contains(path)
    }

    fn value_of(&self, path: &str) -> Option<&ResolvedValue> {
        self.values.get(path)
    }
}

impl ValueType {
    fn is_bool(&self) -> bool {
        matches!(self, ValueType::Bool)
    }

    fn is_int(&self) -> bool {
        matches!(self, ValueType::Int(_) | ValueType::UntypedInt)
    }

    fn concrete_int(&self) -> Option<IntType> {
        if let ValueType::Int(ty) = self {
            Some(*ty)
        } else {
            None
        }
    }

    fn is_string(&self) -> bool {
        matches!(self, ValueType::String)
    }

    fn is_unknown(&self) -> bool {
        matches!(self, ValueType::Unknown)
    }

    fn same_as(&self, other: &ValueType) -> bool {
        match (self, other) {
            (ValueType::Bool, ValueType::Bool)
            | (ValueType::String, ValueType::String)
            | (ValueType::Unknown, ValueType::Unknown) => true,
            (ValueType::UntypedInt, ValueType::UntypedInt) => true,
            (ValueType::UntypedInt, ValueType::Int(_))
            | (ValueType::Int(_), ValueType::UntypedInt) => true,
            (ValueType::Int(left), ValueType::Int(right)) => left == right,
            (ValueType::Enum(left), ValueType::Enum(right)) => left == right,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
    option_types: HashMap<String, ValueType>,
    option_defaults: HashMap<String, ConstValue>,
    option_ranges: HashMap<String, IntRange>,
    option_spans: HashMap<String, Span>,
    option_secrets: HashMap<String, bool>,
    option_always_active: HashMap<String, bool>,
    enum_variants: HashMap<String, String>,
    enum_variant_spans: HashMap<String, Span>,
    schema_items: Vec<Item>,
}

impl SymbolTable {
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn get(&self, path: &str) -> Option<&SymbolInfo> {
        self.symbols.get(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &SymbolInfo)> {
        self.symbols.iter()
    }

    pub fn option_type(&self, path: &str) -> Option<&ValueType> {
        self.option_types.get(path)
    }

    pub fn schema_items(&self) -> &[Item] {
        &self.schema_items
    }

    fn insert_symbol(&mut self, symbol: SymbolInfo) {
        self.symbols.insert(symbol.path.clone(), symbol);
    }

    fn insert_option_type(&mut self, path: String, ty: ValueType) {
        self.option_types.insert(path, ty);
    }

    fn insert_option_default(&mut self, path: String, value: ConstValue) {
        self.option_defaults.insert(path, value);
    }

    fn insert_option_range(&mut self, path: String, range: IntRange) {
        self.option_ranges.insert(path, range);
    }

    fn insert_option_span(&mut self, path: String, span: Span) {
        self.option_spans.insert(path, span);
    }

    fn insert_option_secret(&mut self, path: String, secret: bool) {
        self.option_secrets.insert(path, secret);
    }

    fn insert_option_always_active(&mut self, path: String, is_always_active: bool) {
        self.option_always_active.insert(path, is_always_active);
    }

    fn set_schema_items(&mut self, items: Vec<Item>) {
        self.schema_items = items;
    }

    fn option_range(&self, path: &str) -> Option<&IntRange> {
        self.option_ranges.get(path)
    }

    fn option_span(&self, path: &str) -> Option<Span> {
        self.option_spans.get(path).copied()
    }

    fn option_is_secret(&self, path: &str) -> bool {
        self.option_secrets.get(path).copied().unwrap_or(false)
    }

    fn option_is_always_active(&self, path: &str) -> bool {
        self.option_always_active.get(path).copied().unwrap_or(true)
    }

    fn insert_enum_variant(
        &mut self,
        variant_path: String,
        enum_path: String,
        span: Span,
    ) -> Option<String> {
        self.enum_variant_spans.insert(variant_path.clone(), span);
        self.enum_variants.insert(variant_path, enum_path)
    }

    fn enum_variant_span(&self, variant_path: &str) -> Option<Span> {
        self.enum_variant_spans.get(variant_path).copied()
    }

    fn resolve_path_type(&self, scope: &[String], path: &Path) -> ResolvePathResult {
        let raw = path.to_string();
        let candidates = build_candidate_paths(scope, &raw);

        let mut matches = Vec::new();
        let mut seen = HashSet::new();

        for candidate in candidates {
            if let Some(ty) = self.option_types.get(&candidate) {
                if seen.insert(candidate.clone()) {
                    matches.push((candidate.clone(), ty.clone()));
                }
            }

            if let Some(enum_path) = self.enum_variants.get(&candidate) {
                if seen.insert(candidate.clone()) {
                    matches.push((candidate.clone(), ValueType::Enum(enum_path.clone())));
                }
            }
        }

        if matches.is_empty() {
            return ResolvePathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolvePathResult::Ambiguous(
                matches
                    .into_iter()
                    .map(|(path, _)| path)
                    .collect::<Vec<_>>(),
            );
        }

        let (_, ty) = matches.pop().expect("matches length was checked");
        ResolvePathResult::Resolved(ty)
    }

    fn enum_variant_names(&self, enum_name: &str) -> Option<Vec<String>> {
        let mut variants = self
            .enum_variants
            .iter()
            .filter(|(_, owner)| enum_name_matches(enum_name, owner))
            .filter_map(|(variant_path, _)| variant_path.rsplit("::").next().map(str::to_string))
            .collect::<Vec<_>>();

        if variants.is_empty() {
            return None;
        }

        variants.sort();
        variants.dedup();
        Some(variants)
    }

    fn enum_owner_of_variant(&self, variant_path: &str) -> Option<&str> {
        self.enum_variants.get(variant_path).map(String::as_str)
    }

    fn resolve_option_paths(&self, raw_path: &str) -> Vec<String> {
        self.option_types
            .keys()
            .filter(|candidate| path_matches(candidate, raw_path))
            .cloned()
            .collect::<Vec<_>>()
    }

    fn resolve_enum_variant_paths(&self, raw_path: &str) -> Vec<String> {
        self.enum_variants
            .keys()
            .filter(|candidate| path_matches(candidate, raw_path))
            .cloned()
            .collect::<Vec<_>>()
    }

    fn resolve_option_path_in_scope(&self, scope: &[String], path: &Path) -> ResolveOptionPathResult {
        let raw = path.to_string();
        let candidates = build_candidate_paths(scope, &raw);

        let mut matches = Vec::new();
        for candidate in candidates {
            if let Some(ty) = self.option_types.get(&candidate) {
                matches.push((candidate.clone(), ty.clone()));
            }
        }

        if matches.is_empty() {
            return ResolveOptionPathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolveOptionPathResult::Ambiguous(
                matches.into_iter().map(|(path, _)| path).collect::<Vec<_>>(),
            );
        }

        let (path, ty) = matches.pop().expect("matches length was checked");
        ResolveOptionPathResult::Resolved(path, ty)
    }

    fn resolve_enum_variant_path_in_scope(
        &self,
        scope: &[String],
        path: &Path,
    ) -> ResolveEnumVariantPathResult {
        let raw = path.to_string();
        let candidates = build_candidate_paths(scope, &raw);

        let mut matches = Vec::new();
        for candidate in candidates {
            if let Some(enum_path) = self.enum_variants.get(&candidate) {
                matches.push((candidate.clone(), enum_path.clone()));
            }
        }

        if matches.is_empty() {
            return ResolveEnumVariantPathResult::NotFound;
        }

        if matches.len() > 1 {
            return ResolveEnumVariantPathResult::Ambiguous(
                matches.into_iter().map(|(path, _)| path).collect::<Vec<_>>(),
            );
        }

        let (variant_path, enum_path) = matches.pop().expect("matches length was checked");
        ResolveEnumVariantPathResult::Resolved(variant_path, enum_path)
    }

    fn path_resolves_to_option_in_scope(&self, scope: &[String], path: &Path) -> bool {
        let raw = path.to_string();
        let candidates = build_candidate_paths(scope, &raw);
        candidates
            .iter()
            .any(|candidate| self.option_types.contains_key(candidate))
    }
}

#[derive(Debug, Clone)]
pub struct SemanticReport {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuesStmtOrigin {
    pub source: String,
    pub include_chain: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ValuesAnalysisReport {
    pub values: ValuesFile,
    pub resolved: ResolvedConfig,
    pub stmt_origins: Vec<ValuesStmtOrigin>,
    pub diagnostics: Vec<Diagnostic>,
    pub diagnostic_stmt_indexes: Vec<Option<usize>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlannedExport {
    pub path: String,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportOptions {
    pub include_secrets: bool,
    pub c_prefix: String,
    pub cmake_prefix: String,
}

impl Default for ExportOptions {
    fn default() -> Self {
        Self {
            include_secrets: false,
            c_prefix: "CONFIG_".to_string(),
            cmake_prefix: "CFG_".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedExports {
    pub c_header: String,
    pub cmake: String,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn plan_c_header_exports(
    symbols: &SymbolTable,
    include_secrets: bool,
) -> (Vec<PlannedExport>, Vec<Diagnostic>) {
    plan_c_header_exports_with_prefix(symbols, include_secrets, "CONFIG_")
}

pub fn plan_c_header_exports_with_prefix(
    symbols: &SymbolTable,
    include_secrets: bool,
    prefix: &str,
) -> (Vec<PlannedExport>, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let mut planned = Vec::new();
    let mut used_names: HashMap<String, String> = HashMap::new();

    let mut option_paths = symbols.option_types.keys().cloned().collect::<Vec<_>>();
    option_paths.sort();

    for option_path in option_paths {
        if symbols.option_is_secret(&option_path) && !include_secrets {
            diagnostics.push(
                Diagnostic::warning(
                    "W_SECRET_NOT_EXPORTED",
                    format!(
                        "secret option `{}` is not exported unless `--export-secrets` is enabled",
                        option_path
                    ),
                    symbols.option_span(&option_path).unwrap_or_default(),
                )
                .with_path(option_path),
            );
            continue;
        }

        let export_name = normalize_export_name_with_prefix(&option_path, prefix);
        if let Some(existing) = used_names.get(&export_name) {
            if existing != &option_path {
                diagnostics.push(
                    Diagnostic::error(
                        "E_EXPORT_NAME_COLLISION",
                        format!(
                            "export name `{}` collides between `{}` and `{}`",
                            export_name, existing, option_path
                        ),
                        symbols.option_span(&option_path).unwrap_or_default(),
                    )
                    .with_path(option_path),
                );
                continue;
            }
        }

        used_names.insert(export_name.clone(), option_path.clone());
        planned.push(PlannedExport {
            path: option_path,
            name: export_name,
        });
    }

    (planned, diagnostics)
}

pub fn generate_exports(
    symbols: &SymbolTable,
    resolved: &ResolvedConfig,
    options: &ExportOptions,
) -> GeneratedExports {
    let (planned, mut diagnostics) =
        plan_c_header_exports_with_prefix(symbols, options.include_secrets, &options.c_prefix);

    let resolved_map = resolved
        .options
        .iter()
        .map(|option| (option.path.clone(), option.clone()))
        .collect::<HashMap<_, _>>();

    let mut c_lines = Vec::new();
    let mut cmake_lines = Vec::new();

    for export in planned {
        let Some(option) = resolved_map.get(&export.path) else {
            continue;
        };
        if !option.active {
            continue;
        }
        if symbols.option_is_secret(&export.path) && !options.include_secrets {
            continue;
        }

        let cmake_name = normalize_export_name_with_prefix(&export.path, &options.cmake_prefix);
        match option.value.as_ref() {
            Some(ResolvedValue::Bool(value)) => {
                if *value {
                    c_lines.push(format!("#define {} 1", export.name));
                }
                cmake_lines.push(format!(
                    "set({} {})",
                    cmake_name,
                    if *value { "ON" } else { "OFF" }
                ));
            }
            Some(ResolvedValue::Int(value)) => {
                c_lines.push(format!("#define {} {}", export.name, value));
                cmake_lines.push(format!("set({} {})", cmake_name, value));
            }
            Some(ResolvedValue::String(value)) => {
                let escaped = value.replace('"', "\\\"");
                c_lines.push(format!("#define {} \"{}\"", export.name, escaped));
                cmake_lines.push(format!("set({} \"{}\")", cmake_name, escaped));
            }
            Some(ResolvedValue::EnumVariant(selected_variant)) => {
                cmake_lines.push(format!("set({} \"{}\")", cmake_name, selected_variant));

                let enum_owner = symbols.enum_owner_of_variant(selected_variant);
                let Some(enum_owner) = enum_owner else {
                    diagnostics.push(Diagnostic::error(
                        "E_VALUE_PATH_NOT_ENUM_VARIANT",
                        format!(
                            "resolved enum variant `{}` for `{}` cannot be resolved",
                            selected_variant, export.path
                        ),
                        symbols.option_span(&export.path).unwrap_or_default(),
                    ));
                    continue;
                };

                let mut variants = symbols
                    .enum_variants
                    .iter()
                    .filter(|(_, owner)| owner.as_str() == enum_owner)
                    .map(|(variant_path, _)| variant_path.clone())
                    .collect::<Vec<_>>();
                variants.sort();

                for variant in variants {
                    let variant_name = normalize_export_name_with_prefix(
                        &format!(
                            "{}::{}",
                            export.path,
                            variant.rsplit("::").next().unwrap_or(&variant)
                        ),
                        &options.c_prefix,
                    );
                    let is_selected = variant == *selected_variant;
                    c_lines.push(format!(
                        "#define {} {}",
                        variant_name,
                        if is_selected { 1 } else { 0 }
                    ));
                }
            }
            None => {}
        }
    }

    c_lines.sort();
    cmake_lines.sort();

    GeneratedExports {
        c_header: c_lines.join("\n"),
        cmake: cmake_lines.join("\n"),
        diagnostics,
    }
}

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

pub fn analyze_values_from_path(entry: &FsPath, symbols: &SymbolTable) -> (ValuesFile, Vec<Diagnostic>) {
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
    checker
        .resolved_config()
        .unwrap_or_else(|| build_resolved_config(symbols, &RuntimeState {
            active: HashSet::new(),
            values: HashMap::new(),
            sources: HashMap::new(),
            ctx_references: HashSet::new(),
        }))
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
    (ValuesFile { stmts: values_stmts }, origins, expander.diagnostics)
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
                    if self.declare(scope, &option.name.value, SymbolKind::Option, option.name.span) {
                        self.symbols
                            .insert_option_type(full_path.clone(), option_type_to_value_type(&option.ty));
                        self.symbols
                            .insert_option_span(full_path.clone(), option.name.span);
                        self.symbols.insert_option_secret(
                            full_path.clone(),
                            option_has_secret_attr(option),
                        );
                        self.symbols
                            .insert_option_always_active(full_path.clone(), !in_conditional);
                        if let Some(default) = option.default.clone() {
                            self.symbols.insert_option_default(full_path.clone(), default);
                        }
                        if let Some(range) = extract_range_attr(option) {
                            self.symbols.insert_option_range(full_path, range);
                        }
                    }
                }
                Item::Enum(enum_decl) => {
                    let enum_path = build_full_path(scope, &enum_decl.name.value);
                    if self.declare(scope, &enum_decl.name.value, SymbolKind::Enum, enum_decl.name.span) {
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
                            self.mod_metadata_spans.insert(module_path.clone(), module.span);
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
                format!("symbol `{}` conflicts with previously declared kind", full_path),
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
enum ResolvePathResult {
    Resolved(ValueType),
    NotFound,
    Ambiguous(Vec<String>),
}

#[derive(Debug)]
enum ResolveOptionPathResult {
    Resolved(String, ValueType),
    NotFound,
    Ambiguous(Vec<String>),
}

#[derive(Debug)]
enum ResolveEnumVariantPathResult {
    Resolved(String, String),
    NotFound,
    Ambiguous(Vec<String>),
}

#[derive(Debug, Clone)]
enum ConstScalar {
    Bool(bool),
    Int(i128),
    String(String),
}

struct TypeChecker<'a> {
    symbols: &'a SymbolTable,
    diagnostics: Vec<Diagnostic>,
    activation_edges: HashMap<String, HashSet<String>>,
    used_enum_variants: HashSet<String>,
}

#[derive(Default)]
struct IncludeExpander {
    diagnostics: Vec<Diagnostic>,
    stack: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
struct ExpandedValuesStmt {
    stmt: ValuesStmt,
    origin: ValuesStmtOrigin,
}

impl IncludeExpander {
    fn current_chain(&self, next: &PathBuf) -> Vec<String> {
        self.stack
            .iter()
            .chain(std::iter::once(next))
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
    }

    fn expand_file(&mut self, file: &FsPath, output: &mut Vec<ExpandedValuesStmt>) {
        let canonical = match fs::canonicalize(file) {
            Ok(path) => path,
            Err(_) => {
                self.diagnostics.push(Diagnostic::error(
                    "E_INCLUDE_NOT_FOUND",
                    format!("include file not found: {}", file.display()),
                    Span::default(),
                )
                .with_source(file.display().to_string())
                .with_include_chain(self.current_chain(&file.to_path_buf())));
                return;
            }
        };

        if let Some(index) = self.stack.iter().position(|path| path == &canonical) {
            let chain = self.stack[index..]
                .iter()
                .chain(std::iter::once(&canonical))
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(" -> ");
            self.diagnostics.push(Diagnostic::error(
                "E_INCLUDE_CYCLE",
                format!("include cycle detected: {}", chain),
                Span::default(),
            )
            .with_source(canonical.display().to_string())
            .with_include_chain(self.current_chain(&canonical)));
            return;
        }

        let text = match fs::read_to_string(&canonical) {
            Ok(content) => content,
            Err(_) => {
                self.diagnostics.push(Diagnostic::error(
                    "E_INCLUDE_NOT_FOUND",
                    format!("failed to read include file: {}", canonical.display()),
                    Span::default(),
                )
                .with_source(canonical.display().to_string())
                .with_include_chain(self.current_chain(&canonical)));
                return;
            }
        };

        let (values, mut parse_diags) = parse_values_with_diagnostics(&text);
        for diag in &mut parse_diags {
            if diag.source.is_none() {
                diag.source = Some(canonical.display().to_string());
            }
            if diag.include_chain.is_empty() {
                diag.include_chain = self.current_chain(&canonical);
            }
        }
        self.diagnostics.append(&mut parse_diags);

        self.stack.push(canonical.clone());
        let base_dir = canonical
            .parent()
            .map(FsPath::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));

        for stmt in values.stmts {
            match stmt {
                ValuesStmt::Include(include) => {
                    let target = base_dir.join(include.path.value);
                    self.expand_file(&target, output);
                }
                other => output.push(ExpandedValuesStmt {
                    stmt: other,
                    origin: ValuesStmtOrigin {
                        source: canonical.display().to_string(),
                        include_chain: self.current_chain(&canonical),
                    },
                }),
            }
        }

        self.stack.pop();
    }
}

struct ValuesChecker<'a> {
    symbols: &'a SymbolTable,
    diagnostics: Vec<Diagnostic>,
    diagnostic_stmt_indexes: Vec<Option<usize>>,
    aliases: HashMap<String, String>,
    assigned: HashMap<String, Span>,
    assignments: HashMap<String, ResolvedAssignment>,
    resolved: Option<ResolvedConfig>,
    current_stmt_index: Option<usize>,
}

#[derive(Debug, Clone)]
struct ResolvedAssignment {
    value: ResolvedValue,
    source: ValueSource,
    span: Span,
    stmt_index: Option<usize>,
}

#[derive(Debug, Clone)]
struct ParsedValue {
    actual_type: ValueType,
    resolved_value: Option<ResolvedValue>,
    int_value: Option<i128>,
}

impl<'a> ValuesChecker<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            diagnostics: Vec::new(),
            diagnostic_stmt_indexes: Vec::new(),
            aliases: HashMap::new(),
            assigned: HashMap::new(),
            assignments: HashMap::new(),
            resolved: None,
            current_stmt_index: None,
        }
    }

    fn ingest_context(&mut self, context: &HashMap<String, ResolvedValue>) {
        for (path, value) in context {
            self.assignments.insert(
                path.clone(),
                ResolvedAssignment {
                    value: value.clone(),
                    source: ValueSource::Context,
                    span: self.symbols.option_span(path).unwrap_or_default(),
                    stmt_index: None,
                },
            );
        }
    }

    fn resolved_config(&self) -> Option<ResolvedConfig> {
        self.resolved.clone()
    }

    fn check(&mut self, values: &ValuesFile) {
        for (index, stmt) in values.stmts.iter().enumerate() {
            self.current_stmt_index = Some(index);
            match stmt {
                ValuesStmt::Include(_) => {}
                ValuesStmt::Use(use_stmt) => {
                    self.register_use_alias(use_stmt);
                }
                ValuesStmt::Assign(assign) => {
                    let lhs = self.resolve_assignment_target(&assign.path);
                    if let Some(resolved) = &lhs {
                        self.record_duplicate_assignment(resolved, assign.path.span);
                    }
                    self.check_assignment_value(lhs.as_deref(), &assign.value);
                }
            }
        }
        self.current_stmt_index = None;
    }

    fn record_duplicate_assignment(&mut self, target: &str, span: Span) {
        if let Some(previous) = self.assigned.insert(target.to_string(), span) {
            self.push_diag(Diagnostic::warning(
                "W_DUPLICATE_ASSIGNMENT",
                format!("option `{}` is assigned multiple times; last wins", target),
                span.join(previous),
            ));
        }
    }

    fn register_use_alias(&mut self, use_stmt: &crate::ast::UseStmt) {
        let Some(alias) = &use_stmt.alias else {
            return;
        };

        if self.aliases.contains_key(&alias.value) {
            self.push_diag(Diagnostic::error(
                "E_USE_ALIAS_CONFLICT",
                format!("use alias `{}` is already defined", alias.value),
                alias.span,
            ));
            return;
        }

        self.aliases
            .insert(alias.value.clone(), use_stmt.path.to_string());
    }

    fn resolve_assignment_target(&mut self, path: &Path) -> Option<String> {
        let expanded = expand_with_aliases(path, &self.aliases);
        let candidates = self.symbols.resolve_option_paths(&expanded);

        if candidates.is_empty() {
            self.push_diag(Diagnostic::error(
                "E_SYMBOL_NOT_FOUND",
                format!("assignment target `{}` cannot be resolved", path.to_string()),
                path.span,
            ));
            return None;
        }

        if candidates.len() > 1 {
            self.push_diag(Diagnostic::error(
                "E_AMBIGUOUS_PATH",
                format!(
                    "assignment target `{}` is ambiguous: {}",
                    path.to_string(),
                    candidates.join(", ")
                ),
                path.span,
            ));
            return None;
        }

        let resolved = candidates[0].clone();
        if resolved == "ctx" || resolved.starts_with("ctx::") {
            self.push_diag(Diagnostic::error(
                "E_CONTEXT_ASSIGNMENT_NOT_ALLOWED",
                "assigning values to `ctx` is not allowed",
                path.span,
            ));
            return None;
        }

        Some(resolved)
    }

    fn check_assignment_value(&mut self, target: Option<&str>, value: &ValueExpr) {
        let Some(target) = target else {
            return;
        };

        let Some(expected) = self.symbols.option_type(target).cloned() else {
            self.push_diag(Diagnostic::error(
                "E_SYMBOL_NOT_FOUND",
                format!("assignment target `{}` is not an option", target),
                value.span(),
            )
            .with_path(target));
            return;
        };

        let parsed = self.parse_value_expr(value, &expected);
        let actual = parsed.actual_type;
        if !actual.is_unknown() && !expected.same_as(&actual) {
            self.push_diag(Diagnostic::error(
                "E_TYPE_MISMATCH",
                format!(
                    "value type mismatch for `{}`: expected {:?}, got {:?}",
                    target, expected, actual
                ),
                value.span(),
            )
            .with_path(target));
            return;
        }

        if expected.is_int() {
            if let Some(actual) = parsed.int_value {
                if let Some((min, max)) = int_value_type_bounds(&expected) {
                    if actual < min || actual > max {
                        self.push_diag(
                            Diagnostic::error(
                                "E_TYPE_MISMATCH",
                                format!(
                                    "value `{}` for `{}` is out of type bounds [{}..={}]",
                                    actual, target, min, max
                                ),
                                value.span(),
                            )
                            .with_path(target),
                        );
                        return;
                    }
                }
            }

            if let (Some(range), Some(actual)) = (self.symbols.option_range(target), parsed.int_value) {
                let violates = if range.inclusive {
                    actual < range.start || actual > range.end
                } else {
                    actual < range.start || actual >= range.end
                };

                if violates {
                    let max = if range.inclusive {
                        range.end.to_string()
                    } else {
                        format!("{} (exclusive)", range.end)
                    };
                    let is_secret = self.symbols.option_is_secret(target);
                    self.push_diag(
                        Diagnostic::error(
                            "E_RANGE_VIOLATION",
                            format!(
                                "value `{}` for `{}` is outside range [{}..={}]",
                                actual, target, range.start, max
                            ),
                            value.span(),
                        )
                        .with_path(target)
                        .with_arg("actual", redacted_int_arg(actual, is_secret))
                        .with_arg("min", DiagnosticArgValue::Int(range.start))
                        .with_arg("max", DiagnosticArgValue::Int(range.end))
                        .with_arg("inclusive", DiagnosticArgValue::Bool(range.inclusive)),
                    );
                    return;
                }
            }
        }

        if let Some(resolved_value) = parsed.resolved_value {
            self.assignments.insert(
                target.to_string(),
                ResolvedAssignment {
                    value: resolved_value,
                    source: ValueSource::User,
                    span: value.span(),
                    stmt_index: self.current_stmt_index,
                },
            );
        }
    }

    fn parse_value_expr(&mut self, value: &ValueExpr, expected: &ValueType) -> ParsedValue {
        match value {
            ValueExpr::Bool(raw, _) => ParsedValue {
                actual_type: ValueType::Bool,
                resolved_value: Some(ResolvedValue::Bool(*raw)),
                int_value: None,
            },
            ValueExpr::Int(raw, _) => ParsedValue {
                actual_type: ValueType::UntypedInt,
                resolved_value: Some(ResolvedValue::Int(*raw)),
                int_value: Some(*raw),
            },
            ValueExpr::String(raw, _) => ParsedValue {
                actual_type: ValueType::String,
                resolved_value: Some(ResolvedValue::String(raw.clone())),
                int_value: None,
            },
            ValueExpr::Path(path) => {
                let actual_type = self.value_expr_type(value, expected);
                let resolved_value = {
                    let expanded = expand_with_aliases(path, &self.aliases);
                    let mut matches = self.symbols.resolve_enum_variant_paths(&expanded);
                    if matches.len() == 1 {
                        Some(ResolvedValue::EnumVariant(matches.remove(0)))
                    } else {
                        None
                    }
                };
                ParsedValue {
                    actual_type,
                    int_value: resolved_value.as_ref().and_then(ResolvedValue::as_int),
                    resolved_value,
                }
            }
            ValueExpr::Env { name, .. } => self.parse_env_value(name.value.as_str(), expected, value.span()),
        }
    }

    fn parse_env_value(&mut self, name: &str, expected: &ValueType, span: Span) -> ParsedValue {
        let raw = match std::env::var(name) {
            Ok(value) => value,
            Err(_) => {
                self.push_diag(Diagnostic::error(
                    "E_ENV_NOT_SET",
                    format!("environment variable `{}` is not set", name),
                    span,
                ));
                return ParsedValue {
                    actual_type: ValueType::Unknown,
                    resolved_value: None,
                    int_value: None,
                };
            }
        };

        let (actual_type, resolved_value) = match expected {
            ValueType::Bool => {
                if raw == "true" {
                    (ValueType::Bool, Some(ResolvedValue::Bool(true)))
                } else if raw == "false" {
                    (ValueType::Bool, Some(ResolvedValue::Bool(false)))
                } else {
                    self.push_env_parse_failed(name, &raw, "bool", span);
                    (ValueType::Unknown, None)
                }
            }
            ValueType::Int(int_ty) => match parse_env_int(&raw) {
                Some(value) => {
                    let (min, max) = int_bounds_for_int_type(*int_ty);
                    if value < min || value > max {
                        self.push_env_parse_failed(
                            name,
                            &raw,
                            &format!("{} in range [{}..={}]", int_type_name(*int_ty), min, max),
                            span,
                        );
                        (ValueType::Unknown, None)
                    } else {
                        (ValueType::Int(*int_ty), Some(ResolvedValue::Int(value)))
                    }
                }
                None => {
                    self.push_env_parse_failed(name, &raw, "int", span);
                    (ValueType::Unknown, None)
                }
            },
            ValueType::UntypedInt => match parse_env_int(&raw) {
                Some(value) => (ValueType::UntypedInt, Some(ResolvedValue::Int(value))),
                None => {
                    self.push_env_parse_failed(name, &raw, "int", span);
                    (ValueType::Unknown, None)
                }
            },
            ValueType::String => (ValueType::String, Some(ResolvedValue::String(raw))),
            ValueType::Enum(expected_enum) => {
                let enum_variant = self.resolve_env_enum_variant(name, &raw, expected_enum, span);
                if let Some(variant) = enum_variant {
                    (ValueType::Enum(expected_enum.clone()), Some(ResolvedValue::EnumVariant(variant)))
                } else {
                    (ValueType::Unknown, None)
                }
            }
            ValueType::Unknown => (ValueType::Unknown, None),
        };

        let int_value = resolved_value.as_ref().and_then(ResolvedValue::as_int);
        ParsedValue {
            actual_type,
            resolved_value,
            int_value,
        }
    }

    fn resolve_env_enum_variant(
        &mut self,
        env_name: &str,
        raw: &str,
        expected_enum: &str,
        span: Span,
    ) -> Option<String> {
        let mut candidates = self
            .symbols
            .resolve_enum_variant_paths(raw)
            .into_iter()
            .filter(|candidate| {
                self.symbols
                    .enum_owner_of_variant(candidate)
                    .is_some_and(|owner| enum_name_matches(expected_enum, owner))
            })
            .collect::<Vec<_>>();
        candidates.sort();
        candidates.dedup();

        if candidates.is_empty() {
            self.push_env_parse_failed(env_name, raw, &format!("enum {}", expected_enum), span);
            return None;
        }

        if candidates.len() > 1 {
            self.push_diag(Diagnostic::error(
                "E_AMBIGUOUS_ENUM_VARIANT",
                format!(
                    "env value `{}` is ambiguous for enum `{}`: {}",
                    raw,
                    expected_enum,
                    candidates.join(", ")
                ),
                span,
            ));
            return None;
        }

        candidates.into_iter().next()
    }

    fn value_expr_type(&mut self, value: &ValueExpr, expected: &ValueType) -> ValueType {
        match value {
            ValueExpr::Bool(_, _) => ValueType::Bool,
            ValueExpr::Int(_, _) => ValueType::UntypedInt,
            ValueExpr::String(_, _) => ValueType::String,
            ValueExpr::Env { name, .. } => self.env_value_type(name.value.as_str(), expected, value.span()),
            ValueExpr::Path(path) => {
                let expanded = expand_with_aliases(path, &self.aliases);
                let option_matches = self.symbols.resolve_option_paths(&expanded);
                if !option_matches.is_empty() {
                    let resolved = option_matches[0].clone();
                    self.push_diag(Diagnostic::error(
                        "E_VALUE_PATH_RESOLVES_TO_OPTION",
                        format!("value path `{}` resolves to option", path.to_string()),
                        path.span,
                    )
                    .with_path(resolved));
                    return ValueType::Unknown;
                }

                let mut matches = self.symbols.resolve_enum_variant_paths(&expanded);
                if matches.is_empty() {
                    self.push_diag(Diagnostic::error(
                        "E_VALUE_PATH_NOT_ENUM_VARIANT",
                        format!("value path `{}` is not an enum variant", path.to_string()),
                        path.span,
                    ));
                    return ValueType::Unknown;
                }

                if matches.len() > 1 {
                    self.push_diag(Diagnostic::error(
                        "E_AMBIGUOUS_ENUM_VARIANT",
                        format!(
                            "enum variant `{}` is ambiguous: {}",
                            path.to_string(),
                            matches.join(", ")
                        ),
                        path.span,
                    ));
                    return ValueType::Unknown;
                }

                let resolved = matches.remove(0);
                let enum_name = self
                    .symbols
                    .enum_owner_of_variant(&resolved)
                    .map(|owner| owner.rsplit("::").next().unwrap_or(owner).to_string())
                    .unwrap_or_default();
                ValueType::Enum(enum_name)
            }
        }
    }

    fn env_value_type(&mut self, name: &str, expected: &ValueType, span: Span) -> ValueType {
        self.parse_env_value(name, expected, span).actual_type
    }

    fn push_env_parse_failed(&mut self, name: &str, value: &str, expected: &str, span: Span) {
        self.push_diag(Diagnostic::error(
            "E_ENV_PARSE_FAILED",
            format!(
                "failed to parse env `{}` value `{}` as {}",
                name, value, expected
            ),
            span,
        ));
    }

    fn post_check_diagnostics(&mut self) -> Vec<(Diagnostic, Option<usize>)> {
        let runtime = self.runtime_state();
        self.resolved = Some(build_resolved_config(self.symbols, &runtime));

        let mut diagnostics = Vec::new();
        diagnostics.extend(self.runtime_inactive_assignment_diagnostics(&runtime));
        diagnostics.extend(self.missing_value_diagnostics(&runtime));
        diagnostics.extend(self.missing_context_diagnostics(&runtime));
        diagnostics.extend(self.runtime_require_diagnostics(&runtime));
        diagnostics
    }

    fn runtime_state(&self) -> RuntimeState {
        evaluate_runtime_state(self.symbols, &self.assignments)
    }

    fn runtime_inactive_assignment_diagnostics(
        &self,
        runtime: &RuntimeState,
    ) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        for (path, assignment) in &self.assignments {
            if assignment.source != ValueSource::User {
                continue;
            }
            if runtime.is_active(path) {
                continue;
            }

            diagnostics.push((
                Diagnostic::warning(
                    "W_INACTIVE_ASSIGNMENT",
                    format!(
                        "assignment to inactive option `{}` is ignored by final activation",
                        path
                    ),
                    assignment.span,
                )
                .with_path(path.clone()),
                assignment.stmt_index,
            ));
        }
        diagnostics
    }

    fn missing_value_diagnostics(&self, runtime: &RuntimeState) -> Vec<(Diagnostic, Option<usize>)> {
        let mut option_paths = self
            .symbols
            .option_types
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        option_paths.sort();

        let mut diagnostics = Vec::new();
        for option_path in option_paths {
            if option_path == "ctx" || option_path.starts_with("ctx::") {
                continue;
            }
            if !runtime.is_active(&option_path) {
                continue;
            }
            if runtime.value_of(&option_path).is_some() {
                continue;
            }

            diagnostics.push((
                Diagnostic::error(
                    "E_MISSING_VALUE",
                    format!(
                        "active option `{}` has no value from assignments or defaults",
                        option_path
                    ),
                    self.symbols.option_span(&option_path).unwrap_or_default(),
                )
                .with_path(option_path),
                None,
            ));
        }

        diagnostics
    }

    fn missing_context_diagnostics(&self, runtime: &RuntimeState) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        for ctx_path in &runtime.ctx_references {
            if runtime.value_of(ctx_path).is_some() {
                continue;
            }

            diagnostics.push((
                Diagnostic::error(
                    "E_MISSING_CONTEXT_VALUE",
                    format!(
                        "context option `{}` requires an injected context value",
                        ctx_path
                    ),
                    self.symbols.option_span(ctx_path).unwrap_or_default(),
                )
                .with_path(ctx_path.clone()),
                None,
            ));
        }
        diagnostics
    }

    fn runtime_require_diagnostics(&self, runtime: &RuntimeState) -> Vec<(Diagnostic, Option<usize>)> {
        let mut diagnostics = Vec::new();
        let mut scope = Vec::new();
        collect_runtime_require_diagnostics(
            self.symbols,
            self.symbols.schema_items(),
            &mut scope,
            runtime,
            &mut diagnostics,
        );
        diagnostics
    }

    fn push_diag(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
        self.diagnostic_stmt_indexes.push(self.current_stmt_index);
    }
}

impl<'a> TypeChecker<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            diagnostics: Vec::new(),
            activation_edges: HashMap::new(),
            used_enum_variants: HashSet::new(),
        }
    }

    fn check_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        let activation_deps = Vec::new();
        self.check_items_with_activation(items, scope, &activation_deps);
        self.check_activation_cycles();
        self.check_unused_enum_variants();
    }

    fn check_items_with_activation(
        &mut self,
        items: &[Item],
        scope: &mut Vec<String>,
        activation_deps: &[String],
    ) {
        for item in items {
            match item {
                Item::Use(_) | Item::Enum(_) => {}
                Item::Mod(module) => {
                    scope.push(module.name.value.clone());
                    self.check_items_with_activation(&module.items, scope, activation_deps);
                    scope.pop();
                }
                Item::Option(option) => {
                    self.check_option(option, scope);
                    let option_path = build_full_path(scope, &option.name.value);
                    self.record_activation_dependencies(&option_path, activation_deps);
                }
                Item::Require(require) => {
                    self.check_require_stmt(require, scope, None);
                }
                Item::Constraint(constraint) => {
                    for item in &constraint.items {
                        if let ConstraintItem::Require(require) = item {
                            self.check_require_stmt(require, scope, None);
                        }
                    }
                }
                Item::When(when_block) => {
                    self.expect_bool_expr(&when_block.condition, scope, None);
                    self.check_inactive_value_references(&when_block.condition, scope);

                    let condition_deps =
                        self.collect_condition_option_dependencies(&when_block.condition, scope);
                    let next_deps = merge_dependency_paths(activation_deps, &condition_deps);
                    self.check_items_with_activation(&when_block.items, scope, &next_deps);
                }
                Item::Match(match_block) => {
                    self.check_match_block(match_block, scope);

                    let match_expr_deps =
                        self.collect_condition_option_dependencies(&match_block.expr, scope);
                    let match_deps = merge_dependency_paths(activation_deps, &match_expr_deps);

                    for case in &match_block.cases {
                        let mut case_deps = match_deps.clone();
                        if let Some(guard) = &case.guard {
                            self.expect_bool_expr(guard, scope, None);
                            self.check_inactive_value_references(guard, scope);

                            let guard_deps =
                                self.collect_condition_option_dependencies(guard, scope);
                            case_deps = merge_dependency_paths(&case_deps, &guard_deps);
                        }
                        self.check_items_with_activation(&case.items, scope, &case_deps);
                    }
                }
            }
        }
    }

    fn record_activation_dependencies(&mut self, option_path: &str, activation_deps: &[String]) {
        if activation_deps.is_empty() {
            return;
        }

        let entry = self
            .activation_edges
            .entry(option_path.to_string())
            .or_default();
        for dep in activation_deps {
            entry.insert(dep.clone());
        }
    }


    fn check_option(&mut self, option: &OptionDecl, scope: &[String]) {
        self.lint_option_doc(option, scope);
        self.check_option_default(option, scope);

        if let Some(attached) = &option.attached_constraints {
            let self_ty = option_type_to_value_type(&option.ty);
            for require in &attached.requires {
                self.check_require_stmt(require, scope, Some(&self_ty));
            }
        }
    }

    fn check_option_default(&mut self, option: &OptionDecl, scope: &[String]) {
        let Some(default) = &option.default else {
            return;
        };

        self.mark_default_enum_variant_usage(default, scope);

        let option_path = build_full_path(scope, &option.name.value);
        if option_path == "ctx" || option_path.starts_with("ctx::") {
            self.diagnostics.push(
                Diagnostic::error(
                    "E_CONTEXT_DEFAULT_NOT_ALLOWED",
                    "`ctx` option cannot define schema default value",
                    default.span(),
                )
                .with_path(option_path),
            );
            return;
        }

        if let ConstValue::EnumPath(path) = default {
            if self.symbols.path_resolves_to_option_in_scope(scope, path) {
                self.diagnostics.push(
                    Diagnostic::error(
                        "E_DEFAULT_NOT_CONSTANT",
                        format!(
                            "default value for option `{}` must be a constant enum variant or literal",
                            option_path
                        ),
                        path.span,
                    )
                    .with_path(option_path),
                );
                return;
            }
        }

        let expected = option_type_to_value_type(&option.ty);
        let actual = self.infer_const_type(default, scope);
        let type_matches = match (&expected, &actual) {
            (ValueType::Enum(expected_name), ValueType::Enum(actual_name)) => {
                enum_name_matches(expected_name, actual_name)
            }
            _ => expected.same_as(&actual),
        };

        if !type_matches {
            self.diagnostics.push(
                Diagnostic::error(
                    "E_DEFAULT_TYPE_MISMATCH",
                    format!("default value type mismatch for option `{}`", option_path),
                    default.span(),
                )
                .with_path(option_path),
            );
            return;
        }

        if let ConstValue::Int(value, span) = default {
            if let Some((min, max)) = int_type_bounds(&option.ty) {
                if *value < min || *value > max {
                    let is_secret = self.symbols.option_is_secret(&option_path);
                    self.diagnostics.push(
                        Diagnostic::error(
                            "E_DEFAULT_OUT_OF_RANGE",
                            format!(
                                "default value `{}` for option `{}` is out of range [{}..={}]",
                                value, option_path, min, max
                            ),
                            *span,
                        )
                        .with_path(option_path)
                        .with_arg("actual", redacted_int_arg(*value, is_secret))
                        .with_arg("min", DiagnosticArgValue::Int(min))
                        .with_arg("max", DiagnosticArgValue::Int(max)),
                    );
                }
            }
        }
    }

    fn infer_const_type(&self, value: &ConstValue, scope: &[String]) -> ValueType {
        match value {
            ConstValue::Bool(_, _) => ValueType::Bool,
            ConstValue::Int(_, _) => ValueType::UntypedInt,
            ConstValue::String(_, _) => ValueType::String,
            ConstValue::EnumPath(path) => match self.symbols.resolve_path_type(scope, path) {
                ResolvePathResult::Resolved(ty) => ty,
                ResolvePathResult::NotFound | ResolvePathResult::Ambiguous(_) => ValueType::Unknown,
            },
        }
    }

    fn check_require_stmt(
        &mut self,
        require: &crate::ast::RequireStmt,
        scope: &[String],
        self_ty: Option<&ValueType>,
    ) {
        self.lint_require_msg(require);
        self.expect_bool_expr(&require.expr, scope, self_ty);

        if self.evaluate_const_bool_expr(&require.expr).is_some_and(|result| !result) {
            self.diagnostics.push(Diagnostic::error(
                "E_REQUIRE_FAILED",
                "require condition is statically false",
                require.span,
            ));
        }
    }

    fn evaluate_const_bool_expr(&self, expr: &Expr) -> Option<bool> {
        match expr {
            Expr::Bool(value, _) => Some(*value),
            Expr::Unary {
                op: UnaryOp::Not,
                expr,
                ..
            } => self.evaluate_const_bool_expr(expr).map(|value| !value),
            Expr::Binary {
                op,
                left,
                right,
                ..
            } => {
                let left = self.evaluate_const_scalar(left)?;
                let right = self.evaluate_const_scalar(right)?;
                match op {
                    BinaryOp::Or => match (left, right) {
                        (ConstScalar::Bool(l), ConstScalar::Bool(r)) => Some(l || r),
                        _ => None,
                    },
                    BinaryOp::And => match (left, right) {
                        (ConstScalar::Bool(l), ConstScalar::Bool(r)) => Some(l && r),
                        _ => None,
                    },
                    BinaryOp::Eq => Some(match (left, right) {
                        (ConstScalar::Bool(l), ConstScalar::Bool(r)) => l == r,
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l == r,
                        (ConstScalar::String(l), ConstScalar::String(r)) => l == r,
                        _ => return None,
                    }),
                    BinaryOp::Ne => Some(match (left, right) {
                        (ConstScalar::Bool(l), ConstScalar::Bool(r)) => l != r,
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l != r,
                        (ConstScalar::String(l), ConstScalar::String(r)) => l != r,
                        _ => return None,
                    }),
                    BinaryOp::Lt => Some(match (left, right) {
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l < r,
                        _ => return None,
                    }),
                    BinaryOp::Le => Some(match (left, right) {
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l <= r,
                        _ => return None,
                    }),
                    BinaryOp::Gt => Some(match (left, right) {
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l > r,
                        _ => return None,
                    }),
                    BinaryOp::Ge => Some(match (left, right) {
                        (ConstScalar::Int(l), ConstScalar::Int(r)) => l >= r,
                        _ => return None,
                    }),
                }
            }
            Expr::Group { expr, .. } => self.evaluate_const_bool_expr(expr),
            _ => None,
        }
    }

    fn evaluate_const_scalar(&self, expr: &Expr) -> Option<ConstScalar> {
        match expr {
            Expr::Bool(value, _) => Some(ConstScalar::Bool(*value)),
            Expr::Int(value, _) => Some(ConstScalar::Int(*value)),
            Expr::String(value, _) => Some(ConstScalar::String(value.clone())),
            Expr::Group { expr, .. } => self.evaluate_const_scalar(expr),
            Expr::Unary {
                op: UnaryOp::Not,
                expr,
                ..
            } => self
                .evaluate_const_bool_expr(expr)
                .map(|value| ConstScalar::Bool(!value)),
            Expr::Binary { .. } => None,
            _ => None,
        }
    }

    fn mark_default_enum_variant_usage(&mut self, default: &ConstValue, scope: &[String]) {
        let ConstValue::EnumPath(path) = default else {
            return;
        };

        match self.symbols.resolve_enum_variant_path_in_scope(scope, path) {
            ResolveEnumVariantPathResult::Resolved(variant_path, _) => {
                self.used_enum_variants.insert(variant_path);
            }
            ResolveEnumVariantPathResult::NotFound => {}
            ResolveEnumVariantPathResult::Ambiguous(candidates) => {
                let _ = candidates;
            }
        }
    }

    fn check_unused_enum_variants(&mut self) {
        let mut variants = self
            .symbols
            .enum_variants
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        variants.sort();

        for variant_path in variants {
            if self.used_enum_variants.contains(&variant_path) {
                continue;
            }

            self.diagnostics.push(Diagnostic::warning(
                "L_UNUSED_ENUM_VARIANT",
                format!("enum variant `{}` is never referenced", variant_path),
                self.symbols
                    .enum_variant_span(&variant_path)
                    .unwrap_or_default(),
            ));
        }
    }

    fn lint_option_doc(&mut self, option: &OptionDecl, scope: &[String]) {
        if !option.meta.doc.is_empty() {
            return;
        }

        let option_path = build_full_path(scope, &option.name.value);
        self.diagnostics.push(
            Diagnostic::warning(
                "L_MISSING_DOC",
                format!("option `{}` is missing documentation comment", option_path),
                option.name.span,
            )
            .with_path(option_path),
        );
    }

    fn lint_require_msg(&mut self, require: &crate::ast::RequireStmt) {
        let has_msg = require
            .meta
            .attrs
            .iter()
            .any(|attr| matches!(attr.kind, AttrKind::Msg(_)));
        if !has_msg {
            self.diagnostics.push(Diagnostic::warning(
                "L_REQUIRE_MISSING_MSG",
                "require! is missing #[msg(\"...\")] for stable i18n key",
                require.span,
            ));
        }
    }

    fn collect_condition_option_dependencies(&self, expr: &Expr, scope: &[String]) -> Vec<String> {
        let mut deps = Vec::new();
        self.collect_condition_option_dependencies_into(expr, scope, &mut deps);
        deps.sort();
        deps.dedup();
        deps
    }

    fn collect_condition_option_dependencies_into(
        &self,
        expr: &Expr,
        scope: &[String],
        out: &mut Vec<String>,
    ) {
        match expr {
            Expr::Path(path) => {
                if let ResolveOptionPathResult::Resolved(option_path, _) =
                    self.symbols.resolve_option_path_in_scope(scope, path)
                {
                    out.push(option_path);
                }
            }
            Expr::Unary { expr, .. } => {
                self.collect_condition_option_dependencies_into(expr, scope, out)
            }
            Expr::Binary { left, right, .. } => {
                self.collect_condition_option_dependencies_into(left, scope, out);
                self.collect_condition_option_dependencies_into(right, scope, out);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.collect_condition_option_dependencies_into(arg, scope, out);
                }
            }
            Expr::InRange { expr, .. } => {
                self.collect_condition_option_dependencies_into(expr, scope, out)
            }
            Expr::InSet { expr, elems, .. } => {
                self.collect_condition_option_dependencies_into(expr, scope, out);
                for elem in elems {
                    if let InSetElem::Path(path) = elem {
                        if let ResolveOptionPathResult::Resolved(option_path, _) =
                            self.symbols.resolve_option_path_in_scope(scope, path)
                        {
                            out.push(option_path);
                        }
                    }
                }
            }
            Expr::Group { expr, .. } => {
                self.collect_condition_option_dependencies_into(expr, scope, out)
            }
            Expr::Bool(_, _) | Expr::Int(_, _) | Expr::String(_, _) | Expr::SelfValue(_) => {}
        }
    }

    fn check_activation_cycles(&mut self) {
        let mut nodes = self
            .activation_edges
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for deps in self.activation_edges.values() {
            nodes.extend(deps.iter().cloned());
        }
        nodes.sort();
        nodes.dedup();

        let mut visiting = HashSet::new();
        let mut visited = HashSet::new();
        let mut stack = Vec::new();
        let mut reported = HashSet::new();

        for node in nodes {
            self.check_activation_cycle_from(
                &node,
                &mut visiting,
                &mut visited,
                &mut stack,
                &mut reported,
            );
        }
    }

    fn check_activation_cycle_from(
        &mut self,
        node: &str,
        visiting: &mut HashSet<String>,
        visited: &mut HashSet<String>,
        stack: &mut Vec<String>,
        reported: &mut HashSet<String>,
    ) {
        if visiting.contains(node) {
            if let Some(pos) = stack.iter().position(|item| item == node) {
                let mut cycle = stack[pos..].to_vec();
                cycle.push(node.to_string());
                let signature = canonical_cycle_signature(&cycle);
                if reported.insert(signature) {
                    let chain = cycle.join(" -> ");
                    for cycle_node in cycle.iter().take(cycle.len().saturating_sub(1)) {
                        self.diagnostics.push(
                            Diagnostic::error(
                                "E_CIRCULAR_ACTIVATION",
                                format!("activation dependency cycle detected: {}", chain),
                                self.symbols.option_span(cycle_node).unwrap_or_default(),
                            )
                            .with_path(cycle_node.clone()),
                        );
                    }
                }
            }
            return;
        }

        if visited.contains(node) {
            return;
        }

        visiting.insert(node.to_string());
        stack.push(node.to_string());

        let deps = self
            .activation_edges
            .get(node)
            .cloned()
            .unwrap_or_default();
        for dep in deps {
            self.check_activation_cycle_from(&dep, visiting, visited, stack, reported);
        }

        stack.pop();
        visiting.remove(node);
        visited.insert(node.to_string());
    }

    fn check_inactive_value_references(&mut self, expr: &Expr, scope: &[String]) {
        match expr {
            Expr::Path(path) => match self.symbols.resolve_option_path_in_scope(scope, path) {
                ResolveOptionPathResult::Resolved(option_path, ty) => {
                    if !self.symbols.option_is_always_active(&option_path) && !ty.is_bool() {
                        self.diagnostics.push(
                            Diagnostic::error(
                                "E_INACTIVE_VALUE_REFERENCE",
                                format!(
                                    "activation condition cannot read inactive non-bool option `{}`",
                                    option_path
                                ),
                                path.span,
                            )
                            .with_path(option_path),
                        );
                    }
                }
                ResolveOptionPathResult::NotFound => {}
                ResolveOptionPathResult::Ambiguous(candidates) => {
                    let _ = candidates;
                }
            },
            Expr::Unary { expr, .. } => self.check_inactive_value_references(expr, scope),
            Expr::Binary { left, right, .. } => {
                self.check_inactive_value_references(left, scope);
                self.check_inactive_value_references(right, scope);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.check_inactive_value_references(arg, scope);
                }
            }
            Expr::InRange { expr, .. } => self.check_inactive_value_references(expr, scope),
            Expr::InSet { expr, elems, .. } => {
                self.check_inactive_value_references(expr, scope);
                for elem in elems {
                    if let InSetElem::Path(path) = elem {
                        self.check_inactive_value_references(&Expr::Path(path.clone()), scope);
                    }
                }
            }
            Expr::Group { expr, .. } => self.check_inactive_value_references(expr, scope),
            Expr::Bool(_, _) | Expr::Int(_, _) | Expr::String(_, _) | Expr::SelfValue(_) => {}
        }
    }

    fn check_match_block(&mut self, block: &MatchBlock, scope: &[String]) {
        let scrutinee_ty = self.infer_expr_type(&block.expr, scope, None);
        let ValueType::Enum(enum_name) = scrutinee_ty else {
            self.diagnostics.push(Diagnostic::error(
                "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE",
                "match scrutinee must resolve to always-active enum option",
                block.expr.span(),
            ));
            return;
        };

        let Some(scrutinee_option_path) = self.match_scrutinee_option_path(&block.expr, scope) else {
            self.diagnostics.push(Diagnostic::error(
                "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE",
                "match scrutinee must resolve to always-active enum option",
                block.expr.span(),
            ));
            return;
        };

        if !self.symbols.option_is_always_active(&scrutinee_option_path) {
            self.diagnostics.push(
                Diagnostic::error(
                    "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE",
                    format!(
                        "match scrutinee option `{}` is not always-active",
                        scrutinee_option_path
                    ),
                    block.expr.span(),
                )
                .with_path(scrutinee_option_path),
            );
            return;
        }

        let variants = self
            .symbols
            .enum_variant_names(&enum_name)
            .unwrap_or_default();

        if variants.is_empty() {
            self.diagnostics.push(Diagnostic::error(
                "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE",
                format!("match enum `{}` has no declared variants", enum_name),
                block.expr.span(),
            ));
            return;
        }

        let universe: HashSet<String> = variants.into_iter().collect();
        let mut covered = HashSet::new();

        for case in &block.cases {
            let case_set = self.resolve_case_variants(&enum_name, &case.pattern, scope);
            let Some(case_set) = case_set else {
                continue;
            };

            let unreachable = case_set.iter().all(|v| covered.contains(v));
            if unreachable {
                self.diagnostics.push(Diagnostic::warning(
                    "W_UNREACHABLE_CASE",
                    "case is unreachable because variants are already covered",
                    case.pattern.span(),
                ));
            }

            let overlap: Vec<String> = case_set
                .iter()
                .filter(|variant| covered.contains(*variant))
                .cloned()
                .collect();
            if !overlap.is_empty() {
                self.diagnostics.push(Diagnostic::error(
                    "E_MATCH_OVERLAP",
                    format!("case overlaps with previous variants: {}", overlap.join(", ")),
                    case.pattern.span(),
                ));
            }

            covered.extend(case_set);
        }

        if covered != universe {
            self.diagnostics.push(Diagnostic::warning(
                "W_NON_EXHAUSTIVE_MATCH",
                "match does not cover all enum variants",
                block.span,
            ));
        }
    }

    fn match_scrutinee_option_path(&self, expr: &Expr, scope: &[String]) -> Option<String> {
        match expr {
            Expr::Path(path) => {
                if let ResolveOptionPathResult::Resolved(option_path, ValueType::Enum(_)) =
                    self.symbols.resolve_option_path_in_scope(scope, path)
                {
                    Some(option_path)
                } else {
                    None
                }
            }
            Expr::Group { expr, .. } => self.match_scrutinee_option_path(expr, scope),
            _ => None,
        }
    }

    fn resolve_case_variants(
        &mut self,
        enum_name: &str,
        pattern: &MatchPat,
        scope: &[String],
    ) -> Option<HashSet<String>> {
        let all = self
            .symbols
            .enum_variant_names(enum_name)
            .unwrap_or_default();
        let all_set: HashSet<String> = all.iter().cloned().collect();

        match pattern {
            MatchPat::Wildcard(_) => Some(all_set),
            MatchPat::Paths(paths, span) => {
                let mut result = HashSet::new();
                for path in paths {
                    let ty = self.resolve_path_type(path, scope);
                    let ValueType::Enum(found_enum) = ty else {
                        self.diagnostics.push(Diagnostic::error(
                            "E_ENUM_VARIANT_NOT_FOUND",
                            format!("`{}` is not an enum variant path", path.to_string()),
                            path.span,
                        ));
                        continue;
                    };

                    if !enum_name_matches(enum_name, &found_enum) {
                        self.diagnostics.push(Diagnostic::error(
                            "E_ENUM_VARIANT_NOT_FOUND",
                            format!(
                                "variant `{}` does not belong to enum `{}`",
                                path.to_string(),
                                enum_name
                            ),
                            path.span,
                        ));
                        continue;
                    }

                    let Some(variant_name) = path.segments.last().map(|seg| seg.value.clone()) else {
                        self.diagnostics.push(Diagnostic::error(
                            "E_ENUM_VARIANT_NOT_FOUND",
                            "empty variant path",
                            *span,
                        ));
                        continue;
                    };

                    if !all_set.contains(&variant_name) {
                        self.diagnostics.push(Diagnostic::error(
                            "E_ENUM_VARIANT_NOT_FOUND",
                            format!("variant `{}` is not declared in enum `{}`", variant_name, enum_name),
                            path.span,
                        ));
                        continue;
                    }

                    if let ResolveEnumVariantPathResult::Resolved(variant_path, resolved_enum) =
                        self.symbols.resolve_enum_variant_path_in_scope(scope, path)
                    {
                        if enum_name_matches(enum_name, &resolved_enum) {
                            self.used_enum_variants.insert(variant_path);
                        }
                    }

                    result.insert(variant_name);
                }
                Some(result)
            }
        }
    }

    fn expect_bool_expr(&mut self, expr: &Expr, scope: &[String], self_ty: Option<&ValueType>) {
        let ty = self.infer_expr_type(expr, scope, self_ty);
        if !ty.is_unknown() && !ty.is_bool() {
            self.diagnostics.push(Diagnostic::error(
                "E_EXPECT_BOOL",
                "expression in boolean context must evaluate to bool",
                expr.span(),
            ));
        }
    }

    fn infer_expr_type(
        &mut self,
        expr: &Expr,
        scope: &[String],
        self_ty: Option<&ValueType>,
    ) -> ValueType {
        match expr {
            Expr::Bool(_, _) => ValueType::Bool,
            Expr::Int(_, _) => ValueType::UntypedInt,
            Expr::String(_, _) => ValueType::String,
            Expr::SelfValue(span) => {
                if let Some(ty) = self_ty {
                    return ty.clone();
                }

                self.diagnostics.push(Diagnostic::error(
                    "E_SELF_OUTSIDE_ATTACHED_BLOCK",
                    "`self` can only be used in option attached constraints",
                    *span,
                ));
                ValueType::Unknown
            }
            Expr::Path(path) => self.resolve_path_type(path, scope),
            Expr::Call { name, args, span } => {
                let arg_types = args
                    .iter()
                    .map(|arg| self.infer_expr_type(arg, scope, self_ty))
                    .collect::<Vec<_>>();
                self.check_call(name.value.as_str(), &arg_types, *span)
            }
            Expr::Unary { op, expr, span } => {
                let operand = self.infer_expr_type(expr, scope, self_ty);
                match op {
                    UnaryOp::Not => {
                        if !operand.is_unknown() && !operand.is_bool() {
                            self.diagnostics.push(Diagnostic::error(
                                "E_EXPECT_BOOL",
                                "`!` expects a bool operand",
                                *span,
                            ));
                        }
                        ValueType::Bool
                    }
                }
            }
            Expr::Binary {
                op,
                left,
                right,
                span,
            } => {
                let mut left_ty = self.infer_expr_type(left, scope, self_ty);
                let mut right_ty = self.infer_expr_type(right, scope, self_ty);

                if matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Le
                        | BinaryOp::Gt
                        | BinaryOp::Ge
                ) && is_untyped_int_literal_expr(left)
                    && is_untyped_int_literal_expr(right)
                {
                    self.diagnostics.push(Diagnostic::error(
                        "E_UNTYPED_INT_LITERAL",
                        "integer literal expression requires a typed context",
                        *span,
                    ));
                }

                if matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Lt
                        | BinaryOp::Le
                        | BinaryOp::Gt
                        | BinaryOp::Ge
                ) {
                    let right_int = right_ty.concrete_int();
                    self.coerce_untyped_int_expr(left, &mut left_ty, right_int);

                    let left_int = left_ty.concrete_int();
                    self.coerce_untyped_int_expr(right, &mut right_ty, left_int);
                }

                self.check_binary(*op, &left_ty, &right_ty, *span)
            }
            Expr::InRange { expr, span, .. } => {
                let ty = self.infer_expr_type(expr, scope, self_ty);
                if !ty.is_unknown() && !ty.is_int() {
                    self.diagnostics.push(Diagnostic::error(
                        "E_EXPECT_INT",
                        "`in <range>` expects an integer expression",
                        *span,
                    ));
                }
                ValueType::Bool
            }
            Expr::InSet { expr, elems, span } => {
                let left_ty = self.infer_expr_type(expr, scope, self_ty);
                for elem in elems {
                    if let InSetElem::Path(path) = elem {
                        if self.symbols.path_resolves_to_option_in_scope(scope, path) {
                            self.diagnostics.push(Diagnostic::error(
                                "E_IN_NOT_CONSTANT",
                                "`in { ... }` elements must be constants, option paths are not allowed",
                                path.span,
                            ));
                        }
                    }

                    let elem_ty = self.infer_set_elem_type(elem, scope);
                    if !left_ty.is_unknown() && !elem_ty.is_unknown() && !left_ty.same_as(&elem_ty) {
                        self.diagnostics.push(Diagnostic::error(
                            "E_EXPECT_SAME_TYPE",
                            "`in { ... }` element type does not match expression type",
                            *span,
                        ));
                    }
                }
                ValueType::Bool
            }
            Expr::Group { expr, .. } => self.infer_expr_type(expr, scope, self_ty),
        }
    }

    fn infer_set_elem_type(&mut self, elem: &InSetElem, scope: &[String]) -> ValueType {
        match elem {
            InSetElem::Int(_, _) => ValueType::UntypedInt,
            InSetElem::Path(path) => self.resolve_path_type(path, scope),
        }
    }

    fn coerce_untyped_int_expr(
        &mut self,
        expr: &Expr,
        inferred: &mut ValueType,
        expected_int: Option<IntType>,
    ) {
        if !matches!(inferred, ValueType::UntypedInt) {
            return;
        }

        let Some(int_ty) = expected_int else {
            return;
        };

        let Some(value) = untyped_int_literal_value(expr) else {
            return;
        };

        let (min, max) = int_bounds_for_int_type(int_ty);
        if value < min || value > max {
            self.diagnostics.push(Diagnostic::error(
                "E_TYPE_MISMATCH",
                format!(
                    "integer literal `{}` is out of range for {} [{}..={}]",
                    value,
                    int_type_name(int_ty),
                    min,
                    max
                ),
                expr.span(),
            ));
            return;
        }

        *inferred = ValueType::Int(int_ty);
    }

    fn resolve_path_type(&mut self, path: &Path, scope: &[String]) -> ValueType {
        match self.symbols.resolve_path_type(scope, path) {
            ResolvePathResult::Resolved(ty) => ty,
            ResolvePathResult::NotFound => {
                self.diagnostics.push(Diagnostic::error(
                    "E_SYMBOL_NOT_FOUND",
                    format!("path `{}` cannot be resolved", path.to_string()),
                    path.span,
                ));
                ValueType::Unknown
            }
            ResolvePathResult::Ambiguous(candidates) => {
                self.diagnostics.push(Diagnostic::error(
                    "E_AMBIGUOUS_PATH",
                    format!(
                        "path `{}` is ambiguous, candidates: {}",
                        path.to_string(),
                        candidates.join(", ")
                    ),
                    path.span,
                ));
                ValueType::Unknown
            }
        }
    }

    fn check_call(&mut self, name: &str, args: &[ValueType], span: Span) -> ValueType {
        match name {
            "len" => {
                if args.len() != 1 || !args[0].is_string() {
                    self.diagnostics.push(Diagnostic::error(
                        "E_CALL_TYPE_MISMATCH",
                        "len(s) expects exactly one string argument",
                        span,
                    ));
                    return ValueType::Unknown;
                }
                ValueType::Int(IntType::I32)
            }
            "matches" => {
                if args.len() != 2 || !args[0].is_string() || !args[1].is_string() {
                    self.diagnostics.push(Diagnostic::error(
                        "E_CALL_TYPE_MISMATCH",
                        "matches(s, re) expects two string arguments",
                        span,
                    ));
                    return ValueType::Unknown;
                }
                ValueType::Bool
            }
            _ => {
                self.diagnostics.push(Diagnostic::error(
                    "E_UNKNOWN_FUNCTION",
                    format!("unknown function `{}`", name),
                    span,
                ));
                ValueType::Unknown
            }
        }
    }

    fn check_binary(
        &mut self,
        op: BinaryOp,
        left: &ValueType,
        right: &ValueType,
        span: Span,
    ) -> ValueType {
        match op {
            BinaryOp::Or | BinaryOp::And => {
                if (!left.is_unknown() && !left.is_bool()) || (!right.is_unknown() && !right.is_bool()) {
                    self.diagnostics.push(Diagnostic::error(
                        "E_EXPECT_BOOL",
                        "logical operators expect bool operands",
                        span,
                    ));
                }
                ValueType::Bool
            }
            BinaryOp::Eq | BinaryOp::Ne => {
                if !left.is_unknown() && !right.is_unknown() && !left.same_as(right) {
                    self.diagnostics.push(Diagnostic::error(
                        "E_EXPECT_SAME_TYPE",
                        "comparison requires operands of the same type",
                        span,
                    ));
                }
                ValueType::Bool
            }
            BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                if (!left.is_unknown() && !left.is_int()) || (!right.is_unknown() && !right.is_int()) {
                    self.diagnostics.push(Diagnostic::error(
                        "E_EXPECT_INT",
                        "relational operators expect integer operands",
                        span,
                    ));
                }
                if let (Some(left_int), Some(right_int)) = (left.concrete_int(), right.concrete_int()) {
                    if left_int != right_int {
                        self.diagnostics.push(Diagnostic::error(
                            "E_EXPECT_SAME_TYPE",
                            "relational operators require operands of the same integer type",
                            span,
                        ));
                    }
                }
                ValueType::Bool
            }
        }
    }
}

fn promote_strict_diagnostics(diagnostics: &mut [Diagnostic]) {
    for diagnostic in diagnostics {
        if diagnostic.code == "W_INACTIVE_ASSIGNMENT" {
            diagnostic.code = "E_INACTIVE_ASSIGNMENT".to_string();
            diagnostic.severity = Severity::Error;
        } else if diagnostic.code == "W_DUPLICATE_ASSIGNMENT" {
            diagnostic.code = "E_DUPLICATE_ASSIGNMENT".to_string();
            diagnostic.severity = Severity::Error;
        } else if diagnostic.code == "W_NON_EXHAUSTIVE_MATCH" {
            diagnostic.code = "E_NON_EXHAUSTIVE_MATCH".to_string();
            diagnostic.severity = Severity::Error;
        } else if diagnostic.code == "L_REQUIRE_MISSING_MSG" {
            diagnostic.code = "E_REQUIRE_MISSING_MSG".to_string();
            diagnostic.severity = Severity::Error;
        }
    }
}

fn merge_dependency_paths(base: &[String], extra: &[String]) -> Vec<String> {
    let mut merged = Vec::with_capacity(base.len() + extra.len());
    merged.extend(base.iter().cloned());
    merged.extend(extra.iter().cloned());
    merged.sort();
    merged.dedup();
    merged
}

fn canonical_cycle_signature(cycle: &[String]) -> String {
    if cycle.len() <= 1 {
        return cycle.join("->");
    }

    let sequence = &cycle[..cycle.len() - 1];
    if sequence.is_empty() {
        return String::new();
    }

    let mut best: Option<String> = None;
    for offset in 0..sequence.len() {
        let rotated = (0..sequence.len())
            .map(|index| sequence[(offset + index) % sequence.len()].clone())
            .collect::<Vec<_>>()
            .join("->");
        if best.as_ref().map_or(true, |current| rotated < *current) {
            best = Some(rotated);
        }
    }

    best.unwrap_or_default()
}

fn option_has_secret_attr(option: &OptionDecl) -> bool {
    option
        .meta
        .attrs
        .iter()
        .any(|attr| matches!(attr.kind, AttrKind::Secret))
}

fn normalize_export_name_with_prefix(path: &str, prefix: &str) -> String {
    let mut normalized = String::new();
    let mut prev_underscore = false;

    for ch in path.chars() {
        if ch.is_ascii_alphanumeric() {
            normalized.push(ch.to_ascii_uppercase());
            prev_underscore = false;
        } else if !prev_underscore {
            normalized.push('_');
            prev_underscore = true;
        }
    }

    let normalized = normalized.trim_matches('_').to_string();
    let normalized = if normalized
        .chars()
        .next()
        .map(|ch| ch.is_ascii_alphabetic())
        .unwrap_or(false)
    {
        normalized
    } else if normalized.is_empty() {
        "X".to_string()
    } else {
        format!("X_{}", normalized)
    };

    format!("{}{}", prefix, normalized)
}

fn module_has_metadata(module: &crate::ast::ModDecl) -> bool {
    !module.meta.attrs.is_empty() || !module.meta.doc.is_empty()
}

fn extract_range_attr(option: &OptionDecl) -> Option<IntRange> {
    option.meta.attrs.iter().find_map(|attr| {
        if let AttrKind::Range(range) = &attr.kind {
            Some(range.clone())
        } else {
            None
        }
    })
}

fn option_type_to_value_type(ty: &Type) -> ValueType {
    match ty {
        Type::Bool(_) => ValueType::Bool,
        Type::U8(_) => ValueType::Int(IntType::U8),
        Type::U16(_) => ValueType::Int(IntType::U16),
        Type::U32(_) => ValueType::Int(IntType::U32),
        Type::I32(_) => ValueType::Int(IntType::I32),
        Type::String(_) => ValueType::String,
        Type::Named(path) => {
            let enum_name = path
                .segments
                .last()
                .map(|segment| segment.value.clone())
                .unwrap_or_else(|| path.to_string());
            ValueType::Enum(enum_name)
        }
    }
}

fn build_full_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

fn build_candidate_paths(scope: &[String], raw_path: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    for i in (0..=scope.len()).rev() {
        let prefix = &scope[..i];
        if prefix.is_empty() {
            candidates.push(raw_path.to_string());
        } else {
            candidates.push(format!("{}::{}", prefix.join("::"), raw_path));
        }
    }
    candidates
}

fn expand_with_aliases(path: &Path, aliases: &HashMap<String, String>) -> String {
    if path.segments.is_empty() {
        return path.to_string();
    }

    let head = &path.segments[0].value;
    if let Some(prefix) = aliases.get(head) {
        let suffix = path
            .segments
            .iter()
            .skip(1)
            .map(|seg| seg.value.clone())
            .collect::<Vec<_>>();
        if suffix.is_empty() {
            return prefix.clone();
        }
        return format!("{}::{}", prefix, suffix.join("::"));
    }

    path.to_string()
}

fn path_matches(candidate: &str, raw_path: &str) -> bool {
    candidate == raw_path || candidate.ends_with(&format!("::{}", raw_path))
}

fn parse_env_int(raw: &str) -> Option<i128> {
    let cleaned = raw.replace('_', "");
    if let Some(hex) = cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        i128::from_str_radix(hex, 16).ok()
    } else {
        cleaned.parse::<i128>().ok()
    }
}

fn redacted_int_arg(value: i128, secret: bool) -> DiagnosticArgValue {
    if secret {
        DiagnosticArgValue::String("[redacted]".to_string())
    } else {
        DiagnosticArgValue::Int(value)
    }
}

fn int_type_name(int_type: IntType) -> &'static str {
    match int_type {
        IntType::U8 => "u8",
        IntType::U16 => "u16",
        IntType::U32 => "u32",
        IntType::I32 => "i32",
    }
}

fn int_bounds_for_int_type(int_type: IntType) -> (i128, i128) {
    match int_type {
        IntType::U8 => (u8::MIN as i128, u8::MAX as i128),
        IntType::U16 => (u16::MIN as i128, u16::MAX as i128),
        IntType::U32 => (u32::MIN as i128, u32::MAX as i128),
        IntType::I32 => (i32::MIN as i128, i32::MAX as i128),
    }
}

fn int_value_type_bounds(value_type: &ValueType) -> Option<(i128, i128)> {
    if let ValueType::Int(int_type) = value_type {
        Some(int_bounds_for_int_type(*int_type))
    } else {
        None
    }
}

fn is_untyped_int_literal_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_, _) => true,
        Expr::Group { expr, .. } => is_untyped_int_literal_expr(expr),
        _ => false,
    }
}

fn untyped_int_literal_value(expr: &Expr) -> Option<i128> {
    match expr {
        Expr::Int(value, _) => Some(*value),
        Expr::Group { expr, .. } => untyped_int_literal_value(expr),
        _ => None,
    }
}

fn int_type_bounds(ty: &Type) -> Option<(i128, i128)> {
    match ty {
        Type::U8(_) => Some((u8::MIN as i128, u8::MAX as i128)),
        Type::U16(_) => Some((u16::MIN as i128, u16::MAX as i128)),
        Type::U32(_) => Some((u32::MIN as i128, u32::MAX as i128)),
        Type::I32(_) => Some((i32::MIN as i128, i32::MAX as i128)),
        _ => None,
    }
}

fn enum_name_matches(expected: &str, candidate: &str) -> bool {
    if expected == candidate {
        return true;
    }

    let expected_base = expected.rsplit("::").next().unwrap_or(expected);
    let candidate_base = candidate.rsplit("::").next().unwrap_or(candidate);
    expected_base == candidate_base
}

fn evaluate_runtime_state(
    symbols: &SymbolTable,
    assignments: &HashMap<String, ResolvedAssignment>,
) -> RuntimeState {
    let mut values = assignments
        .iter()
        .map(|(path, assignment)| (path.clone(), assignment.value.clone()))
        .collect::<HashMap<_, _>>();
    let mut sources = assignments
        .iter()
        .map(|(path, assignment)| (path.clone(), assignment.source))
        .collect::<HashMap<_, _>>();

    for (path, default) in &symbols.option_defaults {
        if values.contains_key(path) {
            continue;
        }
        if let Some(value) = resolve_const_value(default, symbols) {
            values.insert(path.clone(), value);
            sources.insert(path.clone(), ValueSource::Default);
        }
    }

    let mut active = symbols
        .option_types
        .keys()
        .filter(|path| symbols.option_is_always_active(path))
        .cloned()
        .collect::<HashSet<_>>();
    let mut ctx_references = HashSet::new();

    let mut changed = true;
    let mut guard = 0usize;
    while changed && guard < 64 {
        guard += 1;
        let snapshot = RuntimeState {
            active: active.clone(),
            values: values.clone(),
            sources: sources.clone(),
            ctx_references: HashSet::new(),
        };
        let eval = evaluate_activation_once(symbols, &snapshot);
        changed = eval.active != active;
        active = eval.active;
        ctx_references.extend(eval.ctx_references);
    }

    RuntimeState {
        active,
        values,
        sources,
        ctx_references,
    }
}

fn evaluate_activation_once(symbols: &SymbolTable, runtime: &RuntimeState) -> RuntimeState {
    let mut active = symbols
        .option_types
        .keys()
        .filter(|path| symbols.option_is_always_active(path))
        .cloned()
        .collect::<HashSet<_>>();
    let mut scope = Vec::new();
    let mut ctx_references = HashSet::new();
    collect_active_options(
        symbols,
        symbols.schema_items(),
        &mut scope,
        runtime,
        true,
        &mut active,
        &mut ctx_references,
    );

    RuntimeState {
        active,
        values: runtime.values.clone(),
        sources: runtime.sources.clone(),
        ctx_references,
    }
}

fn collect_active_options(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    runtime: &RuntimeState,
    guard_active: bool,
    active: &mut HashSet<String>,
    ctx_references: &mut HashSet<String>,
) {
    for item in items {
        match item {
            Item::Use(_) | Item::Require(_) | Item::Constraint(_) | Item::Enum(_) => {}
            Item::Option(option) => {
                let option_path = build_full_path(scope, &option.name.value);
                if guard_active {
                    active.insert(option_path);
                }
            }
            Item::Mod(module) => {
                scope.push(module.name.value.clone());
                collect_active_options(
                    symbols,
                    &module.items,
                    scope,
                    runtime,
                    guard_active,
                    active,
                    ctx_references,
                );
                scope.pop();
            }
            Item::When(when_block) => {
                let cond = eval_expr_as_bool(
                    &when_block.condition,
                    symbols,
                    scope,
                    runtime,
                    ctx_references,
                )
                .unwrap_or(false);
                collect_active_options(
                    symbols,
                    &when_block.items,
                    scope,
                    runtime,
                    guard_active && cond,
                    active,
                    ctx_references,
                );
            }
            Item::Match(match_block) => {
                let selected = select_match_case_index(
                    &match_block,
                    symbols,
                    scope,
                    runtime,
                    ctx_references,
                );
                for (index, case) in match_block.cases.iter().enumerate() {
                    let case_active = selected.is_some_and(|selected_index| selected_index == index);
                    collect_active_options(
                        symbols,
                        &case.items,
                        scope,
                        runtime,
                        guard_active && case_active,
                        active,
                        ctx_references,
                    );
                }
            }
        }
    }
}

fn select_match_case_index(
    block: &MatchBlock,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> Option<usize> {
    let scrutinee = eval_expr(
        &block.expr,
        symbols,
        scope,
        runtime,
        ctx_references,
        true,
    )?;

    for (index, case) in block.cases.iter().enumerate() {
        if !match_pattern_matches(&case.pattern, &scrutinee, symbols, scope, runtime, ctx_references) {
            continue;
        }

        if let Some(guard) = &case.guard {
            if !eval_expr_as_bool(guard, symbols, scope, runtime, ctx_references).unwrap_or(false) {
                continue;
            }
        }

        return Some(index);
    }
    None
}

fn match_pattern_matches(
    pattern: &MatchPat,
    scrutinee: &ResolvedValue,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> bool {
    match pattern {
        MatchPat::Wildcard(_) => true,
        MatchPat::Paths(paths, _) => paths.iter().any(|path| {
            eval_path_as_enum_variant(path, symbols, scope, runtime, ctx_references)
                .is_some_and(|variant| scrutinee.as_enum_variant().is_some_and(|current| current == variant))
        }),
    }
}

fn build_resolved_config(symbols: &SymbolTable, runtime: &RuntimeState) -> ResolvedConfig {
    let mut paths = symbols.option_types.keys().cloned().collect::<Vec<_>>();
    paths.sort();

    let options = paths
        .into_iter()
        .map(|path| ResolvedOption {
            active: runtime.active.contains(&path),
            value: runtime.values.get(&path).cloned(),
            source: runtime.sources.get(&path).copied(),
            path,
        })
        .collect::<Vec<_>>();

    ResolvedConfig { options }
}

fn resolve_const_value(value: &ConstValue, symbols: &SymbolTable) -> Option<ResolvedValue> {
    match value {
        ConstValue::Bool(raw, _) => Some(ResolvedValue::Bool(*raw)),
        ConstValue::Int(raw, _) => Some(ResolvedValue::Int(*raw)),
        ConstValue::String(raw, _) => Some(ResolvedValue::String(raw.clone())),
        ConstValue::EnumPath(path) => {
            let resolved = symbols.resolve_enum_variant_paths(&path.to_string());
            if resolved.len() == 1 {
                Some(ResolvedValue::EnumVariant(resolved[0].clone()))
            } else {
                None
            }
        }
    }
}

fn collect_runtime_require_diagnostics(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    runtime: &RuntimeState,
    diagnostics: &mut Vec<(Diagnostic, Option<usize>)>,
) {
    for item in items {
        match item {
            Item::Use(_) | Item::Enum(_) | Item::Option(_) => {}
            Item::Require(require) => {
                if !eval_expr_as_bool(&require.expr, symbols, scope, runtime, &mut HashSet::new())
                    .unwrap_or(false)
                {
                    diagnostics.push((
                        Diagnostic::error(
                            "E_REQUIRE_FAILED",
                            "require condition failed at runtime",
                            require.span,
                        ),
                        None,
                    ));
                }
            }
            Item::Constraint(constraint) => {
                for child in &constraint.items {
                    if let ConstraintItem::Require(require) = child {
                        if !eval_expr_as_bool(
                            &require.expr,
                            symbols,
                            scope,
                            runtime,
                            &mut HashSet::new(),
                        )
                        .unwrap_or(false)
                        {
                            diagnostics.push((
                                Diagnostic::error(
                                    "E_REQUIRE_FAILED",
                                    "require condition failed at runtime",
                                    require.span,
                                ),
                                None,
                            ));
                        }
                    }
                }
            }
            Item::Mod(module) => {
                scope.push(module.name.value.clone());
                collect_runtime_require_diagnostics(
                    symbols,
                    &module.items,
                    scope,
                    runtime,
                    diagnostics,
                );
                scope.pop();
            }
            Item::When(when_block) => {
                if eval_expr_as_bool(
                    &when_block.condition,
                    symbols,
                    scope,
                    runtime,
                    &mut HashSet::new(),
                )
                .unwrap_or(false)
                {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &when_block.items,
                        scope,
                        runtime,
                        diagnostics,
                    );
                }
            }
            Item::Match(match_block) => {
                if let Some(index) = select_match_case_index(
                    match_block,
                    symbols,
                    scope,
                    runtime,
                    &mut HashSet::new(),
                ) {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &match_block.cases[index].items,
                        scope,
                        runtime,
                        diagnostics,
                    );
                }
            }
        }
    }
}

fn eval_expr_as_bool(
    expr: &Expr,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> Option<bool> {
    eval_expr(expr, symbols, scope, runtime, ctx_references, true).and_then(|value| value.as_bool())
}

fn eval_expr(
    expr: &Expr,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
    inactive_bool_as_false: bool,
) -> Option<ResolvedValue> {
    match expr {
        Expr::Bool(value, _) => Some(ResolvedValue::Bool(*value)),
        Expr::Int(value, _) => Some(ResolvedValue::Int(*value)),
        Expr::String(value, _) => Some(ResolvedValue::String(value.clone())),
        Expr::SelfValue(_) => None,
        Expr::Path(path) => eval_path_value(
            path,
            symbols,
            scope,
            runtime,
            ctx_references,
            inactive_bool_as_false,
        ),
        Expr::Call { name, args, .. } => eval_call(name.value.as_str(), args, symbols, scope, runtime, ctx_references),
        Expr::Unary {
            op: UnaryOp::Not,
            expr,
            ..
        } => eval_expr_as_bool(expr, symbols, scope, runtime, ctx_references).map(|value| ResolvedValue::Bool(!value)),
        Expr::Binary {
            op,
            left,
            right,
            ..
        } => eval_binary_expr(*op, left, right, symbols, scope, runtime, ctx_references),
        Expr::InRange { expr, range, .. } => {
            let value = eval_expr(expr, symbols, scope, runtime, ctx_references, inactive_bool_as_false)?;
            let int_value = value.as_int()?;
            let result = if range.inclusive {
                int_value >= range.start && int_value <= range.end
            } else {
                int_value >= range.start && int_value < range.end
            };
            Some(ResolvedValue::Bool(result))
        }
        Expr::InSet { expr, elems, .. } => {
            let left = eval_expr(expr, symbols, scope, runtime, ctx_references, inactive_bool_as_false)?;
            let result = elems.iter().any(|elem| {
                let right = match elem {
                    InSetElem::Int(value, _) => Some(ResolvedValue::Int(*value)),
                    InSetElem::Path(path) => eval_path_value(
                        path,
                        symbols,
                        scope,
                        runtime,
                        ctx_references,
                        inactive_bool_as_false,
                    ),
                };
                right.is_some_and(|right_value| values_equal(&left, &right_value))
            });
            Some(ResolvedValue::Bool(result))
        }
        Expr::Group { expr, .. } => eval_expr(expr, symbols, scope, runtime, ctx_references, inactive_bool_as_false),
    }
}

fn eval_binary_expr(
    op: BinaryOp,
    left: &Expr,
    right: &Expr,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> Option<ResolvedValue> {
    match op {
        BinaryOp::Or => Some(ResolvedValue::Bool(
            eval_expr_as_bool(left, symbols, scope, runtime, ctx_references)?
                || eval_expr_as_bool(right, symbols, scope, runtime, ctx_references)?,
        )),
        BinaryOp::And => Some(ResolvedValue::Bool(
            eval_expr_as_bool(left, symbols, scope, runtime, ctx_references)?
                && eval_expr_as_bool(right, symbols, scope, runtime, ctx_references)?,
        )),
        BinaryOp::Eq => {
            let lhs = eval_expr(left, symbols, scope, runtime, ctx_references, true)?;
            let rhs = eval_expr(right, symbols, scope, runtime, ctx_references, true)?;
            Some(ResolvedValue::Bool(values_equal(&lhs, &rhs)))
        }
        BinaryOp::Ne => {
            let lhs = eval_expr(left, symbols, scope, runtime, ctx_references, true)?;
            let rhs = eval_expr(right, symbols, scope, runtime, ctx_references, true)?;
            Some(ResolvedValue::Bool(!values_equal(&lhs, &rhs)))
        }
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            let lhs = eval_expr(left, symbols, scope, runtime, ctx_references, true)?.as_int()?;
            let rhs = eval_expr(right, symbols, scope, runtime, ctx_references, true)?.as_int()?;
            let result = match op {
                BinaryOp::Lt => lhs < rhs,
                BinaryOp::Le => lhs <= rhs,
                BinaryOp::Gt => lhs > rhs,
                BinaryOp::Ge => lhs >= rhs,
                _ => unreachable!(),
            };
            Some(ResolvedValue::Bool(result))
        }
    }
}

fn values_equal(left: &ResolvedValue, right: &ResolvedValue) -> bool {
    match (left, right) {
        (ResolvedValue::Bool(lhs), ResolvedValue::Bool(rhs)) => lhs == rhs,
        (ResolvedValue::Int(lhs), ResolvedValue::Int(rhs)) => lhs == rhs,
        (ResolvedValue::String(lhs), ResolvedValue::String(rhs)) => lhs == rhs,
        (ResolvedValue::EnumVariant(lhs), ResolvedValue::EnumVariant(rhs)) => lhs == rhs,
        _ => false,
    }
}

fn eval_call(
    name: &str,
    args: &[Expr],
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> Option<ResolvedValue> {
    match name {
        "len" => {
            if args.len() != 1 {
                return None;
            }
            let value = eval_expr(&args[0], symbols, scope, runtime, ctx_references, true)?;
            Some(ResolvedValue::Int(value.as_string()?.chars().count() as i128))
        }
        "matches" => {
            if args.len() != 2 {
                return None;
            }
            let input = eval_expr(&args[0], symbols, scope, runtime, ctx_references, true)?;
            let pattern = eval_expr(&args[1], symbols, scope, runtime, ctx_references, true)?;
            let input = input.as_string()?;
            let pattern = pattern.as_string()?;
            Some(ResolvedValue::Bool(input.contains(pattern)))
        }
        _ => None,
    }
}

fn eval_path_value(
    path: &Path,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
    inactive_bool_as_false: bool,
) -> Option<ResolvedValue> {
    match symbols.resolve_option_path_in_scope(scope, path) {
        ResolveOptionPathResult::Resolved(option_path, ty) => {
            if option_path == "ctx" || option_path.starts_with("ctx::") {
                ctx_references.insert(option_path.clone());
            }

            if !runtime.active.contains(&option_path) {
                if inactive_bool_as_false && ty.is_bool() {
                    return Some(ResolvedValue::Bool(false));
                }
                return None;
            }

            runtime.values.get(&option_path).cloned()
        }
        ResolveOptionPathResult::NotFound | ResolveOptionPathResult::Ambiguous(_) => {
            eval_path_as_enum_variant(path, symbols, scope, runtime, ctx_references)
                .map(|variant| ResolvedValue::EnumVariant(variant.to_string()))
        }
    }
}

fn eval_path_as_enum_variant(
    path: &Path,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut HashSet<String>,
) -> Option<String> {
    let _ = runtime;
    let _ = ctx_references;
    match symbols.resolve_enum_variant_path_in_scope(scope, path) {
        ResolveEnumVariantPathResult::Resolved(variant, _) => {
            symbols.enum_variants.get(&variant).map(|_| variant)
        }
        ResolveEnumVariantPathResult::NotFound | ResolveEnumVariantPathResult::Ambiguous(_) => None,
    }
}
