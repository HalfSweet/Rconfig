use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path as FsPath, PathBuf};

use crate::ast::{
    AttrKind, BinaryOp, ConstValue, ConstraintItem, Expr, File, InSetElem, IntRange, Item,
    MatchBlock, MatchPat, OptionDecl, Path, Type, UnaryOp, ValueExpr, ValuesFile, ValuesStmt,
};
use crate::error::{Diagnostic, Severity};
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
    Int,
    String,
    Enum(String),
    Unknown,
}

impl ValueType {
    fn is_bool(&self) -> bool {
        matches!(self, ValueType::Bool)
    }

    fn is_int(&self) -> bool {
        matches!(self, ValueType::Int)
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
            | (ValueType::Int, ValueType::Int)
            | (ValueType::String, ValueType::String)
            | (ValueType::Unknown, ValueType::Unknown) => true,
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
    option_always_active: HashMap<String, bool>,
    enum_variants: HashMap<String, String>,
    enum_variant_spans: HashMap<String, Span>,
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

    fn insert_option_always_active(&mut self, path: String, is_always_active: bool) {
        self.option_always_active.insert(path, is_always_active);
    }

    fn option_default(&self, path: &str) -> Option<&ConstValue> {
        self.option_defaults.get(path)
    }

    fn option_range(&self, path: &str) -> Option<&IntRange> {
        self.option_ranges.get(path)
    }

    fn option_span(&self, path: &str) -> Option<Span> {
        self.option_spans.get(path).copied()
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
    pub stmt_origins: Vec<ValuesStmtOrigin>,
    pub diagnostics: Vec<Diagnostic>,
    pub diagnostic_stmt_indexes: Vec<Option<usize>>,
}

pub fn analyze_schema(file: &File) -> SemanticReport {
    let mut collector = SymbolCollector::default();
    let mut root_scope = Vec::new();
    collector.collect_items(&file.items, &mut root_scope);

    let mut diagnostics = collector.diagnostics;
    let symbols = collector.symbols;

    let mut checker = TypeChecker::new(&symbols);
    let mut scope = Vec::new();
    checker.check_items(&file.items, &mut scope);
    diagnostics.extend(checker.diagnostics);

    SemanticReport {
        symbols,
        diagnostics,
    }
}

pub fn analyze_schema_strict(file: &File) -> SemanticReport {
    let mut report = analyze_schema(file);
    promote_strict_diagnostics(&mut report.diagnostics);
    report
}

pub fn analyze_values(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    analyze_values_with_stmt_indexes(values, symbols).0
}

pub fn analyze_values_strict(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    let mut diagnostics = analyze_values(values, symbols);
    promote_strict_diagnostics(&mut diagnostics);
    diagnostics
}

fn analyze_values_with_stmt_indexes(
    values: &ValuesFile,
    symbols: &SymbolTable,
) -> (Vec<Diagnostic>, Vec<Option<usize>>) {
    let mut checker = ValuesChecker::new(symbols);
    checker.check(values);

    let missing_diags = checker.missing_value_diagnostics();
    let mut diagnostics = checker.diagnostics;
    let mut stmt_indexes = checker.diagnostic_stmt_indexes;
    for diagnostic in missing_diags {
        diagnostics.push(diagnostic);
        stmt_indexes.push(None);
    }

    (diagnostics, stmt_indexes)
}

pub fn analyze_values_from_path_report(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> ValuesAnalysisReport {
    let (values, stmt_origins, mut diagnostics) = expand_values_includes_with_origins(entry);
    let mut diagnostic_stmt_indexes = vec![None; diagnostics.len()];
    let (mut semantic, mut semantic_stmt_indexes) = analyze_values_with_stmt_indexes(&values, symbols);
    diagnostics.append(&mut semantic);
    diagnostic_stmt_indexes.append(&mut semantic_stmt_indexes);
    ValuesAnalysisReport {
        values,
        stmt_origins,
        diagnostics,
        diagnostic_stmt_indexes,
    }
}

pub fn analyze_values_from_path_report_strict(
    entry: &FsPath,
    symbols: &SymbolTable,
) -> ValuesAnalysisReport {
    let mut report = analyze_values_from_path_report(entry, symbols);
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
    current_stmt_index: Option<usize>,
}

impl<'a> ValuesChecker<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            diagnostics: Vec::new(),
            diagnostic_stmt_indexes: Vec::new(),
            aliases: HashMap::new(),
            assigned: HashMap::new(),
            current_stmt_index: None,
        }
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
                        self.warn_inactive_assignment(resolved, assign.path.span);
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

    fn warn_inactive_assignment(&mut self, target: &str, span: Span) {
        if self.symbols.option_is_always_active(target) {
            return;
        }

        self.push_diag(
            Diagnostic::warning(
                "W_INACTIVE_ASSIGNMENT",
                format!(
                    "assignment to conditionally active option `{}` may be ignored",
                    target
                ),
                span,
            )
            .with_path(target),
        );
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

        let actual = self.value_expr_type(value, &expected);
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

        if matches!(expected, ValueType::Int) {
            if let (Some(range), Some(actual)) = (
                self.symbols.option_range(target),
                extract_int_from_value_expr(value),
            ) {
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
                    self.push_diag(
                        Diagnostic::error(
                            "E_RANGE_VIOLATION",
                            format!(
                                "value `{}` for `{}` is outside range [{}..={}]",
                                actual, target, range.start, max
                            ),
                            value.span(),
                        )
                        .with_path(target),
                    );
                }
            }
        }
    }

    fn value_expr_type(&mut self, value: &ValueExpr, expected: &ValueType) -> ValueType {
        match value {
            ValueExpr::Bool(_, _) => ValueType::Bool,
            ValueExpr::Int(_, _) => ValueType::Int,
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
        let raw = match std::env::var(name) {
            Ok(value) => value,
            Err(_) => {
                self.push_diag(Diagnostic::error(
                    "E_ENV_NOT_SET",
                    format!("environment variable `{}` is not set", name),
                    span,
                ));
                return ValueType::Unknown;
            }
        };

        match expected {
            ValueType::Bool => {
                if raw == "true" || raw == "false" {
                    ValueType::Bool
                } else {
                    self.push_env_parse_failed(name, &raw, "bool", span);
                    ValueType::Unknown
                }
            }
            ValueType::Int => {
                if parse_env_int(&raw).is_some() {
                    ValueType::Int
                } else {
                    self.push_env_parse_failed(name, &raw, "int", span);
                    ValueType::Unknown
                }
            }
            ValueType::String => ValueType::String,
            ValueType::Enum(_) => ValueType::Unknown,
            ValueType::Unknown => ValueType::Unknown,
        }
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

    fn missing_value_diagnostics(&self) -> Vec<Diagnostic> {
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
                if self.symbols.option_default(&option_path).is_none() {
                    diagnostics.push(
                        Diagnostic::error(
                            "E_MISSING_CONTEXT_VALUE",
                            format!(
                                "context option `{}` requires an injected context value",
                                option_path
                            ),
                            self.symbols.option_span(&option_path).unwrap_or_default(),
                        )
                        .with_path(option_path),
                    );
                }
                continue;
            }
            if !self.symbols.option_is_always_active(&option_path) {
                continue;
            }
            if self.symbols.option_default(&option_path).is_some() {
                continue;
            }
            if self.assigned.contains_key(&option_path) {
                continue;
            }

            diagnostics.push(
                Diagnostic::error(
                    "E_MISSING_VALUE",
                    format!(
                        "active option `{}` has no value from assignments or defaults",
                        option_path
                    ),
                    self.symbols.option_span(&option_path).unwrap_or_default(),
                )
                .with_path(option_path),
            );
        }

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
                    self.diagnostics.push(
                        Diagnostic::error(
                            "E_DEFAULT_OUT_OF_RANGE",
                            format!(
                                "default value `{}` for option `{}` is out of range [{}..={}]",
                                value, option_path, min, max
                            ),
                            *span,
                        )
                        .with_path(option_path),
                    );
                }
            }
        }
    }

    fn infer_const_type(&self, value: &ConstValue, scope: &[String]) -> ValueType {
        match value {
            ConstValue::Bool(_, _) => ValueType::Bool,
            ConstValue::Int(_, _) => ValueType::Int,
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
                    self.diagnostics.push(
                        Diagnostic::error(
                            "E_CIRCULAR_ACTIVATION",
                            format!("activation dependency cycle detected: {}", chain),
                            self.symbols.option_span(node).unwrap_or_default(),
                        )
                        .with_path(node.to_string()),
                    );
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
                "match scrutinee must resolve to enum option",
                block.expr.span(),
            ));
            return;
        };

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
            Expr::Int(_, _) => ValueType::Int,
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
                let left_ty = self.infer_expr_type(left, scope, self_ty);
                let right_ty = self.infer_expr_type(right, scope, self_ty);

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
            InSetElem::Int(_, _) => ValueType::Int,
            InSetElem::Path(path) => self.resolve_path_type(path, scope),
        }
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
                ValueType::Int
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
                ValueType::Bool
            }
        }
    }
}

fn promote_strict_diagnostics(diagnostics: &mut [Diagnostic]) {
    for diagnostic in diagnostics {
        if diagnostic.code == "W_INACTIVE_ASSIGNMENT" {
            diagnostic.code = "E_INACTIVE_ASSIGNMENT";
            diagnostic.severity = Severity::Error;
        } else if diagnostic.code == "L_REQUIRE_MISSING_MSG" {
            diagnostic.code = "E_REQUIRE_MISSING_MSG";
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

fn extract_int_from_value_expr(value: &ValueExpr) -> Option<i128> {
    if let ValueExpr::Int(value, _) = value {
        Some(*value)
    } else {
        None
    }
}

fn option_type_to_value_type(ty: &Type) -> ValueType {
    match ty {
        Type::Bool(_) => ValueType::Bool,
        Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::I32(_) => ValueType::Int,
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

fn is_untyped_int_literal_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_, _) => true,
        Expr::Group { expr, .. } => is_untyped_int_literal_expr(expr),
        _ => false,
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
