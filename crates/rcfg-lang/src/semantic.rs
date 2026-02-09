use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path as FsPath, PathBuf};

use crate::ast::{
    BinaryOp, ConstraintItem, Expr, File, InSetElem, Item, MatchBlock, MatchPat, OptionDecl,
    Path, Type, UnaryOp, ValueExpr, ValuesFile, ValuesStmt,
};
use crate::error::Diagnostic;
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
    enum_variants: HashMap<String, String>,
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

    fn insert_enum_variant(&mut self, variant_path: String, enum_path: String) -> Option<String> {
        self.enum_variants.insert(variant_path, enum_path)
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

pub fn analyze_values(values: &ValuesFile, symbols: &SymbolTable) -> Vec<Diagnostic> {
    analyze_values_with_stmt_indexes(values, symbols).0
}

fn analyze_values_with_stmt_indexes(
    values: &ValuesFile,
    symbols: &SymbolTable,
) -> (Vec<Diagnostic>, Vec<Option<usize>>) {
    let mut checker = ValuesChecker::new(symbols);
    checker.check(values);
    (checker.diagnostics, checker.diagnostic_stmt_indexes)
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

pub fn analyze_values_from_path(entry: &FsPath, symbols: &SymbolTable) -> (ValuesFile, Vec<Diagnostic>) {
    let report = analyze_values_from_path_report(entry, symbols);
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
}

impl SymbolCollector {
    fn collect_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        for item in items {
            match item {
                Item::Use(_) | Item::Require(_) | Item::Constraint(_) => {}
                Item::Option(option) => {
                    let full_path = build_full_path(scope, &option.name.value);
                    if self.declare(scope, &option.name.value, SymbolKind::Option, option.name.span) {
                        self.symbols
                            .insert_option_type(full_path, option_type_to_value_type(&option.ty));
                    }
                }
                Item::Enum(enum_decl) => {
                    let enum_path = build_full_path(scope, &enum_decl.name.value);
                    if self.declare(scope, &enum_decl.name.value, SymbolKind::Enum, enum_decl.name.span) {
                        for variant in &enum_decl.variants {
                            let variant_path = format!("{}::{}", enum_path, variant.name.value);
                            if self
                                .symbols
                                .insert_enum_variant(variant_path.clone(), enum_path.clone())
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
                    self.declare(scope, &module.name.value, SymbolKind::Mod, module.name.span);
                    scope.push(module.name.value.clone());
                    self.collect_items(&module.items, scope);
                    scope.pop();
                }
                Item::When(when_block) => {
                    self.collect_items(&when_block.items, scope);
                }
                Item::Match(match_block) => {
                    for case in &match_block.cases {
                        self.collect_items(&case.items, scope);
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

struct TypeChecker<'a> {
    symbols: &'a SymbolTable,
    diagnostics: Vec<Diagnostic>,
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
        }
    }

    fn check_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        for item in items {
            match item {
                Item::Use(_) | Item::Enum(_) => {}
                Item::Mod(module) => {
                    scope.push(module.name.value.clone());
                    self.check_items(&module.items, scope);
                    scope.pop();
                }
                Item::Option(option) => {
                    self.check_option(option, scope);
                }
                Item::Require(require) => {
                    self.expect_bool_expr(&require.expr, scope, None);
                }
                Item::Constraint(constraint) => {
                    for item in &constraint.items {
                        if let ConstraintItem::Require(require) = item {
                            self.expect_bool_expr(&require.expr, scope, None);
                        }
                    }
                }
                Item::When(when_block) => {
                    self.expect_bool_expr(&when_block.condition, scope, None);
                    self.check_items(&when_block.items, scope);
                }
                Item::Match(match_block) => {
                    self.check_match_block(match_block, scope);
                    for case in &match_block.cases {
                        if let Some(guard) = &case.guard {
                            self.expect_bool_expr(guard, scope, None);
                        }
                        self.check_items(&case.items, scope);
                    }
                }
            }
        }
    }

    fn check_option(&mut self, option: &OptionDecl, scope: &[String]) {
        if let Some(attached) = &option.attached_constraints {
            let self_ty = option_type_to_value_type(&option.ty);
            for require in &attached.requires {
                self.expect_bool_expr(&require.expr, scope, Some(&self_ty));
            }
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

fn enum_name_matches(expected: &str, candidate: &str) -> bool {
    if expected == candidate {
        return true;
    }

    let expected_base = expected.rsplit("::").next().unwrap_or(expected);
    let candidate_base = candidate.rsplit("::").next().unwrap_or(candidate);
    expected_base == candidate_base
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use crate::parser::{parse_schema_with_diagnostics, parse_values_with_diagnostics};

    use super::{analyze_schema, SymbolKind};

    fn symbols_from(src: &str) -> super::SymbolTable {
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");
        analyze_schema(&file).symbols
    }

    #[test]
    fn allows_open_module_redeclaration() {
        let src = r#"
mod uart {
  option enable: bool = false;
}

mod uart {
  option baud: u32 = 115200;
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .all(|diag| diag.code != "E_SYMBOL_REDEFINED" && diag.code != "E_SYMBOL_KIND_CONFLICT"),
            "semantic diagnostics: {:#?}",
            report.diagnostics
        );
        assert!(matches!(
            report.symbols.get("uart"),
            Some(info) if info.kind == SymbolKind::Mod
        ));
        assert!(matches!(
            report.symbols.get("uart::enable"),
            Some(info) if info.kind == SymbolKind::Option
        ));
        assert!(matches!(
            report.symbols.get("uart::baud"),
            Some(info) if info.kind == SymbolKind::Option
        ));
    }

    #[test]
    fn reports_symbol_redefined() {
        let src = r#"
mod a {
  option x: bool = false;
}

mod a {
  option x: bool = true;
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_SYMBOL_REDEFINED"),
            "expected E_SYMBOL_REDEFINED, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_symbol_kind_conflict() {
        let src = r#"
mod foo { }
option foo: bool = false;
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_SYMBOL_KIND_CONFLICT"),
            "expected E_SYMBOL_KIND_CONFLICT, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn checks_when_condition_must_be_bool() {
        let src = r#"
mod app {
  option retries: u32 = 3;
  when retries {
    option timeout: u32 = 100;
  }
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_EXPECT_BOOL"),
            "expected E_EXPECT_BOOL, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn checks_binary_type_compatibility() {
        let src = r#"
mod app {
  option enabled: bool = false;
  option retries: u32 = 3;
  require!(enabled == retries);
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_EXPECT_SAME_TYPE"),
            "expected E_EXPECT_SAME_TYPE, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_self_outside_attached_constraints() {
        let src = r#"
mod app {
  require!(self == 1);
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_SELF_OUTSIDE_ATTACHED_BLOCK"),
            "expected E_SELF_OUTSIDE_ATTACHED_BLOCK, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_non_exhaustive_match() {
        let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::on => { }
  }
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "W_NON_EXHAUSTIVE_MATCH"),
            "expected W_NON_EXHAUSTIVE_MATCH, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_match_overlap() {
        let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::off => { }
    case Mode::off | Mode::on => { }
  }
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_MATCH_OVERLAP"),
            "expected E_MATCH_OVERLAP, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_unreachable_case_after_wildcard() {
        let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case _ => { }
    case Mode::on => { }
  }
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "W_UNREACHABLE_CASE"),
            "expected W_UNREACHABLE_CASE, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn values_detect_type_mismatch() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
app::enabled = 1;
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "E_TYPE_MISMATCH"),
            "expected E_TYPE_MISMATCH, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn values_detect_ctx_assignment() {
        let schema_src = r#"
mod ctx {
  option arch: string;
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
ctx::arch = "arm";
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "E_CONTEXT_ASSIGNMENT_NOT_ALLOWED"),
            "expected E_CONTEXT_ASSIGNMENT_NOT_ALLOWED, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn values_support_use_alias_for_assignment_target() {
        let schema_src = r#"
mod sdk {
  mod usart1 {
    option enable: bool = false;
  }
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
use sdk::usart1 as uart;
uart::enable = true;
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags.is_empty(),
            "expected no diagnostics, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn values_report_value_path_resolves_to_option() {
        let schema_src = r#"
mod app {
  option a: bool = false;
  option b: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
app::a = app::b;
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "E_VALUE_PATH_RESOLVES_TO_OPTION"),
            "expected E_VALUE_PATH_RESOLVES_TO_OPTION, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn values_env_reports_missing_var() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);
        let missing = "RCFG_TEST_MISSING_ENV";
        unsafe {
            std::env::remove_var(missing);
        }

        let values_src = format!("app::enabled = env(\"{}\");", missing);
        let (values, diags) = parse_values_with_diagnostics(&values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "E_ENV_NOT_SET"),
            "expected E_ENV_NOT_SET, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn values_env_reports_parse_failure_for_bool() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);
        let key = "RCFG_TEST_BOOL_ENV";
        unsafe {
            std::env::set_var(key, "TRUE");
        }

        let values_src = format!("app::enabled = env(\"{}\");", key);
        let (values, diags) = parse_values_with_diagnostics(&values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "E_ENV_PARSE_FAILED"),
            "expected E_ENV_PARSE_FAILED, got: {semantic_diags:#?}"
        );

        unsafe {
            std::env::remove_var(key);
        }
    }

    #[test]
    fn values_warn_duplicate_assignments() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
app::enabled = true;
app::enabled = false;
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        assert!(
            semantic_diags
                .iter()
                .any(|diag| diag.code == "W_DUPLICATE_ASSIGNMENT"),
            "expected W_DUPLICATE_ASSIGNMENT, got: {semantic_diags:#?}"
        );
    }

    #[test]
    fn include_expander_reports_not_found() {
        let missing = PathBuf::from("/tmp/rcfg-not-found-include.rcfgv");
        let (_values, diags) = super::expand_values_includes_from_path(&missing);
        assert!(
            diags.iter().any(|diag| diag.code == "E_INCLUDE_NOT_FOUND"),
            "expected E_INCLUDE_NOT_FOUND, got: {diags:#?}"
        );
        let diag = diags
            .iter()
            .find(|diag| diag.code == "E_INCLUDE_NOT_FOUND")
            .expect("missing E_INCLUDE_NOT_FOUND");
        assert!(diag.source.is_some(), "expected source in diagnostic");
        assert!(
            !diag.include_chain.is_empty(),
            "expected include chain in diagnostic"
        );
    }

    #[test]
    fn include_expander_reports_cycle() {
        let tmp = std::env::temp_dir().join(format!(
            "rcfg_include_cycle_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let a = tmp.join("a.rcfgv");
        let b = tmp.join("b.rcfgv");
        std::fs::write(&a, "include \"b.rcfgv\";\n").expect("write a");
        std::fs::write(&b, "include \"a.rcfgv\";\n").expect("write b");

        let (_values, diags) = super::expand_values_includes_from_path(&a);
        assert!(
            diags.iter().any(|diag| diag.code == "E_INCLUDE_CYCLE"),
            "expected E_INCLUDE_CYCLE, got: {diags:#?}"
        );
        let diag = diags
            .iter()
            .find(|diag| diag.code == "E_INCLUDE_CYCLE")
            .expect("missing E_INCLUDE_CYCLE");
        assert!(diag.source.is_some(), "expected source in diagnostic");
        assert!(
            diag.include_chain.len() >= 2,
            "expected include chain with at least two nodes"
        );

        let _ = std::fs::remove_file(&a);
        let _ = std::fs::remove_file(&b);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn include_expander_provides_stmt_origins() {
        let tmp = std::env::temp_dir().join(format!(
            "rcfg_stmt_origin_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let base = tmp.join("base.rcfgv");
        let root = tmp.join("root.rcfgv");
        std::fs::write(&base, "app::enabled = true;\n").expect("write base");
        std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

        let (values, origins, diags) = super::expand_values_includes_with_origins(&root);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");
        assert_eq!(values.stmts.len(), 1);
        assert_eq!(origins.len(), 1);
        assert!(origins[0].source.ends_with("base.rcfgv"));
        assert!(
            origins[0]
                .include_chain
                .iter()
                .any(|item| item.ends_with("root.rcfgv")),
            "expected include chain to contain root file"
        );

        let _ = std::fs::remove_file(&base);
        let _ = std::fs::remove_file(&root);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn analyze_values_from_path_runs_include_and_semantic_checks() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let tmp = std::env::temp_dir().join(format!(
            "rcfg_values_from_path_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let base = tmp.join("base.rcfgv");
        let root = tmp.join("root.rcfgv");

        std::fs::write(&base, "app::enabled = 1;\n").expect("write base");
        std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

        let (_expanded, diags) = super::analyze_values_from_path(&root, &symbols);
        assert!(
            diags.iter().any(|diag| diag.code == "E_TYPE_MISMATCH"),
            "expected E_TYPE_MISMATCH, got: {diags:#?}"
        );

        let _ = std::fs::remove_file(&base);
        let _ = std::fs::remove_file(&root);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn analyze_values_from_path_report_returns_origins() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let tmp = std::env::temp_dir().join(format!(
            "rcfg_report_origin_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let base = tmp.join("base.rcfgv");
        let root = tmp.join("root.rcfgv");
        std::fs::write(&base, "app::enabled = true;\n").expect("write base");
        std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

        let report = super::analyze_values_from_path_report(&root, &symbols);
        assert_eq!(report.values.stmts.len(), 1);
        assert_eq!(report.stmt_origins.len(), 1);
        assert_eq!(report.diagnostics.len(), report.diagnostic_stmt_indexes.len());
        assert!(report.stmt_origins[0].source.ends_with("base.rcfgv"));

        let _ = std::fs::remove_file(&base);
        let _ = std::fs::remove_file(&root);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn values_report_maps_semantic_diag_to_stmt_index() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let tmp = std::env::temp_dir().join(format!(
            "rcfg_diag_stmt_map_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let values = tmp.join("values.rcfgv");
        std::fs::write(&values, "app::enabled = 1;\n").expect("write values");

        let report = super::analyze_values_from_path_report(&values, &symbols);
        let index = report
            .diagnostics
            .iter()
            .position(|diag| diag.code == "E_TYPE_MISMATCH")
            .expect("expected E_TYPE_MISMATCH");
        assert_eq!(report.diagnostic_stmt_indexes[index], Some(0));

        let _ = std::fs::remove_file(&values);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn include_expander_attaches_source_to_parse_diagnostics() {
        let tmp = std::env::temp_dir().join(format!(
            "rcfg_parse_diag_source_{}",
            std::process::id()
        ));
        let _ = std::fs::create_dir_all(&tmp);

        let bad = tmp.join("bad.rcfgv");
        std::fs::write(&bad, "include ;\n").expect("write bad");

        let (_values, diags) = super::expand_values_includes_from_path(&bad);
        let diag = diags
            .iter()
            .find(|diag| diag.code == "E_PARSE_EXPECTED_TOKEN")
            .expect("expected parse diagnostic");
        assert!(
            diag.source.as_deref().is_some_and(|source| source.ends_with("bad.rcfgv")),
            "expected source to include bad.rcfgv, got: {diag:#?}"
        );
        assert!(
            !diag.include_chain.is_empty(),
            "expected include_chain in parse diagnostic"
        );

        let _ = std::fs::remove_file(&bad);
        let _ = std::fs::remove_dir(&tmp);
    }

    #[test]
    fn values_type_mismatch_diag_contains_option_path() {
        let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
        let symbols = symbols_from(schema_src);

        let values_src = r#"
app::enabled = 1;
"#;
        let (values, diags) = parse_values_with_diagnostics(values_src);
        assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

        let semantic_diags = super::analyze_values(&values, &symbols);
        let diag = semantic_diags
            .iter()
            .find(|diag| diag.code == "E_TYPE_MISMATCH")
            .expect("missing E_TYPE_MISMATCH");
        assert_eq!(diag.path.as_deref(), Some("app::enabled"));
    }
}
