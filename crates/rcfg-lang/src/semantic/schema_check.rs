use super::*;

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
    require_counters: HashMap<String, usize>,
}

impl<'a> TypeChecker<'a> {
    fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            diagnostics: Vec::new(),
            activation_edges: HashMap::new(),
            used_enum_variants: HashSet::new(),
            require_counters: HashMap::new(),
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
        let ordinal = next_require_ordinal(&mut self.require_counters, scope);
        let require_key = require_message_key(require, scope, ordinal);

        self.lint_require_msg(require, &require_key);
        self.expect_bool_expr(&require.expr, scope, self_ty);

        if self
            .evaluate_const_bool_expr(&require.expr)
            .is_some_and(|result| !result)
        {
            self.diagnostics.push(
                Diagnostic::error(
                    "E_REQUIRE_FAILED",
                    "require condition is statically false",
                    require.span,
                )
                .with_message_key(require_key),
            );
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
                op, left, right, ..
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
            .map(|path| path.as_str().to_string())
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

    fn lint_require_msg(&mut self, require: &crate::ast::RequireStmt, require_key: &str) {
        let has_msg = require
            .meta
            .attrs
            .iter()
            .any(|attr| matches!(attr.kind, AttrKind::Msg(_)));
        if !has_msg {
            self.diagnostics.push(
                Diagnostic::warning(
                    "L_REQUIRE_MISSING_MSG",
                    "require! is missing #[msg(\"...\")] for stable i18n key",
                    require.span,
                )
                .with_message_key(require_key.to_string())
                .with_note(format!("auto-generated require key: {}", require_key)),
            );
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
        let mut nodes = self.activation_edges.keys().cloned().collect::<Vec<_>>();
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

        let deps = self.activation_edges.get(node).cloned().unwrap_or_default();
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

        let Some(scrutinee_option_path) = self.match_scrutinee_option_path(&block.expr, scope)
        else {
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
                    format!(
                        "case overlaps with previous variants: {}",
                        overlap.join(", ")
                    ),
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

                    let Some(variant_name) = path.segments.last().map(|seg| seg.value.clone())
                    else {
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
                            format!(
                                "variant `{}` is not declared in enum `{}`",
                                variant_name, enum_name
                            ),
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
                    if !left_ty.is_unknown() && !elem_ty.is_unknown() && !left_ty.same_as(&elem_ty)
                    {
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
                if (!left.is_unknown() && !left.is_bool())
                    || (!right.is_unknown() && !right.is_bool())
                {
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
                if (!left.is_unknown() && !left.is_int())
                    || (!right.is_unknown() && !right.is_int())
                {
                    self.diagnostics.push(Diagnostic::error(
                        "E_EXPECT_INT",
                        "relational operators expect integer operands",
                        span,
                    ));
                }
                if let (Some(left_int), Some(right_int)) =
                    (left.concrete_int(), right.concrete_int())
                {
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
