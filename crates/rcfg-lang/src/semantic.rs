use std::collections::{HashMap, HashSet};

use crate::ast::{
    BinaryOp, ConstraintItem, Expr, File, InSetElem, Item, MatchBlock, MatchPat, OptionDecl,
    Path, Type, UnaryOp,
};
use crate::error::Diagnostic;
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
}

#[derive(Debug, Clone)]
pub struct SemanticReport {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
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
    use crate::parser::parse_schema_with_diagnostics;

    use super::{analyze_schema, SymbolKind};

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
}
