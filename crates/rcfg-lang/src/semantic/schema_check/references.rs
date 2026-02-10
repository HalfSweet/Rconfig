use super::*;
use crate::ast::{Attr, AttrArg, ExportStmt, RequireStmt, UseStmt};

pub(super) struct ReferenceCollector<'a> {
    symbols: &'a mut SymbolTable,
    aliases: HashMap<String, String>,
}

impl<'a> ReferenceCollector<'a> {
    pub(super) fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            aliases: HashMap::new(),
        }
    }

    pub(super) fn collect_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        for item in items {
            match item {
                Item::Use(use_stmt) => {
                    self.collect_attrs(&use_stmt.meta.attrs, scope);
                    self.record_symbol_path_reference(&use_stmt.path, scope);
                    self.register_use_alias(use_stmt);
                }
                Item::Mod(module) => {
                    self.collect_attrs(&module.meta.attrs, scope);
                    scope.push(module.name.value.clone());
                    self.collect_items(&module.items, scope);
                    scope.pop();
                }
                Item::Enum(enum_decl) => {
                    self.collect_attrs(&enum_decl.meta.attrs, scope);
                    for variant in &enum_decl.variants {
                        self.collect_attrs(&variant.meta.attrs, scope);
                    }
                }
                Item::Option(option) => {
                    self.collect_attrs(&option.meta.attrs, scope);

                    if let Type::Named(path) = &option.ty {
                        self.record_symbol_path_reference(path, scope);
                    }

                    if let Some(ConstValue::EnumPath(path)) = &option.default {
                        self.record_enum_variant_path_reference(path, scope);
                    }

                    if let Some(attached) = &option.attached_constraints {
                        self.collect_attrs(&attached.attrs, scope);
                        for require in &attached.requires {
                            self.collect_require(require, scope);
                        }
                    }
                }
                Item::Patch(patch) => {
                    self.collect_attrs(&patch.meta.attrs, scope);
                    self.record_symbol_path_reference(&patch.target, scope);

                    for stmt in &patch.stmts {
                        let PatchStmt::Default(default_stmt) = stmt;
                        self.record_option_path_reference(&default_stmt.path, scope);
                        if let ConstValue::EnumPath(path) = &default_stmt.value {
                            self.record_enum_variant_path_reference(path, scope);
                        }
                    }
                }
                Item::Export(export) => {
                    self.collect_attrs(&export.meta.attrs, scope);
                    for stmt in &export.stmts {
                        let ExportStmt::Set(set_stmt) = stmt;
                        if let ConstValue::EnumPath(path) = &set_stmt.value {
                            self.record_enum_variant_path_reference(path, scope);
                        }
                    }
                }
                Item::Require(require) => {
                    self.collect_require(require, scope);
                }
                Item::Constraint(constraint) => {
                    self.collect_attrs(&constraint.meta.attrs, scope);
                    for item in &constraint.items {
                        match item {
                            ConstraintItem::Require(require) => {
                                self.collect_require(require, scope);
                            }
                            ConstraintItem::Attr(attr) => {
                                self.collect_attr(attr, scope);
                            }
                            ConstraintItem::Doc(_) => {}
                        }
                    }
                }
                Item::When(when_block) => {
                    self.collect_attrs(&when_block.meta.attrs, scope);
                    self.collect_expr_references(&when_block.condition, scope);
                    self.collect_items(&when_block.items, scope);
                }
                Item::Match(match_block) => {
                    self.collect_attrs(&match_block.meta.attrs, scope);
                    self.collect_expr_references(&match_block.expr, scope);

                    for case in &match_block.cases {
                        self.collect_attrs(&case.meta.attrs, scope);
                        self.collect_match_pattern_references(&case.pattern, scope);
                        if let Some(guard) = &case.guard {
                            self.collect_expr_references(guard, scope);
                        }
                        self.collect_items(&case.items, scope);
                    }
                }
            }
        }
    }

    fn collect_require(&mut self, require: &RequireStmt, scope: &[String]) {
        self.collect_attrs(&require.meta.attrs, scope);
        self.collect_expr_references(&require.expr, scope);
    }

    fn collect_match_pattern_references(&mut self, pattern: &MatchPat, scope: &[String]) {
        let MatchPat::Paths(paths, _) = pattern else {
            return;
        };

        for path in paths {
            self.record_enum_variant_path_reference(path, scope);
        }
    }

    fn collect_attrs(&mut self, attrs: &[Attr], scope: &[String]) {
        for attr in attrs {
            self.collect_attr(attr, scope);
        }
    }

    fn collect_attr(&mut self, attr: &Attr, scope: &[String]) {
        match &attr.kind {
            AttrKind::Cfg(expr) => self.collect_expr_references(expr, scope),
            AttrKind::Other { args, .. } => {
                for arg in args {
                    if let AttrArg::Expr(expr) = arg {
                        self.collect_expr_references(expr, scope);
                    }
                }
            }
            AttrKind::Range(_) | AttrKind::Unit(_) | AttrKind::Msg(_) | AttrKind::Secret => {}
        }
    }

    fn collect_expr_references(&mut self, expr: &Expr, scope: &[String]) {
        match expr {
            Expr::Path(path) => self.record_path_reference(path, scope),
            Expr::Call { args, .. } => {
                for arg in args {
                    self.collect_expr_references(arg, scope);
                }
            }
            Expr::Unary { expr, .. } => {
                self.collect_expr_references(expr, scope);
            }
            Expr::Binary { left, right, .. } => {
                self.collect_expr_references(left, scope);
                self.collect_expr_references(right, scope);
            }
            Expr::InRange { expr, .. } => {
                self.collect_expr_references(expr, scope);
            }
            Expr::InSet { expr, elems, .. } => {
                self.collect_expr_references(expr, scope);
                for elem in elems {
                    if let InSetElem::Path(path) = elem {
                        self.record_path_reference(path, scope);
                    }
                }
            }
            Expr::Group { expr, .. } => {
                self.collect_expr_references(expr, scope);
            }
            Expr::Bool(_, _) | Expr::Int(_, _) | Expr::String(_, _) | Expr::SelfValue(_) => {}
        }
    }

    fn record_path_reference(&mut self, path: &Path, scope: &[String]) {
        let expanded = expand_with_aliases(path, &self.aliases);

        if let ResolveOptionPathResult::Resolved(option_path, _) = self
            .symbols
            .resolve_option_path_raw_in_scope(scope, &expanded)
        {
            self.symbols.insert_symbol_reference(option_path, path.span);
            return;
        }

        if let ResolveEnumVariantPathResult::Resolved(variant_path, _) = self
            .symbols
            .resolve_enum_variant_path_raw_in_scope(scope, &expanded)
        {
            self.symbols
                .insert_symbol_reference(variant_path, path.span);
            return;
        }

        if let Some(symbol_path) = self.resolve_symbol_path_raw_in_scope(scope, &expanded) {
            self.symbols.insert_symbol_reference(symbol_path, path.span);
        }
    }

    fn record_option_path_reference(&mut self, path: &Path, scope: &[String]) {
        let expanded = expand_with_aliases(path, &self.aliases);
        if let ResolveOptionPathResult::Resolved(option_path, _) = self
            .symbols
            .resolve_option_path_raw_in_scope(scope, &expanded)
        {
            self.symbols.insert_symbol_reference(option_path, path.span);
        }
    }

    fn record_enum_variant_path_reference(&mut self, path: &Path, scope: &[String]) {
        let expanded = expand_with_aliases(path, &self.aliases);
        match self
            .symbols
            .resolve_enum_variant_path_raw_in_scope(scope, &expanded)
        {
            ResolveEnumVariantPathResult::Resolved(variant_path, _) => {
                self.symbols
                    .insert_symbol_reference(variant_path, path.span);
            }
            ResolveEnumVariantPathResult::NotFound | ResolveEnumVariantPathResult::Ambiguous(_) => {
                let mut matches = self.symbols.resolve_enum_variant_paths(&expanded);
                matches.sort();
                matches.dedup();
                if matches.len() == 1 {
                    self.symbols
                        .insert_symbol_reference(matches[0].clone(), path.span);
                }
            }
        }
    }

    fn record_symbol_path_reference(&mut self, path: &Path, scope: &[String]) {
        let expanded = expand_with_aliases(path, &self.aliases);
        if let Some(symbol_path) = self.resolve_symbol_path_raw_in_scope(scope, &expanded) {
            self.symbols.insert_symbol_reference(symbol_path, path.span);
        }
    }

    fn resolve_symbol_path_raw_in_scope(&self, scope: &[String], raw_path: &str) -> Option<String> {
        let mut candidates = build_candidate_paths(scope, raw_path)
            .into_iter()
            .filter(|candidate| self.symbols.get(candidate).is_some())
            .collect::<Vec<_>>();
        candidates.sort();
        candidates.dedup();

        if candidates.len() == 1 {
            candidates.into_iter().next()
        } else {
            None
        }
    }

    fn register_use_alias(&mut self, use_stmt: &UseStmt) {
        let Some(alias) = &use_stmt.alias else {
            return;
        };
        self.aliases
            .insert(alias.value.clone(), use_stmt.path.to_string());
    }
}
