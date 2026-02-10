use super::*;

#[derive(Debug, Clone, Copy)]
struct DeclInfo {
    kind: SymbolKind,
    span: Span,
}

#[derive(Default)]
pub(super) struct SymbolCollector {
    pub(super) symbols: SymbolTable,
    pub(super) diagnostics: Vec<Diagnostic>,
    scope_members: HashMap<String, HashMap<String, DeclInfo>>,
    mod_metadata_spans: HashMap<String, Span>,
}

impl SymbolCollector {
    pub(super) fn collect_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
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
