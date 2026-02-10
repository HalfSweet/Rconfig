use super::*;

pub(super) fn evaluate_runtime_state(
    symbols: &SymbolTable,
    assignments: &BTreeMap<String, ResolvedAssignment>,
) -> RuntimeState {
    let mut values = assignments
        .iter()
        .map(|(path, assignment)| (path.clone(), assignment.value.clone()))
        .collect::<BTreeMap<_, _>>();
    let mut sources = assignments
        .iter()
        .map(|(path, assignment)| (path.clone(), assignment.source))
        .collect::<BTreeMap<_, _>>();

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
        .collect::<BTreeSet<_>>();
    let mut ctx_references = BTreeSet::new();

    let mut changed = true;
    let mut guard = 0usize;
    while changed && guard < 64 {
        guard += 1;
        let snapshot = RuntimeState {
            active: active.clone(),
            values: values.clone(),
            sources: sources.clone(),
            ctx_references: BTreeSet::new(),
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

pub(super) fn evaluate_activation_once(
    symbols: &SymbolTable,
    runtime: &RuntimeState,
) -> RuntimeState {
    let mut active = symbols
        .option_types
        .keys()
        .filter(|path| symbols.option_is_always_active(path))
        .cloned()
        .collect::<BTreeSet<_>>();
    let mut scope = Vec::new();
    let mut ctx_references = BTreeSet::new();
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

pub(super) fn collect_active_options(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    runtime: &RuntimeState,
    guard_active: bool,
    active: &mut BTreeSet<String>,
    ctx_references: &mut BTreeSet<String>,
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
                let selected =
                    select_match_case_index(&match_block, symbols, scope, runtime, ctx_references);
                for (index, case) in match_block.cases.iter().enumerate() {
                    let case_active =
                        selected.is_some_and(|selected_index| selected_index == index);
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

pub(super) fn select_match_case_index(
    block: &MatchBlock,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut BTreeSet<String>,
) -> Option<usize> {
    let scrutinee = eval_expr(&block.expr, symbols, scope, runtime, ctx_references, true)?;

    for (index, case) in block.cases.iter().enumerate() {
        if !match_pattern_matches(
            &case.pattern,
            &scrutinee,
            symbols,
            scope,
            runtime,
            ctx_references,
        ) {
            continue;
        }

        if let Some(guard) = &case.guard {
            if !eval_expr_as_bool(guard, symbols, scope, runtime, ctx_references).unwrap_or(false) {
                return None;
            }
        }

        return Some(index);
    }
    None
}

pub(super) fn match_pattern_matches(
    pattern: &MatchPat,
    scrutinee: &ResolvedValue,
    symbols: &SymbolTable,
    scope: &[String],
    runtime: &RuntimeState,
    ctx_references: &mut BTreeSet<String>,
) -> bool {
    match pattern {
        MatchPat::Wildcard(_) => true,
        MatchPat::Paths(paths, _) => paths.iter().any(|path| {
            eval_path_as_enum_variant(path, symbols, scope, runtime, ctx_references).is_some_and(
                |variant| {
                    scrutinee
                        .as_enum_variant()
                        .is_some_and(|current| current == variant)
                },
            )
        }),
    }
}

pub(super) fn build_resolved_config(
    symbols: &SymbolTable,
    runtime: &RuntimeState,
) -> ResolvedConfig {
    let mut paths = symbols.option_types.keys().cloned().collect::<Vec<_>>();
    paths.sort();

    let options = paths
        .into_iter()
        .map(|path| ResolvedOption {
            active: runtime.active.contains(&path),
            value_type: symbols.option_type(&path).cloned(),
            value: runtime.values.get(&path).cloned(),
            source: runtime.sources.get(&path).copied(),
            path,
        })
        .collect::<Vec<_>>();

    ResolvedConfig { options }
}

pub(super) fn resolve_const_value(
    value: &ConstValue,
    symbols: &SymbolTable,
) -> Option<ResolvedValue> {
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

pub(super) fn collect_runtime_require_diagnostics(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    runtime: &RuntimeState,
    diagnostics: &mut Vec<(Diagnostic, Option<usize>)>,
    require_counters: &mut HashMap<String, usize>,
) {
    for item in items {
        match item {
            Item::Use(_) | Item::Enum(_) | Item::Option(_) => {}
            Item::Require(require) => {
                let ordinal = next_require_ordinal(require_counters, scope);
                let require_key = require_message_key(require, scope, ordinal);
                if !eval_expr_as_bool(&require.expr, symbols, scope, runtime, &mut BTreeSet::new())
                    .unwrap_or(false)
                {
                    diagnostics.push((
                        Diagnostic::error(
                            "E_REQUIRE_FAILED",
                            "require condition failed at runtime",
                            require.span,
                        )
                        .with_message_key(require_key),
                        None,
                    ));
                }
            }
            Item::Constraint(constraint) => {
                for child in &constraint.items {
                    if let ConstraintItem::Require(require) = child {
                        let ordinal = next_require_ordinal(require_counters, scope);
                        let require_key = require_message_key(require, scope, ordinal);
                        if !eval_expr_as_bool(
                            &require.expr,
                            symbols,
                            scope,
                            runtime,
                            &mut BTreeSet::new(),
                        )
                        .unwrap_or(false)
                        {
                            diagnostics.push((
                                Diagnostic::error(
                                    "E_REQUIRE_FAILED",
                                    "require condition failed at runtime",
                                    require.span,
                                )
                                .with_message_key(require_key),
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
                    require_counters,
                );
                scope.pop();
            }
            Item::When(when_block) => {
                if eval_expr_as_bool(
                    &when_block.condition,
                    symbols,
                    scope,
                    runtime,
                    &mut BTreeSet::new(),
                )
                .unwrap_or(false)
                {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &when_block.items,
                        scope,
                        runtime,
                        diagnostics,
                        require_counters,
                    );
                }
            }
            Item::Match(match_block) => {
                if let Some(index) = select_match_case_index(
                    match_block,
                    symbols,
                    scope,
                    runtime,
                    &mut BTreeSet::new(),
                ) {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &match_block.cases[index].items,
                        scope,
                        runtime,
                        diagnostics,
                        require_counters,
                    );
                }
            }
        }
    }
}
