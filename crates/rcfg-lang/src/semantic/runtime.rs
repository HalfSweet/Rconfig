use super::*;

pub(super) fn evaluate_runtime_state(
    symbols: &SymbolTable,
    assignments: &BTreeMap<String, ResolvedAssignment>,
) -> RuntimeState {
    let (mut values, mut sources) = compose_runtime_values(symbols, assignments, &BTreeMap::new());
    let (mut active, mut ctx_references) = evaluate_activation_fixpoint(symbols, &values, &sources);

    let mut guard = 0usize;
    while guard < 64 {
        guard += 1;

        let snapshot = RuntimeState {
            active: active.clone(),
            values: values.clone(),
            sources: sources.clone(),
            ctx_references: BTreeSet::new(),
        };
        let (patch_defaults, patch_ctx_references) =
            collect_runtime_patch_defaults(symbols, &snapshot);

        let (next_values, next_sources) =
            compose_runtime_values(symbols, assignments, &patch_defaults);
        let (next_active, mut next_ctx_references) =
            evaluate_activation_fixpoint(symbols, &next_values, &next_sources);
        next_ctx_references.extend(patch_ctx_references);

        if next_values == values && next_sources == sources && next_active == active {
            ctx_references = next_ctx_references;
            break;
        }

        values = next_values;
        sources = next_sources;
        active = next_active;
        ctx_references = next_ctx_references;
    }

    RuntimeState {
        active,
        values,
        sources,
        ctx_references,
    }
}

fn compose_runtime_values(
    symbols: &SymbolTable,
    assignments: &BTreeMap<String, ResolvedAssignment>,
    patch_defaults: &BTreeMap<String, ResolvedValue>,
) -> (
    BTreeMap<String, ResolvedValue>,
    BTreeMap<String, ValueSource>,
) {
    let mut values = BTreeMap::new();
    let mut sources = BTreeMap::new();

    for (path, default) in &symbols.option_defaults {
        if let Some(value) = resolve_const_value(default, symbols) {
            values.insert(path.as_str().to_string(), value);
            sources.insert(path.as_str().to_string(), ValueSource::Default);
        }
    }

    for (path, value) in patch_defaults {
        values.insert(path.clone(), value.clone());
        sources.insert(path.clone(), ValueSource::Patch);
    }

    for (path, assignment) in assignments {
        values.insert(path.clone(), assignment.value.clone());
        sources.insert(path.clone(), assignment.source);
    }

    (values, sources)
}

fn evaluate_activation_fixpoint(
    symbols: &SymbolTable,
    values: &BTreeMap<String, ResolvedValue>,
    sources: &BTreeMap<String, ValueSource>,
) -> (BTreeSet<String>, BTreeSet<String>) {
    let mut active = symbols
        .option_types
        .keys()
        .filter(|path| symbols.option_is_always_active(path.as_str()))
        .map(|path| path.as_str().to_string())
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

    (active, ctx_references)
}

fn collect_runtime_patch_defaults(
    symbols: &SymbolTable,
    runtime: &RuntimeState,
) -> (BTreeMap<String, ResolvedValue>, BTreeSet<String>) {
    let mut patch_defaults = BTreeMap::new();
    let mut ctx_references = BTreeSet::new();
    let mut scope = Vec::new();
    let mut aliases = HashMap::new();
    collect_patch_defaults_from_items(
        symbols,
        symbols.schema_items(),
        &mut scope,
        &mut aliases,
        runtime,
        true,
        &mut patch_defaults,
        &mut ctx_references,
    );
    (patch_defaults, ctx_references)
}

fn collect_patch_defaults_from_items(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    aliases: &mut HashMap<String, String>,
    runtime: &RuntimeState,
    guard_active: bool,
    patch_defaults: &mut BTreeMap<String, ResolvedValue>,
    ctx_references: &mut BTreeSet<String>,
) {
    for item in items {
        match item {
            Item::Use(use_stmt) => {
                if let Some(alias) = &use_stmt.alias {
                    aliases.insert(alias.value.clone(), use_stmt.path.to_string());
                }
            }
            Item::Require(_)
            | Item::Constraint(_)
            | Item::Enum(_)
            | Item::Option(_)
            | Item::Export(_) => {}
            Item::Patch(patch) => {
                if !guard_active {
                    continue;
                }

                let Some(target_prefix) =
                    resolve_patch_target_path(symbols, scope, &patch.target, aliases)
                else {
                    continue;
                };

                for stmt in &patch.stmts {
                    match stmt {
                        PatchStmt::Default(default_stmt) => {
                            let Some(option_path) = resolve_patch_default_option_path(
                                symbols,
                                &target_prefix,
                                &default_stmt.path,
                                aliases,
                            ) else {
                                continue;
                            };

                            if let Some(value) = resolve_const_value(&default_stmt.value, symbols) {
                                patch_defaults.insert(option_path, value);
                            }
                        }
                    }
                }
            }
            Item::Mod(module) => {
                scope.push(module.name.value.clone());
                collect_patch_defaults_from_items(
                    symbols,
                    &module.items,
                    scope,
                    aliases,
                    runtime,
                    guard_active,
                    patch_defaults,
                    ctx_references,
                );
                scope.pop();
            }
            Item::When(when_block) => {
                let cond = eval_expr_as_bool(
                    &when_block.condition,
                    symbols,
                    scope,
                    aliases,
                    runtime,
                    ctx_references,
                )
                .unwrap_or(false);
                collect_patch_defaults_from_items(
                    symbols,
                    &when_block.items,
                    scope,
                    aliases,
                    runtime,
                    guard_active && cond,
                    patch_defaults,
                    ctx_references,
                );
            }
            Item::Match(match_block) => {
                let selected = select_match_case_index(
                    match_block,
                    symbols,
                    scope,
                    aliases,
                    runtime,
                    ctx_references,
                );
                if let Some(index) = selected {
                    collect_patch_defaults_from_items(
                        symbols,
                        &match_block.cases[index].items,
                        scope,
                        aliases,
                        runtime,
                        guard_active,
                        patch_defaults,
                        ctx_references,
                    );
                }
            }
        }
    }
}

fn resolve_patch_target_path(
    symbols: &SymbolTable,
    scope: &[String],
    target: &Path,
    aliases: &HashMap<String, String>,
) -> Option<String> {
    let expanded = expand_with_aliases(target, aliases);
    let mut matches = build_candidate_paths(scope, &expanded)
        .into_iter()
        .filter(|candidate| symbols.get(candidate).is_some())
        .collect::<Vec<_>>();
    matches.sort();
    matches.dedup();

    if matches.len() == 1 {
        Some(matches.remove(0))
    } else {
        None
    }
}

fn resolve_patch_default_option_path(
    symbols: &SymbolTable,
    target_prefix: &str,
    path: &Path,
    aliases: &HashMap<String, String>,
) -> Option<String> {
    let expanded = expand_with_aliases(path, aliases);
    let mut nested = symbols.resolve_option_paths(&format!("{}::{}", target_prefix, expanded));
    if nested.len() == 1 {
        return Some(nested.remove(0));
    }

    let mut direct = symbols.resolve_option_paths(&expanded);
    if direct.len() == 1 {
        return Some(direct.remove(0));
    }

    None
}

pub(super) fn evaluate_activation_once(
    symbols: &SymbolTable,
    runtime: &RuntimeState,
) -> RuntimeState {
    let mut active = symbols
        .option_types
        .keys()
        .filter(|path| symbols.option_is_always_active(path.as_str()))
        .map(|path| path.as_str().to_string())
        .collect::<BTreeSet<_>>();
    let mut scope = Vec::new();
    let mut aliases = HashMap::new();
    let mut ctx_references = BTreeSet::new();
    collect_active_options(
        symbols,
        symbols.schema_items(),
        &mut scope,
        &mut aliases,
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
    aliases: &mut HashMap<String, String>,
    runtime: &RuntimeState,
    guard_active: bool,
    active: &mut BTreeSet<String>,
    ctx_references: &mut BTreeSet<String>,
) {
    for item in items {
        match item {
            Item::Use(use_stmt) => {
                if let Some(alias) = &use_stmt.alias {
                    aliases.insert(alias.value.clone(), use_stmt.path.to_string());
                }
            }
            Item::Require(_)
            | Item::Constraint(_)
            | Item::Enum(_)
            | Item::Patch(_)
            | Item::Export(_) => {}
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
                    aliases,
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
                    aliases,
                    runtime,
                    ctx_references,
                )
                .unwrap_or(false);
                collect_active_options(
                    symbols,
                    &when_block.items,
                    scope,
                    aliases,
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
                    aliases,
                    runtime,
                    ctx_references,
                );
                for (index, case) in match_block.cases.iter().enumerate() {
                    let case_active =
                        selected.is_some_and(|selected_index| selected_index == index);
                    collect_active_options(
                        symbols,
                        &case.items,
                        scope,
                        aliases,
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
    aliases: &HashMap<String, String>,
    runtime: &RuntimeState,
    ctx_references: &mut BTreeSet<String>,
) -> Option<usize> {
    let scrutinee = eval_expr(
        &block.expr,
        symbols,
        scope,
        aliases,
        runtime,
        ctx_references,
        true,
    )?;

    for (index, case) in block.cases.iter().enumerate() {
        if !match_pattern_matches(
            &case.pattern,
            &scrutinee,
            symbols,
            scope,
            aliases,
            runtime,
            ctx_references,
        ) {
            continue;
        }

        if let Some(guard) = &case.guard {
            if !eval_expr_as_bool(guard, symbols, scope, aliases, runtime, ctx_references)
                .unwrap_or(false)
            {
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
    aliases: &HashMap<String, String>,
    runtime: &RuntimeState,
    ctx_references: &mut BTreeSet<String>,
) -> bool {
    match pattern {
        MatchPat::Wildcard(_) => true,
        MatchPat::Paths(paths, _) => paths.iter().any(|path| {
            eval_path_as_enum_variant(path, symbols, scope, aliases, runtime, ctx_references)
                .is_some_and(|variant| {
                    scrutinee
                        .as_enum_variant()
                        .is_some_and(|current| current == variant)
                })
        }),
    }
}

pub(super) fn build_resolved_config(
    symbols: &SymbolTable,
    runtime: &RuntimeState,
) -> ResolvedConfig {
    let mut paths = symbols
        .option_types
        .keys()
        .map(|path| path.as_str().to_string())
        .collect::<Vec<_>>();
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
    aliases: &mut HashMap<String, String>,
    runtime: &RuntimeState,
    diagnostics: &mut Vec<(Diagnostic, Option<usize>)>,
    require_counters: &mut HashMap<String, usize>,
) {
    for item in items {
        match item {
            Item::Use(use_stmt) => {
                if let Some(alias) = &use_stmt.alias {
                    aliases.insert(alias.value.clone(), use_stmt.path.to_string());
                }
            }
            Item::Enum(_) | Item::Option(_) | Item::Patch(_) | Item::Export(_) => {}
            Item::Require(require) => {
                let ordinal = next_require_ordinal(require_counters, scope);
                let require_key = require_message_key(require, scope, ordinal);
                if !eval_expr_as_bool(
                    &require.expr,
                    symbols,
                    scope,
                    aliases,
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
            Item::Constraint(constraint) => {
                for child in &constraint.items {
                    if let ConstraintItem::Require(require) = child {
                        let ordinal = next_require_ordinal(require_counters, scope);
                        let require_key = require_message_key(require, scope, ordinal);
                        if !eval_expr_as_bool(
                            &require.expr,
                            symbols,
                            scope,
                            aliases,
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
                    aliases,
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
                    aliases,
                    runtime,
                    &mut BTreeSet::new(),
                )
                .unwrap_or(false)
                {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &when_block.items,
                        scope,
                        aliases,
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
                    aliases,
                    runtime,
                    &mut BTreeSet::new(),
                ) {
                    collect_runtime_require_diagnostics(
                        symbols,
                        &match_block.cases[index].items,
                        scope,
                        aliases,
                        runtime,
                        diagnostics,
                        require_counters,
                    );
                }
            }
        }
    }
}
