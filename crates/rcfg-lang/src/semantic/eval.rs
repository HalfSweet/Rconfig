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
