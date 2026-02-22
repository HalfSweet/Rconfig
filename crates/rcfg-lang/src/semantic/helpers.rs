use super::*;

pub(super) fn promote_strict_diagnostics(diagnostics: &mut [Diagnostic]) {
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

pub(super) fn merge_dependency_paths(base: &[String], extra: &[String]) -> Vec<String> {
    let mut merged = Vec::with_capacity(base.len() + extra.len());
    merged.extend(base.iter().cloned());
    merged.extend(extra.iter().cloned());
    merged.sort();
    merged.dedup();
    merged
}

pub(super) fn canonical_cycle_signature(cycle: &[String]) -> String {
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
        if best.as_ref().is_none_or(|current| rotated < *current) {
            best = Some(rotated);
        }
    }

    best.unwrap_or_default()
}

pub(super) fn option_has_secret_attr(option: &OptionDecl) -> bool {
    option
        .meta
        .attrs
        .iter()
        .any(|attr| matches!(attr.kind, AttrKind::Secret))
}

pub(super) fn normalize_export_name_with_prefix(path: &str, prefix: &str) -> String {
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

pub(super) fn module_has_metadata(module: &crate::ast::ModDecl) -> bool {
    !module.meta.attrs.is_empty() || !module.meta.doc.is_empty()
}

pub(super) fn extract_range_attr(option: &OptionDecl) -> Option<IntRange> {
    option.meta.attrs.iter().find_map(|attr| {
        if let AttrKind::Range(range) = &attr.kind {
            Some(range.clone())
        } else {
            None
        }
    })
}

pub(super) fn option_type_to_value_type(ty: &Type) -> ValueType {
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

pub(super) fn build_full_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

pub(super) fn build_candidate_paths(scope: &[String], raw_path: &str) -> Vec<String> {
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

pub(super) fn expand_with_aliases(path: &Path, aliases: &HashMap<String, String>) -> String {
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

pub(super) fn path_matches(candidate: &str, raw_path: &str) -> bool {
    candidate == raw_path || candidate.ends_with(&format!("::{}", raw_path))
}

pub(super) fn parse_env_int(raw: &str) -> Option<i128> {
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

pub(super) fn require_scope_key(scope: &[String]) -> String {
    if scope.is_empty() {
        "root".to_string()
    } else {
        scope.join(".")
    }
}

pub(super) fn next_require_ordinal(
    counters: &mut HashMap<String, usize>,
    scope: &[String],
) -> usize {
    let key = require_scope_key(scope);
    let entry = counters.entry(key).or_insert(0);
    *entry += 1;
    *entry
}

pub(super) fn require_message_key(
    require: &crate::ast::RequireStmt,
    scope: &[String],
    ordinal: usize,
) -> String {
    if let Some(message) = require.meta.attrs.iter().find_map(|attr| match &attr.kind {
        AttrKind::Msg(value) => Some(value.clone()),
        _ => None,
    }) {
        return message;
    }

    format!("main.{}.require.{}", require_scope_key(scope), ordinal)
}

pub(super) fn redacted_int_arg(value: i128, secret: bool) -> DiagnosticArgValue {
    if secret {
        DiagnosticArgValue::String("[redacted]".to_string())
    } else {
        DiagnosticArgValue::Int(value)
    }
}

pub(super) fn int_type_name(int_type: IntType) -> &'static str {
    match int_type {
        IntType::U8 => "u8",
        IntType::U16 => "u16",
        IntType::U32 => "u32",
        IntType::I32 => "i32",
    }
}

pub(super) fn int_bounds_for_int_type(int_type: IntType) -> (i128, i128) {
    match int_type {
        IntType::U8 => (u8::MIN as i128, u8::MAX as i128),
        IntType::U16 => (u16::MIN as i128, u16::MAX as i128),
        IntType::U32 => (u32::MIN as i128, u32::MAX as i128),
        IntType::I32 => (i32::MIN as i128, i32::MAX as i128),
    }
}

pub(super) fn int_value_type_bounds(value_type: &ValueType) -> Option<(i128, i128)> {
    if let ValueType::Int(int_type) = value_type {
        Some(int_bounds_for_int_type(*int_type))
    } else {
        None
    }
}

pub(super) fn is_untyped_int_literal_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_, _) => true,
        Expr::Group { expr, .. } => is_untyped_int_literal_expr(expr),
        _ => false,
    }
}

pub(super) fn untyped_int_literal_value(expr: &Expr) -> Option<i128> {
    match expr {
        Expr::Int(value, _) => Some(*value),
        Expr::Group { expr, .. } => untyped_int_literal_value(expr),
        _ => None,
    }
}

pub(super) fn int_type_bounds(ty: &Type) -> Option<(i128, i128)> {
    match ty {
        Type::U8(_) => Some((u8::MIN as i128, u8::MAX as i128)),
        Type::U16(_) => Some((u16::MIN as i128, u16::MAX as i128)),
        Type::U32(_) => Some((u32::MIN as i128, u32::MAX as i128)),
        Type::I32(_) => Some((i32::MIN as i128, i32::MAX as i128)),
        _ => None,
    }
}

pub(super) fn enum_name_matches(expected: &str, candidate: &str) -> bool {
    if expected == candidate {
        return true;
    }

    let expected_base = expected.rsplit("::").next().unwrap_or(expected);
    let candidate_base = candidate.rsplit("::").next().unwrap_or(candidate);
    expected_base == candidate_base
}
