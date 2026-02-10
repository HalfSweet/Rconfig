use std::collections::{BTreeMap, HashMap, HashSet};

use rcfg_lang::{IntType, ResolvedConfig, ResolvedValue, SymbolKind, ValueType};

pub(crate) fn render_schema_ir_json(
    symbols: &rcfg_lang::SymbolTable,
    schema: &rcfg_lang::File,
    package_name: Option<&str>,
) -> serde_json::Value {
    let mut options = Vec::new();
    let mut enums = Vec::new();
    let mut mods = Vec::new();
    let doc_sections = collect_doc_sections_index(schema);
    let i18n_overrides = collect_i18n_key_overrides_index(schema);
    let package = package_name.unwrap_or("main");

    let mut symbols_list = symbols.iter().collect::<Vec<_>>();
    symbols_list.sort_by(|(left, _), (right, _)| left.cmp(right));

    for (path, info) in symbols_list {
        let (summary, help) = doc_sections.get(path).cloned().unwrap_or((None, None));
        let (label_override, help_override) =
            i18n_overrides.get(path).cloned().unwrap_or((None, None));
        let label_key = label_override.unwrap_or_else(|| i18n_symbol_key(package, path, "label"));
        let help_key = help_override.unwrap_or_else(|| i18n_symbol_key(package, path, "help"));

        let path_with_package = with_package_prefix(path, package_name);
        let entry = serde_json::json!({
            "path": path_with_package,
            "summary": summary,
            "help": help,
            "label_key": label_key,
            "help_key": help_key,
        });
        match info.kind {
            SymbolKind::Option => options.push(entry),
            SymbolKind::Enum => enums.push(entry),
            SymbolKind::Mod => mods.push(entry),
        }
    }

    serde_json::json!({
        "tool_version": env!("CARGO_PKG_VERSION"),
        "symbols": {
            "mods": mods,
            "enums": enums,
            "options": options,
        }
    })
}

pub(crate) fn i18n_symbol_key(package: &str, path: &str, suffix: &str) -> String {
    let normalized = normalize_i18n_path(package, path);
    if normalized.is_empty() {
        format!("{}.{}", package, suffix)
    } else {
        format!("{}.{}.{}", package, normalized, suffix)
    }
}

pub(crate) fn resolve_i18n_keys(
    package: &str,
    path: &str,
    attrs: &[rcfg_lang::Attr],
) -> (String, String) {
    let (label_override, help_override) = i18n_key_overrides_from_attrs(attrs);

    let label_key = label_override.unwrap_or_else(|| i18n_symbol_key(package, path, "label"));
    let help_key = help_override.unwrap_or_else(|| i18n_symbol_key(package, path, "help"));

    (label_key, help_key)
}

pub(crate) fn i18n_key_overrides_from_attrs(
    attrs: &[rcfg_lang::Attr],
) -> (Option<String>, Option<String>) {
    (
        extract_i18n_key_override(attrs, "label_key"),
        extract_i18n_key_override(attrs, "help_key"),
    )
}

fn extract_i18n_key_override(attrs: &[rcfg_lang::Attr], name: &str) -> Option<String> {
    attrs
        .iter()
        .find_map(|attr| match &attr.kind {
            rcfg_lang::AttrKind::Other { name: attr_name, args } if attr_name == name => {
                args.first().and_then(|arg| match arg {
                    rcfg_lang::ast::AttrArg::Str(value) if !value.trim().is_empty() => {
                        Some(value.clone())
                    }
                    _ => None,
                })
            }
            _ => None,
        })
}

fn collect_i18n_key_overrides_index(
    schema: &rcfg_lang::File,
) -> HashMap<String, (Option<String>, Option<String>)> {
    let mut out = HashMap::new();
    let mut scope = Vec::new();
    collect_item_i18n_key_overrides(&schema.items, &mut scope, &mut out);
    out
}

fn collect_item_i18n_key_overrides(
    items: &[rcfg_lang::Item],
    scope: &mut Vec<String>,
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
) {
    for item in items {
        match item {
            rcfg_lang::Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                let (label_key, help_key) = i18n_key_overrides_from_attrs(&module.meta.attrs);
                insert_i18n_key_overrides(out, path, label_key, help_key);

                scope.push(module.name.value.clone());
                collect_item_i18n_key_overrides(&module.items, scope, out);
                scope.pop();
            }
            rcfg_lang::Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                let (label_key, help_key) = i18n_key_overrides_from_attrs(&enum_decl.meta.attrs);
                insert_i18n_key_overrides(out, path, label_key, help_key);
            }
            rcfg_lang::Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                let (label_key, help_key) = i18n_key_overrides_from_attrs(&option.meta.attrs);
                insert_i18n_key_overrides(out, path, label_key, help_key);
            }
            rcfg_lang::Item::When(when_block) => {
                collect_item_i18n_key_overrides(&when_block.items, scope, out);
            }
            rcfg_lang::Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_item_i18n_key_overrides(&case.items, scope, out);
                }
            }
            rcfg_lang::Item::Use(_)
            | rcfg_lang::Item::Require(_)
            | rcfg_lang::Item::Constraint(_)
            | rcfg_lang::Item::Patch(_)
            | rcfg_lang::Item::Export(_) => {}
        }
    }
}

fn insert_i18n_key_overrides(
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
    path: String,
    label_key: Option<String>,
    help_key: Option<String>,
) {
    let entry = out.entry(path).or_insert((None, None));
    if entry.0.is_none() && label_key.is_some() {
        entry.0 = label_key;
    }
    if entry.1.is_none() && help_key.is_some() {
        entry.1 = help_key;
    }
}

fn normalize_i18n_path(package: &str, path: &str) -> String {
    let stripped = if path == package {
        ""
    } else if let Some(rest) = path.strip_prefix(&format!("{}::", package)) {
        rest
    } else {
        path
    };

    stripped.replace("::", ".")
}

fn collect_doc_sections_index(
    schema: &rcfg_lang::File,
) -> HashMap<String, (Option<String>, Option<String>)> {
    let mut out = HashMap::new();
    let mut scope = Vec::new();
    collect_item_doc_sections(&schema.items, &mut scope, &mut out);
    out
}

fn collect_item_doc_sections(
    items: &[rcfg_lang::Item],
    scope: &mut Vec<String>,
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
) {
    for item in items {
        match item {
            rcfg_lang::Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                let (summary, help) = split_doc_summary_help(&module.meta.doc);
                insert_doc_sections(out, path, summary, help);

                scope.push(module.name.value.clone());
                collect_item_doc_sections(&module.items, scope, out);
                scope.pop();
            }
            rcfg_lang::Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                let (summary, help) = split_doc_summary_help(&enum_decl.meta.doc);
                insert_doc_sections(out, path, summary, help);
            }
            rcfg_lang::Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                let (summary, help) = split_doc_summary_help(&option.meta.doc);
                insert_doc_sections(out, path, summary, help);
            }
            rcfg_lang::Item::When(when_block) => {
                collect_item_doc_sections(&when_block.items, scope, out);
            }
            rcfg_lang::Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_item_doc_sections(&case.items, scope, out);
                }
            }
            rcfg_lang::Item::Use(_)
            | rcfg_lang::Item::Require(_)
            | rcfg_lang::Item::Constraint(_)
            | rcfg_lang::Item::Patch(_)
            | rcfg_lang::Item::Export(_) => {}
        }
    }
}

fn insert_doc_sections(
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
    path: String,
    summary: Option<String>,
    help: Option<String>,
) {
    let entry = out.entry(path).or_insert((None, None));
    if entry.0.is_none() && summary.is_some() {
        entry.0 = summary;
    }
    if entry.1.is_none() && help.is_some() {
        entry.1 = help;
    }
}

pub(crate) fn split_doc_summary_help(
    doc: &[rcfg_lang::Spanned<String>],
) -> (Option<String>, Option<String>) {
    let mut summary_lines = Vec::new();
    let mut help_lines = Vec::new();
    let mut in_help = false;

    for line in doc.iter().map(|entry| entry.value.trim_end().to_string()) {
        if !in_help {
            if line.trim().is_empty() {
                if !summary_lines.is_empty() {
                    in_help = true;
                }
                continue;
            }
            summary_lines.push(line);
            continue;
        }

        help_lines.push(line);
    }

    while help_lines
        .first()
        .is_some_and(|line| line.trim().is_empty())
    {
        help_lines.remove(0);
    }
    while help_lines.last().is_some_and(|line| line.trim().is_empty()) {
        help_lines.pop();
    }

    let summary = if summary_lines.is_empty() {
        None
    } else {
        Some(summary_lines.join("\n"))
    };
    let help = if help_lines.is_empty() {
        None
    } else {
        Some(help_lines.join("\n"))
    };

    (summary, help)
}

pub(crate) fn scoped_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

pub(crate) fn render_resolved_json(
    resolved: &ResolvedConfig,
    include_secrets: bool,
    symbols: &rcfg_lang::SymbolTable,
    package_name: Option<&str>,
) -> serde_json::Value {
    let mut map = BTreeMap::new();
    let secret_paths = resolved
        .options
        .iter()
        .filter(|option| symbols.option_is_secret(&option.path))
        .map(|option| option.path.as_str())
        .collect::<HashSet<_>>();

    for option in &resolved.options {
        let is_secret = secret_paths.contains(option.path.as_str());
        let path_with_package = with_package_prefix(option.path.as_str(), package_name);
        let value = if is_secret && !include_secrets {
            serde_json::json!({"redacted": true, "value": null})
        } else {
            serde_json::json!({
                "redacted": false,
                "value": option
                    .value
                    .as_ref()
                    .map(resolved_value_to_json)
                    .unwrap_or(serde_json::Value::Null),
            })
        };
        map.insert(
            path_with_package,
            serde_json::json!({
                "active": option.active,
                "type": option
                    .value_type
                    .as_ref()
                    .map(value_type_to_json)
                    .unwrap_or(serde_json::Value::Null),
                "source": option.source.map(|source| match source {
                    rcfg_lang::ValueSource::User => "user",
                    rcfg_lang::ValueSource::Patch => "patch",
                    rcfg_lang::ValueSource::Default => "default",
                    rcfg_lang::ValueSource::Context => "context",
                }),
                "data": value,
            }),
        );
    }
    serde_json::json!({"options": map})
}

fn with_package_prefix(path: &str, package_name: Option<&str>) -> String {
    let Some(package_name) = package_name else {
        return path.to_string();
    };
    if path == "ctx" || path.starts_with("ctx::") || path == package_name {
        return path.to_string();
    }

    let prefix = format!("{}::", package_name);
    if path.starts_with(&prefix) {
        path.to_string()
    } else {
        format!("{}{}", prefix, path)
    }
}

fn resolved_value_to_json(value: &ResolvedValue) -> serde_json::Value {
    match value {
        ResolvedValue::Bool(raw) => serde_json::json!(raw),
        ResolvedValue::Int(raw) => serde_json::json!(raw),
        ResolvedValue::String(raw) => serde_json::json!(raw),
        ResolvedValue::EnumVariant(raw) => serde_json::json!(raw),
    }
}

fn value_type_to_json(value_type: &ValueType) -> serde_json::Value {
    match value_type {
        ValueType::Bool => serde_json::json!("bool"),
        ValueType::Int(int_type) => serde_json::json!(match int_type {
            IntType::U8 => "u8",
            IntType::U16 => "u16",
            IntType::U32 => "u32",
            IntType::I32 => "i32",
        }),
        ValueType::UntypedInt => serde_json::json!("untyped-int"),
        ValueType::String => serde_json::json!("string"),
        ValueType::Enum(name) => serde_json::json!(format!("enum:{name}")),
        ValueType::Unknown => serde_json::json!("unknown"),
    }
}
