use std::collections::{BTreeMap, HashMap};

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
    let package = package_name.unwrap_or("main");

    let mut symbols_list = symbols.iter().collect::<Vec<_>>();
    symbols_list.sort_by(|(left, _), (right, _)| left.cmp(right));

    for (path, info) in symbols_list {
        let (summary, help) = doc_sections.get(path).cloned().unwrap_or((None, None));
        let entry = serde_json::json!({
            "path": path,
            "summary": summary,
            "help": help,
            "label_key": i18n_symbol_key(package, path, "label"),
            "help_key": i18n_symbol_key(package, path, "help"),
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
    format!("{}.{}.{}", package, path.replace("::", "."), suffix)
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
            | rcfg_lang::Item::Constraint(_) => {}
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
) -> serde_json::Value {
    let mut map = BTreeMap::new();
    for option in &resolved.options {
        let is_secret = symbols.option_is_secret(&option.path);
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
        option.path.clone(),
        serde_json::json!({
            "active": option.active,
            "type": option
                .value_type
                .as_ref()
                .map(value_type_to_json)
                .unwrap_or(serde_json::Value::Null),
            "source": option.source.map(|source| match source {
                rcfg_lang::ValueSource::User => "user",
                rcfg_lang::ValueSource::Default => "default",
                rcfg_lang::ValueSource::Context => "context",
            }),
                "data": value,
            }),
        );
    }
    serde_json::json!({"options": map})
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
