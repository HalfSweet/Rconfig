use std::collections::{BTreeMap, HashMap};

use rcfg_lang::AttrKind;

use crate::cli::render::{i18n_symbol_key, scoped_path, split_doc_summary_help};

pub(crate) fn collect_i18n_template_strings(
    schema: &rcfg_lang::File,
    package: &str,
) -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    let mut scope = Vec::new();
    let mut require_counters = HashMap::new();
    collect_item_i18n_strings(
        &schema.items,
        &mut scope,
        package,
        &mut require_counters,
        &mut out,
    );
    out
}

fn collect_item_i18n_strings(
    items: &[rcfg_lang::Item],
    scope: &mut Vec<String>,
    package: &str,
    require_counters: &mut HashMap<String, usize>,
    out: &mut BTreeMap<String, String>,
) {
    for item in items {
        match item {
            rcfg_lang::Item::Use(_) => {}
            rcfg_lang::Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                push_doc_i18n_strings(&path, &module.meta.doc, package, out);

                scope.push(module.name.value.clone());
                collect_item_i18n_strings(&module.items, scope, package, require_counters, out);
                scope.pop();
            }
            rcfg_lang::Item::Enum(enum_decl) => {
                let enum_path = scoped_path(scope, &enum_decl.name.value);
                push_doc_i18n_strings(&enum_path, &enum_decl.meta.doc, package, out);

                for variant in &enum_decl.variants {
                    let variant_path = format!("{}::{}", enum_path, variant.name.value);
                    push_doc_i18n_strings(&variant_path, &variant.meta.doc, package, out);
                }
            }
            rcfg_lang::Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                push_doc_i18n_strings(&path, &option.meta.doc, package, out);

                if let Some(attached) = &option.attached_constraints {
                    for require in &attached.requires {
                        push_require_i18n_string(require, scope, package, require_counters, out);
                    }
                }
            }
            rcfg_lang::Item::Require(require) => {
                push_require_i18n_string(require, scope, package, require_counters, out);
            }
            rcfg_lang::Item::Constraint(constraint) => {
                for item in &constraint.items {
                    if let rcfg_lang::ast::ConstraintItem::Require(require) = item {
                        push_require_i18n_string(require, scope, package, require_counters, out);
                    }
                }
            }
            rcfg_lang::Item::When(when_block) => {
                collect_item_i18n_strings(&when_block.items, scope, package, require_counters, out);
            }
            rcfg_lang::Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_item_i18n_strings(&case.items, scope, package, require_counters, out);
                }
            }
        }
    }
}

fn push_doc_i18n_strings(
    path: &str,
    doc: &[rcfg_lang::Spanned<String>],
    package: &str,
    out: &mut BTreeMap<String, String>,
) {
    let (summary, help) = split_doc_summary_help(doc);
    let label_key = i18n_symbol_key(package, path, "label");
    let help_key = i18n_symbol_key(package, path, "help");

    insert_i18n_string(out, label_key, summary.unwrap_or_default());
    insert_i18n_string(out, help_key, help.unwrap_or_default());
}

fn push_require_i18n_string(
    require: &rcfg_lang::ast::RequireStmt,
    scope: &[String],
    package: &str,
    require_counters: &mut HashMap<String, usize>,
    out: &mut BTreeMap<String, String>,
) {
    let ordinal = next_require_ordinal(require_counters, scope);
    let key = require_i18n_message_key(require, scope, ordinal, package);
    insert_i18n_string(out, key, "require condition failed".to_string());
}

fn insert_i18n_string(out: &mut BTreeMap<String, String>, key: String, fallback: String) {
    let entry = out.entry(key).or_default();
    if entry.is_empty() && !fallback.is_empty() {
        *entry = fallback;
    }
}

fn require_scope_key(scope: &[String]) -> String {
    if scope.is_empty() {
        "root".to_string()
    } else {
        scope.join(".")
    }
}

fn next_require_ordinal(counters: &mut HashMap<String, usize>, scope: &[String]) -> usize {
    let key = require_scope_key(scope);
    let entry = counters.entry(key).or_insert(0);
    *entry += 1;
    *entry
}

fn require_i18n_message_key(
    require: &rcfg_lang::ast::RequireStmt,
    scope: &[String],
    ordinal: usize,
    package: &str,
) -> String {
    if let Some(message) = require.meta.attrs.iter().find_map(|attr| match &attr.kind {
        AttrKind::Msg(value) => Some(value.clone()),
        _ => None,
    }) {
        return message;
    }

    format!("{}.{}.require.{}", package, require_scope_key(scope), ordinal)
}

pub(crate) fn render_i18n_template_toml(
    locale: &str,
    strings: &BTreeMap<String, String>,
) -> Result<String, String> {
    let locale = locale.trim();
    if locale.is_empty() {
        return Err("locale cannot be empty".to_string());
    }

    let mut string_map = toml::map::Map::new();
    for (key, value) in strings {
        string_map.insert(key.clone(), toml::Value::String(value.clone()));
    }

    let mut root = toml::map::Map::new();
    root.insert(
        "locale".to_string(),
        toml::Value::String(locale.to_string()),
    );
    root.insert("strings".to_string(), toml::Value::Table(string_map));

    let body = toml::to_string_pretty(&toml::Value::Table(root))
        .map_err(|err| format!("failed to serialize i18n template: {err}"))?;

    Ok(format!("# generated by rcfg i18n extract\n{body}"))
}
