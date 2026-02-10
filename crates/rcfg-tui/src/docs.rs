use std::collections::BTreeMap;

use rcfg_lang::{Attr, AttrKind, File, Item, Spanned};

#[derive(Debug, Clone, Default)]
pub struct NodeDocEntry {
    pub summary: Option<String>,
    pub help: Option<String>,
    pub label_key: Option<String>,
    pub help_key: Option<String>,
}

pub fn build_doc_index(schema: &File) -> BTreeMap<String, NodeDocEntry> {
    let mut out = BTreeMap::new();
    let mut scope = Vec::new();
    collect_docs(&schema.items, &mut scope, &mut out);
    out
}

pub fn i18n_symbol_key(package: &str, path: &str, suffix: &str) -> String {
    let normalized = normalize_i18n_path(package, path);
    if normalized.is_empty() {
        format!("{}.{}", package, suffix)
    } else {
        format!("{}.{}.{}", package, normalized, suffix)
    }
}

fn collect_docs(items: &[Item], scope: &mut Vec<String>, out: &mut BTreeMap<String, NodeDocEntry>) {
    for item in items {
        match item {
            Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                insert_entry(out, path.clone(), &module.meta.doc, &module.meta.attrs);

                scope.push(module.name.value.clone());
                collect_docs(&module.items, scope, out);
                scope.pop();
            }
            Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                insert_entry(out, path, &enum_decl.meta.doc, &enum_decl.meta.attrs);
            }
            Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                insert_entry(out, path, &option.meta.doc, &option.meta.attrs);
            }
            Item::When(when_block) => {
                collect_docs(&when_block.items, scope, out);
            }
            Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_docs(&case.items, scope, out);
                }
            }
            Item::Use(_)
            | Item::Require(_)
            | Item::Constraint(_)
            | Item::Patch(_)
            | Item::Export(_) => {}
        }
    }
}

fn insert_entry(
    out: &mut BTreeMap<String, NodeDocEntry>,
    path: String,
    doc: &[Spanned<String>],
    attrs: &[Attr],
) {
    let (summary, help) = split_doc_summary_help(doc);
    let (label_key, help_key) = i18n_key_overrides_from_attrs(attrs);

    let entry = out.entry(path).or_default();
    if entry.summary.is_none() {
        entry.summary = summary;
    }
    if entry.help.is_none() {
        entry.help = help;
    }
    if entry.label_key.is_none() {
        entry.label_key = label_key;
    }
    if entry.help_key.is_none() {
        entry.help_key = help_key;
    }
}

fn i18n_key_overrides_from_attrs(attrs: &[Attr]) -> (Option<String>, Option<String>) {
    (
        extract_i18n_key_override(attrs, "label_key"),
        extract_i18n_key_override(attrs, "help_key"),
    )
}

fn extract_i18n_key_override(attrs: &[Attr], name: &str) -> Option<String> {
    attrs.iter().find_map(|attr| match &attr.kind {
        AttrKind::Other {
            name: attr_name,
            args,
        } if attr_name == name => args.first().and_then(|arg| match arg {
            rcfg_lang::ast::AttrArg::Str(value) if !value.trim().is_empty() => Some(value.clone()),
            _ => None,
        }),
        _ => None,
    })
}

fn split_doc_summary_help(doc: &[Spanned<String>]) -> (Option<String>, Option<String>) {
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

fn scoped_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}
