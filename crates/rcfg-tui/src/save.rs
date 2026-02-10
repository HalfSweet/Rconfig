use std::collections::BTreeMap;

use rcfg_app::AppSession;
use rcfg_lang::{ResolvedConfig, ResolvedValue, ValuesFile};

pub fn baseline_resolved(session: &AppSession) -> ResolvedConfig {
    session.resolve(&ValuesFile { stmts: Vec::new() })
}

pub fn build_minimal_overrides(
    baseline: &ResolvedConfig,
    current: &ResolvedConfig,
    user_values: &BTreeMap<String, ResolvedValue>,
) -> BTreeMap<String, ResolvedValue> {
    let baseline_values = baseline
        .options
        .iter()
        .map(|option| (option.path.clone(), option.value.clone()))
        .collect::<BTreeMap<_, _>>();

    let current_values = current
        .options
        .iter()
        .map(|option| (option.path.clone(), option.value.clone(), option.source))
        .collect::<Vec<_>>();

    let mut out = BTreeMap::new();

    for (path, current_value, source) in current_values {
        let Some(current_value) = current_value else {
            continue;
        };
        if source != Some(rcfg_lang::ValueSource::User) {
            continue;
        }

        if !user_values.contains_key(&path) {
            continue;
        }

        let baseline_value = baseline_values.get(&path).cloned().flatten();
        if baseline_value.as_ref() == Some(&current_value) {
            continue;
        }

        out.insert(path, current_value);
    }

    out
}

pub fn render_values(overrides: &BTreeMap<String, ResolvedValue>) -> String {
    let mut lines = Vec::new();
    for (path, value) in overrides {
        lines.push(format!("{} = {};", path, render_value(value)));
    }

    if lines.is_empty() {
        "\n".to_string()
    } else {
        format!("{}\n", lines.join("\n"))
    }
}

fn render_value(value: &ResolvedValue) -> String {
    match value {
        ResolvedValue::Bool(raw) => raw.to_string(),
        ResolvedValue::Int(raw) => raw.to_string(),
        ResolvedValue::String(raw) => format!("\"{}\"", escape_string(raw)),
        ResolvedValue::EnumVariant(raw) => raw.clone(),
    }
}

fn escape_string(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}
