use std::fs;
use std::path::Path;

use rcfg_lang::{Diagnostic, Severity};

use crate::cli::args::OutputFormat;
use crate::cli::loaders::I18nCatalog;

fn diagnostic_arg_to_json(value: &rcfg_lang::DiagnosticArgValue) -> serde_json::Value {
    match value {
        rcfg_lang::DiagnosticArgValue::Bool(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::Int(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::String(raw) => serde_json::json!(raw),
    }
}

fn diagnostic_to_json(diag: &Diagnostic) -> serde_json::Value {
    let args = diag
        .args
        .iter()
        .map(|(key, value)| (key.clone(), diagnostic_arg_to_json(value)))
        .collect::<serde_json::Map<_, _>>();
    let related = diag
        .related
        .iter()
        .map(|item| {
            serde_json::json!({
                "message": item.message,
                "path": item.path,
                "span": {
                    "start": item.span.start,
                    "end": item.span.end,
                }
            })
        })
        .collect::<Vec<_>>();

    serde_json::json!({
        "severity": match diag.severity { Severity::Error => "error", Severity::Warning => "warning" },
        "code": diag.code,
        "message": diag.message,
        "message_key": diag.message_key,
        "note": diag.note,
        "args": args,
        "related": related,
        "path": diag.path,
        "source": diag.source,
        "include_chain": diag.include_chain,
        "span": {
            "start": diag.span.start,
            "end": diag.span.end,
        }
    })
}

pub(crate) fn print_diagnostics(
    diags: &[Diagnostic],
    format: OutputFormat,
    i18n: Option<&I18nCatalog>,
) {
    match format {
        OutputFormat::Human => {
            for diag in diags {
                let level = match diag.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                };
                let path = diag.path.clone().unwrap_or_default();
                let message = localize_diagnostic_message(diag, i18n);
                if path.is_empty() {
                    println!("{} {}: {}", level, diag.code, message);
                } else {
                    println!("{} {} [{}]: {}", level, diag.code, path, message);
                }
                if let Some(note) = diag.note.as_ref().filter(|text| !text.trim().is_empty()) {
                    println!("  note: {}", note);
                }
            }
        }
        OutputFormat::Json => {
            let payload = diags.iter().map(diagnostic_to_json).collect::<Vec<_>>();
            if let Ok(text) = serde_json::to_string_pretty(&payload) {
                println!("{}", text);
            }
        }
    }
}

fn localize_diagnostic_message(diag: &Diagnostic, i18n: Option<&I18nCatalog>) -> String {
    let Some(i18n) = i18n else {
        return diag.message.clone();
    };

    if let Some(message) = diag
        .message_key
        .as_ref()
        .and_then(|key| i18n.strings.get(key))
        .filter(|text| !text.is_empty())
    {
        return message.clone();
    }

    if let Some(message) = i18n.strings.get(&diag.code).filter(|text| !text.is_empty()) {
        return message.clone();
    }

    diag.message.clone()
}

pub(crate) fn write_diagnostics_json(
    path: Option<&Path>,
    diagnostics: &[Diagnostic],
) -> Result<(), String> {
    let Some(path) = path else {
        return Ok(());
    };
    let payload = diagnostics
        .iter()
        .map(diagnostic_to_json)
        .collect::<Vec<_>>();

    fs::write(
        path,
        serde_json::to_string_pretty(&payload)
            .map_err(|err| format!("failed to serialize diagnostics json: {err}"))?,
    )
    .map_err(|err| format!("failed to write {}: {err}", path.display()))
}
