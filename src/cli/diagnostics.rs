use std::fs;
use std::path::Path;

use rcfg_lang::{Diagnostic, Severity};

use crate::cli::args::OutputFormat;
use rcfg_app::{I18nCatalog, diagnostic_to_json, localize_diagnostic_message};

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
