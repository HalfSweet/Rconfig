use std::fs;

use rcfg_lang::{Diagnostic, Severity};
use rcfg_app::AppSession;

use crate::cli::args::{I18nCommand, OutputFormat};
use crate::cli::{
    collect_i18n_template_strings, print_diagnostics, render_i18n_template_toml,
};

pub(crate) fn execute(
    session: &AppSession,
    command: I18nCommand,
    parse_diags: Vec<Diagnostic>,
) -> Result<(), String> {
    let all = parse_diags;
    if !all.is_empty() {
        print_diagnostics(&all, OutputFormat::Human, session.i18n());
    }
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("i18n extract blocked by diagnostics".to_string());
    }

    match command {
        I18nCommand::Extract { out, locale } => {
            let package = session.package_name().unwrap_or("main");
            let strings = collect_i18n_template_strings(session.schema_file(), package);
            let rendered = render_i18n_template_toml(&locale, &strings)?;
            if let Some(parent) = out.parent() {
                fs::create_dir_all(parent).map_err(|err| {
                    format!("failed to create output dir {}: {err}", parent.display())
                })?;
            }
            fs::write(&out, rendered)
                .map_err(|err| format!("failed to write {}: {err}", out.display()))?;
        }
    }

    Ok(())
}
