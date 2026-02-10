use std::path::Path;

use rcfg_app::AppSession;
use rcfg_lang::{Diagnostic, Severity};

use crate::cli::args::OutputFormat;
use crate::cli::print_diagnostics;

pub(crate) fn execute(
    session: &AppSession,
    values: &Path,
    format: OutputFormat,
    parse_diags: Vec<Diagnostic>,
) -> Result<(), String> {
    let values_report = session.analyze_values_from_path(values);
    let mut all = parse_diags;
    all.extend(values_report.diagnostics);
    print_diagnostics(&all, format, session.i18n());
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("check failed with diagnostics".to_string());
    }
    Ok(())
}
