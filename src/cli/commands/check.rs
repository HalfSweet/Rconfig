use std::collections::HashMap;
use std::path::Path;

use rcfg_lang::{Diagnostic, ResolvedValue, Severity, SymbolTable};

use crate::cli::args::OutputFormat;
use crate::cli::{I18nCatalog, analyze_values_report, print_diagnostics};

pub(crate) fn execute(
    values: &Path,
    format: OutputFormat,
    parse_diags: Vec<Diagnostic>,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    strict: bool,
    i18n: Option<&I18nCatalog>,
) -> Result<(), String> {
    let values_report = analyze_values_report(values, symbols, context, strict);
    let mut all = parse_diags;
    all.extend(values_report.diagnostics);
    print_diagnostics(&all, format, i18n);
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("check failed with diagnostics".to_string());
    }
    Ok(())
}
