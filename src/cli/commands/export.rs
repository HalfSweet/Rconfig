use std::collections::HashMap;
use std::fs;
use std::path::Path;

use rcfg_lang::{
    generate_exports, BoolFalseExportStyle, Diagnostic, EnumExportStyle, ExportOptions,
    ResolvedValue, Severity, SymbolTable,
};

use crate::cli::args::OutputFormat;
use crate::cli::{analyze_values_report, print_diagnostics, resolve_with_context, I18nCatalog};

pub(crate) fn execute(
    values: &Path,
    out_h: &Path,
    out_cmake: &Path,
    export_secrets: bool,
    c_prefix: String,
    cmake_prefix: String,
    bool_false_style: BoolFalseExportStyle,
    enum_export_style: EnumExportStyle,
    format: OutputFormat,
    parse_diags: Vec<Diagnostic>,
    symbols: &SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    strict: bool,
    i18n: Option<&I18nCatalog>,
) -> Result<(), String> {
    let values_report = analyze_values_report(values, symbols, context, strict);
    let mut all = parse_diags;
    all.extend(values_report.diagnostics.clone());

    let resolved = resolve_with_context(&values_report.values, symbols, context);
    let exports = generate_exports(
        symbols,
        &resolved,
        &ExportOptions {
            include_secrets: export_secrets,
            c_prefix,
            cmake_prefix,
            bool_false_style,
            enum_export_style,
        },
    );
    all.extend(exports.diagnostics.clone());

    print_diagnostics(&all, format, i18n);
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("export blocked by diagnostics".to_string());
    }

    fs::write(out_h, exports.c_header)
        .map_err(|err| format!("failed to write {}: {err}", out_h.display()))?;
    fs::write(out_cmake, exports.cmake)
        .map_err(|err| format!("failed to write {}: {err}", out_cmake.display()))?;

    Ok(())
}
