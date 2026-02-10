use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::Path;

use rcfg_lang::{
    BoolFalseExportStyle, Diagnostic, EnumExportStyle, ExportNameRule, ExportOptions,
    IntExportFormat, ResolvedValue, Severity, SymbolTable, generate_exports,
};

use crate::cli::args::OutputFormat;
use crate::cli::{I18nCatalog, analyze_values_report, print_diagnostics, resolve_with_context};

pub(crate) fn execute(
    values: &Path,
    out_h: &Path,
    out_cmake: &Path,
    export_secrets: bool,
    c_prefix: String,
    cmake_prefix: String,
    bool_false_style: BoolFalseExportStyle,
    enum_export_style: EnumExportStyle,
    int_export_format: IntExportFormat,
    export_name_rule: ExportNameRule,
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
            int_export_format,
            export_name_rule,
        },
    );
    all.extend(exports.diagnostics.clone());

    print_diagnostics(&all, format, i18n);
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("export blocked by diagnostics".to_string());
    }

    write_if_changed(out_h, &exports.c_header)?;
    write_if_changed(out_cmake, &exports.cmake)?;

    Ok(())
}

fn write_if_changed(path: &Path, next_content: &str) -> Result<(), String> {
    let next_hash = content_hash(next_content);

    if let Ok(existing_content) = fs::read_to_string(path)
        && existing_content.len() == next_content.len()
        && content_hash(&existing_content) == next_hash
        && existing_content == next_content
    {
        return Ok(());
    }

    fs::write(path, next_content)
        .map_err(|err| format!("failed to write {}: {err}", path.display()))
}

fn content_hash(content: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    content.hash(&mut hasher);
    hasher.finish()
}
