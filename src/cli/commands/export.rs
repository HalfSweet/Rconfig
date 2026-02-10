use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use rcfg_lang::{
    BoolFalseExportStyle, Diagnostic, EnumExportStyle, ExportNameRule, ExportOptions,
    IntExportFormat, ResolvedValue, Severity, SymbolTable, builtin_exporter_names,
    create_builtin_exporter,
};

use crate::cli::args::OutputFormat;
use crate::cli::{I18nCatalog, analyze_values_report, print_diagnostics, resolve_with_context};

#[derive(Debug, Clone)]
pub(crate) struct ExportTarget {
    pub(crate) format_name: String,
    pub(crate) out: PathBuf,
}

pub(crate) fn build_targets(
    out_h: Option<PathBuf>,
    out_cmake: Option<PathBuf>,
    formats: Vec<String>,
    out: Vec<PathBuf>,
) -> Result<Vec<ExportTarget>, String> {
    if !formats.is_empty() || !out.is_empty() {
        if out_h.is_some() || out_cmake.is_some() {
            return Err(
                "cannot mix legacy flags `--out-h/--out-cmake` with `--format/--out`".to_string(),
            );
        }

        if formats.is_empty() || out.is_empty() {
            return Err(
                "`--format` and `--out` must be provided together for export targets".to_string(),
            );
        }

        if formats.len() != out.len() {
            return Err(format!(
                "`--format` count ({}) must match `--out` count ({})",
                formats.len(),
                out.len()
            ));
        }

        return Ok(formats
            .into_iter()
            .zip(out)
            .map(|(format_name, out)| ExportTarget { format_name, out })
            .collect());
    }

    match (out_h, out_cmake) {
        (Some(out_h), Some(out_cmake)) => Ok(vec![
            ExportTarget {
                format_name: "c-header".to_string(),
                out: out_h,
            },
            ExportTarget {
                format_name: "cmake".to_string(),
                out: out_cmake,
            },
        ]),
        (None, None) => {
            Err("missing export targets: use `--out-h/--out-cmake` or `--format/--out`".to_string())
        }
        _ => Err("`--out-h` and `--out-cmake` must be provided together".to_string()),
    }
}

pub(crate) fn execute(
    values: &Path,
    targets: &[ExportTarget],
    export_secrets: bool,
    c_prefix: String,
    cmake_prefix: String,
    bool_false_style: BoolFalseExportStyle,
    enum_export_style: EnumExportStyle,
    int_export_format: IntExportFormat,
    export_name_rule: ExportNameRule,
    diag_format: OutputFormat,
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

    let export_options = ExportOptions {
        include_secrets: export_secrets,
        c_prefix,
        cmake_prefix,
        bool_false_style,
        enum_export_style,
        int_export_format,
        export_name_rule,
    };

    let mut rendered_outputs = Vec::new();
    for target in targets {
        let Some(exporter) = create_builtin_exporter(&target.format_name) else {
            let mut available = builtin_exporter_names();
            available.sort();
            return Err(format!(
                "unsupported export format `{}` (available: {})",
                target.format_name,
                available.join(", ")
            ));
        };

        let rendered = exporter.render(symbols, &resolved, &export_options);
        all.extend(rendered.diagnostics);
        rendered_outputs.push((target.out.as_path(), rendered.content));
    }

    dedup_diagnostics(&mut all);

    print_diagnostics(&all, diag_format, i18n);
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("export blocked by diagnostics".to_string());
    }

    for (path, content) in rendered_outputs {
        write_if_changed(path, &content)?;
    }

    Ok(())
}

fn dedup_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    let mut seen = HashSet::new();
    diagnostics.retain(|diag| {
        let key = format!(
            "{:?}|{}|{}|{}|{:?}|{:?}|{}|{}",
            diag.severity,
            diag.code,
            diag.message,
            diag.span.start,
            diag.path,
            diag.source,
            diag.include_chain.join(">"),
            diag.span.end
        );
        seen.insert(key)
    });
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
