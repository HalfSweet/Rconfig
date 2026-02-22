use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use rcfg_app::AppSession;
use rcfg_lang::{
    BoolFalseExportStyle, Diagnostic, DiagnosticArgValue, EnumExportStyle, ExportNameRule,
    ExportOptions, IntExportFormat, RelatedInfo, Severity, builtin_exporter_names,
    create_builtin_exporter,
};

use crate::cli::args::OutputFormat;
use crate::cli::print_diagnostics;

#[derive(Debug, Clone)]
pub(crate) struct ExportTarget {
    pub(crate) format_name: String,
    pub(crate) out: PathBuf,
}

#[derive(Debug, Clone)]
pub(crate) struct ExportCommandOptions<'a> {
    pub(crate) values: &'a Path,
    pub(crate) targets: &'a [ExportTarget],
    pub(crate) export_secrets: bool,
    pub(crate) export_context: bool,
    pub(crate) c_prefix: String,
    pub(crate) cmake_prefix: String,
    pub(crate) bool_false_style: BoolFalseExportStyle,
    pub(crate) enum_export_style: EnumExportStyle,
    pub(crate) int_export_format: IntExportFormat,
    pub(crate) export_name_rule: ExportNameRule,
    pub(crate) diag_format: OutputFormat,
    pub(crate) parse_diags: Vec<Diagnostic>,
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
    session: &AppSession,
    options: ExportCommandOptions<'_>,
) -> Result<(), String> {
    let values_report = session.analyze_values_from_path(options.values);
    let mut all = options.parse_diags;
    all.extend(values_report.diagnostics.clone());

    let resolved = session.resolve(&values_report.values);

    let export_options = ExportOptions {
        include_secrets: options.export_secrets,
        include_context: options.export_context,
        c_prefix: options.c_prefix,
        cmake_prefix: options.cmake_prefix,
        bool_false_style: options.bool_false_style,
        enum_export_style: options.enum_export_style,
        int_export_format: options.int_export_format,
        export_name_rule: options.export_name_rule,
    };

    let mut rendered_outputs = Vec::new();
    for target in options.targets {
        let Some(exporter) = create_builtin_exporter(&target.format_name) else {
            let mut available = builtin_exporter_names();
            available.sort();
            return Err(format!(
                "unsupported export format `{}` (available: {})",
                target.format_name,
                available.join(", ")
            ));
        };

        let rendered = exporter.render(session.symbols(), &resolved, &export_options);
        all.extend(rendered.diagnostics);
        rendered_outputs.push((target.out.as_path(), rendered.content));
    }

    dedup_diagnostics(&mut all);

    print_diagnostics(&all, options.diag_format, session.i18n());
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        return Err("export blocked by diagnostics".to_string());
    }

    for (path, content) in rendered_outputs {
        write_if_changed(path, &content)?;
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum DiagnosticArgDedupValue {
    Bool(bool),
    Int(i128),
    String(String),
}

impl From<&DiagnosticArgValue> for DiagnosticArgDedupValue {
    fn from(value: &DiagnosticArgValue) -> Self {
        match value {
            DiagnosticArgValue::Bool(raw) => Self::Bool(*raw),
            DiagnosticArgValue::Int(raw) => Self::Int(*raw),
            DiagnosticArgValue::String(raw) => Self::String(raw.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RelatedInfoDedupKey {
    span_start: usize,
    span_end: usize,
    path: Option<String>,
    message: String,
}

impl From<&RelatedInfo> for RelatedInfoDedupKey {
    fn from(value: &RelatedInfo) -> Self {
        Self {
            span_start: value.span.start,
            span_end: value.span.end,
            path: value.path.clone(),
            message: value.message.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DiagnosticDedupKey {
    severity: u8,
    code: String,
    message: String,
    span_start: usize,
    span_end: usize,
    path: Option<String>,
    source: Option<String>,
    include_chain: Vec<String>,
    args: Vec<(String, DiagnosticArgDedupValue)>,
    message_key: Option<String>,
    note: Option<String>,
    related: Vec<RelatedInfoDedupKey>,
}

fn diagnostic_dedup_key(diag: &Diagnostic) -> DiagnosticDedupKey {
    let mut args = diag
        .args
        .iter()
        .map(|(key, value)| (key.clone(), DiagnosticArgDedupValue::from(value)))
        .collect::<Vec<_>>();
    args.sort_by(|left, right| left.0.cmp(&right.0));

    let mut related = diag
        .related
        .iter()
        .map(RelatedInfoDedupKey::from)
        .collect::<Vec<_>>();
    related.sort_by(|left, right| {
        (
            left.span_start,
            left.span_end,
            left.path.as_deref(),
            left.message.as_str(),
        )
            .cmp(&(
                right.span_start,
                right.span_end,
                right.path.as_deref(),
                right.message.as_str(),
            ))
    });

    DiagnosticDedupKey {
        severity: if diag.severity == Severity::Error {
            1
        } else {
            0
        },
        code: diag.code.clone(),
        message: diag.message.clone(),
        span_start: diag.span.start,
        span_end: diag.span.end,
        path: diag.path.clone(),
        source: diag.source.clone(),
        include_chain: diag.include_chain.clone(),
        args,
        message_key: diag.message_key.clone(),
        note: diag.note.clone(),
        related,
    }
}

fn dedup_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    let mut seen = HashSet::new();
    diagnostics.retain(|diag| seen.insert(diagnostic_dedup_key(diag)));
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
