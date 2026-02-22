use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use rcfg_lang::{
    Severity, format_schema, format_values, parse_schema_with_diagnostics,
    parse_values_with_diagnostics,
};

use crate::cli::args::OutputFormat;
use crate::cli::print_diagnostics;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FmtKind {
    Schema,
    Values,
}

pub(crate) fn execute(
    files: Vec<PathBuf>,
    check: bool,
    stdin: bool,
    kind: Option<FmtKind>,
) -> Result<(), String> {
    if stdin {
        if !files.is_empty() {
            return Err("cannot use file paths together with `--stdin`".to_string());
        }

        let kind =
            kind.ok_or_else(|| "`--kind schema|values` is required with `--stdin`".to_string())?;
        let mut input = String::new();
        io::stdin()
            .read_to_string(&mut input)
            .map_err(|err| format!("failed to read stdin: {err}"))?;

        let formatted = format_by_kind("<stdin>", &input, kind)?;
        if check {
            if formatted != input {
                return Err("fmt check failed: stdin requires formatting".to_string());
            }
            return Ok(());
        }

        print!("{formatted}");
        return Ok(());
    }

    if files.is_empty() {
        return Err("missing input files for `rcfg fmt`".to_string());
    }

    let mut needs_format = Vec::new();
    for file in files {
        let source = fs::read_to_string(&file)
            .map_err(|err| format!("failed to read {}: {err}", file.display()))?;
        let kind = kind.unwrap_or(detect_kind_from_path(&file)?);
        let formatted = format_by_kind(&file.display().to_string(), &source, kind)?;

        if check {
            if formatted != source {
                needs_format.push(file);
            }
            continue;
        }

        if formatted != source {
            fs::write(&file, formatted)
                .map_err(|err| format!("failed to write {}: {err}", file.display()))?;
        }
    }

    if needs_format.is_empty() {
        return Ok(());
    }

    for file in &needs_format {
        eprintln!("would reformat {}", file.display());
    }
    Err(format!(
        "fmt check failed: {} file(s) need formatting",
        needs_format.len()
    ))
}

fn format_by_kind(label: &str, source: &str, kind: FmtKind) -> Result<String, String> {
    match kind {
        FmtKind::Schema => {
            let (file, diagnostics) = parse_schema_with_diagnostics(source);
            if has_parse_errors(&diagnostics) {
                print_diagnostics(&diagnostics, OutputFormat::Human, None);
                return Err(format!("failed to parse schema: {label}"));
            }
            Ok(format_schema(source, &file))
        }
        FmtKind::Values => {
            let (values, diagnostics) = parse_values_with_diagnostics(source);
            if has_parse_errors(&diagnostics) {
                print_diagnostics(&diagnostics, OutputFormat::Human, None);
                return Err(format!("failed to parse values: {label}"));
            }
            Ok(format_values(source, &values))
        }
    }
}

fn has_parse_errors(diagnostics: &[rcfg_lang::Diagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
}

fn detect_kind_from_path(path: &Path) -> Result<FmtKind, String> {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("rcfg") => Ok(FmtKind::Schema),
        Some("rcfgv") => Ok(FmtKind::Values),
        _ => Err(format!(
            "cannot detect file kind for {} (expected .rcfg or .rcfgv)",
            path.display()
        )),
    }
}
