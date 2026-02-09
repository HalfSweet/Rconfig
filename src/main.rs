use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use clap::{ArgAction, Parser, Subcommand, ValueEnum};
use rcfg_lang::parser::parse_schema_with_diagnostics;
use rcfg_lang::{
    analyze_schema, analyze_schema_strict, analyze_values_from_path_report_with_context,
    analyze_values_from_path_report_with_context_strict, generate_exports, resolve_values,
    resolve_values_with_context, Diagnostic, ExportOptions, ResolvedConfig, ResolvedValue, Severity,
    ValuesAnalysisReport,
};

#[derive(Debug, Parser)]
#[command(name = "rcfg", version, about = "Rconfig DSL CLI")]
struct Cli {
    #[arg(long, global = true)]
    schema: PathBuf,

    #[arg(long, global = true, default_value_t = false, action = ArgAction::SetTrue)]
    strict: bool,

    #[arg(long, global = true)]
    context: Option<PathBuf>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Check {
        #[arg(long)]
        values: PathBuf,

        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
    },
    Export {
        #[arg(long)]
        values: PathBuf,

        #[arg(long)]
        out_h: PathBuf,

        #[arg(long)]
        out_cmake: PathBuf,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        export_secrets: bool,

        #[arg(long, default_value = "CONFIG_")]
        c_prefix: String,

        #[arg(long, default_value = "CFG_")]
        cmake_prefix: String,

        #[arg(long, value_enum, default_value_t = OutputFormat::Human)]
        format: OutputFormat,
    },
    Dump {
        #[arg(long)]
        values: PathBuf,

        #[arg(long)]
        out: PathBuf,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        include_secrets: bool,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum OutputFormat {
    Human,
    Json,
}

fn main() {
    let cli = Cli::parse();
    let code = match run(cli) {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    };
    std::process::exit(code);
}

fn run(cli: Cli) -> Result<(), String> {
    let schema_text = fs::read_to_string(&cli.schema)
        .map_err(|err| format!("failed to read schema {}: {err}", cli.schema.display()))?;
    let (schema_file, mut parse_diags) = parse_schema_with_diagnostics(&schema_text);
    if parse_diags.is_empty() {
        let schema_report = if cli.strict {
            analyze_schema_strict(&schema_file)
        } else {
            analyze_schema(&schema_file)
        };
        parse_diags.extend(schema_report.diagnostics.clone());

        let context = load_context(cli.context.as_deref())?;
        match cli.command {
            Commands::Check { values, format } => {
                let values_report = analyze_values_report(
                    &values,
                    &schema_report.symbols,
                    &context,
                    cli.strict,
                );
                let mut all = parse_diags;
                all.extend(values_report.diagnostics);
                print_diagnostics(&all, format);
                if all.iter().any(|diag| diag.severity == Severity::Error) {
                    return Err("check failed with diagnostics".to_string());
                }
            }
            Commands::Export {
                values,
                out_h,
                out_cmake,
                export_secrets,
                c_prefix,
                cmake_prefix,
                format,
            } => {
                let values_report = analyze_values_report(
                    &values,
                    &schema_report.symbols,
                    &context,
                    cli.strict,
                );
                let mut all = parse_diags;
                all.extend(values_report.diagnostics.clone());

                let resolved = resolve_with_context(
                    &values_report.values,
                    &schema_report.symbols,
                    &context,
                );
                let exports = generate_exports(
                    &schema_report.symbols,
                    &resolved,
                    &ExportOptions {
                        include_secrets: export_secrets,
                        c_prefix,
                        cmake_prefix,
                    },
                );
                all.extend(exports.diagnostics.clone());

                print_diagnostics(&all, format);
                if all.iter().any(|diag| diag.severity == Severity::Error) {
                    return Err("export blocked by diagnostics".to_string());
                }

                fs::write(&out_h, exports.c_header)
                    .map_err(|err| format!("failed to write {}: {err}", out_h.display()))?;
                fs::write(&out_cmake, exports.cmake)
                    .map_err(|err| format!("failed to write {}: {err}", out_cmake.display()))?;
            }
            Commands::Dump {
                values,
                out,
                include_secrets,
            } => {
                let values_report = analyze_values_report(
                    &values,
                    &schema_report.symbols,
                    &context,
                    cli.strict,
                );
                let mut all = parse_diags;
                all.extend(values_report.diagnostics.clone());
                if all.iter().any(|diag| diag.severity == Severity::Error) {
                    print_diagnostics(&all, OutputFormat::Human);
                    return Err("dump blocked by diagnostics".to_string());
                }

                let resolved = resolve_with_context(
                    &values_report.values,
                    &schema_report.symbols,
                    &context,
                );
                let rendered = render_resolved_json(&resolved, include_secrets, &schema_report.symbols);
                fs::write(
                    &out,
                    serde_json::to_string_pretty(&rendered)
                        .map_err(|err| format!("failed to serialize dump json: {err}"))?,
                )
                .map_err(|err| format!("failed to write {}: {err}", out.display()))?;
            }
        }
    } else {
        print_diagnostics(&parse_diags, OutputFormat::Human);
        return Err("schema parse failed".to_string());
    }

    Ok(())
}

fn analyze_values_report(
    values: &Path,
    symbols: &rcfg_lang::SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    strict: bool,
) -> ValuesAnalysisReport {
    if strict {
        analyze_values_from_path_report_with_context_strict(values, symbols, context)
    } else {
        analyze_values_from_path_report_with_context(values, symbols, context)
    }
}

fn resolve_with_context(
    values: &rcfg_lang::ValuesFile,
    symbols: &rcfg_lang::SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ResolvedConfig {
    if context.is_empty() {
        resolve_values(values, symbols)
    } else {
        resolve_values_with_context(values, symbols, context)
    }
}

fn load_context(path: Option<&Path>) -> Result<HashMap<String, ResolvedValue>, String> {
    let Some(path) = path else {
        return Ok(HashMap::new());
    };
    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read context {}: {err}", path.display()))?;
    let json = serde_json::from_str::<serde_json::Value>(&text)
        .map_err(|err| format!("failed to parse context json {}: {err}", path.display()))?;

    let mut out = HashMap::new();
    let obj = json
        .as_object()
        .ok_or_else(|| "context json must be a key-value object".to_string())?;
    for (key, value) in obj {
        let resolved = if let Some(raw) = value.as_bool() {
            ResolvedValue::Bool(raw)
        } else if let Some(raw) = value.as_i64() {
            ResolvedValue::Int(raw as i128)
        } else if let Some(raw) = value.as_str() {
            if raw.contains("::") {
                ResolvedValue::EnumVariant(raw.to_string())
            } else {
                ResolvedValue::String(raw.to_string())
            }
        } else {
            return Err(format!("unsupported context value for key `{}`", key));
        };
        out.insert(key.clone(), resolved);
    }
    Ok(out)
}

fn print_diagnostics(diags: &[Diagnostic], format: OutputFormat) {
    match format {
        OutputFormat::Human => {
            for diag in diags {
                let level = match diag.severity {
                    Severity::Error => "error",
                    Severity::Warning => "warning",
                };
                let path = diag.path.clone().unwrap_or_default();
                if path.is_empty() {
                    println!("{} {}: {}", level, diag.code, diag.message);
                } else {
                    println!("{} {} [{}]: {}", level, diag.code, path, diag.message);
                }
            }
        }
        OutputFormat::Json => {
            let payload = diags
                .iter()
                .map(|diag| {
                    serde_json::json!({
                        "severity": match diag.severity { Severity::Error => "error", Severity::Warning => "warning" },
                        "code": diag.code,
                        "message": diag.message,
                        "path": diag.path,
                        "source": diag.source,
                        "include_chain": diag.include_chain,
                        "span": {
                            "start": diag.span.start,
                            "end": diag.span.end,
                        }
                    })
                })
                .collect::<Vec<_>>();
            if let Ok(text) = serde_json::to_string_pretty(&payload) {
                println!("{}", text);
            }
        }
    }
}

fn render_resolved_json(
    resolved: &ResolvedConfig,
    include_secrets: bool,
    symbols: &rcfg_lang::SymbolTable,
) -> serde_json::Value {
    let mut map = BTreeMap::new();
    for option in &resolved.options {
        let is_secret = symbols
            .get(&option.path)
            .is_some_and(|_| {
                let (planned, _diags) = rcfg_lang::plan_c_header_exports(symbols, false);
                !planned.iter().any(|entry| entry.path == option.path)
            });
        let value = if is_secret && !include_secrets {
            serde_json::json!({"redacted": true, "value": null})
        } else {
            serde_json::json!({
                "redacted": false,
                "value": option
                    .value
                    .as_ref()
                    .map(resolved_value_to_json)
                    .unwrap_or(serde_json::Value::Null),
            })
        };
        map.insert(
            option.path.clone(),
            serde_json::json!({
                "active": option.active,
                "source": option.source.map(|source| match source {
                    rcfg_lang::ValueSource::User => "user",
                    rcfg_lang::ValueSource::Default => "default",
                    rcfg_lang::ValueSource::Context => "context",
                }),
                "data": value,
            }),
        );
    }
    serde_json::json!({"options": map})
}

fn resolved_value_to_json(value: &ResolvedValue) -> serde_json::Value {
    match value {
        ResolvedValue::Bool(raw) => serde_json::json!(raw),
        ResolvedValue::Int(raw) => serde_json::json!(raw),
        ResolvedValue::String(raw) => serde_json::json!(raw),
        ResolvedValue::EnumVariant(raw) => serde_json::json!(raw),
    }
}
