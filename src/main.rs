use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use clap::{ArgAction, Parser, Subcommand, ValueEnum};
use rcfg_lang::parser::parse_schema_with_diagnostics;
use rcfg_lang::{
    AttrKind, Diagnostic, ExportOptions, ResolvedConfig, ResolvedValue, Severity, SymbolKind,
    ValuesAnalysisReport, analyze_schema, analyze_schema_strict,
    analyze_values_from_path_report_with_context,
    analyze_values_from_path_report_with_context_strict, generate_exports, resolve_values,
    resolve_values_with_context,
};

#[derive(Debug, Parser)]
#[command(name = "rcfg", version, about = "Rconfig DSL CLI")]
struct Cli {
    #[arg(long, global = true)]
    schema: Option<PathBuf>,

    #[arg(long, global = true)]
    manifest: Option<PathBuf>,

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

        #[arg(long)]
        out_schema_ir: Option<PathBuf>,

        #[arg(long)]
        out_diagnostics: Option<PathBuf>,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        include_secrets: bool,
    },
    I18n {
        #[command(subcommand)]
        command: I18nCommand,
    },
}

#[derive(Debug, Subcommand)]
enum I18nCommand {
    Extract {
        #[arg(long)]
        out: PathBuf,

        #[arg(long, default_value = "en")]
        locale: String,
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
    let manifest = load_manifest(cli.manifest.as_deref())?;
    let schema_path = resolve_schema_path(cli.schema.as_deref(), manifest.as_ref())?;

    let schema_text = fs::read_to_string(&schema_path)
        .map_err(|err| format!("failed to read schema {}: {err}", schema_path.display()))?;
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
                let values_report =
                    analyze_values_report(&values, &schema_report.symbols, &context, cli.strict);
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
                let values_report =
                    analyze_values_report(&values, &schema_report.symbols, &context, cli.strict);
                let mut all = parse_diags;
                all.extend(values_report.diagnostics.clone());

                let resolved =
                    resolve_with_context(&values_report.values, &schema_report.symbols, &context);
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
                out_schema_ir,
                out_diagnostics,
                include_secrets,
            } => {
                let values_report =
                    analyze_values_report(&values, &schema_report.symbols, &context, cli.strict);
                let mut all = parse_diags;
                all.extend(values_report.diagnostics.clone());
                write_diagnostics_json(out_diagnostics.as_deref(), &all)?;
                if all.iter().any(|diag| diag.severity == Severity::Error) {
                    print_diagnostics(&all, OutputFormat::Human);
                    return Err("dump blocked by diagnostics".to_string());
                }

                let resolved =
                    resolve_with_context(&values_report.values, &schema_report.symbols, &context);
                let rendered =
                    render_resolved_json(&resolved, include_secrets, &schema_report.symbols);
                fs::write(
                    &out,
                    serde_json::to_string_pretty(&rendered)
                        .map_err(|err| format!("failed to serialize dump json: {err}"))?,
                )
                .map_err(|err| format!("failed to write {}: {err}", out.display()))?;

                if let Some(schema_ir_path) = out_schema_ir.as_deref() {
                    let schema_ir = render_schema_ir_json(
                        &schema_report.symbols,
                        &schema_file,
                        manifest
                            .as_ref()
                            .and_then(|model| model.package_name.as_deref()),
                    );
                    fs::write(
                        schema_ir_path,
                        serde_json::to_string_pretty(&schema_ir)
                            .map_err(|err| format!("failed to serialize schema_ir json: {err}"))?,
                    )
                    .map_err(|err| {
                        format!("failed to write {}: {err}", schema_ir_path.display())
                    })?;
                }
            }
            Commands::I18n { command } => {
                let all = parse_diags;
                if !all.is_empty() {
                    print_diagnostics(&all, OutputFormat::Human);
                }
                if all.iter().any(|diag| diag.severity == Severity::Error) {
                    return Err("i18n extract blocked by diagnostics".to_string());
                }

                match command {
                    I18nCommand::Extract { out, locale } => {
                        let package = manifest
                            .as_ref()
                            .and_then(|model| model.package_name.as_deref())
                            .unwrap_or("main");
                        let strings = collect_i18n_template_strings(&schema_file, package);
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

#[derive(Debug)]
struct ManifestModel {
    schema: PathBuf,
    package_name: Option<String>,
}

fn load_manifest(path: Option<&Path>) -> Result<Option<ManifestModel>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read manifest {}: {err}", path.display()))?;
    let value = text
        .parse::<toml::Value>()
        .map_err(|err| format!("failed to parse manifest {}: {err}", path.display()))?;

    let package_name = value
        .get("package")
        .and_then(toml::Value::as_table)
        .and_then(|package| package.get("name"))
        .and_then(toml::Value::as_str)
        .map(str::to_string);

    let entry = value
        .get("entry")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| "manifest missing [entry] table".to_string())?;
    let schema = entry
        .get("schema")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| "manifest missing entry.schema".to_string())?;

    let base = path.parent().unwrap_or_else(|| Path::new("."));
    Ok(Some(ManifestModel {
        schema: base.join(schema),
        package_name,
    }))
}

fn resolve_schema_path(
    schema: Option<&Path>,
    manifest: Option<&ManifestModel>,
) -> Result<PathBuf, String> {
    if let Some(schema) = schema {
        return Ok(schema.to_path_buf());
    }

    if let Some(manifest) = manifest {
        return Ok(manifest.schema.clone());
    }

    Err("--schema is required (or provide --manifest with entry.schema)".to_string())
}

fn diagnostic_arg_to_json(value: &rcfg_lang::DiagnosticArgValue) -> serde_json::Value {
    match value {
        rcfg_lang::DiagnosticArgValue::Bool(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::Int(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::String(raw) => serde_json::json!(raw),
    }
}

fn diagnostic_to_json(diag: &Diagnostic) -> serde_json::Value {
    let args = diag
        .args
        .iter()
        .map(|(key, value)| (key.clone(), diagnostic_arg_to_json(value)))
        .collect::<serde_json::Map<_, _>>();
    let related = diag
        .related
        .iter()
        .map(|item| {
            serde_json::json!({
                "message": item.message,
                "path": item.path,
                "span": {
                    "start": item.span.start,
                    "end": item.span.end,
                }
            })
        })
        .collect::<Vec<_>>();

    serde_json::json!({
        "severity": match diag.severity { Severity::Error => "error", Severity::Warning => "warning" },
        "code": diag.code,
        "message": diag.message,
        "message_key": diag.message_key,
        "note": diag.note,
        "args": args,
        "related": related,
        "path": diag.path,
        "source": diag.source,
        "include_chain": diag.include_chain,
        "span": {
            "start": diag.span.start,
            "end": diag.span.end,
        }
    })
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
            let payload = diags.iter().map(diagnostic_to_json).collect::<Vec<_>>();
            if let Ok(text) = serde_json::to_string_pretty(&payload) {
                println!("{}", text);
            }
        }
    }
}

fn write_diagnostics_json(path: Option<&Path>, diagnostics: &[Diagnostic]) -> Result<(), String> {
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

fn render_schema_ir_json(
    symbols: &rcfg_lang::SymbolTable,
    schema: &rcfg_lang::File,
    package_name: Option<&str>,
) -> serde_json::Value {
    let mut options = Vec::new();
    let mut enums = Vec::new();
    let mut mods = Vec::new();
    let doc_sections = collect_doc_sections_index(schema);
    let package = package_name.unwrap_or("main");

    let mut symbols_list = symbols.iter().collect::<Vec<_>>();
    symbols_list.sort_by(|(left, _), (right, _)| left.cmp(right));

    for (path, info) in symbols_list {
        let (summary, help) = doc_sections.get(path).cloned().unwrap_or((None, None));
        let entry = serde_json::json!({
            "path": path,
            "summary": summary,
            "help": help,
            "label_key": i18n_symbol_key(package, path, "label"),
            "help_key": i18n_symbol_key(package, path, "help"),
        });
        match info.kind {
            SymbolKind::Option => options.push(entry),
            SymbolKind::Enum => enums.push(entry),
            SymbolKind::Mod => mods.push(entry),
        }
    }

    serde_json::json!({
        "tool_version": env!("CARGO_PKG_VERSION"),
        "symbols": {
            "mods": mods,
            "enums": enums,
            "options": options,
        }
    })
}

fn i18n_symbol_key(package: &str, path: &str, suffix: &str) -> String {
    format!("{}.{}.{}", package, path.replace("::", "."), suffix)
}

fn collect_i18n_template_strings(
    schema: &rcfg_lang::File,
    package: &str,
) -> BTreeMap<String, String> {
    let mut out = BTreeMap::new();
    let mut scope = Vec::new();
    let mut require_counters = HashMap::new();
    collect_item_i18n_strings(
        &schema.items,
        &mut scope,
        package,
        &mut require_counters,
        &mut out,
    );
    out
}

fn collect_item_i18n_strings(
    items: &[rcfg_lang::Item],
    scope: &mut Vec<String>,
    package: &str,
    require_counters: &mut HashMap<String, usize>,
    out: &mut BTreeMap<String, String>,
) {
    for item in items {
        match item {
            rcfg_lang::Item::Use(_) => {}
            rcfg_lang::Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                push_doc_i18n_strings(&path, &module.meta.doc, package, out);

                scope.push(module.name.value.clone());
                collect_item_i18n_strings(&module.items, scope, package, require_counters, out);
                scope.pop();
            }
            rcfg_lang::Item::Enum(enum_decl) => {
                let enum_path = scoped_path(scope, &enum_decl.name.value);
                push_doc_i18n_strings(&enum_path, &enum_decl.meta.doc, package, out);

                for variant in &enum_decl.variants {
                    let variant_path = format!("{}::{}", enum_path, variant.name.value);
                    push_doc_i18n_strings(&variant_path, &variant.meta.doc, package, out);
                }
            }
            rcfg_lang::Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                push_doc_i18n_strings(&path, &option.meta.doc, package, out);

                if let Some(attached) = &option.attached_constraints {
                    for require in &attached.requires {
                        push_require_i18n_string(require, scope, package, require_counters, out);
                    }
                }
            }
            rcfg_lang::Item::Require(require) => {
                push_require_i18n_string(require, scope, package, require_counters, out);
            }
            rcfg_lang::Item::Constraint(constraint) => {
                for item in &constraint.items {
                    if let rcfg_lang::ast::ConstraintItem::Require(require) = item {
                        push_require_i18n_string(require, scope, package, require_counters, out);
                    }
                }
            }
            rcfg_lang::Item::When(when_block) => {
                collect_item_i18n_strings(&when_block.items, scope, package, require_counters, out);
            }
            rcfg_lang::Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_item_i18n_strings(&case.items, scope, package, require_counters, out);
                }
            }
        }
    }
}

fn push_doc_i18n_strings(
    path: &str,
    doc: &[rcfg_lang::Spanned<String>],
    package: &str,
    out: &mut BTreeMap<String, String>,
) {
    let (summary, help) = split_doc_summary_help(doc);
    let label_key = i18n_symbol_key(package, path, "label");
    let help_key = i18n_symbol_key(package, path, "help");

    insert_i18n_string(out, label_key, summary.unwrap_or_default());
    insert_i18n_string(out, help_key, help.unwrap_or_default());
}

fn push_require_i18n_string(
    require: &rcfg_lang::ast::RequireStmt,
    scope: &[String],
    package: &str,
    require_counters: &mut HashMap<String, usize>,
    out: &mut BTreeMap<String, String>,
) {
    let ordinal = next_require_ordinal(require_counters, scope);
    let key = require_i18n_message_key(require, scope, ordinal, package);
    insert_i18n_string(out, key, "require condition failed".to_string());
}

fn insert_i18n_string(out: &mut BTreeMap<String, String>, key: String, fallback: String) {
    let entry = out.entry(key).or_default();
    if entry.is_empty() && !fallback.is_empty() {
        *entry = fallback;
    }
}

fn require_scope_key(scope: &[String]) -> String {
    if scope.is_empty() {
        "root".to_string()
    } else {
        scope.join(".")
    }
}

fn next_require_ordinal(counters: &mut HashMap<String, usize>, scope: &[String]) -> usize {
    let key = require_scope_key(scope);
    let entry = counters.entry(key).or_insert(0);
    *entry += 1;
    *entry
}

fn require_i18n_message_key(
    require: &rcfg_lang::ast::RequireStmt,
    scope: &[String],
    ordinal: usize,
    package: &str,
) -> String {
    if let Some(message) = require.meta.attrs.iter().find_map(|attr| match &attr.kind {
        AttrKind::Msg(value) => Some(value.clone()),
        _ => None,
    }) {
        return message;
    }

    format!(
        "{}.{}.require.{}",
        package,
        require_scope_key(scope),
        ordinal
    )
}

fn render_i18n_template_toml(
    locale: &str,
    strings: &BTreeMap<String, String>,
) -> Result<String, String> {
    let locale = locale.trim();
    if locale.is_empty() {
        return Err("locale cannot be empty".to_string());
    }

    let mut string_map = toml::map::Map::new();
    for (key, value) in strings {
        string_map.insert(key.clone(), toml::Value::String(value.clone()));
    }

    let mut root = toml::map::Map::new();
    root.insert(
        "locale".to_string(),
        toml::Value::String(locale.to_string()),
    );
    root.insert("strings".to_string(), toml::Value::Table(string_map));

    let body = toml::to_string_pretty(&toml::Value::Table(root))
        .map_err(|err| format!("failed to serialize i18n template: {err}"))?;

    Ok(format!("# generated by rcfg i18n extract\n{body}"))
}

fn collect_doc_sections_index(
    schema: &rcfg_lang::File,
) -> HashMap<String, (Option<String>, Option<String>)> {
    let mut out = HashMap::new();
    let mut scope = Vec::new();
    collect_item_doc_sections(&schema.items, &mut scope, &mut out);
    out
}

fn collect_item_doc_sections(
    items: &[rcfg_lang::Item],
    scope: &mut Vec<String>,
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
) {
    for item in items {
        match item {
            rcfg_lang::Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                let (summary, help) = split_doc_summary_help(&module.meta.doc);
                insert_doc_sections(out, path, summary, help);

                scope.push(module.name.value.clone());
                collect_item_doc_sections(&module.items, scope, out);
                scope.pop();
            }
            rcfg_lang::Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                let (summary, help) = split_doc_summary_help(&enum_decl.meta.doc);
                insert_doc_sections(out, path, summary, help);
            }
            rcfg_lang::Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                let (summary, help) = split_doc_summary_help(&option.meta.doc);
                insert_doc_sections(out, path, summary, help);
            }
            rcfg_lang::Item::When(when_block) => {
                collect_item_doc_sections(&when_block.items, scope, out);
            }
            rcfg_lang::Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_item_doc_sections(&case.items, scope, out);
                }
            }
            rcfg_lang::Item::Use(_)
            | rcfg_lang::Item::Require(_)
            | rcfg_lang::Item::Constraint(_) => {}
        }
    }
}

fn insert_doc_sections(
    out: &mut HashMap<String, (Option<String>, Option<String>)>,
    path: String,
    summary: Option<String>,
    help: Option<String>,
) {
    let entry = out.entry(path).or_insert((None, None));
    if entry.0.is_none() && summary.is_some() {
        entry.0 = summary;
    }
    if entry.1.is_none() && help.is_some() {
        entry.1 = help;
    }
}

fn split_doc_summary_help(doc: &[rcfg_lang::Spanned<String>]) -> (Option<String>, Option<String>) {
    let mut summary_lines = Vec::new();
    let mut help_lines = Vec::new();
    let mut in_help = false;

    for line in doc.iter().map(|entry| entry.value.trim_end().to_string()) {
        if !in_help {
            if line.trim().is_empty() {
                if !summary_lines.is_empty() {
                    in_help = true;
                }
                continue;
            }
            summary_lines.push(line);
            continue;
        }

        help_lines.push(line);
    }

    while help_lines
        .first()
        .is_some_and(|line| line.trim().is_empty())
    {
        help_lines.remove(0);
    }
    while help_lines.last().is_some_and(|line| line.trim().is_empty()) {
        help_lines.pop();
    }

    let summary = if summary_lines.is_empty() {
        None
    } else {
        Some(summary_lines.join("\n"))
    };
    let help = if help_lines.is_empty() {
        None
    } else {
        Some(help_lines.join("\n"))
    };

    (summary, help)
}

fn scoped_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

fn render_resolved_json(
    resolved: &ResolvedConfig,
    include_secrets: bool,
    symbols: &rcfg_lang::SymbolTable,
) -> serde_json::Value {
    let mut map = BTreeMap::new();
    for option in &resolved.options {
        let is_secret = symbols.get(&option.path).is_some_and(|_| {
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
