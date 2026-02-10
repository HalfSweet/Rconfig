use std::collections::HashMap;
use std::fs;
use std::path::Path;

use clap::Parser;
use rcfg_lang::parser::parse_schema_with_diagnostics;
use rcfg_lang::{
    ResolvedConfig, ResolvedValue, ValuesAnalysisReport, analyze_schema, analyze_schema_strict,
    analyze_values_from_path_report_with_context,
    analyze_values_from_path_report_with_context_strict, resolve_values,
    resolve_values_with_context,
};

pub(crate) mod args;
pub(crate) mod commands;
pub(crate) mod diagnostics;
pub(crate) mod i18n_extract;
pub(crate) mod loaders;
pub(crate) mod render;

#[cfg(test)]
mod loaders_tests;

use self::args::{Cli, Commands, OutputFormat};
pub(crate) use self::diagnostics::{print_diagnostics, write_diagnostics_json};
pub(crate) use self::i18n_extract::{collect_i18n_template_strings, render_i18n_template_toml};
pub(crate) use self::loaders::{
    I18nCatalog, ManifestGraph, load_context, load_i18n_catalog, load_manifest_graph,
    resolve_schema_path,
};
pub(crate) use self::render::{render_resolved_json, render_schema_ir_json};

pub(crate) fn entry() -> i32 {
    let cli = Cli::parse();
    let code = match run(cli) {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("{}", err);
            1
        }
    };
    code
}

fn run(cli: Cli) -> Result<(), String> {
    let i18n = load_i18n_catalog(cli.i18n.as_deref())?;
    let manifest_graph = load_manifest_graph(cli.manifest.as_deref())?;
    let root_manifest = manifest_graph.as_ref().map(|graph| &graph.root);
    let schema_path = resolve_schema_path(cli.schema.as_deref(), root_manifest)?;

    let (schema_file, mut parse_diags) = if let Some(graph) = manifest_graph.as_ref() {
        if cli.schema.is_none() {
            load_schema_with_dependencies(graph)?
        } else {
            let schema_text = fs::read_to_string(&schema_path)
                .map_err(|err| format!("failed to read schema {}: {err}", schema_path.display()))?;
            parse_schema_with_diagnostics(&schema_text)
        }
    } else {
        let schema_text = fs::read_to_string(&schema_path)
            .map_err(|err| format!("failed to read schema {}: {err}", schema_path.display()))?;
        parse_schema_with_diagnostics(&schema_text)
    };

    if parse_diags.is_empty() {
        let schema_report = if cli.strict {
            analyze_schema_strict(&schema_file)
        } else {
            analyze_schema(&schema_file)
        };
        parse_diags.extend(schema_report.diagnostics.clone());
        let package_name = root_manifest.map(|model| model.package_name.as_str());

        let context = load_context(cli.context.as_deref())?;
        match cli.command {
            Commands::Check { values, format } => {
                commands::check::execute(
                    &values,
                    format,
                    parse_diags,
                    &schema_report.symbols,
                    &context,
                    cli.strict,
                    i18n.as_ref(),
                )?;
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
                commands::export::execute(
                    &values,
                    &out_h,
                    &out_cmake,
                    export_secrets,
                    c_prefix,
                    cmake_prefix,
                    format,
                    parse_diags,
                    &schema_report.symbols,
                    &context,
                    cli.strict,
                    i18n.as_ref(),
                )?;
            }
            Commands::Dump {
                values,
                out,
                out_schema_ir,
                out_diagnostics,
                include_secrets,
            } => {
                commands::dump::execute(
                    &values,
                    commands::dump::DumpPaths {
                        out: &out,
                        out_schema_ir: out_schema_ir.as_deref(),
                        out_diagnostics: out_diagnostics.as_deref(),
                    },
                    include_secrets,
                    parse_diags,
                    &schema_report.symbols,
                    &schema_file,
                    package_name,
                    &context,
                    cli.strict,
                    i18n.as_ref(),
                )?;
            }
            Commands::I18n { command } => {
                commands::i18n::execute(
                    command,
                    parse_diags,
                    &schema_file,
                    package_name,
                    i18n.as_ref(),
                )?;
            }
        }
    } else {
        print_diagnostics(&parse_diags, OutputFormat::Human, i18n.as_ref());
        return Err("schema parse failed".to_string());
    }

    Ok(())
}

fn load_schema_with_dependencies(graph: &ManifestGraph) -> Result<(rcfg_lang::File, Vec<rcfg_lang::Diagnostic>), String> {
    let mut all_items = Vec::new();
    let mut all_diags = Vec::new();

    for manifest in &graph.packages_depth_first {
        let schema_text = fs::read_to_string(&manifest.schema).map_err(|err| {
            format!(
                "failed to read schema {} for package `{}`: {err}",
                manifest.schema.display(),
                manifest.package_name
            )
        })?;
        let (schema_file, mut diags) = parse_schema_with_diagnostics(&schema_text);
        all_items.extend(schema_file.items);
        all_diags.append(&mut diags);
    }

    Ok((rcfg_lang::File { items: all_items }, all_diags))
}

pub(crate) fn analyze_values_report(
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

pub(crate) fn resolve_with_context(
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
