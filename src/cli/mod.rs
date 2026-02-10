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
                export_formats,
                out,
                export_secrets,
                c_prefix,
                cmake_prefix,
                bool_false_style,
                enum_export_style,
                int_export_format,
                export_name_rule,
                diag_format,
            } => {
                let bool_false_style = match bool_false_style {
                    args::BoolFalseStyle::Omit => rcfg_lang::BoolFalseExportStyle::Omit,
                    args::BoolFalseStyle::DefineZero => rcfg_lang::BoolFalseExportStyle::DefineZero,
                };
                let enum_export_style = match enum_export_style {
                    args::EnumExportStyleArg::OneHot => rcfg_lang::EnumExportStyle::OneHot,
                    args::EnumExportStyleArg::String => rcfg_lang::EnumExportStyle::String,
                };
                let int_export_format = match int_export_format {
                    args::IntExportFormatArg::Decimal => rcfg_lang::IntExportFormat::Decimal,
                    args::IntExportFormatArg::Hex => rcfg_lang::IntExportFormat::Hex,
                };
                let export_name_rule = match export_name_rule {
                    args::ExportNameRuleArg::PkgPath => rcfg_lang::ExportNameRule::PkgPath,
                    args::ExportNameRuleArg::PathOnly => rcfg_lang::ExportNameRule::PathOnly,
                };
                let export_formats = export_formats
                    .into_iter()
                    .map(|item| match item {
                        args::ExportFormatArg::CHeader => "c-header".to_string(),
                        args::ExportFormatArg::Cmake => "cmake".to_string(),
                        args::ExportFormatArg::Rust => "rust".to_string(),
                        args::ExportFormatArg::Python => "python".to_string(),
                    })
                    .collect::<Vec<_>>();
                let targets =
                    commands::export::build_targets(out_h, out_cmake, export_formats, out)?;

                commands::export::execute(
                    &values,
                    &targets,
                    export_secrets,
                    c_prefix,
                    cmake_prefix,
                    bool_false_style,
                    enum_export_style,
                    int_export_format,
                    export_name_rule,
                    diag_format,
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

fn load_schema_with_dependencies(
    graph: &ManifestGraph,
) -> Result<(rcfg_lang::File, Vec<rcfg_lang::Diagnostic>), String> {
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
        all_items.extend(namespace_schema_items_for_package(
            schema_file.items,
            manifest.package_name.as_str(),
        ));
        all_diags.append(&mut diags);
    }

    Ok((rcfg_lang::File { items: all_items }, all_diags))
}

fn namespace_schema_items_for_package(
    items: Vec<rcfg_lang::Item>,
    package_name: &str,
) -> Vec<rcfg_lang::Item> {
    let mut first_namespaced_index = None;
    let mut namespaced_items = Vec::new();

    for (index, item) in items.iter().enumerate() {
        let is_ctx_module = matches!(
            item,
            rcfg_lang::Item::Mod(module) if module.name.value == "ctx"
        );
        if is_ctx_module {
            continue;
        }

        if first_namespaced_index.is_none() {
            first_namespaced_index = Some(index);
        }
        namespaced_items.push(item.clone());
    }

    let mut out = Vec::new();
    let mut inserted_namespace = false;
    for (index, item) in items.into_iter().enumerate() {
        let is_ctx_module = matches!(
            item,
            rcfg_lang::Item::Mod(ref module) if module.name.value == "ctx"
        );

        if !inserted_namespace && first_namespaced_index.is_some_and(|value| value == index) {
            out.push(make_package_namespace_module(
                package_name,
                namespaced_items.clone(),
            ));
            inserted_namespace = true;
        }

        if is_ctx_module {
            out.push(item);
        }
    }

    if !inserted_namespace && !namespaced_items.is_empty() {
        out.push(make_package_namespace_module(
            package_name,
            namespaced_items,
        ));
    }

    out
}

fn make_package_namespace_module(
    package_name: &str,
    items: Vec<rcfg_lang::Item>,
) -> rcfg_lang::Item {
    rcfg_lang::Item::Mod(rcfg_lang::ast::ModDecl {
        meta: rcfg_lang::ast::ItemMeta::empty(),
        name: rcfg_lang::Spanned::new(package_name.to_string(), rcfg_lang::Span::default()),
        items,
        span: rcfg_lang::Span::default(),
    })
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
