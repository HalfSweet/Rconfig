use clap::Parser;
use rcfg_app::{AppLoadOptions, load_session};

pub(crate) mod args;
pub(crate) mod commands;
pub(crate) mod diagnostics;
pub(crate) mod i18n_extract;
pub(crate) mod render;

use self::args::{Cli, Commands, OutputFormat};
pub(crate) use self::diagnostics::{print_diagnostics, write_diagnostics_json};
pub(crate) use self::i18n_extract::{collect_i18n_template_strings, render_i18n_template_toml};
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
    let session = load_session(&AppLoadOptions {
        schema: cli.schema.clone(),
        manifest: cli.manifest.clone(),
        context: cli.context.clone(),
        i18n: cli.i18n.clone(),
        strict: cli.strict,
    })?;

    let parse_diags = session.parse_diagnostics().to_vec();

    if session.symbols().is_empty() && parse_diags.iter().any(|diag| diag.severity == rcfg_lang::Severity::Error)
    {
        print_diagnostics(&parse_diags, OutputFormat::Human, session.i18n());
        return Err("schema parse failed".to_string());
    }

    match cli.command {
        Commands::Check { values, format } => {
            commands::check::execute(
                &session,
                &values,
                format,
                parse_diags,
            )?;
        }
        Commands::Export {
            values,
            out_h,
            out_cmake,
            export_formats,
            out,
            export_secrets,
            export_context,
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
            let targets = commands::export::build_targets(out_h, out_cmake, export_formats, out)?;

            commands::export::execute(
                &session,
                &values,
                &targets,
                export_secrets,
                export_context,
                c_prefix,
                cmake_prefix,
                bool_false_style,
                enum_export_style,
                int_export_format,
                export_name_rule,
                diag_format,
                parse_diags,
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
                &session,
                &values,
                commands::dump::DumpPaths {
                    out: &out,
                    out_schema_ir: out_schema_ir.as_deref(),
                    out_diagnostics: out_diagnostics.as_deref(),
                },
                include_secrets,
                parse_diags,
            )?;
        }
        Commands::I18n { command } => {
            commands::i18n::execute(
                &session,
                command,
                parse_diags,
            )?;
        }
    }

    Ok(())
}
