use std::path::PathBuf;

use clap::{ArgAction, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(name = "rcfg", version, about = "Rconfig DSL CLI")]
pub(crate) struct Cli {
    #[arg(long, global = true)]
    pub(crate) schema: Option<PathBuf>,

    #[arg(long, global = true)]
    pub(crate) manifest: Option<PathBuf>,

    #[arg(long, global = true, default_value_t = false, action = ArgAction::SetTrue)]
    pub(crate) strict: bool,

    #[arg(long, global = true)]
    pub(crate) context: Option<PathBuf>,

    #[arg(long, global = true)]
    pub(crate) i18n: Option<PathBuf>,

    #[command(subcommand)]
    pub(crate) command: Commands,
}

#[derive(Debug, Subcommand)]
pub(crate) enum Commands {
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
pub(crate) enum I18nCommand {
    Extract {
        #[arg(long)]
        out: PathBuf,

        #[arg(long, default_value = "en")]
        locale: String,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum OutputFormat {
    Human,
    Json,
}
