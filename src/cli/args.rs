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
        out_h: Option<PathBuf>,

        #[arg(long)]
        out_cmake: Option<PathBuf>,

        #[arg(long = "format", value_enum, action = ArgAction::Append)]
        export_formats: Vec<ExportFormatArg>,

        #[arg(long = "out", action = ArgAction::Append)]
        out: Vec<PathBuf>,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        export_secrets: bool,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        export_context: bool,

        #[arg(long, default_value = "CONFIG_")]
        c_prefix: String,

        #[arg(long, default_value = "CFG_")]
        cmake_prefix: String,

        #[arg(long, value_enum, default_value_t = BoolFalseStyle::Omit)]
        bool_false_style: BoolFalseStyle,

        #[arg(long, value_enum, default_value_t = EnumExportStyleArg::OneHot)]
        enum_export_style: EnumExportStyleArg,

        #[arg(long, value_enum, default_value_t = IntExportFormatArg::Decimal)]
        int_export_format: IntExportFormatArg,

        #[arg(long, value_enum, default_value_t = ExportNameRuleArg::PkgPath)]
        export_name_rule: ExportNameRuleArg,

        #[arg(long = "diag-format", value_enum, default_value_t = OutputFormat::Human)]
        diag_format: OutputFormat,
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
    Menuconfig {
        #[arg(long)]
        values: Option<PathBuf>,

        #[arg(long)]
        out: Option<PathBuf>,

        #[arg(long, hide = true)]
        script: Option<PathBuf>,
    },
    Fmt {
        #[arg(value_name = "FILE", required_unless_present = "stdin")]
        files: Vec<PathBuf>,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        check: bool,

        #[arg(long, default_value_t = false, action = ArgAction::SetTrue)]
        stdin: bool,

        #[arg(long, value_enum, requires = "stdin")]
        kind: Option<FmtKindArg>,
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
pub(crate) enum BoolFalseStyle {
    Omit,
    #[value(name = "define-0")]
    DefineZero,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum EnumExportStyleArg {
    #[value(name = "one-hot")]
    OneHot,
    String,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum IntExportFormatArg {
    Decimal,
    Hex,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum ExportNameRuleArg {
    #[value(name = "pkg-path")]
    PkgPath,
    #[value(name = "path-only")]
    PathOnly,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum ExportFormatArg {
    #[value(name = "c-header")]
    CHeader,
    Cmake,
    Rust,
    Python,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum OutputFormat {
    Human,
    Json,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
pub(crate) enum FmtKindArg {
    Schema,
    Values,
}
