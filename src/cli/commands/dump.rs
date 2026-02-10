use std::fs;
use std::path::Path;

use rcfg_lang::{Diagnostic, Severity};
use rcfg_app::AppSession;

use crate::cli::{
    print_diagnostics, render_resolved_json, render_schema_ir_json, write_diagnostics_json,
};

pub(crate) struct DumpPaths<'a> {
    pub(crate) out: &'a Path,
    pub(crate) out_schema_ir: Option<&'a Path>,
    pub(crate) out_diagnostics: Option<&'a Path>,
}

pub(crate) fn execute(
    session: &AppSession,
    values: &Path,
    paths: DumpPaths<'_>,
    include_secrets: bool,
    parse_diags: Vec<Diagnostic>,
) -> Result<(), String> {
    let values_report = session.analyze_values_from_path(values);
    let mut all = parse_diags;
    all.extend(values_report.diagnostics.clone());
    write_diagnostics_json(paths.out_diagnostics, &all)?;
    if all.iter().any(|diag| diag.severity == Severity::Error) {
        print_diagnostics(
            &all,
            crate::cli::args::OutputFormat::Human,
            session.i18n(),
        );
        return Err("dump blocked by diagnostics".to_string());
    }

    let resolved = session.resolve(&values_report.values);
    let rendered = render_resolved_json(
        &resolved,
        include_secrets,
        session.symbols(),
        session.package_name(),
    );
    fs::write(
        paths.out,
        serde_json::to_string_pretty(&rendered)
            .map_err(|err| format!("failed to serialize dump json: {err}"))?,
    )
    .map_err(|err| format!("failed to write {}: {err}", paths.out.display()))?;

    if let Some(schema_ir_path) = paths.out_schema_ir {
        let schema_ir = render_schema_ir_json(
            session.symbols(),
            session.schema_file(),
            session.package_name(),
        );
        fs::write(
            schema_ir_path,
            serde_json::to_string_pretty(&schema_ir)
                .map_err(|err| format!("failed to serialize schema_ir json: {err}"))?,
        )
        .map_err(|err| format!("failed to write {}: {err}", schema_ir_path.display()))?;
    }

    Ok(())
}
