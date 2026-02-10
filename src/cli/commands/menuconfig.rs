use rcfg_app::AppSession;

pub(crate) fn execute(
    session: &AppSession,
    values: Option<std::path::PathBuf>,
    out: Option<std::path::PathBuf>,
    script: Option<std::path::PathBuf>,
) -> Result<(), String> {
    let script_mode = script.is_some();
    rcfg_tui::run(rcfg_tui::TuiConfig {
        session: session.clone(),
        initial_values_path: values,
        out_path: out,
        script_path: script,
        script_mode,
    })
    .map_err(|err| err.to_string())
}
