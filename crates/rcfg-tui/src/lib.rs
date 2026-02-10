mod app;
mod docs;
mod event;
mod model;
mod render;
mod runtime;
mod save;
mod state;

use std::path::PathBuf;

use app::App;
use rcfg_app::AppSession;

#[derive(Debug, Clone)]
pub struct TuiConfig {
    pub session: AppSession,
    pub initial_values_path: Option<PathBuf>,
    pub out_path: Option<PathBuf>,
    pub script_path: Option<PathBuf>,
}

#[derive(Debug)]
pub struct TuiError {
    pub message: String,
}

impl std::fmt::Display for TuiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for TuiError {}

pub fn run(config: TuiConfig) -> Result<(), TuiError> {
    let save_target = config
        .out_path
        .clone()
        .or_else(|| config.initial_values_path.clone())
        .unwrap_or_else(|| PathBuf::from(".config.rcfgv"));

    let mut app = App::new(config.session, save_target);

    if let Some(path) = config.initial_values_path.as_deref() {
        app.bootstrap_from_values_path(path);
    } else {
        app.recompute();
    }

    if let Some(script) = config.script_path.as_deref() {
        runtime::run_script_mode(&mut app, script).map_err(|message| TuiError { message })?;
        return Ok(());
    }

    runtime::run_terminal_mode(&mut app).map_err(|message| TuiError { message })
}
