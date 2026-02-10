use std::path::PathBuf;

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
    let _ = config;
    Err(TuiError {
        message: "menuconfig is not implemented yet".to_string(),
    })
}
