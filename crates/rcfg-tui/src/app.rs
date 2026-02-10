use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use rcfg_app::AppSession;
use rcfg_lang::{ResolvedValue, Severity, ValueSource};

use crate::model::ConfigTree;
use crate::save::{baseline_resolved, build_minimal_overrides, render_values};
use crate::state::UiState;

#[derive(Debug)]
pub struct App {
    pub session: AppSession,
    pub state: UiState,
    save_target: PathBuf,
}

impl App {
    pub fn new(session: AppSession, save_target: PathBuf) -> Self {
        let tree = ConfigTree::from_schema(session.symbols());
        let state = UiState::new(tree);

        Self {
            session,
            state,
            save_target,
        }
    }

    pub fn bootstrap_from_values_path(&mut self, path: &Path) {
        if !path.exists() {
            self.recompute();
            return;
        }

        let report = self.session.analyze_values_from_path(path);
        self.state.diagnostics = report.diagnostics.clone();
        self.state.set_active_from_resolved(&report.resolved);
        self.state.user_values = report
            .resolved
            .options
            .iter()
            .filter_map(|option| {
                if option.source == Some(ValueSource::User) {
                    option
                        .value
                        .as_ref()
                        .map(|value| (option.path.clone(), value.clone()))
                } else {
                    None
                }
            })
            .collect::<BTreeMap<_, _>>();
        self.state.dirty = false;
        self.state.clear_quit_confirmation();
    }

    pub fn recompute(&mut self) {
        let values = self.state.to_values_file();
        let diagnostics = self.session.analyze_values(&values);
        let resolved = self.session.resolve(&values);
        self.state.apply_runtime_result(&resolved, diagnostics);
    }

    pub fn has_blocking_errors(&self) -> bool {
        self.state
            .diagnostics
            .iter()
            .any(|diag| diag.severity == Severity::Error)
    }

    pub fn minimal_values_text(&self) -> String {
        let values = self.state.to_values_file();
        let current = self.session.resolve(&values);
        let baseline = baseline_resolved(&self.session);
        let minimal = build_minimal_overrides(&baseline, &current, &self.state.user_values);
        render_values(&minimal)
    }

    pub fn script_summary_json(&self) -> serde_json::Value {
        let mut user_values = BTreeMap::new();
        for (path, value) in &self.state.user_values {
            let value_json = match value {
                ResolvedValue::Bool(raw) => serde_json::json!(raw),
                ResolvedValue::Int(raw) => serde_json::json!(raw),
                ResolvedValue::String(raw) => serde_json::json!(raw),
                ResolvedValue::EnumVariant(raw) => serde_json::json!(raw),
            };
            user_values.insert(path.clone(), value_json);
        }

        let diagnostics = self
            .state
            .diagnostics
            .iter()
            .map(|diag| {
                serde_json::json!({
                    "severity": match diag.severity { Severity::Error => "error", Severity::Warning => "warning" },
                    "code": diag.code,
                    "path": diag.path,
                    "message_key": diag.message_key,
                })
            })
            .collect::<Vec<_>>();

        let active_paths = self
            .state
            .active_paths
            .iter()
            .cloned()
            .collect::<Vec<_>>();

        serde_json::json!({
            "selected_path": self
                .state
                .tree
                .node(self.state.selected)
                .map(|node| node.path.clone())
                .unwrap_or_default(),
            "user_values": user_values,
            "active_paths": active_paths,
            "diagnostics": diagnostics,
        })
    }

    pub fn save_target(&self) -> &Path {
        self.save_target.as_path()
    }
}
