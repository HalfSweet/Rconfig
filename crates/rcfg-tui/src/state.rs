use std::collections::{BTreeMap, BTreeSet};

use rcfg_lang::ast::{AssignStmt, ValuesStmt};
use rcfg_lang::{
    Diagnostic, Path, ResolvedConfig, ResolvedValue, Spanned, Span, ValueExpr, ValuesFile,
};

use crate::model::{ConfigTree, NodeKind};

#[derive(Debug, Clone)]
pub struct UiState {
    pub tree: ConfigTree,
    pub selected: usize,
    pub expanded: BTreeSet<usize>,
    pub user_values: BTreeMap<String, ResolvedValue>,
    pub diagnostics: Vec<Diagnostic>,
    pub active_paths: BTreeSet<String>,
    pub dirty: bool,
    pub pending_quit_confirm: bool,
    pub help_visible: bool,
    pub status_message: Option<String>,
}

impl UiState {
    pub fn new(tree: ConfigTree) -> Self {
        let selected = tree.root_ids.first().copied().unwrap_or(0);
        Self {
            tree,
            selected,
            expanded: BTreeSet::new(),
            user_values: BTreeMap::new(),
            diagnostics: Vec::new(),
            active_paths: BTreeSet::new(),
            dirty: false,
            pending_quit_confirm: false,
            help_visible: false,
            status_message: None,
        }
    }

    pub fn visible_nodes(&self) -> Vec<usize> {
        let mut out = Vec::new();
        for root in &self.tree.root_ids {
            self.collect_visible(*root, &mut out);
        }
        out
    }

    pub fn select_next(&mut self) {
        let visible = self.visible_nodes();
        if visible.is_empty() {
            return;
        }

        let index = visible.iter().position(|id| *id == self.selected).unwrap_or(0);
        let next_index = (index + 1).min(visible.len().saturating_sub(1));
        self.selected = visible[next_index];
        self.pending_quit_confirm = false;
    }

    pub fn select_prev(&mut self) {
        let visible = self.visible_nodes();
        if visible.is_empty() {
            return;
        }

        let index = visible.iter().position(|id| *id == self.selected).unwrap_or(0);
        let prev_index = index.saturating_sub(1);
        self.selected = visible[prev_index];
        self.pending_quit_confirm = false;
    }

    pub fn toggle_selected_expand(&mut self) {
        let Some(node) = self.tree.node(self.selected) else {
            return;
        };
        if node.kind != NodeKind::Module {
            return;
        }

        if !self.expanded.insert(node.id) {
            self.expanded.remove(&node.id);
        }
        self.pending_quit_confirm = false;
    }

    pub fn set_active_from_resolved(&mut self, resolved: &ResolvedConfig) {
        self.active_paths.clear();
        for option in &resolved.options {
            if option.active {
                self.active_paths.insert(option.path.clone());
            }
        }
    }

    pub fn is_selected_active(&self) -> bool {
        self.tree
            .node(self.selected)
            .map(|node| {
                if node.kind == NodeKind::Option {
                    self.active_paths.contains(&node.path)
                } else {
                    true
                }
            })
            .unwrap_or(false)
    }

    pub fn set_bool_value(&mut self, value: bool) -> Result<(), String> {
        let Some(node) = self.tree.node(self.selected) else {
            return Err("no selected node".to_string());
        };
        if node.kind != NodeKind::Option {
            return Err("selected node is not option".to_string());
        }
        if !self.active_paths.contains(&node.path) {
            return Err("inactive option is not editable".to_string());
        }

        self.user_values
            .insert(node.path.clone(), ResolvedValue::Bool(value));
        self.dirty = true;
        self.pending_quit_confirm = false;
        Ok(())
    }

    pub fn set_text_value(&mut self, value: String) -> Result<(), String> {
        let Some(node) = self.tree.node(self.selected) else {
            return Err("no selected node".to_string());
        };
        if node.kind != NodeKind::Option {
            return Err("selected node is not option".to_string());
        }
        if !self.active_paths.contains(&node.path) {
            return Err("inactive option is not editable".to_string());
        }

        self.user_values
            .insert(node.path.clone(), ResolvedValue::String(value));
        self.dirty = true;
        self.pending_quit_confirm = false;
        Ok(())
    }

    pub fn set_int_value(&mut self, value: i128) -> Result<(), String> {
        let Some(node) = self.tree.node(self.selected) else {
            return Err("no selected node".to_string());
        };
        if node.kind != NodeKind::Option {
            return Err("selected node is not option".to_string());
        }
        if !self.active_paths.contains(&node.path) {
            return Err("inactive option is not editable".to_string());
        }

        self.user_values
            .insert(node.path.clone(), ResolvedValue::Int(value));
        self.dirty = true;
        self.pending_quit_confirm = false;
        Ok(())
    }

    pub fn set_enum_value(&mut self, value: String) -> Result<(), String> {
        let Some(node) = self.tree.node(self.selected) else {
            return Err("no selected node".to_string());
        };
        if node.kind != NodeKind::Option {
            return Err("selected node is not option".to_string());
        }
        if !self.active_paths.contains(&node.path) {
            return Err("inactive option is not editable".to_string());
        }

        self.user_values
            .insert(node.path.clone(), ResolvedValue::EnumVariant(value));
        self.dirty = true;
        self.pending_quit_confirm = false;
        Ok(())
    }

    pub fn clear_selected_override(&mut self) {
        if let Some(node) = self.tree.node(self.selected)
            && node.kind == NodeKind::Option
            && self.user_values.remove(&node.path).is_some()
        {
            self.dirty = true;
        }
        self.pending_quit_confirm = false;
    }

    pub fn request_quit(&mut self) -> bool {
        if !self.dirty {
            return true;
        }

        if self.pending_quit_confirm {
            return true;
        }

        self.pending_quit_confirm = true;
        false
    }

    pub fn clear_quit_confirmation(&mut self) {
        self.pending_quit_confirm = false;
    }

    pub fn toggle_help_panel(&mut self) {
        self.help_visible = !self.help_visible;
    }

    pub fn close_help_panel(&mut self) {
        self.help_visible = false;
    }

    pub fn set_status_message(&mut self, message: impl Into<String>) {
        self.status_message = Some(message.into());
    }

    pub fn clear_status_message(&mut self) {
        self.status_message = None;
    }

    pub fn apply_runtime_result(&mut self, resolved: &ResolvedConfig, diagnostics: Vec<Diagnostic>) {
        self.set_active_from_resolved(resolved);
        self.diagnostics = diagnostics;
    }

    pub fn to_values_file(&self) -> ValuesFile {
        let stmts = self
            .user_values
            .iter()
            .map(|(path, value)| {
                let assign = AssignStmt {
                    path: parse_symbol_path(path),
                    value: resolved_value_to_expr(value),
                    span: Span::default(),
                };
                ValuesStmt::Assign(assign)
            })
            .collect::<Vec<_>>();

        ValuesFile { stmts }
    }

    fn collect_visible(&self, node_id: usize, out: &mut Vec<usize>) {
        out.push(node_id);
        if !self.expanded.contains(&node_id) {
            return;
        }
        for child in self.tree.children_of(node_id) {
            self.collect_visible(*child, out);
        }
    }
}

fn parse_symbol_path(path: &str) -> Path {
    let segments = path
        .split("::")
        .map(|segment| Spanned::new(segment.to_string(), Span::default()))
        .collect::<Vec<_>>();
    Path {
        segments,
        span: Span::default(),
    }
}

fn resolved_value_to_expr(value: &ResolvedValue) -> ValueExpr {
    match value {
        ResolvedValue::Bool(raw) => ValueExpr::Bool(*raw, Span::default()),
        ResolvedValue::Int(raw) => ValueExpr::Int(*raw, Span::default()),
        ResolvedValue::String(raw) => ValueExpr::String(raw.clone(), Span::default()),
        ResolvedValue::EnumVariant(raw) => ValueExpr::Path(parse_symbol_path(raw)),
    }
}

#[cfg(test)]
mod tests {
    use rcfg_lang::{analyze_schema, parse_schema};

    use super::*;

    fn sample_state() -> UiState {
        let schema = parse_schema(
            r#"
mod app {
  option enabled: bool = false;
  when enabled {
    option baud: u32 = 115200;
  }
}
"#,
        )
        .expect("parse schema");
        let report = analyze_schema(&schema);
        let tree = ConfigTree::from_schema(&report.symbols);
        let mut state = UiState::new(tree);
        state.active_paths.insert("app::enabled".to_string());
        state
    }

    #[test]
    fn navigation_and_expand_work() {
        let mut state = sample_state();
        let roots = state.tree.root_ids.clone();
        assert!(!roots.is_empty());

        state.toggle_selected_expand();
        assert!(state.expanded.contains(&state.selected));

        let before = state.selected;
        state.select_next();
        assert_ne!(before, state.selected);

        state.select_prev();
        assert_eq!(before, state.selected);
    }

    #[test]
    fn inactive_option_is_not_editable() {
        let mut state = sample_state();

        state.toggle_selected_expand();
        state.select_next();
        state.select_next();

        let error = state
            .set_int_value(9600)
            .expect_err("inactive option should not be editable");
        assert!(error.contains("inactive option"), "{error}");
    }

    #[test]
    fn dirty_quit_requires_double_confirm() {
        let mut state = sample_state();

        state.toggle_selected_expand();
        state.select_next();
        state
            .set_bool_value(true)
            .expect("set selected bool override");

        assert!(!state.request_quit(), "first quit should ask confirm");
        assert!(state.request_quit(), "second quit should pass");
    }

    #[test]
    fn overrides_roundtrip_to_values_file() {
        let mut state = sample_state();
        state.toggle_selected_expand();
        state.select_next();
        state
            .set_bool_value(true)
            .expect("set selected bool override");

        let values = state.to_values_file();
        assert_eq!(values.stmts.len(), 1);
    }
}
