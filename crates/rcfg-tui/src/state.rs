use std::collections::{BTreeMap, BTreeSet};

use rcfg_lang::ast::{AssignStmt, ValuesStmt};
use rcfg_lang::{
    Diagnostic, Path, ResolvedConfig, ResolvedValue, Span, Spanned, ValueExpr, ValueSource,
    ValuesFile,
};

use crate::model::{ConfigTree, NodeKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EditingState {
    pub buffer: String,
    pub cursor_pos: usize,
    pub target_path: String,
}

impl EditingState {
    pub fn new(buffer: String, target_path: String) -> Self {
        let cursor_pos = buffer.len();
        Self {
            buffer,
            cursor_pos,
            target_path,
        }
    }

    pub fn insert_char(&mut self, ch: char) {
        self.buffer.insert(self.cursor_pos, ch);
        self.cursor_pos += ch.len_utf8();
    }

    pub fn insert_str(&mut self, text: &str) {
        self.buffer.insert_str(self.cursor_pos, text);
        self.cursor_pos += text.len();
    }

    pub fn backspace(&mut self) -> bool {
        if self.cursor_pos == 0 {
            return false;
        }
        let prev = previous_char_boundary(&self.buffer, self.cursor_pos);
        self.buffer.drain(prev..self.cursor_pos);
        self.cursor_pos = prev;
        true
    }

    pub fn delete(&mut self) -> bool {
        if self.cursor_pos >= self.buffer.len() {
            return false;
        }
        let next = next_char_boundary(&self.buffer, self.cursor_pos);
        self.buffer.drain(self.cursor_pos..next);
        true
    }

    pub fn move_left(&mut self) {
        if self.cursor_pos > 0 {
            self.cursor_pos = previous_char_boundary(&self.buffer, self.cursor_pos);
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor_pos < self.buffer.len() {
            self.cursor_pos = next_char_boundary(&self.buffer, self.cursor_pos);
        }
    }

    pub fn move_home(&mut self) {
        self.cursor_pos = 0;
    }

    pub fn move_end(&mut self) {
        self.cursor_pos = self.buffer.len();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PickerState {
    pub variants: Vec<String>,
    pub selected: usize,
    pub target_path: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PromptState {
    pub buffer: String,
    pub cursor_pos: usize,
}

impl PromptState {
    pub fn new(buffer: String) -> Self {
        let cursor_pos = buffer.len();
        Self { buffer, cursor_pos }
    }

    pub fn insert_char(&mut self, ch: char) {
        self.buffer.insert(self.cursor_pos, ch);
        self.cursor_pos += ch.len_utf8();
    }

    pub fn insert_str(&mut self, text: &str) {
        self.buffer.insert_str(self.cursor_pos, text);
        self.cursor_pos += text.len();
    }

    pub fn backspace(&mut self) -> bool {
        if self.cursor_pos == 0 {
            return false;
        }
        let prev = previous_char_boundary(&self.buffer, self.cursor_pos);
        self.buffer.drain(prev..self.cursor_pos);
        self.cursor_pos = prev;
        true
    }

    pub fn delete(&mut self) -> bool {
        if self.cursor_pos >= self.buffer.len() {
            return false;
        }
        let next = next_char_boundary(&self.buffer, self.cursor_pos);
        self.buffer.drain(self.cursor_pos..next);
        true
    }

    pub fn move_left(&mut self) {
        if self.cursor_pos > 0 {
            self.cursor_pos = previous_char_boundary(&self.buffer, self.cursor_pos);
        }
    }

    pub fn move_right(&mut self) {
        if self.cursor_pos < self.buffer.len() {
            self.cursor_pos = next_char_boundary(&self.buffer, self.cursor_pos);
        }
    }

    pub fn move_home(&mut self) {
        self.cursor_pos = 0;
    }

    pub fn move_end(&mut self) {
        self.cursor_pos = self.buffer.len();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagFocusState {
    pub selected: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UiMode {
    Normal,
    Editing(EditingState),
    EnumPicker(PickerState),
    SavePrompt(PromptState),
    Help,
    DiagnosticsFocus(DiagFocusState),
}

#[derive(Debug, Clone)]
pub struct UiState {
    pub tree: ConfigTree,
    pub selected: usize,
    pub expanded: BTreeSet<usize>,
    pub cached_visible: Option<Vec<usize>>,
    pub scroll_offset: usize,
    pub tree_viewport_height: u16,
    pub user_values: BTreeMap<String, ResolvedValue>,
    pub resolved_values: BTreeMap<String, (ResolvedValue, ValueSource)>,
    pub diagnostics: Vec<Diagnostic>,
    pub active_paths: BTreeSet<String>,
    pub dirty: bool,
    pub pending_quit_confirm: bool,
    pub mode: UiMode,
    pub status_message: Option<String>,
}

impl UiState {
    pub fn new(tree: ConfigTree) -> Self {
        let selected = tree.root_ids.first().copied().unwrap_or(0);
        Self {
            tree,
            selected,
            expanded: BTreeSet::new(),
            cached_visible: None,
            scroll_offset: 0,
            tree_viewport_height: 0,
            user_values: BTreeMap::new(),
            resolved_values: BTreeMap::new(),
            diagnostics: Vec::new(),
            active_paths: BTreeSet::new(),
            dirty: false,
            pending_quit_confirm: false,
            mode: UiMode::Normal,
            status_message: None,
        }
    }

    pub fn visible_nodes(&self) -> Vec<usize> {
        if let Some(cached) = &self.cached_visible {
            return cached.clone();
        }

        let mut out = Vec::new();
        for root in &self.tree.root_ids {
            self.collect_visible(*root, &mut out);
        }
        out
    }

    pub fn visible_nodes_cached(&mut self) -> &Vec<usize> {
        if self.cached_visible.is_none() {
            let mut computed = Vec::new();
            for root in &self.tree.root_ids {
                self.collect_visible(*root, &mut computed);
            }
            self.cached_visible = Some(computed);
        }

        self.cached_visible
            .as_ref()
            .expect("visible node cache must be initialized")
    }

    pub fn select_next(&mut self) {
        let visible = self.visible_nodes_cached().clone();
        if visible.is_empty() {
            self.scroll_offset = 0;
            return;
        }

        let index = visible
            .iter()
            .position(|id| *id == self.selected)
            .unwrap_or(0);
        let next_index = (index + 1).min(visible.len().saturating_sub(1));
        self.selected = visible[next_index];
        self.ensure_selected_in_view(&visible);
        self.pending_quit_confirm = false;
    }

    pub fn select_prev(&mut self) {
        let visible = self.visible_nodes_cached().clone();
        if visible.is_empty() {
            self.scroll_offset = 0;
            return;
        }

        let index = visible
            .iter()
            .position(|id| *id == self.selected)
            .unwrap_or(0);
        let prev_index = index.saturating_sub(1);
        self.selected = visible[prev_index];
        self.ensure_selected_in_view(&visible);
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
        self.cached_visible = None;
        self.calibrate_scroll_offset();
        self.pending_quit_confirm = false;
    }

    pub fn set_tree_viewport_height(&mut self, height: u16) {
        self.tree_viewport_height = height;
        self.calibrate_scroll_offset();
    }

    pub fn calibrate_scroll_offset(&mut self) {
        let visible = self.visible_nodes_cached().clone();
        if visible.is_empty() {
            self.scroll_offset = 0;
            return;
        }

        self.ensure_selected_in_view(&visible);
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
        if matches!(self.mode, UiMode::Help) {
            self.exit_mode();
            return;
        }

        self.enter_mode(UiMode::Help);
    }

    pub fn close_help_panel(&mut self) {
        if matches!(self.mode, UiMode::Help) {
            self.exit_mode();
        }
    }

    pub fn set_status_message(&mut self, message: impl Into<String>) {
        self.status_message = Some(message.into());
    }

    pub fn clear_status_message(&mut self) {
        self.status_message = None;
    }

    pub fn open_save_prompt(&mut self, default_path: String) {
        self.enter_mode(UiMode::SavePrompt(PromptState::new(default_path)));
    }

    pub fn close_save_prompt(&mut self) {
        if matches!(self.mode, UiMode::SavePrompt(_)) {
            self.exit_mode();
        }
    }

    pub fn save_prompt_path(&self) -> Option<&str> {
        match &self.mode {
            UiMode::SavePrompt(prompt) => Some(prompt.buffer.as_str()),
            _ => None,
        }
    }

    pub fn push_save_prompt_char(&mut self, ch: char) {
        if let UiMode::SavePrompt(prompt) = &mut self.mode {
            prompt.insert_char(ch);
        }
    }

    pub fn push_save_prompt_str(&mut self, text: &str) {
        if let UiMode::SavePrompt(prompt) = &mut self.mode {
            prompt.insert_str(text);
        }
    }

    pub fn pop_save_prompt_char(&mut self) {
        if let UiMode::SavePrompt(prompt) = &mut self.mode {
            prompt.backspace();
        }
    }

    pub fn enter_mode(&mut self, mode: UiMode) {
        self.mode = mode;
        self.pending_quit_confirm = false;
    }

    pub fn exit_mode(&mut self) {
        self.mode = UiMode::Normal;
        self.pending_quit_confirm = false;
    }

    pub fn apply_runtime_result(
        &mut self,
        resolved: &ResolvedConfig,
        diagnostics: Vec<Diagnostic>,
    ) {
        self.cached_visible = None;
        self.set_active_from_resolved(resolved);
        self.resolved_values = resolved
            .options
            .iter()
            .filter_map(|option| {
                let value = option.value.as_ref()?.clone();
                let source = option.source?;
                Some((option.path.clone(), (value, source)))
            })
            .collect::<BTreeMap<_, _>>();
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

    fn ensure_selected_in_view(&mut self, visible: &[usize]) {
        if visible.is_empty() {
            self.scroll_offset = 0;
            return;
        }

        let selected_index = visible
            .iter()
            .position(|id| *id == self.selected)
            .unwrap_or_else(|| {
                self.selected = visible[0];
                0
            });

        let viewport = usize::from(self.tree_viewport_height);
        if viewport == 0 {
            self.scroll_offset = 0;
            return;
        }

        let max_offset = visible.len().saturating_sub(viewport);
        let margin = 2usize.min(viewport.saturating_sub(1));
        let mut offset = self.scroll_offset.min(max_offset);

        if selected_index < offset + margin {
            offset = selected_index.saturating_sub(margin);
        } else {
            let visible_bottom_with_margin = offset + viewport.saturating_sub(1 + margin);
            if selected_index > visible_bottom_with_margin {
                offset = selected_index + margin + 1 - viewport;
            }
        }

        self.scroll_offset = offset.min(max_offset);
    }
}

fn previous_char_boundary(text: &str, index: usize) -> usize {
    if index == 0 {
        return 0;
    }

    let mut cursor = index.min(text.len()) - 1;
    while !text.is_char_boundary(cursor) {
        cursor -= 1;
    }
    cursor
}

fn next_char_boundary(text: &str, index: usize) -> usize {
    let mut cursor = index.min(text.len());
    if cursor >= text.len() {
        return text.len();
    }

    cursor += 1;
    while cursor < text.len() && !text.is_char_boundary(cursor) {
        cursor += 1;
    }
    cursor
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

    #[test]
    fn mode_switch_is_mutually_exclusive() {
        let mut state = sample_state();

        state.toggle_help_panel();
        assert!(matches!(state.mode, UiMode::Help));

        state.open_save_prompt("demo.rcfgv".to_string());
        assert!(matches!(state.mode, UiMode::SavePrompt(_)));

        state.exit_mode();
        assert!(matches!(state.mode, UiMode::Normal));
    }

    #[test]
    fn prompt_state_cursor_edits_utf8_safely() {
        let mut prompt = PromptState::new("ab你".to_string());
        prompt.move_left();
        prompt.backspace();
        assert_eq!(prompt.buffer, "a你");

        prompt.move_home();
        prompt.insert_char('文');
        assert_eq!(prompt.buffer, "文a你");
    }

    #[test]
    fn scroll_keeps_selected_inside_viewport_for_large_tree() {
        let schema = parse_schema(
            r#"
mod app {
  option o00: bool = false;
  option o01: bool = false;
  option o02: bool = false;
  option o03: bool = false;
  option o04: bool = false;
  option o05: bool = false;
  option o06: bool = false;
  option o07: bool = false;
  option o08: bool = false;
  option o09: bool = false;
  option o10: bool = false;
  option o11: bool = false;
  option o12: bool = false;
  option o13: bool = false;
  option o14: bool = false;
  option o15: bool = false;
  option o16: bool = false;
  option o17: bool = false;
  option o18: bool = false;
  option o19: bool = false;
  option o20: bool = false;
  option o21: bool = false;
  option o22: bool = false;
  option o23: bool = false;
  option o24: bool = false;
  option o25: bool = false;
  option o26: bool = false;
  option o27: bool = false;
  option o28: bool = false;
  option o29: bool = false;
  option o30: bool = false;
  option o31: bool = false;
  option o32: bool = false;
  option o33: bool = false;
  option o34: bool = false;
  option o35: bool = false;
  option o36: bool = false;
  option o37: bool = false;
  option o38: bool = false;
  option o39: bool = false;
  option o40: bool = false;
  option o41: bool = false;
  option o42: bool = false;
  option o43: bool = false;
  option o44: bool = false;
  option o45: bool = false;
  option o46: bool = false;
  option o47: bool = false;
  option o48: bool = false;
  option o49: bool = false;
  option o50: bool = false;
  option o51: bool = false;
  option o52: bool = false;
  option o53: bool = false;
  option o54: bool = false;
}
"#,
        )
        .expect("parse schema");

        let report = analyze_schema(&schema);
        let tree = ConfigTree::from_schema(&report.symbols);
        let mut state = UiState::new(tree);
        state.toggle_selected_expand();
        state.set_tree_viewport_height(8);

        for _ in 0..90 {
            state.select_next();

            let visible = state.visible_nodes();
            let selected = visible
                .iter()
                .position(|id| *id == state.selected)
                .expect("selected in visible");

            assert!(selected >= state.scroll_offset);
            assert!(selected < state.scroll_offset + usize::from(state.tree_viewport_height));
        }
    }

    #[test]
    fn resize_calibrates_scroll_offset_within_bounds() {
        let mut state = sample_state();
        state.toggle_selected_expand();
        state.set_tree_viewport_height(1);

        state.select_next();
        state.select_next();
        assert!(state.scroll_offset > 0);

        state.set_tree_viewport_height(20);

        let visible_len = state.visible_nodes().len();
        let viewport = usize::from(state.tree_viewport_height);
        let max_offset = visible_len.saturating_sub(viewport);
        assert!(state.scroll_offset <= max_offset);
    }
}
