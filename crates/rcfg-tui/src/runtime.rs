use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::Duration;

use crossterm::event::{
    self, Event as CrosstermEvent, KeyCode, KeyEvent, KeyEventKind, KeyModifiers,
};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use rcfg_lang::ValueType;

use crate::app::App;
use crate::event::{AppEvent, parse_script_line};
use crate::render::render_frame;
use crate::state::{DiagFocusState, EditingState, PickerState, UiMode};

pub fn run_script_mode(app: &mut App, path: &std::path::Path) -> Result<(), String> {
    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read script {}: {err}", path.display()))?;

    for (index, line) in text.lines().enumerate() {
        let event =
            parse_script_line(line).map_err(|err| format!("script line {}: {err}", index + 1))?;
        let Some(event) = event else {
            continue;
        };

        if apply_event(app, event)? {
            break;
        }
    }

    let json = app.script_summary_json();
    let out = serde_json::to_string_pretty(&json)
        .map_err(|err| format!("failed to serialize script summary: {err}"))?;
    println!("{}", out);
    Ok(())
}

pub fn run_terminal_mode(app: &mut App) -> Result<(), String> {
    enable_raw_mode().map_err(|err| format!("failed to enable raw mode: {err}"))?;

    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen)
        .map_err(|err| format!("failed to enter alternate screen: {err}"))?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal =
        Terminal::new(backend).map_err(|err| format!("failed to create terminal: {err}"))?;

    let result = run_terminal_loop(&mut terminal, app);

    let _ = disable_raw_mode();
    let _ = execute!(terminal.backend_mut(), LeaveAlternateScreen);
    let _ = terminal.show_cursor();

    result
}

fn run_terminal_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut App,
) -> Result<(), String> {
    loop {
        let size = terminal
            .size()
            .map_err(|err| format!("failed to read terminal size: {err}"))?;
        let tree_height = compute_tree_viewport_height(size.height);
        app.state.set_tree_viewport_height(tree_height);

        terminal
            .draw(|frame| render_frame(frame, app))
            .map_err(|err| format!("failed to draw frame: {err}"))?;

        if !event::poll(Duration::from_millis(250))
            .map_err(|err| format!("failed to poll terminal event: {err}"))?
        {
            continue;
        }

        match event::read().map_err(|err| format!("failed to read terminal event: {err}"))? {
            CrosstermEvent::Key(key_event) if key_event.kind == KeyEventKind::Press => {
                if let Some(event) = map_key_event(key_event, &app.state.mode)
                    && apply_event(app, event)?
                {
                    break;
                }
            }
            CrosstermEvent::Paste(text) => {
                if let Some(event) = map_paste_event(text, &app.state.mode)
                    && apply_event(app, event)?
                {
                    break;
                }
            }
            CrosstermEvent::Resize(width, height) => {
                apply_event(app, AppEvent::Resize(width, height))?;
            }
            _ => {}
        }
    }

    Ok(())
}

fn map_key_event(key_event: KeyEvent, mode: &UiMode) -> Option<AppEvent> {
    match mode {
        UiMode::Normal => map_key_event_normal(key_event),
        UiMode::Editing(_) => map_key_event_text_input(key_event, false),
        UiMode::SavePrompt(_) => map_key_event_text_input(key_event, true),
        UiMode::Help => map_key_event_help(key_event),
        UiMode::EnumPicker(_) => map_key_event_enum_picker(key_event),
        UiMode::DiagnosticsFocus(_) => map_key_event_diag_focus(key_event),
    }
}

pub fn apply_event(app: &mut App, event: AppEvent) -> Result<bool, String> {
    if matches!(&app.state.mode, UiMode::Normal) {
        apply_event_normal(app, event)
    } else if matches!(&app.state.mode, UiMode::Editing(_)) {
        apply_event_editing(app, event)
    } else if matches!(&app.state.mode, UiMode::SavePrompt(_)) {
        apply_event_save_prompt(app, event)
    } else if matches!(&app.state.mode, UiMode::Help) {
        apply_event_help(app, event)
    } else if matches!(&app.state.mode, UiMode::EnumPicker(_)) {
        apply_event_enum_picker(app, event)
    } else {
        apply_event_diagnostics_focus(app, event)
    }
}

fn map_key_event_normal(key_event: KeyEvent) -> Option<AppEvent> {
    match (key_event.code, key_event.modifiers) {
        (KeyCode::Up, modifiers) if modifiers.contains(KeyModifiers::SHIFT) => {
            Some(AppEvent::ShiftUp)
        }
        (KeyCode::Down, modifiers) if modifiers.contains(KeyModifiers::SHIFT) => {
            Some(AppEvent::ShiftDown)
        }
        (KeyCode::Up, _) => Some(AppEvent::Up),
        (KeyCode::Down, _) => Some(AppEvent::Down),
        (KeyCode::Enter, _) => Some(AppEvent::Enter),
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
        (KeyCode::Tab, _) => Some(AppEvent::Tab),
        (KeyCode::F(1), _) => Some(AppEvent::F1),
        (KeyCode::Char(' '), _) => Some(AppEvent::Space),
        (KeyCode::Char('s'), modifiers) if modifiers.contains(KeyModifiers::CONTROL) => {
            Some(AppEvent::Save)
        }
        (KeyCode::Char('q'), _) => Some(AppEvent::Quit),
        (KeyCode::Char('d'), _) => Some(AppEvent::Reset),
        _ => None,
    }
}

fn map_key_event_text_input(key_event: KeyEvent, include_save: bool) -> Option<AppEvent> {
    match (key_event.code, key_event.modifiers) {
        (KeyCode::Enter, _) => Some(AppEvent::Enter),
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
        (KeyCode::Left, _) => Some(AppEvent::Left),
        (KeyCode::Right, _) => Some(AppEvent::Right),
        (KeyCode::Home, _) => Some(AppEvent::Home),
        (KeyCode::End, _) => Some(AppEvent::End),
        (KeyCode::Delete, _) => Some(AppEvent::Delete),
        (KeyCode::Backspace, _) => Some(AppEvent::Backspace),
        (KeyCode::Char('s'), modifiers)
            if include_save && modifiers.contains(KeyModifiers::CONTROL) =>
        {
            Some(AppEvent::Save)
        }
        (KeyCode::Char(ch), modifiers)
            if !modifiers.contains(KeyModifiers::CONTROL)
                && !modifiers.contains(KeyModifiers::ALT) =>
        {
            Some(AppEvent::InputChar(ch))
        }
        _ => None,
    }
}

fn map_key_event_help(key_event: KeyEvent) -> Option<AppEvent> {
    match (key_event.code, key_event.modifiers) {
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
        (KeyCode::F(1), _) => Some(AppEvent::F1),
        (KeyCode::Up, _) => Some(AppEvent::Up),
        (KeyCode::Down, _) => Some(AppEvent::Down),
        _ => None,
    }
}

fn map_key_event_enum_picker(key_event: KeyEvent) -> Option<AppEvent> {
    match (key_event.code, key_event.modifiers) {
        (KeyCode::Up, _) => Some(AppEvent::Up),
        (KeyCode::Down, _) => Some(AppEvent::Down),
        (KeyCode::Enter, _) => Some(AppEvent::Enter),
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
        _ => None,
    }
}

fn map_key_event_diag_focus(key_event: KeyEvent) -> Option<AppEvent> {
    match (key_event.code, key_event.modifiers) {
        (KeyCode::Up, _) => Some(AppEvent::Up),
        (KeyCode::Down, _) => Some(AppEvent::Down),
        (KeyCode::Enter, _) => Some(AppEvent::Enter),
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
        (KeyCode::Tab, _) => Some(AppEvent::Tab),
        _ => None,
    }
}

fn map_paste_event(text: String, mode: &UiMode) -> Option<AppEvent> {
    match mode {
        UiMode::Editing(_) | UiMode::SavePrompt(_) => Some(AppEvent::Paste(text)),
        _ => None,
    }
}

fn apply_event_normal(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Up => {
            app.state.select_prev();
            app.state.clear_status_message();
        }
        AppEvent::Down => {
            app.state.select_next();
            app.state.clear_status_message();
        }
        AppEvent::ShiftUp => {
            app.state.scroll_detail_up();
            app.state.clear_status_message();
        }
        AppEvent::ShiftDown => {
            app.state.scroll_detail_down();
            app.state.clear_status_message();
        }
        AppEvent::Enter => {
            let Some(selected_node) = app.state.tree.node(app.state.selected).cloned() else {
                return Ok(false);
            };

            match selected_node.kind {
                crate::model::NodeKind::Module => {
                    app.state.toggle_selected_expand();
                    app.state.clear_status_message();
                }
                crate::model::NodeKind::Option => {
                    if !app.state.active_paths.contains(&selected_node.path) {
                        app.state
                            .set_status_message("inactive option is not editable");
                        return Ok(false);
                    }

                    if matches!(selected_node.value_type, Some(ValueType::Enum(_))) {
                        if selected_node.enum_variants.is_empty() {
                            app.state
                                .set_status_message("enum option has no available variants");
                            return Ok(false);
                        }

                        let current = app.state.resolved_values.get(&selected_node.path).and_then(
                            |(value, _)| match value {
                                rcfg_lang::ResolvedValue::EnumVariant(raw) => {
                                    raw.rsplit("::").next().map(str::to_string)
                                }
                                _ => None,
                            },
                        );

                        let selected_index = current
                            .as_deref()
                            .and_then(|name| {
                                selected_node
                                    .enum_variants
                                    .iter()
                                    .position(|variant| variant == name)
                            })
                            .unwrap_or(0)
                            .min(selected_node.enum_variants.len().saturating_sub(1));

                        app.state.enter_mode(UiMode::EnumPicker(PickerState {
                            variants: selected_node.enum_variants,
                            selected: selected_index,
                            target_path: selected_node.path,
                        }));
                        app.state.clear_status_message();
                        return Ok(false);
                    }

                    let initial = resolved_text_for_option(app, &selected_node.path)
                        .or_else(|| {
                            app.state
                                .user_values
                                .get(&selected_node.path)
                                .map(format_user_value)
                        })
                        .unwrap_or_default();

                    app.state.enter_mode(UiMode::Editing(EditingState::new(
                        initial,
                        selected_node.path,
                    )));
                    app.state.clear_status_message();
                }
                crate::model::NodeKind::Enum => {
                    app.state.clear_status_message();
                }
            }
        }
        AppEvent::Esc => {
            app.state.clear_quit_confirmation();
            app.state.clear_status_message();
        }
        AppEvent::Space => {
            let Some(selected_node) = app.state.tree.node(app.state.selected) else {
                return Ok(false);
            };
            if selected_node.value_type != Some(ValueType::Bool) {
                app.state
                    .set_status_message("selected option is not bool and cannot toggle");
                return Ok(false);
            }

            let selected_path = selected_node.path.clone();
            let current = app
                .state
                .resolved_values
                .get(&selected_path)
                .and_then(|(value, _)| match value {
                    rcfg_lang::ResolvedValue::Bool(raw) => Some(*raw),
                    _ => None,
                })
                .unwrap_or(false);

            if let Err(err) = app.state.set_bool_value(!current) {
                app.state.set_status_message(err);
                return Ok(false);
            }
            app.recompute();
            app.state.clear_status_message();
        }
        AppEvent::F1 => {
            app.state.toggle_help_panel();
            app.state.clear_status_message();
        }
        AppEvent::Save => {
            app.state
                .open_save_prompt(app.save_target().display().to_string());
            app.state
                .set_status_message("edit save path then press Enter to save");
        }
        AppEvent::Quit => {
            if app.state.request_quit() {
                return Ok(true);
            }
            app.state
                .set_status_message("press q again to confirm quit");
        }
        AppEvent::Reset => {
            app.state.clear_selected_override();
            app.recompute();
            app.state.clear_status_message();
        }
        AppEvent::Chars(text) => {
            apply_text_override(app, text)?;
            app.recompute();
            app.state.clear_status_message();
        }
        AppEvent::Tab => {
            if app.state.diagnostics.is_empty() {
                app.state.set_status_message("no diagnostics to focus");
            } else {
                app.state
                    .enter_mode(UiMode::DiagnosticsFocus(DiagFocusState { selected: 0 }));
                app.state.clear_status_message();
            }
        }
        AppEvent::Resize(_, _)
        | AppEvent::InputChar(_)
        | AppEvent::Backspace
        | AppEvent::Delete
        | AppEvent::Left
        | AppEvent::Right
        | AppEvent::Home
        | AppEvent::End
        | AppEvent::Paste(_) => {}
    }

    Ok(false)
}

fn apply_event_help(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Resize(_, height) => {
            app.state
                .set_tree_viewport_height(compute_tree_viewport_height(height));
        }
        AppEvent::Esc | AppEvent::F1 => {
            app.state.close_help_panel();
            app.state.clear_status_message();
        }
        AppEvent::Up => {
            app.state.scroll_help_up();
        }
        AppEvent::Down => {
            app.state.scroll_help_down();
        }
        AppEvent::ShiftUp
        | AppEvent::ShiftDown
        | AppEvent::Left
        | AppEvent::Right
        | AppEvent::Home
        | AppEvent::End
        | AppEvent::Enter
        | AppEvent::Tab
        | AppEvent::Space
        | AppEvent::Save
        | AppEvent::Quit
        | AppEvent::Reset
        | AppEvent::Chars(_)
        | AppEvent::InputChar(_)
        | AppEvent::Backspace
        | AppEvent::Delete
        | AppEvent::Paste(_) => {}
    }

    Ok(false)
}

fn apply_event_save_prompt(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Resize(_, height) => {
            app.state
                .set_tree_viewport_height(compute_tree_viewport_height(height));
        }
        AppEvent::Enter | AppEvent::Save => {
            let target = app
                .state
                .save_prompt_path()
                .map(str::trim)
                .filter(|path| !path.is_empty())
                .map(PathBuf::from)
                .unwrap_or_else(|| app.save_target().to_path_buf());
            perform_save(app, target)?;
        }
        AppEvent::Esc => {
            app.state.close_save_prompt();
            app.state.set_status_message("save canceled");
        }
        AppEvent::InputChar(ch) => {
            app.state.push_save_prompt_char(ch);
        }
        AppEvent::Backspace => {
            app.state.pop_save_prompt_char();
        }
        AppEvent::Chars(text) | AppEvent::Paste(text) => {
            app.state.push_save_prompt_str(&text);
        }
        AppEvent::Left => {
            if let UiMode::SavePrompt(prompt) = &mut app.state.mode {
                prompt.move_left();
            }
        }
        AppEvent::Right => {
            if let UiMode::SavePrompt(prompt) = &mut app.state.mode {
                prompt.move_right();
            }
        }
        AppEvent::Home => {
            if let UiMode::SavePrompt(prompt) = &mut app.state.mode {
                prompt.move_home();
            }
        }
        AppEvent::End => {
            if let UiMode::SavePrompt(prompt) = &mut app.state.mode {
                prompt.move_end();
            }
        }
        AppEvent::Delete => {
            if let UiMode::SavePrompt(prompt) = &mut app.state.mode {
                prompt.delete();
            }
        }
        AppEvent::ShiftUp
        | AppEvent::ShiftDown
        | AppEvent::Up
        | AppEvent::Down
        | AppEvent::Tab
        | AppEvent::Space
        | AppEvent::F1
        | AppEvent::Quit
        | AppEvent::Reset => {}
    }

    Ok(false)
}

fn apply_event_editing(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Resize(_, height) => {
            app.state
                .set_tree_viewport_height(compute_tree_viewport_height(height));
        }
        AppEvent::Enter => {
            let Some(buffer) = editing_buffer(&app.state.mode) else {
                return Ok(false);
            };

            let previous_values = app.state.user_values.clone();
            let previous_errors = app
                .state
                .diagnostics
                .iter()
                .filter(|diag| diag.severity == rcfg_lang::Severity::Error)
                .count();

            match apply_text_override(app, buffer) {
                Ok(()) => {
                    app.recompute();
                    let current_errors = app
                        .state
                        .diagnostics
                        .iter()
                        .filter(|diag| diag.severity == rcfg_lang::Severity::Error)
                        .count();

                    if current_errors > previous_errors {
                        let rejected_message = app
                            .state
                            .diagnostics
                            .iter()
                            .find(|diag| diag.severity == rcfg_lang::Severity::Error)
                            .map(|diag| app.session.localize_diagnostic_message(diag))
                            .unwrap_or_else(|| "edit rejected by constraints".to_string());

                        app.state.user_values = previous_values;
                        app.recompute();
                        app.state
                            .set_status_message(mask_secret_message(app, &rejected_message));
                        return Ok(false);
                    }

                    app.state.exit_mode();
                    app.state.clear_status_message();
                }
                Err(err) => {
                    app.state.set_status_message(mask_secret_message(app, &err));
                }
            }
        }
        AppEvent::Esc => {
            app.state.exit_mode();
            app.state.set_status_message("edit canceled");
        }
        AppEvent::InputChar(ch) => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.insert_char(ch);
            }
        }
        AppEvent::Chars(text) | AppEvent::Paste(text) => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.insert_str(&text);
            }
        }
        AppEvent::Backspace => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.backspace();
            }
        }
        AppEvent::Delete => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.delete();
            }
        }
        AppEvent::Left => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.move_left();
            }
        }
        AppEvent::Right => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.move_right();
            }
        }
        AppEvent::Home => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.move_home();
            }
        }
        AppEvent::End => {
            if let UiMode::Editing(editing) = &mut app.state.mode {
                editing.move_end();
            }
        }
        AppEvent::ShiftUp
        | AppEvent::ShiftDown
        | AppEvent::Up
        | AppEvent::Down
        | AppEvent::Tab
        | AppEvent::Space
        | AppEvent::F1
        | AppEvent::Save
        | AppEvent::Quit
        | AppEvent::Reset => {}
    }

    Ok(false)
}

fn apply_event_enum_picker(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Resize(_, height) => {
            app.state
                .set_tree_viewport_height(compute_tree_viewport_height(height));
        }
        AppEvent::Up => {
            if let UiMode::EnumPicker(picker) = &mut app.state.mode
                && picker.selected > 0
            {
                picker.selected -= 1;
            }
        }
        AppEvent::Down => {
            if let UiMode::EnumPicker(picker) = &mut app.state.mode
                && picker.selected + 1 < picker.variants.len()
            {
                picker.selected += 1;
            }
        }
        AppEvent::Enter => {
            let Some((target_path, selected_variant)) = enum_picker_choice(&app.state.mode) else {
                return Ok(false);
            };

            match app.state.set_enum_value(selected_variant) {
                Ok(()) => {
                    app.recompute();
                    app.state.exit_mode();
                    app.state.clear_status_message();
                }
                Err(err) => {
                    app.state
                        .set_status_message(format!("{}: {err}", target_path));
                }
            }
        }
        AppEvent::Esc => {
            app.state.exit_mode();
            app.state.clear_status_message();
        }
        AppEvent::ShiftUp
        | AppEvent::ShiftDown
        | AppEvent::Left
        | AppEvent::Right
        | AppEvent::Home
        | AppEvent::End
        | AppEvent::Tab
        | AppEvent::Space
        | AppEvent::F1
        | AppEvent::Save
        | AppEvent::Quit
        | AppEvent::Reset
        | AppEvent::Chars(_)
        | AppEvent::InputChar(_)
        | AppEvent::Backspace
        | AppEvent::Delete
        | AppEvent::Paste(_) => {}
    }
    Ok(false)
}

fn apply_event_diagnostics_focus(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Resize(_, height) => {
            app.state
                .set_tree_viewport_height(compute_tree_viewport_height(height));
        }
        AppEvent::Esc | AppEvent::Tab => {
            app.state.exit_mode();
            app.state.clear_status_message();
        }
        AppEvent::Up => {
            if let UiMode::DiagnosticsFocus(focus) = &mut app.state.mode {
                focus.selected = focus.selected.saturating_sub(1);
            }
        }
        AppEvent::Down => {
            if let UiMode::DiagnosticsFocus(focus) = &mut app.state.mode
                && !app.state.diagnostics.is_empty()
            {
                let max_index = app.state.diagnostics.len().saturating_sub(1);
                focus.selected = (focus.selected + 1).min(max_index);
            }
        }
        AppEvent::Enter => {
            let Some(selected) = diag_focus_selected(&app.state.mode) else {
                return Ok(false);
            };

            let Some(diag) = app.state.diagnostics.get(selected) else {
                return Ok(false);
            };

            let Some(path) = diag.path.as_deref() else {
                return Ok(false);
            };

            if let Some(node_id) = app.state.tree.find_node_by_path(path) {
                app.state.select_node(node_id);
                app.state.exit_mode();
                app.state.clear_status_message();
            } else {
                app.state
                    .set_status_message(format!("diagnostic target not found: {path}"));
            }
        }
        AppEvent::ShiftUp
        | AppEvent::ShiftDown
        | AppEvent::Left
        | AppEvent::Right
        | AppEvent::Home
        | AppEvent::End
        | AppEvent::Space
        | AppEvent::F1
        | AppEvent::Save
        | AppEvent::Quit
        | AppEvent::Reset
        | AppEvent::Chars(_)
        | AppEvent::InputChar(_)
        | AppEvent::Backspace
        | AppEvent::Delete
        | AppEvent::Paste(_) => {}
    }
    Ok(false)
}

fn editing_buffer(mode: &UiMode) -> Option<String> {
    match mode {
        UiMode::Editing(editing) => Some(editing.buffer.to_string()),
        _ => None,
    }
}

fn enum_picker_choice(mode: &UiMode) -> Option<(String, String)> {
    match mode {
        UiMode::EnumPicker(picker) => {
            let selected = picker.variants.get(picker.selected)?.clone();
            Some((picker.target_path.clone(), selected))
        }
        _ => None,
    }
}

fn diag_focus_selected(mode: &UiMode) -> Option<usize> {
    match mode {
        UiMode::DiagnosticsFocus(focus) => Some(focus.selected),
        _ => None,
    }
}

fn resolved_text_for_option(app: &App, path: &str) -> Option<String> {
    app.state
        .resolved_values
        .get(path)
        .map(|(value, _)| format_user_value(value))
}

fn format_user_value(value: &rcfg_lang::ResolvedValue) -> String {
    match value {
        rcfg_lang::ResolvedValue::Bool(raw) => raw.to_string(),
        rcfg_lang::ResolvedValue::Int(raw) => raw.to_string(),
        rcfg_lang::ResolvedValue::String(raw) => raw.clone(),
        rcfg_lang::ResolvedValue::EnumVariant(raw) => raw.clone(),
    }
}

fn compute_tree_viewport_height(terminal_height: u16) -> u16 {
    let body_height = terminal_height.saturating_sub(7 + 2);
    body_height.saturating_sub(2)
}

fn perform_save(app: &mut App, target: PathBuf) -> Result<(), String> {
    if app.has_blocking_errors() {
        let error_count = app
            .state
            .diagnostics
            .iter()
            .filter(|diag| diag.severity == rcfg_lang::Severity::Error)
            .count();
        app.state.set_status_message(format!(
            "save blocked: {error_count} error(s) — see diagnostics panel"
        ));
        return Ok(());
    }

    let rendered = app.minimal_values_text();
    fs::write(&target, rendered)
        .map_err(|err| format!("failed to write {}: {err}", target.display()))?;

    app.state.dirty = false;
    app.state.clear_quit_confirmation();
    app.state.close_save_prompt();
    app.state
        .set_status_message(format!("saved: {}", target.display()));

    Ok(())
}

fn mask_secret_message(app: &App, message: &str) -> String {
    let mut masked = message.to_string();

    for node in &app.state.tree.nodes {
        if !node.is_secret {
            continue;
        }

        if let Some(value) = app.state.user_values.get(&node.path) {
            let raw = format_user_value(value);
            if !raw.is_empty() {
                masked = masked.replace(&raw, "***");
            }
        }

        if let Some((value, _)) = app.state.resolved_values.get(&node.path) {
            let raw = format_user_value(value);
            if !raw.is_empty() {
                masked = masked.replace(&raw, "***");
            }
        }
    }

    masked
}

fn apply_text_override(app: &mut App, text: String) -> Result<(), String> {
    let Some(node) = app.state.tree.node(app.state.selected) else {
        return Err("no selected node".to_string());
    };

    let Some(value_type) = node.value_type.clone() else {
        return Err("selected node is not editable option".to_string());
    };

    match value_type {
        ValueType::Bool => {
            let normalized = text.trim().to_lowercase();
            let value = match normalized.as_str() {
                "true" | "1" | "yes" | "on" => true,
                "false" | "0" | "no" | "off" => false,
                _ => {
                    return Err("bool option expects true/false".to_string());
                }
            };
            app.state.set_bool_value(value)
        }
        ValueType::Int(_) | ValueType::UntypedInt => {
            let value = text
                .trim()
                .parse::<i128>()
                .map_err(|_| "int option expects integer literal".to_string())?;
            app.state.set_int_value(value)
        }
        ValueType::Enum(_) => app.state.set_enum_value(text.trim().to_string()),
        ValueType::String | ValueType::Unknown => app.state.set_text_value(text),
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicUsize, Ordering};

    use rcfg_app::{AppLoadOptions, load_session};
    use rcfg_lang::{Diagnostic, Span};

    use super::*;
    use crate::app::App;

    static NEXT_TEST_ID: AtomicUsize = AtomicUsize::new(1);

    fn build_app(schema: &str) -> App {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("target")
            .join("tmp-tests")
            .join("runtime-diagnostics-focus");
        fs::create_dir_all(&root).expect("create runtime test root");

        let test_id = NEXT_TEST_ID.fetch_add(1, Ordering::Relaxed);
        let workdir = root.join(format!("case-{test_id}"));
        fs::create_dir_all(&workdir).expect("create runtime test case dir");

        let schema_path = workdir.join("schema.rcfg");
        fs::write(&schema_path, schema).expect("write schema file");

        let session = load_session(&AppLoadOptions {
            schema: Some(schema_path),
            manifest: None,
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session");

        let mut app = App::new(session, workdir.join("out.rcfgv"));
        app.recompute();
        app
    }

    fn test_diag(path: Option<&str>) -> Diagnostic {
        let diag = Diagnostic::error("E_TEST", "diagnostic for testing", Span::default());
        match path {
            Some(path) => diag.with_path(path),
            None => diag,
        }
    }

    #[test]
    fn tab_toggles_diagnostics_focus_mode() {
        let mut app = build_app(
            r#"
mod app {
  option enabled: bool = false;
}
"#,
        );
        app.state.diagnostics = vec![test_diag(Some("app::enabled"))];

        assert!(matches!(app.state.mode, UiMode::Normal));

        apply_event(&mut app, AppEvent::Tab).expect("enter diagnostics focus");
        assert!(
            matches!(&app.state.mode, UiMode::DiagnosticsFocus(focus) if focus.selected == 0),
            "expected diagnostics focus mode"
        );

        apply_event(&mut app, AppEvent::Tab).expect("exit diagnostics focus");
        assert!(matches!(app.state.mode, UiMode::Normal));
    }

    #[test]
    fn diagnostics_focus_moves_selection_and_enters_jump_target() {
        let mut app = build_app(
            r#"
mod app {
  option enabled: bool = false;
  option baud: u32 = 9600;
}
"#,
        );
        app.state.diagnostics = vec![
            test_diag(Some("app::enabled")),
            test_diag(Some("app::baud")),
        ];

        apply_event(&mut app, AppEvent::Tab).expect("enter diagnostics focus");
        apply_event(&mut app, AppEvent::Down).expect("move focus down");
        apply_event(&mut app, AppEvent::Down).expect("move focus down at boundary");

        let selected_diag = match &app.state.mode {
            UiMode::DiagnosticsFocus(focus) => focus.selected,
            _ => panic!("expected diagnostics focus mode"),
        };
        assert_eq!(selected_diag, 1);

        apply_event(&mut app, AppEvent::Enter).expect("jump to diagnostic node");

        assert!(matches!(app.state.mode, UiMode::Normal));
        let selected_path = app
            .state
            .tree
            .node(app.state.selected)
            .map(|node| node.path.clone())
            .expect("selected node");
        assert_eq!(selected_path, "app::baud");
    }

    #[test]
    fn diagnostics_focus_enter_without_path_is_noop() {
        let mut app = build_app(
            r#"
mod app {
  option enabled: bool = false;
}
"#,
        );
        app.state.diagnostics = vec![test_diag(None)];

        apply_event(&mut app, AppEvent::Tab).expect("enter diagnostics focus");
        let selected_before = app.state.selected;

        apply_event(&mut app, AppEvent::Enter).expect("enter on pathless diagnostic");

        assert_eq!(app.state.selected, selected_before);
        assert!(matches!(app.state.mode, UiMode::DiagnosticsFocus(_)));
    }
}
