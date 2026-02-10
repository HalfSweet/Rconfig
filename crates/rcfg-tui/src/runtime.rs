use std::fs;
use std::io;
use std::path::PathBuf;
use std::time::Duration;

use crossterm::event::{self, Event as CrosstermEvent, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;
use rcfg_lang::ValueType;

use crate::app::App;
use crate::event::{AppEvent, parse_script_line};
use crate::render::render_frame;

pub fn run_script_mode(app: &mut App, path: &std::path::Path) -> Result<(), String> {
    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read script {}: {err}", path.display()))?;

    for (index, line) in text.lines().enumerate() {
        let event = parse_script_line(line)
            .map_err(|err| format!("script line {}: {err}", index + 1))?;
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
                let in_save_prompt = app.state.save_prompt_path().is_some();
                if let Some(event) = map_key_event(key_event, in_save_prompt)
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

fn map_key_event(key_event: KeyEvent, in_save_prompt: bool) -> Option<AppEvent> {
    if in_save_prompt {
        return match (key_event.code, key_event.modifiers) {
            (KeyCode::Enter, _) => Some(AppEvent::Enter),
            (KeyCode::Esc, _) => Some(AppEvent::Esc),
            (KeyCode::Backspace, _) => Some(AppEvent::Backspace),
            (KeyCode::Char('s'), modifiers) if modifiers.contains(KeyModifiers::CONTROL) => {
                Some(AppEvent::Save)
            }
            (KeyCode::Char(ch), modifiers) if modifiers.is_empty() => Some(AppEvent::InputChar(ch)),
            _ => None,
        };
    }

    match (key_event.code, key_event.modifiers) {
        (KeyCode::Up, _) => Some(AppEvent::Up),
        (KeyCode::Down, _) => Some(AppEvent::Down),
        (KeyCode::Enter, _) => Some(AppEvent::Enter),
        (KeyCode::Esc, _) => Some(AppEvent::Esc),
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

pub fn apply_event(app: &mut App, event: AppEvent) -> Result<bool, String> {
    if app.state.help_visible {
        match event {
            AppEvent::Esc | AppEvent::F1 => {
                app.state.close_help_panel();
                app.state.clear_status_message();
            }
            _ => {}
        }
        return Ok(false);
    }

    if app.state.save_prompt_path().is_some() {
        match event {
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
            AppEvent::Chars(text) => {
                if let Some(path) = app.state.save_prompt_path.as_mut() {
                    path.push_str(&text);
                }
            }
            _ => {}
        }
        return Ok(false);
    }

    match event {
        AppEvent::Up => {
            app.state.select_prev();
            app.state.clear_status_message();
        }
        AppEvent::Down => {
            app.state.select_next();
            app.state.clear_status_message();
        }
        AppEvent::Enter => {
            app.state.toggle_selected_expand();
            app.state.clear_status_message();
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
                .session
                .resolve(&app.state.to_values_file())
                .options
                .into_iter()
                .find(|option| option.path == selected_path)
                .and_then(|option| option.value)
                .and_then(|value| match value {
                    rcfg_lang::ResolvedValue::Bool(raw) => Some(raw),
                    _ => None,
                })
                .unwrap_or(false);

            app.state.set_bool_value(!current)?;
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
        AppEvent::InputChar(_) | AppEvent::Backspace => {}
        AppEvent::Resize(_, _) => {}
    }

    Ok(false)
}

fn perform_save(app: &mut App, target: PathBuf) -> Result<(), String> {
    if app.has_blocking_errors() {
        app.state
            .set_status_message("save blocked: resolve diagnostics first");
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
