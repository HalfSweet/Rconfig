use std::fs;
use std::io::{self, Write};

use crate::app::App;
use crate::event::{AppEvent, parse_script_line};
use crate::render::render_text;

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
    app.recompute();

    let mut stdout = io::stdout();
    render_text(&app.state, &mut stdout)?;
    stdout.flush().map_err(|err| err.to_string())?;

    Err("interactive terminal mode is not implemented yet".to_string())
}

pub fn apply_event(app: &mut App, event: AppEvent) -> Result<bool, String> {
    match event {
        AppEvent::Up => app.state.select_prev(),
        AppEvent::Down => app.state.select_next(),
        AppEvent::Enter => app.state.toggle_selected_expand(),
        AppEvent::Esc => app.state.clear_quit_confirmation(),
        AppEvent::Space => {
            let current = app
                .state
                .tree
                .node(app.state.selected)
                .and_then(|node| app.state.user_values.get(&node.path))
                .and_then(|value| match value {
                    rcfg_lang::ResolvedValue::Bool(raw) => Some(*raw),
                    _ => None,
                })
                .unwrap_or(false);
            app.state.set_bool_value(!current)?;
            app.recompute();
        }
        AppEvent::F1 => {}
        AppEvent::Save => {
            if app.has_blocking_errors() {
                return Ok(false);
            }
        }
        AppEvent::Quit => {
            if app.state.request_quit() {
                return Ok(true);
            }
        }
        AppEvent::Reset => {
            app.state.clear_selected_override();
            app.recompute();
        }
        AppEvent::Chars(text) => {
            app.state.set_text_value(text)?;
            app.recompute();
        }
        AppEvent::Resize(_, _) => {}
    }

    Ok(false)
}
