use std::fs;
use std::io;
use std::time::Duration;

use crossterm::event::{self, Event as CrosstermEvent, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

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
                if let Some(event) = map_key_event(key_event)
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

fn map_key_event(key_event: KeyEvent) -> Option<AppEvent> {
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
    match event {
        AppEvent::Up => app.state.select_prev(),
        AppEvent::Down => app.state.select_next(),
        AppEvent::Enter => app.state.toggle_selected_expand(),
        AppEvent::Esc => app.state.clear_quit_confirmation(),
        AppEvent::Space => {
            let selected_path = app
                .state
                .tree
                .node(app.state.selected)
                .map(|node| node.path.clone())
                .ok_or_else(|| "no selected node".to_string())?;

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
        }
        AppEvent::F1 => {}
        AppEvent::Save => {
            if app.has_blocking_errors() {
                return Ok(false);
            }

            let rendered = app.minimal_values_text();
            let target = app.save_target();
            fs::write(target, rendered)
                .map_err(|err| format!("failed to write {}: {err}", target.display()))?;
            app.state.dirty = false;
            app.state.clear_quit_confirmation();
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
