#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AppEvent {
    Up,
    Down,
    Enter,
    Esc,
    Space,
    F1,
    Save,
    Quit,
    Reset,
    Chars(String),
    InputChar(char),
    Backspace,
    Resize(u16, u16),
}

pub fn parse_script_line(line: &str) -> Result<Option<AppEvent>, String> {
    let line = line.trim();
    if line.is_empty() || line.starts_with('#') {
        return Ok(None);
    }

    let mut parts = line.splitn(2, char::is_whitespace);
    let command = parts.next().unwrap_or_default();
    let rest = parts.next().unwrap_or_default().trim().to_string();

    let event = match command {
        "up" => AppEvent::Up,
        "down" => AppEvent::Down,
        "enter" => AppEvent::Enter,
        "esc" => AppEvent::Esc,
        "space" => AppEvent::Space,
        "f1" => AppEvent::F1,
        "save" => AppEvent::Save,
        "quit" => AppEvent::Quit,
        "reset" => AppEvent::Reset,
        "chars" => AppEvent::Chars(rest),
        "backspace" => AppEvent::Backspace,
        _ => return Err(format!("unsupported script command `{command}`")),
    };

    Ok(Some(event))
}
