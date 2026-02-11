use std::borrow::Cow;

use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, Paragraph, Wrap};

use rcfg_lang::{ResolvedValue, Severity, ValueType};

use crate::app::App;
use crate::model::NodeKind;
use crate::state::UiMode;

#[derive(Debug, Clone, Copy)]
struct ScrollableParagraph {
    scroll_offset: usize,
    viewport_height: usize,
}

impl ScrollableParagraph {
    fn from_area(scroll_offset: usize, area: Rect) -> Self {
        Self {
            scroll_offset,
            viewport_height: usize::from(area.height.saturating_sub(2)).max(1),
        }
    }

    fn visible_lines<'a>(&self, lines: &[Line<'a>]) -> Vec<Line<'a>> {
        if lines.is_empty() {
            return Vec::new();
        }

        let max_offset = lines.len().saturating_sub(self.viewport_height);
        let start = self.scroll_offset.min(max_offset);
        let end = (start + self.viewport_height).min(lines.len());
        lines[start..end].to_vec()
    }
}

pub fn render_frame(frame: &mut Frame<'_>, app: &App) {
    let areas = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(8),
            Constraint::Length(7),
            Constraint::Length(2),
        ])
        .split(frame.area());

    let body = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(45), Constraint::Percentage(55)])
        .split(areas[0]);

    render_tree_panel(frame, app, body[0]);
    render_detail_panel(frame, app, body[1]);
    render_diagnostics_panel(frame, app, areas[1]);
    render_status_bar(frame, app, areas[2]);

    match &app.state.mode {
        UiMode::SavePrompt(_) => render_save_overlay(frame, app),
        UiMode::Help => render_help_overlay(frame, app),
        UiMode::Editing(_) => render_editing_overlay(frame, app),
        UiMode::EnumPicker(_) => render_enum_picker_overlay(frame, app),
        UiMode::DiagnosticsFocus(_) | UiMode::Normal => {}
    }
}

fn render_tree_panel(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let visible: Cow<'_, [usize]> = app
        .state
        .cached_visible_nodes()
        .map(Cow::Borrowed)
        .unwrap_or_else(|| Cow::Owned(app.state.visible_nodes()));
    let viewport = usize::from(app.state.tree_viewport_height);
    let start = app
        .state
        .scroll_offset
        .min(visible.len().saturating_sub(viewport.max(1)));
    let end = if viewport == 0 {
        visible.len()
    } else {
        (start + viewport).min(visible.len())
    };

    let items = visible[start..end]
        .into_iter()
        .filter_map(|node_id| {
            let node = app.state.tree.node(*node_id)?;
            let indent = "  ".repeat(node.depth);
            let marker = if *node_id == app.state.selected {
                ">"
            } else {
                " "
            };
            let overridden =
                node.kind == NodeKind::Option && app.state.user_values.contains_key(&node.path);
            let override_mark = if overridden { "*" } else { " " };
            let expand = if node.kind == NodeKind::Module {
                if app.state.expanded.contains(&node.id) {
                    "▾"
                } else {
                    "▸"
                }
            } else {
                " "
            };
            let status = if node.kind == NodeKind::Option {
                if app.state.active_paths.contains(&node.path) {
                    ""
                } else {
                    " (inactive)"
                }
            } else {
                ""
            };
            let value_summary = if node.kind == NodeKind::Option {
                app.state
                    .resolved_values
                    .get(&node.path)
                    .map(|(value, source)| {
                        let display_value = if node.is_secret {
                            "***".to_string()
                        } else {
                            format_value(value)
                        };
                        format!(" = {} [{}]", display_value, format_value_source(*source))
                    })
                    .unwrap_or_default()
            } else {
                String::new()
            };
            let text = format!(
                "{}{}{}{} {} {}{}{}",
                marker,
                indent,
                override_mark,
                expand,
                icon(node.kind),
                node.name,
                status,
                value_summary
            );
            let mut style = Style::default();
            if node.kind == NodeKind::Option && !app.state.active_paths.contains(&node.path) {
                style = style.fg(Color::DarkGray);
            } else if overridden {
                style = style.fg(Color::Green);
            }
            if *node_id == app.state.selected {
                style = style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
            }

            Some(ListItem::new(Line::from(text)).style(style))
        })
        .collect::<Vec<_>>();

    let tree = List::new(items).block(Block::default().title("Tree").borders(Borders::ALL));

    frame.render_widget(tree, area);
}

fn render_detail_panel(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let mut lines = Vec::new();

    if let Some(node) = app.state.tree.node(app.state.selected) {
        lines.push(Line::from(vec![
            Span::styled("path: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(node.path.clone()),
        ]));

        lines.push(Line::from(vec![
            Span::styled("kind: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(kind_label(node.kind)),
        ]));

        lines.push(Line::from(vec![
            Span::styled("type: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(format_value_type(node.value_type.as_ref())),
        ]));

        let active = if node.kind == NodeKind::Option {
            app.state.active_paths.contains(&node.path)
        } else {
            true
        };
        lines.push(Line::from(vec![
            Span::styled("active: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(if active { "yes" } else { "no" }),
        ]));

        let override_text = app
            .state
            .user_values
            .get(&node.path)
            .map(|value| {
                if node.is_secret {
                    "***".to_string()
                } else {
                    format_value(value)
                }
            })
            .unwrap_or_else(|| "<none>".to_string());
        lines.push(Line::from(vec![
            Span::styled("override: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(override_text),
        ]));

        let (resolved_text, source_text) = app
            .state
            .resolved_values
            .get(&node.path)
            .map(|(value, source)| {
                let text = if node.is_secret {
                    "***".to_string()
                } else {
                    format_value(value)
                };
                (text, format_value_source(*source))
            })
            .unwrap_or_else(|| ("<none>".to_string(), "-".to_string()));
        lines.push(Line::from(vec![
            Span::styled("resolved: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(resolved_text),
        ]));
        lines.push(Line::from(vec![
            Span::styled("source: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(source_text),
        ]));

        let secret_text = if node.is_secret { "yes" } else { "no" };
        lines.push(Line::from(vec![
            Span::styled("secret: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(secret_text),
        ]));

        if let Some(range) = &node.range {
            let rendered = if range.inclusive {
                format!("[{}, {}]", range.start, range.end)
            } else {
                format!("[{}, {})", range.start, range.end)
            };
            lines.push(Line::from(vec![
                Span::styled("range: ", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(rendered),
            ]));
        }

        if node.kind == NodeKind::Option && !node.enum_variants.is_empty() {
            lines.push(Line::from(vec![
                Span::styled("variants: ", Style::default().add_modifier(Modifier::BOLD)),
                Span::raw(node.enum_variants.join(" | ")),
            ]));
        }

        let (summary, help) = app.localized_docs_for_path(&node.path);

        lines.push(Line::default());
        lines.push(Line::styled(
            "summary:",
            Style::default().add_modifier(Modifier::BOLD),
        ));
        lines.push(Line::from(summary.unwrap_or_else(|| "(none)".to_string())));

        lines.push(Line::default());
        lines.push(Line::styled(
            "help:",
            Style::default().add_modifier(Modifier::BOLD),
        ));
        lines.push(Line::from(help.unwrap_or_else(|| "(none)".to_string())));
    }

    let scroller = ScrollableParagraph::from_area(app.state.detail_scroll_offset, area);
    let detail = Paragraph::new(scroller.visible_lines(&lines))
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Details").borders(Borders::ALL));

    frame.render_widget(detail, area);
}

fn render_diagnostics_panel(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let mut lines = Vec::new();
    let diagnostics = &app.state.diagnostics;
    let focused = match &app.state.mode {
        UiMode::DiagnosticsFocus(focus) if !diagnostics.is_empty() => {
            Some(focus.selected.min(diagnostics.len().saturating_sub(1)))
        }
        _ => None,
    };

    if diagnostics.is_empty() {
        lines.push(Line::from("no diagnostics"));
    } else {
        let viewport = usize::from(area.height.saturating_sub(2)).max(1);
        let max_offset = diagnostics.len().saturating_sub(viewport);
        let start = focused
            .map(|selected| selected.saturating_sub(viewport / 2).min(max_offset))
            .unwrap_or(0);
        let end = (start + viewport).min(diagnostics.len());

        for (index, diag) in diagnostics.iter().enumerate().take(end).skip(start) {
            let severity = match diag.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
            };
            let path = diag.path.as_deref().unwrap_or("<global>");
            let message = app.session.localize_diagnostic_message(diag);
            let selected = focused == Some(index);
            let prefix = if selected { ">" } else { " " };
            let style = if selected {
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            } else {
                Style::default()
            };
            lines.push(Line::styled(
                format!("{prefix}[{severity}] {path}: {message}"),
                style,
            ));
        }
    }

    let title = if focused.is_some() {
        "Diagnostics (focus)"
    } else {
        "Diagnostics"
    };

    let diagnostics = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title(title).borders(Borders::ALL));

    frame.render_widget(diagnostics, area);
}

fn render_status_bar(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let mut status = format!(
        "dirty={} | diagnostics={} | q quit | Ctrl+S save | F1 help",
        app.state.dirty,
        app.state.diagnostics.len()
    );

    if app.state.pending_quit_confirm {
        status.push_str(" | press q again to confirm exit");
    }

    if let Some(message) = &app.state.status_message {
        status.push_str(" | ");
        status.push_str(message);
    }

    let bar = Paragraph::new(status)
        .style(Style::default().fg(Color::Cyan))
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(bar, area);
}

fn render_save_overlay(frame: &mut Frame<'_>, app: &App) {
    let popup_area = centered_rect(70, 30, frame.area());
    frame.render_widget(Clear, popup_area);

    let mut lines = Vec::new();
    lines.push(Line::styled(
        "Save",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    lines.push(Line::from("Edit path and press Enter (Esc cancel)"));
    lines.push(Line::default());

    let (path, cursor_pos) = match &app.state.mode {
        UiMode::SavePrompt(prompt) => (prompt.buffer.to_string(), prompt.cursor_pos),
        _ => (app.save_target().display().to_string(), 0),
    };
    let cursor = cursor_pos.min(path.len());
    let (left, right) = path.split_at(cursor);
    lines.push(Line::from(vec![
        Span::raw(left.to_string()),
        Span::styled(
            " ",
            Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(right.to_string()),
    ]));

    let panel = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Save Path").borders(Borders::ALL));

    frame.render_widget(panel, popup_area);
}

fn render_help_overlay(frame: &mut Frame<'_>, app: &App) {
    let popup_area = centered_rect(75, 70, frame.area());
    frame.render_widget(Clear, popup_area);

    let shortcuts = [
        (
            "Up / Down",
            "Normal: move selection; Editing: no-op; DiagnosticsFocus: move diagnostic; Help: scroll docs",
        ),
        (
            "Enter",
            "Normal: toggle module or edit option; Editing: submit; EnumPicker: confirm; DiagnosticsFocus: jump",
        ),
        ("Space", "Toggle bool (Normal mode only)"),
        (
            "Esc",
            "Exit current mode (Editing/EnumPicker/SavePrompt/Help/DiagnosticsFocus)",
        ),
        ("F1", "Toggle help overlay"),
        ("Ctrl+S", "Open save prompt"),
        ("d", "Clear selected user override (Normal mode only)"),
        (
            "q",
            "Quit (dirty state requires double confirm, Normal mode only)",
        ),
        ("Tab", "Switch focus between Tree and Diagnostics"),
        (
            "Shift+Up / Shift+Down",
            "Scroll details panel (Normal mode only)",
        ),
        (
            "Left / Right / Home / End",
            "Move cursor in Editing/SavePrompt",
        ),
        ("Delete", "Delete char after cursor in Editing/SavePrompt"),
        (
            "Backspace",
            "Delete char before cursor in Editing/SavePrompt",
        ),
    ];

    let mut lines = Vec::new();
    lines.push(Line::styled(
        "Help (F1/Esc close)",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    lines.push(Line::default());

    lines.push(Line::styled(
        "Shortcuts:",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    for (key, action) in shortcuts {
        lines.push(Line::from(format!("{key:<26}{action}")));
    }

    lines.push(Line::default());
    lines.push(Line::styled(
        "Current Node:",
        Style::default().add_modifier(Modifier::BOLD),
    ));

    if let Some(node) = app.state.tree.node(app.state.selected) {
        let (label_key, help_key) = app.doc_keys_for_path(&node.path);
        let (summary, help) = app.localized_docs_for_path(&node.path);

        lines.push(Line::from(format!("path: {}", node.path)));
        lines.push(Line::from(format!("label_key: {label_key}")));
        lines.push(Line::from(format!("help_key:  {help_key}")));
        lines.push(Line::default());

        lines.push(Line::styled(
            "summary:",
            Style::default().add_modifier(Modifier::BOLD),
        ));
        lines.push(Line::from(summary.unwrap_or_else(|| "(none)".to_string())));
        lines.push(Line::default());

        lines.push(Line::styled(
            "help:",
            Style::default().add_modifier(Modifier::BOLD),
        ));
        lines.push(Line::from(help.unwrap_or_else(|| "(none)".to_string())));
    } else {
        lines.push(Line::from("(no selected node)"));
    }

    let scroller = ScrollableParagraph::from_area(app.state.help_scroll_offset, popup_area);
    let panel = Paragraph::new(scroller.visible_lines(&lines))
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Help").borders(Borders::ALL));

    frame.render_widget(panel, popup_area);
}

fn render_editing_overlay(frame: &mut Frame<'_>, app: &App) {
    let UiMode::Editing(editing) = &app.state.mode else {
        return;
    };

    let popup_area = centered_rect(75, 30, frame.area());
    frame.render_widget(Clear, popup_area);

    let cursor = editing.cursor_pos.min(editing.buffer.len());
    let (left, right) = editing.buffer.split_at(cursor);

    let mut lines = Vec::new();
    lines.push(Line::styled(
        "Edit Value",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    lines.push(Line::from(format!(
        "path: {} (Enter submit, Esc cancel)",
        editing.target_path
    )));
    lines.push(Line::default());
    lines.push(Line::from(vec![
        Span::raw(left.to_string()),
        Span::styled(
            " ",
            Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw(right.to_string()),
    ]));

    let panel = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Editing").borders(Borders::ALL));

    frame.render_widget(panel, popup_area);
}

fn render_enum_picker_overlay(frame: &mut Frame<'_>, app: &App) {
    let UiMode::EnumPicker(picker) = &app.state.mode else {
        return;
    };

    let popup_area = centered_rect(60, 45, frame.area());
    frame.render_widget(Clear, popup_area);

    let mut lines = Vec::new();
    lines.push(Line::styled(
        "Select Enum Variant",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    lines.push(Line::from(format!(
        "path: {} (Up/Down choose, Enter confirm, Esc cancel)",
        picker.target_path
    )));
    lines.push(Line::default());

    for (index, variant) in picker.variants.iter().enumerate() {
        let marker = if index == picker.selected { ">" } else { " " };
        lines.push(Line::from(format!("{marker} {variant}")));
    }

    let panel = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Enum Picker").borders(Borders::ALL));

    frame.render_widget(panel, popup_area);
}

fn centered_rect(percent_x: u16, percent_y: u16, area: Rect) -> Rect {
    let vertical = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(area);

    let horizontal = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(vertical[1]);

    horizontal[1]
}

fn icon(kind: NodeKind) -> &'static str {
    match kind {
        NodeKind::Module => "[M]",
        NodeKind::Option => "[O]",
        NodeKind::Enum => "[E]",
    }
}

fn kind_label(kind: NodeKind) -> &'static str {
    match kind {
        NodeKind::Module => "module",
        NodeKind::Option => "option",
        NodeKind::Enum => "enum",
    }
}

fn format_value_type(value_type: Option<&ValueType>) -> String {
    match value_type {
        Some(ValueType::Bool) => "bool".to_string(),
        Some(ValueType::String) => "string".to_string(),
        Some(ValueType::Unknown) => "unknown".to_string(),
        Some(ValueType::UntypedInt) => "int".to_string(),
        Some(ValueType::Int(ty)) => format!("{ty:?}").to_lowercase(),
        Some(ValueType::Enum(name)) => format!("enum<{name}>"),
        None => "-".to_string(),
    }
}

fn format_value(value: &ResolvedValue) -> String {
    match value {
        ResolvedValue::Bool(raw) => raw.to_string(),
        ResolvedValue::Int(raw) => raw.to_string(),
        ResolvedValue::String(raw) => raw.clone(),
        ResolvedValue::EnumVariant(raw) => raw.clone(),
    }
}

fn format_value_source(source: rcfg_lang::ValueSource) -> String {
    match source {
        rcfg_lang::ValueSource::User => "user".to_string(),
        rcfg_lang::ValueSource::Patch => "patch".to_string(),
        rcfg_lang::ValueSource::Default => "default".to_string(),
        rcfg_lang::ValueSource::Context => "context".to_string(),
    }
}
