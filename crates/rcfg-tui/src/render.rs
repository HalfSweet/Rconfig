use ratatui::Frame;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, Paragraph, Wrap};

use rcfg_lang::{ResolvedValue, Severity, ValueType};

use crate::app::App;
use crate::model::NodeKind;
use crate::state::UiMode;

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
        UiMode::EnumPicker(_) | UiMode::DiagnosticsFocus(_) | UiMode::Normal => {}
    }
}

fn render_tree_panel(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let visible = app.state.visible_nodes();
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
            let depth = depth_of(app, *node_id);
            let indent = "  ".repeat(depth);
            let marker = if *node_id == app.state.selected {
                ">"
            } else {
                " "
            };
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
            let text = format!(
                "{}{}{} {} {}{}",
                marker,
                indent,
                expand,
                icon(node.kind.clone()),
                node.name,
                status
            );
            let mut style = Style::default();
            if node.kind == NodeKind::Option && !app.state.active_paths.contains(&node.path) {
                style = style.fg(Color::DarkGray);
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
            Span::raw(kind_label(node.kind.clone())),
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
            .map(format_value)
            .unwrap_or_else(|| "<none>".to_string());
        lines.push(Line::from(vec![
            Span::styled("override: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(override_text),
        ]));

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

    let detail = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Details").borders(Borders::ALL));

    frame.render_widget(detail, area);
}

fn render_diagnostics_panel(frame: &mut Frame<'_>, app: &App, area: Rect) {
    let mut lines = Vec::new();

    if app.state.diagnostics.is_empty() {
        lines.push(Line::from("no diagnostics"));
    } else {
        for diag in app.state.diagnostics.iter().take(4) {
            let severity = match diag.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
            };
            let path = diag.path.as_deref().unwrap_or("<global>");
            let message = app.session.localize_diagnostic_message(diag);
            lines.push(Line::from(format!("[{severity}] {path}: {message}")));
        }
        if app.state.diagnostics.len() > 4 {
            lines.push(Line::from("..."));
        }
    }

    let diagnostics = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Diagnostics").borders(Borders::ALL));

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

    let path = app
        .state
        .save_prompt_path()
        .map(str::to_string)
        .unwrap_or_else(|| app.save_target().display().to_string());
    lines.push(Line::from(path));

    let panel = Paragraph::new(lines)
        .wrap(Wrap { trim: false })
        .block(Block::default().title("Save Path").borders(Borders::ALL));

    frame.render_widget(panel, popup_area);
}

fn render_help_overlay(frame: &mut Frame<'_>, app: &App) {
    let popup_area = centered_rect(75, 70, frame.area());
    frame.render_widget(Clear, popup_area);

    let mut lines = Vec::new();
    lines.push(Line::styled(
        "Help (F1/Esc close)",
        Style::default().add_modifier(Modifier::BOLD),
    ));
    lines.push(Line::default());

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
    }

    let panel = Paragraph::new(lines)
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

fn depth_of(app: &App, node_id: usize) -> usize {
    let mut depth = 0usize;
    let mut current = app.state.tree.node(node_id).and_then(|node| node.parent_id);
    while let Some(parent_id) = current {
        depth += 1;
        current = app
            .state
            .tree
            .node(parent_id)
            .and_then(|node| node.parent_id);
    }
    depth
}
