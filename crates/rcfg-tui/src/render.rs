use std::io::Write;

use crate::model::NodeKind;
use crate::state::UiState;

pub fn render_text(state: &UiState, writer: &mut dyn Write) -> Result<(), String> {
    writeln!(writer, "rcfg menuconfig").map_err(|err| err.to_string())?;
    writeln!(writer, "selected: {}", selected_path(state)).map_err(|err| err.to_string())?;
    writeln!(writer, "dirty: {}", state.dirty).map_err(|err| err.to_string())?;
    writeln!(writer, "diagnostics: {}", state.diagnostics.len()).map_err(|err| err.to_string())?;

    for node_id in state.visible_nodes() {
        let Some(node) = state.tree.node(node_id) else {
            continue;
        };
        let marker = if node_id == state.selected { ">" } else { " " };
        let depth = depth_of(state, node_id);
        let indent = "  ".repeat(depth);
        let status = if node.kind == NodeKind::Option {
            if state.active_paths.contains(&node.path) {
                "active"
            } else {
                "inactive"
            }
        } else {
            ""
        };

        writeln!(writer, "{}{}{} {} {}", marker, indent, icon(node.kind.clone()), node.path, status)
            .map_err(|err| err.to_string())?;
    }

    Ok(())
}

fn icon(kind: NodeKind) -> &'static str {
    match kind {
        NodeKind::Module => "[M]",
        NodeKind::Option => "[O]",
        NodeKind::Enum => "[E]",
    }
}

fn selected_path(state: &UiState) -> String {
    state
        .tree
        .node(state.selected)
        .map(|node| node.path.clone())
        .unwrap_or_default()
}

fn depth_of(state: &UiState, node_id: usize) -> usize {
    let mut depth = 0usize;
    let mut current = state.tree.node(node_id).and_then(|node| node.parent_id);
    while let Some(parent_id) = current {
        depth += 1;
        current = state.tree.node(parent_id).and_then(|node| node.parent_id);
    }
    depth
}
