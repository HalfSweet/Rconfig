use tower_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionResponse, Position, Url,
};

use crate::document::ProjectSnapshot;
use crate::position::lsp_position_to_offset;

pub fn provide(project: &ProjectSnapshot, uri: &Url, position: Position) -> Option<CompletionResponse> {
    let analysis = project.analysis.as_ref()?;
    let source = project.schema_docs.get(uri)?;
    let offset = lsp_position_to_offset(source, position);

    let prefix = completion_prefix(source, offset);

    let mut items = Vec::new();
    for (path, info) in analysis.symbols.iter() {
        if !path.contains(&prefix) && !path.starts_with(&prefix) {
            continue;
        }

        let label = path
            .rsplit("::")
            .next()
            .map(str::to_string)
            .unwrap_or_else(|| path.to_string());

        let kind = match info.kind {
            rcfg_lang::SymbolKind::Mod => CompletionItemKind::MODULE,
            rcfg_lang::SymbolKind::Enum => CompletionItemKind::ENUM,
            rcfg_lang::SymbolKind::Option => CompletionItemKind::FIELD,
        };

        items.push(CompletionItem {
            label,
            kind: Some(kind),
            detail: Some(path.to_string()),
            insert_text: Some(path.to_string()),
            ..CompletionItem::default()
        });
    }

    for occurrence in analysis.position_index.occurrences() {
        if !occurrence.path.contains("::") {
            continue;
        }
        if !occurrence.path.contains(&prefix) && !occurrence.path.starts_with(&prefix) {
            continue;
        }

        if analysis
            .symbols
            .enum_owner_of_variant(&occurrence.path)
            .is_some()
        {
            let label = occurrence
                .path
                .rsplit("::")
                .next()
                .unwrap_or(occurrence.path.as_str())
                .to_string();

            items.push(CompletionItem {
                label,
                kind: Some(CompletionItemKind::ENUM_MEMBER),
                detail: Some(occurrence.path.clone()),
                insert_text: Some(occurrence.path.clone()),
                ..CompletionItem::default()
            });
        }
    }

    items.sort_by(|left, right| left.label.cmp(&right.label));
    items.dedup_by(|left, right| {
        left.label == right.label
            && left.kind == right.kind
            && left.insert_text == right.insert_text
    });

    Some(CompletionResponse::Array(items))
}

fn completion_prefix(text: &str, offset: usize) -> String {
    let clamped = offset.min(text.len());
    let left = &text[..clamped];
    let token = left
        .split_whitespace()
        .next_back()
        .unwrap_or_default()
        .trim_matches(|ch: char| !ch.is_ascii_alphanumeric() && ch != ':' && ch != '_');

    token.to_string()
}
