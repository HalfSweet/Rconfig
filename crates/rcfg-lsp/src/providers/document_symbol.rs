use rcfg_lang::ast::Item;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use crate::document::ProjectSnapshot;
use crate::position::span_to_lsp_range;

#[allow(deprecated)]
pub fn provide(
    project: &ProjectSnapshot,
    uri: &tower_lsp::lsp_types::Url,
) -> Option<Vec<DocumentSymbol>> {
    let analysis = project.analysis.as_ref()?;
    let text = project.schema_docs.get(uri)?;

    let mut symbols = Vec::new();
    for item in analysis.symbols.schema_items() {
        if let Some(symbol) = build_symbol(item, text) {
            symbols.push(symbol);
        }
    }

    Some(symbols)
}

#[allow(deprecated)]
fn build_symbol(item: &Item, text: &str) -> Option<DocumentSymbol> {
    match item {
        Item::Mod(module) => {
            let mut children = Vec::new();
            for child in &module.items {
                if let Some(symbol) = build_symbol(child, text) {
                    children.push(symbol);
                }
            }
            Some(DocumentSymbol {
                name: module.name.value.clone(),
                detail: Some("mod".to_string()),
                kind: SymbolKind::MODULE,
                tags: None,
                deprecated: None,
                range: span_to_lsp_range(text, module.span),
                selection_range: span_to_lsp_range(text, module.name.span),
                children: Some(children),
            })
        }
        Item::Enum(enum_decl) => {
            let mut children = Vec::new();
            for variant in &enum_decl.variants {
                children.push(DocumentSymbol {
                    name: variant.name.value.clone(),
                    detail: Some("variant".to_string()),
                    kind: SymbolKind::ENUM_MEMBER,
                    tags: None,
                    deprecated: None,
                    range: span_to_lsp_range(text, variant.span),
                    selection_range: span_to_lsp_range(text, variant.name.span),
                    children: None,
                });
            }

            Some(DocumentSymbol {
                name: enum_decl.name.value.clone(),
                detail: Some("enum".to_string()),
                kind: SymbolKind::ENUM,
                tags: None,
                deprecated: None,
                range: span_to_lsp_range(text, enum_decl.span),
                selection_range: span_to_lsp_range(text, enum_decl.name.span),
                children: Some(children),
            })
        }
        Item::Option(option) => Some(DocumentSymbol {
            name: option.name.value.clone(),
            detail: Some("option".to_string()),
            kind: SymbolKind::FIELD,
            tags: None,
            deprecated: None,
            range: span_to_lsp_range(text, option.span),
            selection_range: span_to_lsp_range(text, option.name.span),
            children: None,
        }),
        _ => None,
    }
}
