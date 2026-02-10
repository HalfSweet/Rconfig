use rcfg_lang::Span;
use tower_lsp::lsp_types::{Position, Range};

pub fn span_to_lsp_range(_text: &str, span: Span) -> Range {
    Range {
        start: Position::new(0, span.start as u32),
        end: Position::new(0, span.end as u32),
    }
}
