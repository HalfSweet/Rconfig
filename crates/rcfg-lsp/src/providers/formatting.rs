use rcfg_lang::{
    Severity, format_schema, format_values, parse_schema_with_diagnostics,
    parse_values_with_diagnostics,
};
use tower_lsp::lsp_types::{Position, Range, TextEdit};

use crate::document::DocumentKind;

pub fn provide(text: &str, kind: DocumentKind) -> Option<Vec<TextEdit>> {
    let formatted = match kind {
        DocumentKind::Schema => format_schema_text(text)?,
        DocumentKind::Values => format_values_text(text)?,
        DocumentKind::Unknown => return None,
    };

    if formatted == text {
        return Some(Vec::new());
    }

    Some(vec![TextEdit {
        range: full_document_range(text),
        new_text: formatted,
    }])
}

fn format_schema_text(text: &str) -> Option<String> {
    let (file, diagnostics) = parse_schema_with_diagnostics(text);
    if has_parse_errors(&diagnostics) {
        return None;
    }
    Some(format_schema(text, &file))
}

fn format_values_text(text: &str) -> Option<String> {
    let (values, diagnostics) = parse_values_with_diagnostics(text);
    if has_parse_errors(&diagnostics) {
        return None;
    }
    Some(format_values(text, &values))
}

fn has_parse_errors(diagnostics: &[rcfg_lang::Diagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
}

fn full_document_range(text: &str) -> Range {
    let mut line = 0u32;
    let mut character = 0u32;

    for ch in text.chars() {
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }

    Range {
        start: Position::new(0, 0),
        end: Position::new(line, character),
    }
}
