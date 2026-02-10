use rcfg_lang::Span;
use tower_lsp::lsp_types::{Position, Range};

fn line_starts(text: &str) -> Vec<usize> {
    let mut starts = vec![0usize];
    for (index, byte) in text.as_bytes().iter().enumerate() {
        if *byte == b'\n' {
            starts.push(index + 1);
        }
    }
    starts
}

pub fn offset_to_lsp_position(text: &str, offset: usize) -> Position {
    let starts = line_starts(text);
    let clamped = offset.min(text.len());
    let line = starts.partition_point(|start| *start <= clamped).saturating_sub(1);
    let line_start = starts[line];

    let utf16_col = text[line_start..clamped]
        .chars()
        .map(|ch| ch.len_utf16() as u32)
        .sum::<u32>();

    Position::new(line as u32, utf16_col)
}

pub fn lsp_position_to_offset(text: &str, position: Position) -> usize {
    let starts = line_starts(text);
    let line = (position.line as usize).min(starts.len().saturating_sub(1));
    let line_start = starts[line];
    let line_end = starts
        .get(line + 1)
        .copied()
        .unwrap_or(text.len());

    let target_utf16 = position.character;
    let mut current_utf16 = 0u32;
    let mut current_offset = line_start;

    for ch in text[line_start..line_end].chars() {
        let next_utf16 = current_utf16 + ch.len_utf16() as u32;
        if next_utf16 > target_utf16 {
            break;
        }

        current_utf16 = next_utf16;
        current_offset += ch.len_utf8();
    }

    if target_utf16 > current_utf16 {
        line_end
    } else {
        current_offset.min(line_end)
    }
}

pub fn span_to_lsp_range(text: &str, span: Span) -> Range {
    let clamped_span = Span {
        start: span.start.min(text.len()),
        end: span.end.min(text.len()),
    };
    Range {
        start: offset_to_lsp_position(text, clamped_span.start),
        end: offset_to_lsp_position(text, clamped_span.end),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset_to_position_handles_ascii() {
        let text = "abc\ndef\n";
        assert_eq!(offset_to_lsp_position(text, 0), Position::new(0, 0));
        assert_eq!(offset_to_lsp_position(text, 2), Position::new(0, 2));
        assert_eq!(offset_to_lsp_position(text, 4), Position::new(1, 0));
        assert_eq!(offset_to_lsp_position(text, text.len()), Position::new(2, 0));
    }

    #[test]
    fn offset_to_position_handles_utf16_characters() {
        let text = "中🙂a\n";
        assert_eq!(offset_to_lsp_position(text, 0), Position::new(0, 0));
        assert_eq!(offset_to_lsp_position(text, "中".len()), Position::new(0, 1));
        assert_eq!(
            offset_to_lsp_position(text, "中🙂".len()),
            Position::new(0, 3)
        );
        assert_eq!(
            offset_to_lsp_position(text, "中🙂a".len()),
            Position::new(0, 4)
        );
    }

    #[test]
    fn position_to_offset_handles_utf16_characters() {
        let text = "中🙂a\n";
        assert_eq!(lsp_position_to_offset(text, Position::new(0, 0)), 0);
        assert_eq!(
            lsp_position_to_offset(text, Position::new(0, 1)),
            "中".len()
        );
        assert_eq!(
            lsp_position_to_offset(text, Position::new(0, 3)),
            "中🙂".len()
        );
        assert_eq!(
            lsp_position_to_offset(text, Position::new(0, 4)),
            "中🙂a".len()
        );
    }

    #[test]
    fn position_to_offset_clamps_out_of_range() {
        let text = "abc\n";
        assert_eq!(lsp_position_to_offset(text, Position::new(0, 999)), 4);
        assert_eq!(lsp_position_to_offset(text, Position::new(999, 0)), text.len());
    }

    #[test]
    fn span_to_range_handles_multiline() {
        let text = "a\n中🙂b\n";
        let start = "a\n".len();
        let end = start + "中🙂".len();
        let range = span_to_lsp_range(text, Span::new(start, end));
        assert_eq!(range.start, Position::new(1, 0));
        assert_eq!(range.end, Position::new(1, 3));
    }

    #[test]
    fn span_to_range_clamps() {
        let text = "abc";
        let range = span_to_lsp_range(text, Span::new(0, 100));
        assert_eq!(range.start, Position::new(0, 0));
        assert_eq!(range.end, Position::new(0, 3));
    }
}
