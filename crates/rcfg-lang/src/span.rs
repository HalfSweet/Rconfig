#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

impl LineCol {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextPosition {
    pub line: usize,
    pub character: usize,
}

impl TextPosition {
    pub fn new(line: usize, character: usize) -> Self {
        Self { line, character }
    }
}

#[derive(Debug, Clone)]
pub struct SourceIndex {
    source_len: usize,
    line_starts: Vec<usize>,
}

impl SourceIndex {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0usize];
        for (index, byte) in source.as_bytes().iter().enumerate() {
            if *byte == b'\n' {
                line_starts.push(index + 1);
            }
        }
        Self {
            source_len: source.len(),
            line_starts,
        }
    }

    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    pub fn clamp_offset(&self, offset: usize) -> usize {
        offset.min(self.source_len)
    }

    pub fn offset_to_position(&self, offset: usize) -> TextPosition {
        let clamped = self.clamp_offset(offset);
        let line_index = self
            .line_starts
            .partition_point(|start| *start <= clamped)
            .saturating_sub(1);
        let line_start = self.line_starts[line_index];
        TextPosition::new(line_index, clamped.saturating_sub(line_start))
    }

    pub fn offset_to_line_col(&self, offset: usize) -> LineCol {
        let pos = self.offset_to_position(offset);
        LineCol::new(pos.line + 1, pos.character + 1)
    }

    pub fn position_to_offset(&self, position: TextPosition) -> Option<usize> {
        let line_start = *self.line_starts.get(position.line)?;
        let line_end = self
            .line_starts
            .get(position.line + 1)
            .copied()
            .unwrap_or(self.source_len);

        let offset = line_start.saturating_add(position.character);
        if offset > line_end {
            None
        } else {
            Some(offset)
        }
    }

    pub fn line_col_to_offset(&self, line_col: LineCol) -> Option<usize> {
        if line_col.line == 0 || line_col.column == 0 {
            return None;
        }
        self.position_to_offset(TextPosition::new(line_col.line - 1, line_col.column - 1))
    }

    pub fn span_to_line_cols(&self, span: Span) -> (LineCol, LineCol) {
        (
            self.offset_to_line_col(span.start),
            self.offset_to_line_col(span.end),
        )
    }

    pub fn span_to_positions(&self, span: Span) -> (TextPosition, TextPosition) {
        (
            self.offset_to_position(span.start),
            self.offset_to_position(span.end),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}
