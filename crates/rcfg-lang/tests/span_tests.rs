use rcfg_lang::{LineCol, SourceIndex, Span, TextPosition};

#[test]
fn converts_offset_to_line_col_and_back() {
    let source = "alpha\nbeta\n";
    let index = SourceIndex::new(source);

    assert_eq!(index.line_count(), 3);
    assert_eq!(index.offset_to_line_col(0), LineCol::new(1, 1));
    assert_eq!(index.offset_to_line_col(6), LineCol::new(2, 1));
    assert_eq!(index.offset_to_line_col(source.len()), LineCol::new(3, 1));

    assert_eq!(index.line_col_to_offset(LineCol::new(1, 1)), Some(0));
    assert_eq!(index.line_col_to_offset(LineCol::new(2, 3)), Some(8));
    assert_eq!(index.line_col_to_offset(LineCol::new(3, 1)), Some(source.len()));
    assert_eq!(index.line_col_to_offset(LineCol::new(4, 1)), None);
}

#[test]
fn converts_offset_to_zero_based_position_and_back() {
    let source = "ab\ncd";
    let index = SourceIndex::new(source);

    assert_eq!(index.offset_to_position(0), TextPosition::new(0, 0));
    assert_eq!(index.offset_to_position(3), TextPosition::new(1, 0));
    assert_eq!(index.position_to_offset(TextPosition::new(1, 2)), Some(5));
    assert_eq!(index.position_to_offset(TextPosition::new(2, 0)), None);
}

#[test]
fn converts_span_to_positions() {
    let source = "one\ntwo\nthree";
    let index = SourceIndex::new(source);

    let span = Span::new(4, 7);
    let (start, end) = index.span_to_line_cols(span);
    assert_eq!(start, LineCol::new(2, 1));
    assert_eq!(end, LineCol::new(2, 4));

    let (start_zero, end_zero) = index.span_to_positions(span);
    assert_eq!(start_zero, TextPosition::new(1, 0));
    assert_eq!(end_zero, TextPosition::new(1, 3));
}
