use crate::error::Diagnostic;
use crate::span::{Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Integer,
    String,
    DocComment,
    KwUse,
    KwMod,
    KwEnum,
    KwOption,
    KwConstraint,
    KwRequire,
    KwWhen,
    KwMatch,
    KwCase,
    KwIf,
    KwAs,
    KwPatch,
    KwDefault,
    KwExport,
    KwInclude,
    KwIn,
    KwTrue,
    KwFalse,
    KwSelf,
    KwFn,
    KwLet,
    KwMut,
    KwPub,
    KwImpl,
    KwTrait,
    KwStruct,
    KwType,
    KwWhere,
    KwFor,
    KwWhile,
    KwLoop,
    KwReturn,
    KwBreak,
    KwContinue,
    KwSuper,
    KwCrate,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,
    Dot,
    DotDot,
    DotDotEq,
    ColonColon,
    Arrow,
    Pipe,
    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Bang,
    AndAnd,
    OrOr,
    Minus,
    Hash,
    Eof,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Self { kind, lexeme, span }
    }
}

#[derive(Debug, Default)]
pub struct Lexer {
    diagnostics: Vec<Diagnostic>,
}

impl Lexer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn tokenize(mut self, source: &str) -> (Vec<Token>, Vec<Diagnostic>) {
        let bytes = source.as_bytes();
        let mut cursor = 0usize;
        let mut tokens = Vec::new();

        while cursor < bytes.len() {
            let b = bytes[cursor];
            if is_whitespace(b) {
                cursor += 1;
                continue;
            }

            if b == b'/'
                && let Some((kind, next, text)) = self.lex_comment(source, cursor)
            {
                if kind == TokenKind::DocComment {
                    tokens.push(Token::new(kind, text, Span::new(cursor, next)));
                }
                cursor = next;
                continue;
            }

            if is_ident_start(b) {
                let start = cursor;
                cursor += 1;
                while cursor < bytes.len() && is_ident_continue(bytes[cursor]) {
                    cursor += 1;
                }
                let text = &source[start..cursor];
                let kind = keyword_token(text).unwrap_or(TokenKind::Ident);
                tokens.push(Token::new(kind, text.to_string(), Span::new(start, cursor)));
                continue;
            }

            if is_digit(b) {
                let start = cursor;
                cursor = lex_integer(source, cursor);
                tokens.push(Token::new(
                    TokenKind::Integer,
                    source[start..cursor].to_string(),
                    Span::new(start, cursor),
                ));
                continue;
            }

            if b == b'"' {
                let start = cursor;
                match self.lex_string(source, cursor) {
                    Ok((end, text)) => {
                        tokens.push(Token::new(TokenKind::String, text, Span::new(start, end)));
                        cursor = end;
                    }
                    Err((end, msg)) => {
                        self.diagnostics.push(Diagnostic::error(
                            "E_PARSE_INVALID_LITERAL",
                            msg,
                            Span::new(start, end),
                        ));
                        cursor = end;
                    }
                }
                continue;
            }

            let start = cursor;
            let (kind, len) = match b {
                b'{' => (TokenKind::LBrace, 1),
                b'}' => (TokenKind::RBrace, 1),
                b'(' => (TokenKind::LParen, 1),
                b')' => (TokenKind::RParen, 1),
                b'[' => (TokenKind::LBracket, 1),
                b']' => (TokenKind::RBracket, 1),
                b',' => (TokenKind::Comma, 1),
                b';' => (TokenKind::Semicolon, 1),
                b'#' => (TokenKind::Hash, 1),
                b'-' => {
                    if source[start..].starts_with("->") {
                        (TokenKind::Arrow, 2)
                    } else {
                        (TokenKind::Minus, 1)
                    }
                }
                b':' => {
                    if source[start..].starts_with("::") {
                        (TokenKind::ColonColon, 2)
                    } else {
                        (TokenKind::Colon, 1)
                    }
                }
                b'.' => {
                    if source[start..].starts_with("..=") {
                        (TokenKind::DotDotEq, 3)
                    } else if source[start..].starts_with("..") {
                        (TokenKind::DotDot, 2)
                    } else {
                        (TokenKind::Dot, 1)
                    }
                }
                b'=' => {
                    if source[start..].starts_with("==") {
                        (TokenKind::EqEq, 2)
                    } else if source[start..].starts_with("=>") {
                        (TokenKind::Arrow, 2)
                    } else {
                        (TokenKind::Eq, 1)
                    }
                }
                b'!' => {
                    if source[start..].starts_with("!=") {
                        (TokenKind::Ne, 2)
                    } else {
                        (TokenKind::Bang, 1)
                    }
                }
                b'<' => {
                    if source[start..].starts_with("<=") {
                        (TokenKind::Le, 2)
                    } else {
                        (TokenKind::Lt, 1)
                    }
                }
                b'>' => {
                    if source[start..].starts_with(">=") {
                        (TokenKind::Ge, 2)
                    } else {
                        (TokenKind::Gt, 1)
                    }
                }
                b'&' => {
                    if source[start..].starts_with("&&") {
                        (TokenKind::AndAnd, 2)
                    } else {
                        (TokenKind::Unknown, 1)
                    }
                }
                b'|' => {
                    if source[start..].starts_with("||") {
                        (TokenKind::OrOr, 2)
                    } else {
                        (TokenKind::Pipe, 1)
                    }
                }
                _ => (TokenKind::Unknown, unknown_token_len(source, start)),
            };

            cursor += len;
            let text = source.get(start..cursor).map_or_else(
                || String::from_utf8_lossy(&bytes[start..cursor]).into_owned(),
                ToString::to_string,
            );

            if kind == TokenKind::Unknown {
                self.diagnostics.push(Diagnostic::error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    format!("unexpected character `{}`", text),
                    Span::new(start, cursor),
                ));
            } else {
                tokens.push(Token::new(kind, text, Span::new(start, cursor)));
            }
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            String::new(),
            Span::new(source.len(), source.len()),
        ));

        (tokens, self.diagnostics)
    }

    fn lex_comment(&mut self, source: &str, start: usize) -> Option<(TokenKind, usize, String)> {
        if !source[start..].starts_with("//") {
            return None;
        }

        let is_doc = source[start..].starts_with("///") && !source[start..].starts_with("////");
        let mut end = start;
        while end < source.len() {
            let b = source.as_bytes()[end];
            end += 1;
            if b == b'\n' {
                break;
            }
        }

        if !is_doc {
            return Some((TokenKind::Unknown, end, String::new()));
        }

        let raw = &source[start..end];
        let content = raw
            .strip_prefix("///")
            .unwrap_or(raw)
            .trim_start()
            .trim_end_matches('\n')
            .to_string();
        Some((TokenKind::DocComment, end, content))
    }

    fn lex_string(
        &mut self,
        source: &str,
        start: usize,
    ) -> Result<(usize, String), (usize, String)> {
        let bytes = source.as_bytes();
        let mut i = start + 1;
        let mut output = String::new();
        while i < bytes.len() {
            let b = bytes[i];
            if b == b'"' {
                return Ok((i + 1, output));
            }
            if b == b'\\' {
                i += 1;
                if i >= bytes.len() {
                    return Err((i, "unterminated escape in string".to_string()));
                }
                let esc = bytes[i];
                let ch = match esc {
                    b'"' => '"',
                    b'\\' => '\\',
                    b'n' => '\n',
                    b'r' => '\r',
                    b't' => '\t',
                    _ => {
                        let esc_len = unknown_token_len(source, i);
                        let escaped = source
                            .get(i..i + esc_len)
                            .unwrap_or("\u{FFFD}")
                            .to_string();
                        return Err((i + esc_len, format!("unsupported escape `\\{escaped}`")));
                    }
                };
                output.push(ch);
                i += 1;
                continue;
            }
            let ch_len = unknown_token_len(source, i);
            output.push_str(source.get(i..i + ch_len).unwrap_or("\u{FFFD}"));
            i += ch_len;
        }

        Err((source.len(), "unterminated string literal".to_string()))
    }
}

fn keyword_token(text: &str) -> Option<TokenKind> {
    Some(match text {
        "use" => TokenKind::KwUse,
        "mod" => TokenKind::KwMod,
        "enum" => TokenKind::KwEnum,
        "option" => TokenKind::KwOption,
        "constraint" => TokenKind::KwConstraint,
        "require" => TokenKind::KwRequire,
        "when" => TokenKind::KwWhen,
        "match" => TokenKind::KwMatch,
        "case" => TokenKind::KwCase,
        "if" => TokenKind::KwIf,
        "as" => TokenKind::KwAs,
        "patch" => TokenKind::KwPatch,
        "default" => TokenKind::KwDefault,
        "export" => TokenKind::KwExport,
        "include" => TokenKind::KwInclude,
        "in" => TokenKind::KwIn,
        "true" => TokenKind::KwTrue,
        "false" => TokenKind::KwFalse,
        "self" => TokenKind::KwSelf,
        "fn" => TokenKind::KwFn,
        "let" => TokenKind::KwLet,
        "mut" => TokenKind::KwMut,
        "pub" => TokenKind::KwPub,
        "impl" => TokenKind::KwImpl,
        "trait" => TokenKind::KwTrait,
        "struct" => TokenKind::KwStruct,
        "type" => TokenKind::KwType,
        "where" => TokenKind::KwWhere,
        "for" => TokenKind::KwFor,
        "while" => TokenKind::KwWhile,
        "loop" => TokenKind::KwLoop,
        "return" => TokenKind::KwReturn,
        "break" => TokenKind::KwBreak,
        "continue" => TokenKind::KwContinue,
        "super" => TokenKind::KwSuper,
        "crate" => TokenKind::KwCrate,
        _ => return None,
    })
}

fn is_whitespace(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\r' | b'\n')
}

fn is_digit(b: u8) -> bool {
    b.is_ascii_digit()
}

fn is_ident_start(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_'
}

fn is_ident_continue(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn lex_integer(source: &str, mut cursor: usize) -> usize {
    let bytes = source.as_bytes();

    if source[cursor..].starts_with("0x") || source[cursor..].starts_with("0X") {
        cursor += 2;
        while cursor < bytes.len() {
            let b = bytes[cursor];
            if b.is_ascii_hexdigit() || b == b'_' {
                cursor += 1;
            } else {
                break;
            }
        }
        return cursor;
    }

    while cursor < bytes.len() {
        let b = bytes[cursor];
        if b.is_ascii_digit() || b == b'_' {
            cursor += 1;
        } else {
            break;
        }
    }
    cursor
}

fn unknown_token_len(source: &str, start: usize) -> usize {
    if start >= source.len() {
        return 1;
    }

    if source.is_char_boundary(start) {
        return source[start..]
            .chars()
            .next()
            .map_or(1, char::len_utf8);
    }

    (start + 1..=source.len())
        .find(|idx| source.is_char_boundary(*idx))
        .map_or(1, |next| next - start)
}

pub fn collect_doc_comments(tokens: &[Token], start: usize) -> (Vec<Spanned<String>>, usize) {
    let mut docs = Vec::new();
    let mut index = start;
    while let Some(token) = tokens.get(index) {
        if token.kind != TokenKind::DocComment {
            break;
        }
        docs.push(Spanned::new(token.lexeme.clone(), token.span));
        index += 1;
    }
    (docs, index)
}
