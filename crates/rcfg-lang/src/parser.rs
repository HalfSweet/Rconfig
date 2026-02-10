use crate::ast::{
    AssignStmt, Attr, AttrArg, AttrKind, BinaryOp, ConstValue, ConstraintBlock, ConstraintItem,
    EnumDecl, EnumVariant, ExportBlock, ExportSetStmt, ExportStmt, Expr, File, InSetElem,
    IncludeStmt, IntRange, Item, ItemMeta, MatchBlock, MatchCase, MatchPat, ModDecl,
    OptionAttachedConstraints, OptionDecl, PatchBlock, PatchDefaultStmt, PatchStmt, Path,
    RequireStmt, Type, UnaryOp, UseStmt, ValueExpr, ValuesFile, ValuesStmt, WhenBlock,
};
use crate::error::{Diagnostic, Severity};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::span::{Span, Spanned};

pub fn parse_schema(source: &str) -> Result<File, Vec<Diagnostic>> {
    let (file, diagnostics) = parse_schema_with_diagnostics(source);
    if diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
    {
        Err(diagnostics)
    } else {
        Ok(file)
    }
}

pub fn parse_schema_with_diagnostics(source: &str) -> (File, Vec<Diagnostic>) {
    let (tokens, mut diagnostics) = Lexer::new().tokenize(source);
    let mut parser = Parser::new(tokens, source.len());
    let file = parser.parse_schema_file();
    diagnostics.extend(parser.diagnostics);
    (file, diagnostics)
}

pub fn parse_values(source: &str) -> Result<ValuesFile, Vec<Diagnostic>> {
    let (file, diagnostics) = parse_values_with_diagnostics(source);
    if diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
    {
        Err(diagnostics)
    } else {
        Ok(file)
    }
}

pub fn parse_values_with_diagnostics(source: &str) -> (ValuesFile, Vec<Diagnostic>) {
    let (tokens, mut diagnostics) = Lexer::new().tokenize(source);
    let mut parser = Parser::new(tokens, source.len());
    let file = parser.parse_values_file();
    diagnostics.extend(parser.diagnostics);
    (file, diagnostics)
}

struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    source_len: usize,
    diagnostics: Vec<Diagnostic>,
}

impl Parser {
    fn new(tokens: Vec<Token>, source_len: usize) -> Self {
        Self {
            tokens,
            cursor: 0,
            source_len,
            diagnostics: Vec::new(),
        }
    }

    fn parse_schema_file(&mut self) -> File {
        let mut items = Vec::new();
        while !self.at(TokenKind::Eof) {
            let start = self.cursor;
            if self.at(TokenKind::RBrace) {
                let token = self.bump();
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "unexpected `}` at file scope",
                    token.span,
                );
                continue;
            }

            if let Some(item) = self.parse_schema_item(false) {
                items.push(item);
            }

            if self.cursor == start {
                self.synchronize_item();
            }
        }
        File { items }
    }

    fn parse_values_file(&mut self) -> ValuesFile {
        let mut stmts = Vec::new();
        while !self.at(TokenKind::Eof) {
            let start = self.cursor;
            if self.at(TokenKind::RBrace) {
                let token = self.bump();
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "unexpected `}` in values file",
                    token.span,
                );
                continue;
            }

            let stmt = match self.peek().kind {
                TokenKind::KwInclude => self.parse_include_stmt().map(ValuesStmt::Include),
                TokenKind::KwUse => self.parse_use_stmt(ItemMeta::empty()).map(ValuesStmt::Use),
                _ => self.parse_assign_stmt().map(ValuesStmt::Assign),
            };

            if let Some(stmt) = stmt {
                stmts.push(stmt);
            }

            if self.cursor == start {
                self.synchronize_item();
            }
        }

        ValuesFile { stmts }
    }

    fn parse_schema_item(&mut self, in_conditional: bool) -> Option<Item> {
        let meta = self.parse_item_meta();
        let token = self.peek().clone();
        match token.kind {
            TokenKind::KwUse => self.parse_use_stmt(meta).map(Item::Use),
            TokenKind::KwMod => self.parse_mod_decl(meta, in_conditional).map(Item::Mod),
            TokenKind::KwEnum => {
                if in_conditional {
                    self.push_error(
                        "E_ILLEGAL_TYPE_DECL_IN_CONDITIONAL",
                        "enum declaration is not allowed in when/match/case blocks",
                        token.span,
                    );
                }
                self.parse_enum_decl(meta).map(Item::Enum)
            }
            TokenKind::KwOption => self.parse_option_decl(meta).map(Item::Option),
            TokenKind::KwRequire => self.parse_require_stmt(meta).map(Item::Require),
            TokenKind::KwConstraint => self.parse_constraint_block(meta).map(Item::Constraint),
            TokenKind::KwWhen => self.parse_when_block(meta).map(Item::When),
            TokenKind::KwMatch => self.parse_match_block(meta).map(Item::Match),
            TokenKind::KwPatch => self.parse_patch_block(meta).map(Item::Patch),
            TokenKind::KwExport => self.parse_export_block(meta).map(Item::Export),
            TokenKind::Eof | TokenKind::RBrace => None,
            _ => {
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    format!("unexpected token `{}`", token.lexeme),
                    token.span,
                );
                self.bump();
                None
            }
        }
    }

    fn parse_item_meta(&mut self) -> ItemMeta {
        let mut meta = ItemMeta::empty();

        loop {
            if self.at(TokenKind::DocComment) {
                let token = self.bump();
                meta.doc.push(Spanned::new(token.lexeme, token.span));
                continue;
            }

            if self.at(TokenKind::Hash) {
                if let Some(attr) = self.parse_attr() {
                    meta.attrs.push(attr);
                }
                continue;
            }

            break;
        }

        meta
    }

    fn parse_attr(&mut self) -> Option<Attr> {
        let hash = self.expect(TokenKind::Hash, "E_PARSE_EXPECTED_TOKEN", "expected `#`")?;
        self.expect(
            TokenKind::LBracket,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `[` after `#`",
        )?;

        let name = self.parse_ident("attribute name")?;
        let kind = match name.value.as_str() {
            "range" => {
                self.expect(
                    TokenKind::LParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `(` after `range`",
                )?;
                let range = self.parse_int_range();
                self.expect(
                    TokenKind::RParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `)` after range argument",
                )?;
                range.map(AttrKind::Range)
            }
            "unit" => {
                self.expect(
                    TokenKind::LParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `(` after `unit`",
                )?;
                let value = self.parse_string_literal("unit attribute value")?;
                self.expect(
                    TokenKind::RParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `)` after unit attribute",
                )?;
                Some(AttrKind::Unit(value.value))
            }
            "msg" => {
                self.expect(
                    TokenKind::LParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `(` after `msg`",
                )?;
                let value = self.parse_string_literal("msg attribute value")?;
                self.expect(
                    TokenKind::RParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `)` after msg attribute",
                )?;
                Some(AttrKind::Msg(value.value))
            }
            "secret" => Some(AttrKind::Secret),
            "cfg" => {
                self.expect(
                    TokenKind::LParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `(` after `cfg`",
                )?;
                let expr = self.parse_expr()?;
                self.expect(
                    TokenKind::RParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `)` after cfg expression",
                )?;
                Some(AttrKind::Cfg(expr))
            }
            _ => {
                let mut args = Vec::new();
                if self.eat(TokenKind::LParen).is_some() {
                    if !self.at(TokenKind::RParen) {
                        loop {
                            if self.at(TokenKind::String) {
                                if let Some(text) = self.parse_string_literal("attribute argument")
                                {
                                    args.push(AttrArg::Str(text.value));
                                }
                            } else if let Some(expr) = self.parse_expr() {
                                args.push(AttrArg::Expr(expr));
                            }

                            if self.eat(TokenKind::Comma).is_none() {
                                break;
                            }
                        }
                    }
                    self.expect(
                        TokenKind::RParen,
                        "E_PARSE_EXPECTED_TOKEN",
                        "expected `)` after attribute arguments",
                    )?;
                }
                Some(AttrKind::Other {
                    name: name.value,
                    args,
                })
            }
        }?;

        let close = self.expect(
            TokenKind::RBracket,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `]` to close attribute",
        )?;

        Some(Attr {
            kind,
            span: hash.span.join(close.span),
        })
    }

    fn parse_use_stmt(&mut self, meta: ItemMeta) -> Option<UseStmt> {
        let start = self.expect(TokenKind::KwUse, "E_PARSE_EXPECTED_TOKEN", "expected `use`")?;
        let path = self.parse_path()?;
        let alias = if self.eat(TokenKind::KwAs).is_some() {
            Some(self.parse_ident("alias name")?)
        } else {
            None
        };
        let semi = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after use statement",
        )?;

        Some(UseStmt {
            meta,
            path,
            alias,
            span: start.span.join(semi.span),
        })
    }

    fn parse_mod_decl(&mut self, meta: ItemMeta, in_conditional: bool) -> Option<ModDecl> {
        let start = self.expect(TokenKind::KwMod, "E_PARSE_EXPECTED_TOKEN", "expected `mod`")?;
        let name = self.parse_ident("module name")?;
        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` in module declaration",
        )?;

        let mut items = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let mark = self.cursor;
            if let Some(item) = self.parse_schema_item(in_conditional) {
                items.push(item);
            }
            if self.cursor == mark {
                self.synchronize_item();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close module",
        )?;

        Some(ModDecl {
            meta,
            name,
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_enum_decl(&mut self, meta: ItemMeta) -> Option<EnumDecl> {
        let start = self.expect(
            TokenKind::KwEnum,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `enum`",
        )?;
        let name = self.parse_ident("enum name")?;
        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` in enum declaration",
        )?;

        let mut variants = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let variant_meta = self.parse_item_meta();
            if self.at(TokenKind::RBrace) {
                break;
            }
            let variant_name = self.parse_ident("enum variant")?;
            variants.push(EnumVariant {
                meta: variant_meta,
                span: variant_name.span,
                name: variant_name,
            });

            if self.eat(TokenKind::Comma).is_none() {
                break;
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close enum",
        )?;

        Some(EnumDecl {
            meta,
            name,
            variants,
            span: start.span.join(end.span),
        })
    }

    fn parse_option_decl(&mut self, meta: ItemMeta) -> Option<OptionDecl> {
        let start = self.expect(
            TokenKind::KwOption,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `option`",
        )?;
        let name = self.parse_ident("option name")?;
        self.expect(
            TokenKind::Colon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `:` after option name",
        )?;
        let ty = self.parse_type()?;

        let default = if self.eat(TokenKind::Eq).is_some() {
            self.parse_const_value()
        } else {
            None
        };

        let attached_constraints = if self.at(TokenKind::LBrace) {
            self.parse_option_attached_constraints()
        } else {
            None
        };

        let end = if let Some(semi) = self.eat(TokenKind::Semicolon) {
            semi.span
        } else if let Some(attached) = &attached_constraints {
            attached.span
        } else {
            let span = self.peek().span;
            self.push_error(
                "E_PARSE_EXPECTED_TOKEN",
                "expected `;` or attached constraint block after option declaration",
                span,
            );
            span
        };

        Some(OptionDecl {
            meta,
            name,
            ty,
            default,
            attached_constraints,
            span: start.span.join(end),
        })
    }

    fn parse_option_attached_constraints(&mut self) -> Option<OptionAttachedConstraints> {
        let open = self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` for option attached constraints",
        )?;

        let mut attrs = Vec::new();
        let mut doc = Vec::new();
        let mut requires = Vec::new();

        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let meta = self.parse_item_meta();
            if self.at(TokenKind::KwRequire) {
                if let Some(req) = self.parse_require_stmt(meta) {
                    requires.push(req);
                }
                continue;
            }

            if !meta.attrs.is_empty() || !meta.doc.is_empty() {
                attrs.extend(meta.attrs);
                doc.extend(meta.doc);
                continue;
            }

            let token = self.peek().clone();
            self.push_error(
                "E_PARSE_UNEXPECTED_TOKEN",
                "only require!/attr/doc are allowed in option attached constraint block",
                token.span,
            );
            self.bump();
            self.synchronize_in_block();
        }

        let close = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close option attached constraints",
        )?;

        Some(OptionAttachedConstraints {
            attrs,
            doc,
            requires,
            span: open.span.join(close.span),
        })
    }

    fn parse_constraint_block(&mut self, meta: ItemMeta) -> Option<ConstraintBlock> {
        let start = self.expect(
            TokenKind::KwConstraint,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `constraint`",
        )?;
        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after `constraint`",
        )?;

        let mut items = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let leading = self.parse_item_meta();
            if self.at(TokenKind::KwRequire) {
                if let Some(req) = self.parse_require_stmt(leading) {
                    items.push(ConstraintItem::Require(req));
                }
                continue;
            }

            if !leading.doc.is_empty() {
                items.extend(leading.doc.into_iter().map(ConstraintItem::Doc));
            }
            if !leading.attrs.is_empty() {
                items.extend(leading.attrs.into_iter().map(ConstraintItem::Attr));
            }

            if !matches!(self.peek().kind, TokenKind::RBrace | TokenKind::Eof) {
                let token = self.bump();
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "only require!/attr/doc are allowed in constraint block",
                    token.span,
                );
                self.synchronize_in_block();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close constraint block",
        )?;

        Some(ConstraintBlock {
            meta,
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_require_stmt(&mut self, meta: ItemMeta) -> Option<RequireStmt> {
        let start = self.expect(
            TokenKind::KwRequire,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `require`",
        )?;
        self.expect(
            TokenKind::Bang,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `!` after `require`",
        )?;
        self.expect(
            TokenKind::LParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `(` after `require!`",
        )?;

        let expr = self.parse_expr()?;

        self.expect(
            TokenKind::RParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `)` after require expression",
        )?;

        let end = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after require statement",
        )?;

        Some(RequireStmt {
            meta,
            span: start.span.join(end.span),
            expr,
        })
    }

    fn parse_when_block(&mut self, meta: ItemMeta) -> Option<WhenBlock> {
        let start = self.expect(
            TokenKind::KwWhen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `when`",
        )?;
        let condition = self.parse_expr()?;

        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after when condition",
        )?;

        let mut items = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let mark = self.cursor;
            if let Some(item) = self.parse_schema_item(true) {
                items.push(item);
            }
            if self.cursor == mark {
                self.synchronize_in_block();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close when block",
        )?;

        Some(WhenBlock {
            meta,
            condition,
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_match_block(&mut self, meta: ItemMeta) -> Option<MatchBlock> {
        let start = self.expect(
            TokenKind::KwMatch,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `match`",
        )?;
        let expr = self.parse_expr()?;

        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after match expression",
        )?;

        let mut cases = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let case_meta = self.parse_item_meta();
            if self.at(TokenKind::RBrace) {
                break;
            }
            if !self.at(TokenKind::KwCase) {
                let token = self.bump();
                self.push_error(
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `case` in match block",
                    token.span,
                );
                self.synchronize_in_block();
                continue;
            }

            if let Some(case) = self.parse_match_case(case_meta) {
                cases.push(case);
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close match block",
        )?;

        Some(MatchBlock {
            meta,
            expr,
            cases,
            span: start.span.join(end.span),
        })
    }

    fn parse_match_case(&mut self, meta: ItemMeta) -> Option<MatchCase> {
        let start = self.expect(
            TokenKind::KwCase,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `case`",
        )?;
        let pattern = self.parse_match_pattern()?;
        let guard = if self.eat(TokenKind::KwIf).is_some() {
            self.parse_expr()
        } else {
            None
        };

        self.expect(
            TokenKind::Arrow,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `=>` after case pattern",
        )?;
        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after `=>`",
        )?;

        let mut items = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let mark = self.cursor;
            if let Some(item) = self.parse_schema_item(true) {
                items.push(item);
            }
            if self.cursor == mark {
                self.synchronize_in_block();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close case body",
        )?;

        Some(MatchCase {
            meta,
            pattern,
            guard,
            items,
            span: start.span.join(end.span),
        })
    }

    fn parse_match_pattern(&mut self) -> Option<MatchPat> {
        if self.peek().kind == TokenKind::Ident && self.peek().lexeme == "_" {
            let token = self.bump();
            return Some(MatchPat::Wildcard(token.span));
        }

        let mut paths = Vec::new();
        let first = self.parse_path()?;
        let mut span = first.span;
        paths.push(first);

        while self.eat(TokenKind::Pipe).is_some() {
            let path = self.parse_path()?;
            span = span.join(path.span);
            paths.push(path);
        }

        Some(MatchPat::Paths(paths, span))
    }

    fn parse_patch_block(&mut self, meta: ItemMeta) -> Option<PatchBlock> {
        let start = self.expect(
            TokenKind::KwPatch,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `patch`",
        )?;
        let target = self.parse_path()?;
        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after patch target",
        )?;

        let mut stmts = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let mark = self.cursor;

            let leading = self.parse_item_meta();
            if !leading.doc.is_empty() || !leading.attrs.is_empty() {
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "only default statements are allowed in patch block",
                    self.prev_span(),
                );
            }

            if self.at(TokenKind::KwDefault) {
                if let Some(stmt) = self.parse_patch_default_stmt() {
                    stmts.push(stmt);
                }
            } else {
                let token = self.bump();
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "only default statements are allowed in patch block",
                    token.span,
                );
            }

            if self.cursor == mark {
                self.synchronize_in_block();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close patch block",
        )?;

        Some(PatchBlock {
            meta,
            target,
            stmts,
            span: start.span.join(end.span),
        })
    }

    fn parse_patch_default_stmt(&mut self) -> Option<PatchStmt> {
        let start = self.expect(
            TokenKind::KwDefault,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `default` in patch block",
        )?;
        let path = self.parse_path()?;
        self.expect(
            TokenKind::Eq,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `=` in patch default statement",
        )?;
        let value = self.parse_const_value()?;
        let end = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after patch default statement",
        )?;

        Some(PatchStmt::Default(PatchDefaultStmt {
            path,
            value,
            span: start.span.join(end.span),
        }))
    }

    fn parse_export_block(&mut self, meta: ItemMeta) -> Option<ExportBlock> {
        let start = self.expect(
            TokenKind::KwExport,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `export`",
        )?;

        while !self.at(TokenKind::LBrace)
            && !self.at(TokenKind::Eof)
            && !self.at(TokenKind::Semicolon)
        {
            self.bump();
        }

        if self.at(TokenKind::Semicolon) {
            self.push_error(
                "E_PARSE_EXPECTED_TOKEN",
                "expected `{` after export declaration",
                self.peek().span,
            );
            self.bump();
            return None;
        }

        self.expect(
            TokenKind::LBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `{` after export declaration",
        )?;

        let mut stmts = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let mark = self.cursor;

            let leading = self.parse_item_meta();
            if !leading.doc.is_empty() || !leading.attrs.is_empty() {
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "only key/value statements are allowed in export block",
                    self.prev_span(),
                );
            }

            if let Some(stmt) = self.parse_export_set_stmt() {
                stmts.push(stmt);
            }

            if self.cursor == mark {
                self.synchronize_in_block();
            }
        }

        let end = self.expect(
            TokenKind::RBrace,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `}` to close export block",
        )?;

        Some(ExportBlock {
            meta,
            stmts,
            span: start.span.join(end.span),
        })
    }

    fn parse_export_set_stmt(&mut self) -> Option<ExportStmt> {
        let key = self.parse_ident("export key")?;
        self.expect(
            TokenKind::Eq,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `=` in export statement",
        )?;
        let value = self.parse_const_value()?;
        let end = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after export statement",
        )?;

        let span = key.span.join(end.span);

        Some(ExportStmt::Set(ExportSetStmt {
            key,
            value,
            span,
        }))
    }

    fn parse_type(&mut self) -> Option<Type> {
        let path = self.parse_path()?;
        if path.segments.len() == 1 {
            let name = path.segments[0].value.as_str();
            let span = path.span;
            return Some(match name {
                "bool" => Type::Bool(span),
                "u8" => Type::U8(span),
                "u16" => Type::U16(span),
                "u32" => Type::U32(span),
                "i32" => Type::I32(span),
                "string" => Type::String(span),
                _ => Type::Named(path),
            });
        }

        Some(Type::Named(path))
    }

    fn parse_const_value(&mut self) -> Option<ConstValue> {
        match self.peek().kind {
            TokenKind::KwTrue => {
                let token = self.bump();
                Some(ConstValue::Bool(true, token.span))
            }
            TokenKind::KwFalse => {
                let token = self.bump();
                Some(ConstValue::Bool(false, token.span))
            }
            TokenKind::String => {
                let token = self.bump();
                Some(ConstValue::String(token.lexeme, token.span))
            }
            TokenKind::Integer | TokenKind::Minus => self
                .parse_int_literal()
                .map(|(value, span)| ConstValue::Int(value, span)),
            TokenKind::Ident => self.parse_path().map(ConstValue::EnumPath),
            _ => {
                let token = self.peek().clone();
                self.push_error(
                    "E_DEFAULT_NOT_CONSTANT",
                    "default value must be a literal or enum variant path",
                    token.span,
                );
                None
            }
        }
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> Option<Expr> {
        let mut expr = self.parse_logic_and()?;
        while self.eat(TokenKind::OrOr).is_some() {
            let right = self.parse_logic_and()?;
            let span = expr.span().join(right.span());
            expr = Expr::Binary {
                op: BinaryOp::Or,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            };
        }
        Some(expr)
    }

    fn parse_logic_and(&mut self) -> Option<Expr> {
        let mut expr = self.parse_equality()?;
        while self.eat(TokenKind::AndAnd).is_some() {
            let right = self.parse_equality()?;
            let span = expr.span().join(right.span());
            expr = Expr::Binary {
                op: BinaryOp::And,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            };
        }
        Some(expr)
    }

    fn parse_equality(&mut self) -> Option<Expr> {
        let mut expr = self.parse_relational()?;
        loop {
            let op = if self.eat(TokenKind::EqEq).is_some() {
                Some(BinaryOp::Eq)
            } else if self.eat(TokenKind::Ne).is_some() {
                Some(BinaryOp::Ne)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_relational()?;
            let span = expr.span().join(right.span());
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            };
        }
        Some(expr)
    }

    fn parse_relational(&mut self) -> Option<Expr> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = if self.eat(TokenKind::Lt).is_some() {
                Some(BinaryOp::Lt)
            } else if self.eat(TokenKind::Le).is_some() {
                Some(BinaryOp::Le)
            } else if self.eat(TokenKind::Gt).is_some() {
                Some(BinaryOp::Gt)
            } else if self.eat(TokenKind::Ge).is_some() {
                Some(BinaryOp::Ge)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_unary()?;
            let span = expr.span().join(right.span());
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
                span,
            };
        }
        Some(expr)
    }

    fn parse_unary(&mut self) -> Option<Expr> {
        if self.eat(TokenKind::Bang).is_some() {
            let expr = self.parse_unary()?;
            let span = self.prev_span().join(expr.span());
            return Some(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
                span,
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Option<Expr> {
        let expr = self.parse_primary()?;
        if self.eat(TokenKind::KwIn).is_some() {
            if self.eat(TokenKind::LBrace).is_some() {
                let mut elems = Vec::new();
                if !self.at(TokenKind::RBrace) {
                    loop {
                        if self.at(TokenKind::Integer) || self.at(TokenKind::Minus) {
                            if let Some((value, span)) = self.parse_int_literal() {
                                elems.push(InSetElem::Int(value, span));
                            }
                        } else if let Some(path) = self.parse_path() {
                            elems.push(InSetElem::Path(path));
                        }

                        if self.eat(TokenKind::Comma).is_none() {
                            break;
                        }
                    }
                }

                let end = self.expect(
                    TokenKind::RBrace,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `}` after set literal",
                )?;
                let span = expr.span().join(end.span);
                return Some(Expr::InSet {
                    expr: Box::new(expr),
                    elems,
                    span,
                });
            }

            let start = self.parse_int_literal()?;
            let (inclusive, op_span) = if let Some(tok) = self.eat(TokenKind::DotDotEq) {
                (true, tok.span)
            } else if let Some(tok) = self.eat(TokenKind::DotDot) {
                (false, tok.span)
            } else {
                let span = self.peek().span;
                self.push_error(
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `..` or `..=` after start of range",
                    span,
                );
                return None;
            };

            let end = self.parse_int_literal()?;
            let range = IntRange {
                start: start.0,
                end: end.0,
                inclusive,
                span: start.1.join(op_span).join(end.1),
            };
            let span = expr.span().join(range.span);
            return Some(Expr::InRange {
                expr: Box::new(expr),
                range,
                span,
            });
        }

        Some(expr)
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::KwTrue => {
                self.bump();
                Some(Expr::Bool(true, token.span))
            }
            TokenKind::KwFalse => {
                self.bump();
                Some(Expr::Bool(false, token.span))
            }
            TokenKind::String => {
                self.bump();
                Some(Expr::String(token.lexeme, token.span))
            }
            TokenKind::Integer | TokenKind::Minus => {
                let (value, span) = self.parse_int_literal()?;
                Some(Expr::Int(value, span))
            }
            TokenKind::KwSelf => {
                self.bump();
                Some(Expr::SelfValue(token.span))
            }
            TokenKind::Ident => {
                if self.peek_n(1).map(|token| token.kind) == Some(TokenKind::LParen) {
                    self.parse_call_expr()
                } else {
                    self.parse_path().map(Expr::Path)
                }
            }
            TokenKind::LParen => {
                let open = self.bump();
                let expr = self.parse_expr()?;
                let close = self.expect(
                    TokenKind::RParen,
                    "E_PARSE_EXPECTED_TOKEN",
                    "expected `)` after expression",
                )?;
                Some(Expr::Group {
                    expr: Box::new(expr),
                    span: open.span.join(close.span),
                })
            }
            _ => {
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "expected expression",
                    token.span,
                );
                None
            }
        }
    }

    fn parse_call_expr(&mut self) -> Option<Expr> {
        let name = self.parse_ident("function name")?;
        self.expect(
            TokenKind::LParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `(` after function name",
        )?;
        let mut args = Vec::new();
        if !self.at(TokenKind::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if self.eat(TokenKind::Comma).is_none() {
                    break;
                }
            }
        }
        let end = self.expect(
            TokenKind::RParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `)` after function arguments",
        )?;

        Some(Expr::Call {
            name: name.clone(),
            args,
            span: name.span.join(end.span),
        })
    }

    fn parse_int_range(&mut self) -> Option<IntRange> {
        let start = self.parse_int_literal()?;
        let (inclusive, op_span) = if let Some(token) = self.eat(TokenKind::DotDotEq) {
            (true, token.span)
        } else if let Some(token) = self.eat(TokenKind::DotDot) {
            (false, token.span)
        } else {
            let span = self.peek().span;
            self.push_error(
                "E_PARSE_EXPECTED_TOKEN",
                "expected `..` or `..=` in range",
                span,
            );
            return None;
        };
        let end = self.parse_int_literal()?;
        Some(IntRange {
            start: start.0,
            end: end.0,
            inclusive,
            span: start.1.join(op_span).join(end.1),
        })
    }

    fn parse_path(&mut self) -> Option<Path> {
        let first = self.parse_ident("path segment")?;
        let mut segments = vec![first.clone()];
        let mut span = first.span;
        while self.eat(TokenKind::ColonColon).is_some() {
            let segment = self.parse_ident("path segment")?;
            span = span.join(segment.span);
            segments.push(segment);
        }
        Some(Path { segments, span })
    }

    fn parse_ident(&mut self, context: &str) -> Option<Spanned<String>> {
        let token = self.peek().clone();
        if token.kind == TokenKind::Ident {
            self.bump();
            return Some(Spanned::new(token.lexeme, token.span));
        }

        if is_keyword_like(token.kind) {
            self.push_error(
                "E_PARSE_RESERVED_KEYWORD_AS_IDENT",
                format!("keyword `{}` cannot be used as {}", token.lexeme, context),
                token.span,
            );
            self.bump();
            return None;
        }

        self.push_error(
            "E_PARSE_EXPECTED_TOKEN",
            format!("expected {}", context),
            token.span,
        );
        None
    }

    fn parse_string_literal(&mut self, context: &str) -> Option<Spanned<String>> {
        let token = self.peek().clone();
        if token.kind == TokenKind::String {
            self.bump();
            return Some(Spanned::new(token.lexeme, token.span));
        }

        self.push_error(
            "E_PARSE_EXPECTED_TOKEN",
            format!("expected string literal for {}", context),
            token.span,
        );
        None
    }

    fn parse_int_literal(&mut self) -> Option<(i128, Span)> {
        let mut sign = 1i128;
        let mut span = self.peek().span;
        if let Some(minus) = self.eat(TokenKind::Minus) {
            sign = -1;
            span = minus.span;
        }

        let token = self.peek().clone();
        if token.kind != TokenKind::Integer {
            self.push_error(
                "E_PARSE_INVALID_LITERAL",
                "expected integer literal",
                token.span,
            );
            return None;
        }

        self.bump();
        span = span.join(token.span);

        let parsed = parse_integer_text(&token.lexeme);
        let Ok(value) = parsed else {
            self.push_error(
                "E_INT_LITERAL_OUT_OF_RANGE",
                format!("integer literal `{}` is out of range", token.lexeme),
                token.span,
            );
            return None;
        };

        Some((value * sign, span))
    }

    fn parse_include_stmt(&mut self) -> Option<IncludeStmt> {
        let start = self.expect(
            TokenKind::KwInclude,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `include`",
        )?;
        let path = self.parse_string_literal("include path")?;
        let semi = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after include statement",
        )?;

        Some(IncludeStmt {
            path,
            span: start.span.join(semi.span),
        })
    }

    fn parse_assign_stmt(&mut self) -> Option<AssignStmt> {
        let path = self.parse_path()?;
        self.expect(
            TokenKind::Eq,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `=` in assignment",
        )?;
        let value = self.parse_value_expr()?;
        let semi = self.expect(
            TokenKind::Semicolon,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `;` after assignment",
        )?;
        Some(AssignStmt {
            span: path.span.join(semi.span),
            path,
            value,
        })
    }

    fn parse_value_expr(&mut self) -> Option<ValueExpr> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::KwTrue => {
                self.bump();
                Some(ValueExpr::Bool(true, token.span))
            }
            TokenKind::KwFalse => {
                self.bump();
                Some(ValueExpr::Bool(false, token.span))
            }
            TokenKind::String => {
                self.bump();
                Some(ValueExpr::String(token.lexeme, token.span))
            }
            TokenKind::Integer | TokenKind::Minus => {
                let (value, span) = self.parse_int_literal()?;
                Some(ValueExpr::Int(value, span))
            }
            TokenKind::Ident if token.lexeme == "env" => self.parse_env_call(),
            TokenKind::Ident => self.parse_path().map(ValueExpr::Path),
            _ => {
                self.push_error(
                    "E_PARSE_UNEXPECTED_TOKEN",
                    "expected value expression",
                    token.span,
                );
                None
            }
        }
    }

    fn parse_env_call(&mut self) -> Option<ValueExpr> {
        let name = self.parse_ident("env")?;
        self.expect(
            TokenKind::LParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `(` after env",
        )?;
        let key = self.parse_string_literal("env variable name")?;
        let fallback = if self.eat(TokenKind::Comma).is_some() {
            Some(self.parse_string_literal("env fallback")?)
        } else {
            None
        };
        let end = self.expect(
            TokenKind::RParen,
            "E_PARSE_EXPECTED_TOKEN",
            "expected `)` after env arguments",
        )?;
        Some(ValueExpr::Env {
            name: key,
            fallback,
            span: name.span.join(end.span),
        })
    }

    fn skip_reserved_item_body(&mut self) {
        if self.eat(TokenKind::LBrace).is_some() {
            let mut depth = 1usize;
            while depth > 0 && !self.at(TokenKind::Eof) {
                let token = self.bump();
                match token.kind {
                    TokenKind::LBrace => depth += 1,
                    TokenKind::RBrace => depth -= 1,
                    _ => {}
                }
            }
            return;
        }

        while !self.at(TokenKind::Semicolon)
            && !self.at(TokenKind::Eof)
            && !self.at(TokenKind::RBrace)
        {
            self.bump();
        }
        self.eat(TokenKind::Semicolon);
    }

    fn synchronize_item(&mut self) {
        while !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Semicolon) {
                self.bump();
                return;
            }
            if self.at(TokenKind::RBrace) {
                return;
            }
            self.bump();
        }
    }

    fn synchronize_in_block(&mut self) {
        while !self.at(TokenKind::Eof) {
            if self.at(TokenKind::Semicolon) {
                self.bump();
                return;
            }
            if self.at(TokenKind::RBrace) {
                return;
            }
            self.bump();
        }
    }

    fn expect(&mut self, kind: TokenKind, code: &'static str, message: &str) -> Option<Token> {
        if self.at(kind) {
            return Some(self.bump());
        }
        let span = self.peek().span;
        self.push_error(code, message, span);
        None
    }

    fn eat(&mut self, kind: TokenKind) -> Option<Token> {
        if self.at(kind) {
            Some(self.bump())
        } else {
            None
        }
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.cursor)
            .unwrap_or_else(|| self.tokens.last().expect("parser must hold EOF token"))
    }

    fn peek_n(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + offset)
    }

    fn bump(&mut self) -> Token {
        let token = self
            .tokens
            .get(self.cursor)
            .cloned()
            .unwrap_or_else(|| Token {
                kind: TokenKind::Eof,
                lexeme: String::new(),
                span: Span::new(self.source_len, self.source_len),
            });
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
        }
        token
    }

    fn prev_span(&self) -> Span {
        if self.cursor == 0 {
            Span::new(0, 0)
        } else {
            self.tokens[self.cursor.saturating_sub(1)].span
        }
    }

    fn push_error(&mut self, code: &'static str, message: impl Into<String>, span: Span) {
        self.diagnostics
            .push(Diagnostic::error(code, message, span));
    }
}

fn is_keyword_like(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::KwUse
            | TokenKind::KwMod
            | TokenKind::KwEnum
            | TokenKind::KwOption
            | TokenKind::KwConstraint
            | TokenKind::KwRequire
            | TokenKind::KwWhen
            | TokenKind::KwMatch
            | TokenKind::KwCase
            | TokenKind::KwIf
            | TokenKind::KwAs
            | TokenKind::KwPatch
            | TokenKind::KwDefault
            | TokenKind::KwExport
            | TokenKind::KwInclude
            | TokenKind::KwIn
            | TokenKind::KwTrue
            | TokenKind::KwFalse
            | TokenKind::KwSelf
            | TokenKind::KwFn
            | TokenKind::KwLet
            | TokenKind::KwMut
            | TokenKind::KwPub
            | TokenKind::KwImpl
            | TokenKind::KwTrait
            | TokenKind::KwStruct
            | TokenKind::KwType
            | TokenKind::KwWhere
            | TokenKind::KwFor
            | TokenKind::KwWhile
            | TokenKind::KwLoop
            | TokenKind::KwReturn
            | TokenKind::KwBreak
            | TokenKind::KwContinue
            | TokenKind::KwSuper
            | TokenKind::KwCrate
    )
}

fn parse_integer_text(text: &str) -> Result<i128, ()> {
    let cleaned = text.replace('_', "");
    if let Some(hex) = cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        i128::from_str_radix(hex, 16).map_err(|_| ())
    } else {
        cleaned.parse::<i128>().map_err(|_| ())
    }
}
