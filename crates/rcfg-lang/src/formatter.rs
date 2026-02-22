use crate::ast::{
    Attr, AttrArg, AttrKind, ConstValue, ConstraintItem, EnumDecl, EnumVariant, ExportBlock,
    ExportStmt, File, Item, ItemMeta, MatchCase, OptionAttachedConstraints, OptionDecl, PatchStmt,
    RequireStmt, Type, ValueExpr, ValuesFile, ValuesStmt,
};
use crate::span::{Span, Spanned};

const INDENT: &str = "    ";

pub fn format_schema(source: &str, file: &File) -> String {
    let mut formatter = Formatter::new(source);
    formatter.format_schema_file(file);
    formatter.finish()
}

pub fn format_values(source: &str, values: &ValuesFile) -> String {
    let mut formatter = Formatter::new(source);
    formatter.format_values_file(values);
    formatter.finish()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValuesStmtKind {
    Use,
    Include,
    Assign,
}

impl ValuesStmtKind {
    fn rank(self) -> u8 {
        match self {
            Self::Use => 0,
            Self::Include => 1,
            Self::Assign => 2,
        }
    }
}

struct Formatter<'a> {
    source: &'a str,
    output: String,
    indent: usize,
}

impl<'a> Formatter<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            output: String::new(),
            indent: 0,
        }
    }

    fn finish(mut self) -> String {
        while self.output.ends_with('\n') {
            self.output.pop();
        }
        self.output.push('\n');
        self.output
    }

    fn format_schema_file(&mut self, file: &File) {
        self.render_spanned_list(
            &file.items,
            0,
            self.source.len(),
            true,
            item_full_span,
            |fmt, item| fmt.render_schema_item(item),
        );
    }

    fn format_values_file(&mut self, values: &ValuesFile) {
        if values.stmts.is_empty() {
            self.emit_comments_between(0, self.source.len(), false);
            return;
        }

        let first_span = values_stmt_span(&values.stmts[0]);
        self.emit_comments_between(0, first_span.start, false);

        for (index, stmt) in values.stmts.iter().enumerate() {
            self.render_values_stmt(stmt);
            let current_span = values_stmt_span(stmt);
            self.append_trailing_comment(current_span.end);

            if let Some(next) = values.stmts.get(index + 1) {
                let next_span = values_stmt_span(next);
                let emitted = self.emit_comments_between(current_span.end, next_span.start, true);
                if emitted == 0
                    && needs_blank_line_between_values(
                        values_stmt_kind(stmt),
                        values_stmt_kind(next),
                    )
                {
                    self.blank_line();
                }
            } else {
                self.emit_comments_between(current_span.end, self.source.len(), true);
            }
        }
    }

    fn render_spanned_list<T, FSpan, FRender>(
        &mut self,
        items: &[T],
        region_start: usize,
        region_end: usize,
        blank_between: bool,
        mut span_of: FSpan,
        mut render: FRender,
    ) where
        FSpan: FnMut(&T) -> Span,
        FRender: FnMut(&mut Self, &T),
    {
        if items.is_empty() {
            self.emit_comments_between(region_start, region_end, false);
            return;
        }

        let first_span = span_of(&items[0]);
        self.emit_comments_between(region_start, first_span.start, false);

        for (index, item) in items.iter().enumerate() {
            render(self, item);
            let current_span = span_of(item);
            self.append_trailing_comment(current_span.end);

            if let Some(next) = items.get(index + 1) {
                let next_span = span_of(next);
                let emitted = self.emit_comments_between(current_span.end, next_span.start, true);
                if blank_between && emitted == 0 {
                    self.blank_line();
                }
            } else {
                self.emit_comments_between(current_span.end, region_end, true);
            }
        }
    }

    fn render_schema_item(&mut self, item: &Item) {
        match item {
            Item::Use(stmt) => {
                self.render_item_meta(&stmt.meta);
                let mut line = format!("use {}", stmt.path);
                if let Some(alias) = &stmt.alias {
                    line.push_str(" as ");
                    line.push_str(&alias.value);
                }
                line.push(';');
                self.write_line(&line);
            }
            Item::Mod(decl) => {
                self.render_item_meta(&decl.meta);
                if decl.items.is_empty() {
                    self.write_line(&format!("mod {} {{ }}", decl.name.value));
                    return;
                }

                self.write_line(&format!("mod {} {{", decl.name.value));
                let (body_start, body_end) = self.block_bounds(decl.span);
                self.indent += 1;
                self.render_spanned_list(
                    &decl.items,
                    body_start,
                    body_end,
                    true,
                    item_full_span,
                    |fmt, child| fmt.render_schema_item(child),
                );
                self.indent = self.indent.saturating_sub(1);
                self.write_line("}");
            }
            Item::Enum(decl) => self.render_enum_decl(decl),
            Item::Option(decl) => self.render_option_decl(decl),
            Item::Export(block) => self.render_export_block(block),
            Item::Patch(block) => {
                self.render_item_meta(&block.meta);
                if block.stmts.is_empty() {
                    self.write_line(&format!("patch {} {{ }}", block.target));
                    return;
                }

                self.write_line(&format!("patch {} {{", block.target));
                let (body_start, body_end) = self.block_bounds(block.span);
                self.indent += 1;
                self.render_spanned_list(
                    &block.stmts,
                    body_start,
                    body_end,
                    true,
                    patch_stmt_span,
                    |fmt, stmt| fmt.render_patch_stmt(stmt),
                );
                self.indent = self.indent.saturating_sub(1);
                self.write_line("}");
            }
            Item::Require(stmt) => self.render_require_stmt(stmt),
            Item::Constraint(block) => {
                self.render_item_meta(&block.meta);
                if block.items.is_empty() {
                    self.write_line("constraint { }");
                    return;
                }

                self.write_line("constraint {");
                let (body_start, body_end) = self.block_bounds(block.span);
                self.indent += 1;
                self.render_spanned_list(
                    &block.items,
                    body_start,
                    body_end,
                    true,
                    constraint_item_span,
                    |fmt, item| fmt.render_constraint_item(item),
                );
                self.indent = self.indent.saturating_sub(1);
                self.write_line("}");
            }
            Item::When(block) => {
                self.render_item_meta(&block.meta);
                if block.items.is_empty() {
                    self.write_line(&format!("when {} {{ }}", block.condition));
                    return;
                }

                self.write_line(&format!("when {} {{", block.condition));
                let (body_start, body_end) = self.block_bounds(block.span);
                self.indent += 1;
                self.render_spanned_list(
                    &block.items,
                    body_start,
                    body_end,
                    true,
                    item_full_span,
                    |fmt, item| fmt.render_schema_item(item),
                );
                self.indent = self.indent.saturating_sub(1);
                self.write_line("}");
            }
            Item::Match(block) => {
                self.render_item_meta(&block.meta);
                if block.cases.is_empty() {
                    self.write_line(&format!("match {} {{ }}", block.expr));
                    return;
                }

                self.write_line(&format!("match {} {{", block.expr));
                let (body_start, body_end) = self.block_bounds(block.span);
                self.indent += 1;
                self.render_spanned_list(
                    &block.cases,
                    body_start,
                    body_end,
                    false,
                    match_case_full_span,
                    |fmt, case| fmt.render_match_case(case),
                );
                self.indent = self.indent.saturating_sub(1);
                self.write_line("}");
            }
        }
    }

    fn render_enum_decl(&mut self, decl: &EnumDecl) {
        self.render_item_meta(&decl.meta);
        if decl.variants.is_empty() {
            self.write_line(&format!("enum {} {{ }}", decl.name.value));
            return;
        }

        if should_inline_enum(decl) {
            let variants = decl
                .variants
                .iter()
                .map(|item| item.name.value.as_str())
                .collect::<Vec<_>>()
                .join(", ");
            self.write_line(&format!("enum {} {{ {} }}", decl.name.value, variants));
            return;
        }

        self.write_line(&format!("enum {} {{", decl.name.value));
        let (body_start, body_end) = self.block_bounds(decl.span);
        self.indent += 1;
        self.render_spanned_list(
            &decl.variants,
            body_start,
            body_end,
            false,
            enum_variant_full_span,
            |fmt, variant| fmt.render_enum_variant(variant),
        );
        self.indent = self.indent.saturating_sub(1);
        self.write_line("}");
    }

    fn render_enum_variant(&mut self, variant: &EnumVariant) {
        self.render_item_meta(&variant.meta);
        self.write_line(&format!("{},", variant.name.value));
    }

    fn render_option_decl(&mut self, decl: &OptionDecl) {
        self.render_item_meta(&decl.meta);
        let mut line = format!("option {}: {}", decl.name.value, render_type_name(&decl.ty));
        if let Some(default) = &decl.default {
            line.push_str(" = ");
            line.push_str(&render_const_value(default));
        }

        let Some(attached) = &decl.attached_constraints else {
            line.push(';');
            self.write_line(&line);
            return;
        };

        if attached.attrs.is_empty() && attached.doc.is_empty() && attached.requires.is_empty() {
            self.write_line(&format!("{line} {{ }}"));
            return;
        }

        self.write_line(&format!("{line} {{"));
        let (body_start, body_end) = self.block_bounds(attached.span);
        self.indent += 1;
        self.render_attached_constraints(attached, body_start, body_end);
        self.indent = self.indent.saturating_sub(1);
        self.write_line("}");
    }

    fn render_attached_constraints(
        &mut self,
        attached: &OptionAttachedConstraints,
        body_start: usize,
        body_end: usize,
    ) {
        #[derive(Clone)]
        enum AttachedNode {
            Doc(Spanned<String>),
            Attr(Attr),
            Require(RequireStmt),
        }

        impl AttachedNode {
            fn span(&self) -> Span {
                match self {
                    Self::Doc(item) => item.span,
                    Self::Attr(item) => item.span,
                    Self::Require(item) => require_full_span(item),
                }
            }
        }

        let mut items = Vec::new();
        items.extend(attached.doc.iter().cloned().map(AttachedNode::Doc));
        items.extend(attached.attrs.iter().cloned().map(AttachedNode::Attr));
        items.extend(attached.requires.iter().cloned().map(AttachedNode::Require));
        items.sort_by_key(|item| item.span().start);

        self.render_spanned_list(
            &items,
            body_start,
            body_end,
            true,
            AttachedNode::span,
            |fmt, item| match item {
                AttachedNode::Doc(doc) => fmt.render_doc_comment(&doc.value),
                AttachedNode::Attr(attr) => fmt.write_line(&render_attr(attr)),
                AttachedNode::Require(req) => fmt.render_require_stmt(req),
            },
        );
    }

    fn render_export_block(&mut self, block: &ExportBlock) {
        self.render_item_meta(&block.meta);
        let header = self.render_export_header(block);

        if block.stmts.is_empty() {
            self.write_line(&format!("{header} {{ }}"));
            return;
        }

        self.write_line(&format!("{header} {{"));
        let (body_start, body_end) = self.block_bounds(block.span);
        self.indent += 1;
        self.render_spanned_list(
            &block.stmts,
            body_start,
            body_end,
            true,
            export_stmt_span,
            |fmt, stmt| fmt.render_export_stmt(stmt),
        );
        self.indent = self.indent.saturating_sub(1);
        self.write_line("}");
    }

    fn render_export_stmt(&mut self, stmt: &ExportStmt) {
        match stmt {
            ExportStmt::Set(item) => {
                self.write_line(&format!(
                    "{} = {};",
                    item.key.value,
                    render_const_value(&item.value)
                ));
            }
        }
    }

    fn render_patch_stmt(&mut self, stmt: &PatchStmt) {
        match stmt {
            PatchStmt::Default(item) => {
                self.write_line(&format!(
                    "default {} = {};",
                    item.path,
                    render_const_value(&item.value)
                ));
            }
        }
    }

    fn render_constraint_item(&mut self, item: &ConstraintItem) {
        match item {
            ConstraintItem::Require(stmt) => self.render_require_stmt(stmt),
            ConstraintItem::Attr(attr) => self.write_line(&render_attr(attr)),
            ConstraintItem::Doc(doc) => self.render_doc_comment(&doc.value),
        }
    }

    fn render_require_stmt(&mut self, stmt: &RequireStmt) {
        self.render_item_meta(&stmt.meta);
        self.write_line(&format!("require!({});", stmt.expr));
    }

    fn render_match_case(&mut self, case: &MatchCase) {
        self.render_item_meta(&case.meta);
        let guard = case
            .guard
            .as_ref()
            .map(|expr| format!(" if {expr}"))
            .unwrap_or_default();
        let head = format!("case {}{} =>", case.pattern, guard);

        if case.items.is_empty() {
            self.write_line(&format!("{head} {{ }}"));
            return;
        }

        self.write_line(&format!("{head} {{"));
        let (body_start, body_end) = self.block_bounds(case.span);
        self.indent += 1;
        self.render_spanned_list(
            &case.items,
            body_start,
            body_end,
            true,
            item_full_span,
            |fmt, item| fmt.render_schema_item(item),
        );
        self.indent = self.indent.saturating_sub(1);
        self.write_line("}");
    }

    fn render_values_stmt(&mut self, stmt: &ValuesStmt) {
        match stmt {
            ValuesStmt::Use(item) => {
                let mut line = format!("use {}", item.path);
                if let Some(alias) = &item.alias {
                    line.push_str(" as ");
                    line.push_str(&alias.value);
                }
                line.push(';');
                self.write_line(&line);
            }
            ValuesStmt::Include(item) => {
                self.write_line(&format!(
                    "include {};",
                    render_string_literal(&item.path.value)
                ));
            }
            ValuesStmt::Assign(item) => {
                self.write_line(&format!(
                    "{} = {};",
                    item.path,
                    render_value_expr(&item.value)
                ));
            }
        }
    }

    fn render_item_meta(&mut self, meta: &ItemMeta) {
        for doc in &meta.doc {
            self.render_doc_comment(&doc.value);
        }
        for attr in &meta.attrs {
            self.write_line(&render_attr(attr));
        }
    }

    fn render_doc_comment(&mut self, text: &str) {
        if text.is_empty() {
            self.write_line("///");
        } else {
            self.write_line(&format!("/// {text}"));
        }
    }

    fn render_export_header(&self, block: &ExportBlock) -> String {
        let start = block.span.start.min(self.source.len());
        let end = block.span.end.min(self.source.len());
        if start >= end {
            return "export".to_string();
        }

        let snippet = &self.source[start..end];
        let Some(open_brace) = snippet.find('{') else {
            return "export".to_string();
        };

        let head = &snippet[..open_brace];
        let mut normalized = String::new();
        for line in head.lines() {
            let raw = line.split("//").next().unwrap_or_default().trim();
            if raw.is_empty() {
                continue;
            }
            if !normalized.is_empty() {
                normalized.push(' ');
            }
            normalized.push_str(raw);
        }

        if normalized.is_empty() {
            "export".to_string()
        } else {
            normalized
        }
    }

    fn block_bounds(&self, span: Span) -> (usize, usize) {
        let open = self
            .first_char_in_span(span, '{')
            .map(|index| index + 1)
            .unwrap_or(span.start);
        let close = self.last_char_in_span(span, '}').unwrap_or(span.end);
        (open, close)
    }

    fn first_char_in_span(&self, span: Span, needle: char) -> Option<usize> {
        let start = span.start.min(self.source.len());
        let end = span.end.min(self.source.len());
        if start >= end {
            return None;
        }
        self.source[start..end]
            .find(needle)
            .map(|offset| start + offset)
    }

    fn last_char_in_span(&self, span: Span, needle: char) -> Option<usize> {
        let start = span.start.min(self.source.len());
        let end = span.end.min(self.source.len());
        if start >= end {
            return None;
        }
        self.source[start..end]
            .rfind(needle)
            .map(|offset| start + offset)
    }

    fn write_line(&mut self, text: &str) {
        self.output.push_str(&INDENT.repeat(self.indent));
        self.output.push_str(text);
        self.output.push('\n');
    }

    fn blank_line(&mut self) {
        if self.output.is_empty() {
            return;
        }
        if self.output.ends_with("\n\n") {
            return;
        }
        if !self.output.ends_with('\n') {
            self.output.push('\n');
        }
        self.output.push('\n');
    }

    fn append_to_last_line(&mut self, suffix: &str) {
        if self.output.ends_with('\n') {
            self.output.pop();
            self.output.push_str(suffix);
            self.output.push('\n');
        } else {
            self.output.push_str(suffix);
        }
    }

    fn append_trailing_comment(&mut self, span_end: usize) {
        if let Some(comment) = self.extract_trailing_comment(span_end) {
            self.append_to_last_line(&format!(" {comment}"));
        }
    }

    fn extract_trailing_comment(&self, span_end: usize) -> Option<String> {
        let bytes = self.source.as_bytes();
        let mut index = span_end.min(bytes.len());

        while index < bytes.len() {
            match bytes[index] {
                b' ' | b'\t' => {
                    index += 1;
                }
                b'\n' | b'\r' => return None,
                b'/' if index + 1 < bytes.len() && bytes[index + 1] == b'/' => {
                    if index + 2 < bytes.len() && bytes[index + 2] == b'/' {
                        return None;
                    }
                    let line_end = self.source[index..]
                        .find('\n')
                        .map_or(self.source.len(), |offset| index + offset);
                    let comment = self.source[index..line_end]
                        .trim_end_matches('\r')
                        .to_string();
                    return if comment.is_empty() {
                        None
                    } else {
                        Some(comment)
                    };
                }
                _ => {
                    index += 1;
                }
            }
        }

        None
    }

    fn emit_comments_between(&mut self, start: usize, end: usize, skip_first_line: bool) -> usize {
        let start = start.min(self.source.len());
        let end = end.min(self.source.len());
        if start >= end {
            return 0;
        }

        let mut slice = &self.source[start..end];
        if skip_first_line {
            let Some(first_newline) = slice.find('\n') else {
                return 0;
            };
            slice = &slice[first_newline + 1..];
        }

        let mut emitted = 0usize;
        let mut pending_blank = false;

        for line in slice.lines() {
            let trimmed = line.trim_start();
            if trimmed.is_empty() {
                if emitted > 0 {
                    pending_blank = true;
                }
                continue;
            }

            if trimmed.starts_with("///") {
                continue;
            }

            if !trimmed.starts_with("//") {
                pending_blank = false;
                continue;
            }

            if pending_blank {
                self.blank_line();
                pending_blank = false;
            }
            self.write_line(trimmed.trim_end());
            emitted += 1;
        }

        emitted
    }
}

fn should_inline_enum(decl: &EnumDecl) -> bool {
    if decl.variants.is_empty() || decl.variants.len() > 4 {
        return false;
    }
    if decl.variants.iter().any(|variant| {
        !variant.meta.doc.is_empty()
            || !variant.meta.attrs.is_empty()
            || variant.name.value.is_empty()
    }) {
        return false;
    }

    let joined_len = decl
        .variants
        .iter()
        .map(|variant| variant.name.value.len())
        .sum::<usize>()
        + (decl.variants.len().saturating_sub(1) * 2);
    joined_len <= 40
}

fn render_attr(attr: &Attr) -> String {
    match &attr.kind {
        AttrKind::Range(range) => {
            if range.inclusive {
                format!("#[range({}..={})]", range.start, range.end)
            } else {
                format!("#[range({}..{})]", range.start, range.end)
            }
        }
        AttrKind::Unit(unit) => format!("#[unit({})]", render_string_literal(unit)),
        AttrKind::Msg(message) => format!("#[msg({})]", render_string_literal(message)),
        AttrKind::Secret => "#[secret]".to_string(),
        AttrKind::Cfg(expr) => format!("#[cfg({expr})]"),
        AttrKind::Other { name, args } => {
            if args.is_empty() {
                format!("#[{name}]")
            } else {
                let rendered = args
                    .iter()
                    .map(render_attr_arg)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("#[{name}({rendered})]")
            }
        }
    }
}

fn render_attr_arg(arg: &AttrArg) -> String {
    match arg {
        AttrArg::Expr(expr) => expr.to_string(),
        AttrArg::Str(text) => render_string_literal(text),
    }
}

fn render_const_value(value: &ConstValue) -> String {
    match value {
        ConstValue::Bool(raw, _) => raw.to_string(),
        ConstValue::Int(raw, _) => raw.to_string(),
        ConstValue::String(raw, _) => render_string_literal(raw),
        ConstValue::EnumPath(path) => path.to_string(),
    }
}

fn render_type_name(ty: &Type) -> String {
    match ty {
        Type::Bool(_) => "bool".to_string(),
        Type::U8(_) => "u8".to_string(),
        Type::U16(_) => "u16".to_string(),
        Type::U32(_) => "u32".to_string(),
        Type::I32(_) => "i32".to_string(),
        Type::String(_) => "string".to_string(),
        Type::Named(path) => path.to_string(),
    }
}

fn render_value_expr(expr: &ValueExpr) -> String {
    match expr {
        ValueExpr::Bool(raw, _) => raw.to_string(),
        ValueExpr::Int(raw, _) => raw.to_string(),
        ValueExpr::String(raw, _) => render_string_literal(raw),
        ValueExpr::Path(path) => path.to_string(),
        ValueExpr::Env { name, fallback, .. } => {
            let mut rendered = format!("env({}", render_string_literal(&name.value));
            if let Some(fallback) = fallback {
                rendered.push_str(", ");
                rendered.push_str(&render_string_literal(&fallback.value));
            }
            rendered.push(')');
            rendered
        }
    }
}

fn render_string_literal(raw: &str) -> String {
    format!("\"{}\"", raw.escape_default())
}

fn values_stmt_kind(stmt: &ValuesStmt) -> ValuesStmtKind {
    match stmt {
        ValuesStmt::Use(_) => ValuesStmtKind::Use,
        ValuesStmt::Include(_) => ValuesStmtKind::Include,
        ValuesStmt::Assign(_) => ValuesStmtKind::Assign,
    }
}

fn needs_blank_line_between_values(current: ValuesStmtKind, next: ValuesStmtKind) -> bool {
    current.rank() < next.rank()
}

fn values_stmt_span(stmt: &ValuesStmt) -> Span {
    match stmt {
        ValuesStmt::Use(item) => item.span,
        ValuesStmt::Include(item) => item.span,
        ValuesStmt::Assign(item) => item.span,
    }
}

fn item_full_span(item: &Item) -> Span {
    match item {
        Item::Use(item) => span_with_meta(item.span, &item.meta),
        Item::Mod(item) => span_with_meta(item.span, &item.meta),
        Item::Enum(item) => span_with_meta(item.span, &item.meta),
        Item::Option(item) => span_with_meta(item.span, &item.meta),
        Item::Export(item) => span_with_meta(item.span, &item.meta),
        Item::Patch(item) => span_with_meta(item.span, &item.meta),
        Item::Require(item) => require_full_span(item),
        Item::Constraint(item) => span_with_meta(item.span, &item.meta),
        Item::When(item) => span_with_meta(item.span, &item.meta),
        Item::Match(item) => span_with_meta(item.span, &item.meta),
    }
}

fn enum_variant_full_span(item: &EnumVariant) -> Span {
    span_with_meta(item.span, &item.meta)
}

fn match_case_full_span(item: &MatchCase) -> Span {
    span_with_meta(item.span, &item.meta)
}

fn require_full_span(item: &RequireStmt) -> Span {
    span_with_meta(item.span, &item.meta)
}

fn constraint_item_span(item: &ConstraintItem) -> Span {
    match item {
        ConstraintItem::Require(item) => require_full_span(item),
        ConstraintItem::Attr(item) => item.span,
        ConstraintItem::Doc(item) => item.span,
    }
}

fn patch_stmt_span(item: &PatchStmt) -> Span {
    match item {
        PatchStmt::Default(item) => item.span,
    }
}

fn export_stmt_span(item: &ExportStmt) -> Span {
    match item {
        ExportStmt::Set(item) => item.span,
    }
}

fn span_with_meta(span: Span, meta: &ItemMeta) -> Span {
    let mut start = span.start;
    for doc in &meta.doc {
        start = start.min(doc.span.start);
    }
    for attr in &meta.attrs {
        start = start.min(attr.span.start);
    }
    Span::new(start, span.end)
}
