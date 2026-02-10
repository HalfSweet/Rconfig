use rcfg_lang::ast::ValuesStmt;
use rcfg_lang::{
    Item, SymbolKind, SymbolOccurrenceRole, parse_schema_with_diagnostics,
    parse_values_with_diagnostics,
};
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, CompletionResponse, Position, Url};

use crate::document::{DocumentKind, ProjectSnapshot};
use crate::position::lsp_position_to_offset;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionMode {
    Schema,
    Use,
    ValuesAssign,
}

#[derive(Debug, Clone)]
struct CompletionContext {
    mode: CompletionMode,
    scope: Vec<String>,
    raw_prefix: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Candidate {
    full_path: String,
    kind: CompletionItemKind,
}

pub fn provide(
    project: &ProjectSnapshot,
    _uri: &Url,
    position: Position,
    source_text: &str,
    kind: DocumentKind,
) -> Option<CompletionResponse> {
    let analysis = project.analysis.as_ref()?;
    let offset = lsp_position_to_offset(source_text, position);

    let context = match kind {
        DocumentKind::Schema => detect_schema_context(source_text, offset),
        DocumentKind::Values => detect_values_context(source_text, offset),
        DocumentKind::Unknown => None,
    }?;

    let candidates = match context.mode {
        CompletionMode::ValuesAssign => option_candidates(analysis),
        CompletionMode::Schema | CompletionMode::Use => schema_candidates(analysis),
    };

    let mut items = candidates
        .into_iter()
        .filter(|candidate| {
            is_candidate_visible(&candidate.full_path, &context.scope, &context.raw_prefix)
        })
        .map(|candidate| {
            let label = candidate
                .full_path
                .rsplit("::")
                .next()
                .unwrap_or(candidate.full_path.as_str())
                .to_string();

            CompletionItem {
                label,
                kind: Some(candidate.kind),
                detail: Some(candidate.full_path.clone()),
                insert_text: Some(insert_text_for_candidate(
                    &candidate.full_path,
                    &context.scope,
                    &context.raw_prefix,
                )),
                ..CompletionItem::default()
            }
        })
        .collect::<Vec<_>>();

    items.sort_by(|left, right| {
        left.label
            .cmp(&right.label)
            .then(left.detail.cmp(&right.detail))
            .then(left.insert_text.cmp(&right.insert_text))
    });
    items.dedup_by(|left, right| left.detail == right.detail && left.kind == right.kind);

    Some(CompletionResponse::Array(items))
}

fn schema_candidates(analysis: &crate::document::ProjectAnalysis) -> Vec<Candidate> {
    let mut candidates = Vec::new();

    for (path, info) in analysis.symbols.iter() {
        let kind = match info.kind {
            SymbolKind::Mod => CompletionItemKind::MODULE,
            SymbolKind::Enum => CompletionItemKind::ENUM,
            SymbolKind::Option => CompletionItemKind::FIELD,
        };

        push_candidate(&mut candidates, path, kind);
    }

    for occurrence in analysis.position_index.occurrences() {
        if occurrence.role != SymbolOccurrenceRole::Definition {
            continue;
        }
        if analysis
            .symbols
            .enum_owner_of_variant(&occurrence.path)
            .is_none()
        {
            continue;
        }

        push_candidate(
            &mut candidates,
            &occurrence.path,
            CompletionItemKind::ENUM_MEMBER,
        );
    }

    candidates
}

fn option_candidates(analysis: &crate::document::ProjectAnalysis) -> Vec<Candidate> {
    let mut candidates = Vec::new();

    for (path, info) in analysis.symbols.iter() {
        if matches!(info.kind, SymbolKind::Option) {
            push_candidate(&mut candidates, path, CompletionItemKind::FIELD);
        }
    }

    candidates
}

fn push_candidate(candidates: &mut Vec<Candidate>, full_path: &str, kind: CompletionItemKind) {
    if candidates
        .iter()
        .any(|existing| existing.full_path == full_path && existing.kind == kind)
    {
        return;
    }

    candidates.push(Candidate {
        full_path: full_path.to_string(),
        kind,
    });
}

fn detect_schema_context(text: &str, offset: usize) -> Option<CompletionContext> {
    let raw_prefix = completion_prefix(text, offset);
    let (file, _) = parse_schema_with_diagnostics(text);
    let mut scope = Vec::new();
    find_scope_for_offset(&file.items, offset, &mut Vec::new(), &mut scope);

    if is_offset_in_use_path(&file.items, offset) {
        return Some(CompletionContext {
            mode: CompletionMode::Use,
            scope,
            raw_prefix,
        });
    }

    if looks_like_use_context(text, offset) {
        return Some(CompletionContext {
            mode: CompletionMode::Use,
            scope,
            raw_prefix,
        });
    }

    if raw_prefix.is_empty() && !looks_like_symbol_context(text, offset) {
        return None;
    }

    Some(CompletionContext {
        mode: CompletionMode::Schema,
        scope,
        raw_prefix,
    })
}

fn detect_values_context(text: &str, offset: usize) -> Option<CompletionContext> {
    let raw_prefix = completion_prefix(text, offset);
    let (values, _) = parse_values_with_diagnostics(text);

    if is_offset_in_values_assign_lhs(&values.stmts, offset) {
        return Some(CompletionContext {
            mode: CompletionMode::ValuesAssign,
            scope: Vec::new(),
            raw_prefix,
        });
    }

    if looks_like_values_assign_lhs(text, offset) {
        return Some(CompletionContext {
            mode: CompletionMode::ValuesAssign,
            scope: Vec::new(),
            raw_prefix,
        });
    }

    None
}

fn find_scope_for_offset(
    items: &[Item],
    offset: usize,
    stack: &mut Vec<String>,
    best: &mut Vec<String>,
) {
    for item in items {
        match item {
            Item::Mod(mod_decl) => {
                if !span_contains(mod_decl.span, offset) {
                    continue;
                }

                stack.push(mod_decl.name.value.clone());
                if stack.len() >= best.len() {
                    *best = stack.clone();
                }
                find_scope_for_offset(&mod_decl.items, offset, stack, best);
                let _ = stack.pop();
            }
            Item::When(when_block) => {
                if span_contains(when_block.span, offset) {
                    find_scope_for_offset(&when_block.items, offset, stack, best);
                }
            }
            Item::Match(match_block) => {
                if !span_contains(match_block.span, offset) {
                    continue;
                }
                for case in &match_block.cases {
                    if span_contains(case.span, offset) {
                        find_scope_for_offset(&case.items, offset, stack, best);
                    }
                }
            }
            _ => {}
        }
    }
}

fn is_offset_in_use_path(items: &[Item], offset: usize) -> bool {
    for item in items {
        match item {
            Item::Use(use_stmt) => {
                if span_contains_with_cursor_tail(use_stmt.path.span, offset) {
                    return true;
                }
            }
            Item::Mod(mod_decl) => {
                if span_contains(mod_decl.span, offset)
                    && is_offset_in_use_path(&mod_decl.items, offset)
                {
                    return true;
                }
            }
            Item::When(when_block) => {
                if span_contains(when_block.span, offset)
                    && is_offset_in_use_path(&when_block.items, offset)
                {
                    return true;
                }
            }
            Item::Match(match_block) => {
                if !span_contains(match_block.span, offset) {
                    continue;
                }
                for case in &match_block.cases {
                    if span_contains(case.span, offset)
                        && is_offset_in_use_path(&case.items, offset)
                    {
                        return true;
                    }
                }
            }
            _ => {}
        }
    }

    false
}

fn is_offset_in_values_assign_lhs(stmts: &[ValuesStmt], offset: usize) -> bool {
    for stmt in stmts {
        if let ValuesStmt::Assign(assign) = stmt
            && span_contains_with_cursor_tail(assign.path.span, offset)
        {
            return true;
        }
    }

    false
}

fn is_candidate_visible(full_path: &str, scope: &[String], raw_prefix: &str) -> bool {
    let possible_prefixes = build_candidate_paths(scope, raw_prefix);
    possible_prefixes
        .iter()
        .filter(|prefix| !prefix.is_empty() || raw_prefix.is_empty())
        .any(|prefix| full_path.starts_with(prefix))
}

fn insert_text_for_candidate(full_path: &str, scope: &[String], raw_prefix: &str) -> String {
    let mut matched_prefixes = build_candidate_paths(scope, raw_prefix)
        .into_iter()
        .filter(|prefix| full_path.starts_with(prefix))
        .collect::<Vec<_>>();

    matched_prefixes.sort_by_key(|prefix| prefix.len());
    matched_prefixes.reverse();

    if let Some(prefix) = matched_prefixes.first()
        && let Some(remaining) = full_path.strip_prefix(prefix)
        && !remaining.is_empty()
    {
        return remaining.to_string();
    }

    let typed_tail = raw_prefix.rsplit("::").next().unwrap_or(raw_prefix);
    let candidate_tail = full_path.rsplit("::").next().unwrap_or(full_path);
    if let Some(stripped) = candidate_tail.strip_prefix(typed_tail) {
        return stripped.to_string();
    }

    if let Some(remaining) = full_path.strip_prefix(raw_prefix)
        && !remaining.is_empty()
    {
        return remaining.to_string();
    }

    full_path.to_string()
}

fn build_candidate_paths(scope: &[String], raw_path: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    for level in (0..=scope.len()).rev() {
        let prefix = &scope[..level];
        if prefix.is_empty() {
            candidates.push(raw_path.to_string());
        } else {
            candidates.push(format!("{}::{raw_path}", prefix.join("::")));
        }
    }
    candidates
}

fn completion_prefix(text: &str, offset: usize) -> String {
    let clamped = offset.min(text.len());
    let mut start = clamped;
    while start > 0 {
        let prev = text.as_bytes()[start - 1];
        if prev.is_ascii_alphanumeric() || prev == b'_' || prev == b':' {
            start -= 1;
        } else {
            break;
        }
    }

    text[start..clamped].to_string()
}

fn span_contains(span: rcfg_lang::Span, offset: usize) -> bool {
    if span.start == span.end {
        offset == span.start
    } else {
        offset >= span.start && offset < span.end
    }
}

fn span_contains_with_cursor_tail(span: rcfg_lang::Span, offset: usize) -> bool {
    span_contains(span, offset) || offset == span.end
}

fn looks_like_use_context(text: &str, offset: usize) -> bool {
    let (line, column) = line_prefix(text, offset);
    let trimmed = line.trim_start();

    if !trimmed.starts_with("use ") {
        return false;
    }

    if let Some(semicolon) = line.find(';')
        && column > semicolon
    {
        return false;
    }

    true
}

fn looks_like_values_assign_lhs(text: &str, offset: usize) -> bool {
    let (line, column) = line_prefix(text, offset);
    let trimmed = line.trim_start();

    if trimmed.starts_with("include ") || trimmed.starts_with("use ") {
        return false;
    }

    if let Some(equal) = line.find('=')
        && column > equal
    {
        return false;
    }

    true
}

fn looks_like_symbol_context(text: &str, offset: usize) -> bool {
    let (line, _) = line_prefix(text, offset);
    let trimmed = line.trim_start();

    trimmed.starts_with("use ")
        || trimmed.starts_with("when ")
        || trimmed.starts_with("require!(")
        || trimmed.contains("==")
        || trimmed.contains("!=")
        || trimmed.ends_with("(")
        || trimmed.ends_with("::")
}

fn line_prefix(text: &str, offset: usize) -> (&str, usize) {
    let clamped = offset.min(text.len());
    let line_start = text[..clamped]
        .rfind('\n')
        .map(|index| index + 1)
        .unwrap_or(0);
    (&text[line_start..clamped], clamped - line_start)
}
