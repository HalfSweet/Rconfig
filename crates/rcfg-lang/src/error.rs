use std::collections::HashMap;

use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticArgValue {
    Bool(bool),
    Int(i128),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelatedInfo {
    pub span: Span,
    pub path: Option<String>,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: String,
    pub message: String,
    pub span: Span,
    pub path: Option<String>,
    pub source: Option<String>,
    pub include_chain: Vec<String>,
    pub args: HashMap<String, DiagnosticArgValue>,
    pub message_key: Option<String>,
    pub note: Option<String>,
    pub related: Vec<RelatedInfo>,
}

impl Diagnostic {
    pub fn error(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Error,
            code: code.into(),
            message: message.into(),
            span,
            path: None,
            source: None,
            include_chain: Vec::new(),
            args: HashMap::new(),
            message_key: None,
            note: None,
            related: Vec::new(),
        }
    }

    pub fn warning(code: impl Into<String>, message: impl Into<String>, span: Span) -> Self {
        Self {
            severity: Severity::Warning,
            code: code.into(),
            message: message.into(),
            span,
            path: None,
            source: None,
            include_chain: Vec::new(),
            args: HashMap::new(),
            message_key: None,
            note: None,
            related: Vec::new(),
        }
    }

    pub fn with_path(mut self, path: impl Into<String>) -> Self {
        self.path = Some(path.into());
        self
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    pub fn with_include_chain(mut self, include_chain: Vec<String>) -> Self {
        self.include_chain = include_chain;
        self
    }

    pub fn with_arg(mut self, key: impl Into<String>, value: DiagnosticArgValue) -> Self {
        self.args.insert(key.into(), value);
        self
    }

    pub fn with_message_key(mut self, message_key: impl Into<String>) -> Self {
        self.message_key = Some(message_key.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    pub fn with_related(mut self, related: RelatedInfo) -> Self {
        self.related.push(related);
        self
    }
}
