use std::collections::{HashMap, HashSet};

use rcfg_lang::{Diagnostic as RcfgDiagnostic, Span, SymbolPositionIndex, SymbolTable};
use tower_lsp::lsp_types::Url;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DocumentKind {
    Schema,
    Values,
    Unknown,
}

impl DocumentKind {
    pub fn from_uri(uri: &Url) -> Self {
        let Some(path) = uri.to_file_path().ok() else {
            return Self::Unknown;
        };

        match path.extension().and_then(|ext| ext.to_str()) {
            Some("rcfg") => Self::Schema,
            Some("rcfgv") => Self::Values,
            _ => Self::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DocumentState {
    pub uri: Url,
    pub version: i32,
    pub text: String,
    pub kind: DocumentKind,
    pub is_open: bool,
}

impl DocumentState {
    pub fn new(uri: Url, version: i32, text: String, kind: DocumentKind) -> Self {
        Self {
            uri,
            version,
            text,
            kind,
            is_open: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SchemaFileSnapshot {
    pub uri: Url,
    pub text: String,
    pub base_offset: usize,
}

impl SchemaFileSnapshot {
    pub fn end_offset(&self) -> usize {
        self.base_offset + self.text.len()
    }

    pub fn contains_global_offset(&self, offset: usize) -> bool {
        if self.base_offset == self.end_offset() {
            offset == self.base_offset
        } else {
            offset >= self.base_offset && offset <= self.end_offset()
        }
    }

    pub fn global_to_local_offset(&self, offset: usize) -> usize {
        offset.saturating_sub(self.base_offset).min(self.text.len())
    }
}

#[derive(Debug, Clone)]
pub struct AliasBinding {
    pub uri: Url,
    pub alias: String,
    pub raw_path: String,
    pub alias_span: Span,
    pub target_path: String,
}

impl AliasBinding {
    pub fn contains_local_offset(&self, offset: usize) -> bool {
        if self.alias_span.start == self.alias_span.end {
            offset == self.alias_span.start
        } else {
            offset >= self.alias_span.start && offset < self.alias_span.end
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProjectAnalysis {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<RcfgDiagnostic>,
    pub position_index: SymbolPositionIndex,
    pub schema_files: Vec<SchemaFileSnapshot>,
    pub alias_bindings: Vec<AliasBinding>,
}

#[derive(Debug, Clone, Default)]
pub struct ProjectSnapshot {
    pub project_key: String,
    pub schema_docs: HashMap<Url, String>,
    pub uri_base_offsets: HashMap<Url, usize>,
    pub analysis: Option<ProjectAnalysis>,
    pub doc_indexes: HashMap<Url, String>,
}

#[derive(Debug, Default)]
pub struct DocumentStore {
    pub documents: HashMap<Url, DocumentState>,
    pub projects: HashMap<String, ProjectSnapshot>,
    pub published_diagnostic_uris: HashSet<Url>,
}

impl DocumentStore {
    pub fn upsert_document(&mut self, uri: Url, version: i32, text: String, kind: DocumentKind) {
        self.documents
            .insert(uri.clone(), DocumentState::new(uri, version, text, kind));
    }

    pub fn close_document(&mut self, uri: &Url) -> Option<DocumentState> {
        self.documents.remove(uri)
    }

    pub fn get_document(&self, uri: &Url) -> Option<&DocumentState> {
        self.documents.get(uri)
    }

    pub fn get_document_mut(&mut self, uri: &Url) -> Option<&mut DocumentState> {
        self.documents.get_mut(uri)
    }

    pub fn open_documents(&self) -> impl Iterator<Item = &DocumentState> {
        self.documents.values().filter(|doc| doc.is_open)
    }

    pub fn mark_diagnostics_published(&mut self, uris: impl IntoIterator<Item = Url>) {
        self.published_diagnostic_uris.extend(uris);
    }
}
