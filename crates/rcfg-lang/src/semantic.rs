use std::collections::HashMap;

use crate::ast::File;
use crate::error::Diagnostic;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Mod,
    Enum,
    Option,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolInfo {
    pub kind: SymbolKind,
    pub path: String,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
}

impl SymbolTable {
    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn get(&self, path: &str) -> Option<&SymbolInfo> {
        self.symbols.get(path)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &SymbolInfo)> {
        self.symbols.iter()
    }
}

#[derive(Debug, Clone)]
pub struct SemanticReport {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn analyze_schema(_file: &File) -> SemanticReport {
    SemanticReport {
        symbols: SymbolTable::default(),
        diagnostics: Vec::new(),
    }
}
