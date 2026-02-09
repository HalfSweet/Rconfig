use std::collections::HashMap;

use crate::ast::{File, Item};
use crate::error::Diagnostic;
use crate::span::Span;

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

    fn insert(&mut self, symbol: SymbolInfo) {
        self.symbols.insert(symbol.path.clone(), symbol);
    }
}

#[derive(Debug, Clone)]
pub struct SemanticReport {
    pub symbols: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn analyze_schema(file: &File) -> SemanticReport {
    let mut collector = SymbolCollector::default();
    let mut scope = Vec::new();
    collector.collect_items(&file.items, &mut scope);

    SemanticReport {
        symbols: collector.symbols,
        diagnostics: collector.diagnostics,
    }
}

#[derive(Debug, Clone, Copy)]
struct DeclInfo {
    kind: SymbolKind,
    span: Span,
}

#[derive(Default)]
struct SymbolCollector {
    symbols: SymbolTable,
    diagnostics: Vec<Diagnostic>,
    scope_members: HashMap<String, HashMap<String, DeclInfo>>,
}

impl SymbolCollector {
    fn collect_items(&mut self, items: &[Item], scope: &mut Vec<String>) {
        for item in items {
            match item {
                Item::Use(_) | Item::Require(_) | Item::Constraint(_) => {}
                Item::Option(option) => {
                    self.declare(scope, &option.name.value, SymbolKind::Option, option.name.span);
                }
                Item::Enum(enum_decl) => {
                    self.declare(scope, &enum_decl.name.value, SymbolKind::Enum, enum_decl.name.span);
                }
                Item::Mod(module) => {
                    self.declare(scope, &module.name.value, SymbolKind::Mod, module.name.span);
                    scope.push(module.name.value.clone());
                    self.collect_items(&module.items, scope);
                    scope.pop();
                }
                Item::When(when_block) => {
                    self.collect_items(&when_block.items, scope);
                }
                Item::Match(match_block) => {
                    for case in &match_block.cases {
                        self.collect_items(&case.items, scope);
                    }
                }
            }
        }
    }

    fn declare(&mut self, scope: &[String], name: &str, kind: SymbolKind, span: Span) {
        let scope_key = scope.join("::");
        let full_path = if scope_key.is_empty() {
            name.to_string()
        } else {
            format!("{}::{}", scope_key, name)
        };

        let members = self.scope_members.entry(scope_key).or_default();
        if let Some(existing) = members.get(name) {
            if existing.kind == kind {
                if kind == SymbolKind::Mod {
                    return;
                }
                self.diagnostics.push(Diagnostic::error(
                    "E_SYMBOL_REDEFINED",
                    format!("symbol `{}` is redefined", full_path),
                    span,
                ));
                return;
            }

            self.diagnostics.push(Diagnostic::error(
                "E_SYMBOL_KIND_CONFLICT",
                format!("symbol `{}` conflicts with previously declared kind", full_path),
                span.join(existing.span),
            ));
            return;
        }

        members.insert(name.to_string(), DeclInfo { kind, span });
        self.symbols.insert(SymbolInfo {
            kind,
            path: full_path,
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_schema_with_diagnostics;

    use super::{analyze_schema, SymbolKind};

    #[test]
    fn allows_open_module_redeclaration() {
        let src = r#"
mod uart {
  option enable: bool = false;
}

mod uart {
  option baud: u32 = 115200;
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report.diagnostics.is_empty(),
            "semantic diagnostics: {:#?}",
            report.diagnostics
        );
        assert!(matches!(
            report.symbols.get("uart"),
            Some(info) if info.kind == SymbolKind::Mod
        ));
        assert!(matches!(
            report.symbols.get("uart::enable"),
            Some(info) if info.kind == SymbolKind::Option
        ));
        assert!(matches!(
            report.symbols.get("uart::baud"),
            Some(info) if info.kind == SymbolKind::Option
        ));
    }

    #[test]
    fn reports_symbol_redefined() {
        let src = r#"
mod a {
  option x: bool = false;
}

mod a {
  option x: bool = true;
}
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_SYMBOL_REDEFINED"),
            "expected E_SYMBOL_REDEFINED, got: {:#?}",
            report.diagnostics
        );
    }

    #[test]
    fn reports_symbol_kind_conflict() {
        let src = r#"
mod foo { }
option foo: bool = false;
"#;
        let (file, parse_diags) = parse_schema_with_diagnostics(src);
        assert!(parse_diags.is_empty(), "parse diagnostics: {parse_diags:#?}");

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .any(|diag| diag.code == "E_SYMBOL_KIND_CONFLICT"),
            "expected E_SYMBOL_KIND_CONFLICT, got: {:#?}",
            report.diagnostics
        );
    }
}
