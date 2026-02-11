use std::collections::BTreeMap;

use rcfg_lang::{Item, SymbolKind, SymbolTable, ValueType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NodeKind {
    Module,
    Option,
    Enum,
}

#[derive(Debug, Clone)]
pub struct ConfigNode {
    pub id: usize,
    pub parent_id: Option<usize>,
    pub path: String,
    pub name: String,
    pub kind: NodeKind,
    pub value_type: Option<ValueType>,
    pub enum_variants: Vec<String>,
    pub children: Vec<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct ConfigTree {
    pub nodes: Vec<ConfigNode>,
    pub root_ids: Vec<usize>,
}

impl ConfigTree {
    pub fn from_schema(symbols: &SymbolTable) -> Self {
        let mut tree = Self::default();
        let mut scope = Vec::new();
        let mut next_id = 0usize;
        let enum_variants = enum_variants_by_enum_path(symbols.schema_items());
        collect_items(
            symbols,
            symbols.schema_items(),
            &mut scope,
            None,
            &mut next_id,
            &enum_variants,
            &mut tree,
        );
        tree
    }

    pub fn node(&self, id: usize) -> Option<&ConfigNode> {
        self.nodes.get(id)
    }

    pub fn children_of(&self, id: usize) -> &[usize] {
        self.nodes
            .get(id)
            .map(|node| node.children.as_slice())
            .unwrap_or_default()
    }
}

fn collect_items(
    symbols: &SymbolTable,
    items: &[Item],
    scope: &mut Vec<String>,
    parent: Option<usize>,
    next_id: &mut usize,
    enum_variants: &BTreeMap<String, Vec<String>>,
    tree: &mut ConfigTree,
) {
    for item in items {
        match item {
            Item::Mod(module) => {
                let path = scoped_path(scope, &module.name.value);
                let id = push_node(
                    tree,
                    next_id,
                    parent,
                    path,
                    module.name.value.clone(),
                    NodeKind::Module,
                    None,
                    Vec::new(),
                );

                scope.push(module.name.value.clone());
                collect_items(
                    symbols,
                    &module.items,
                    scope,
                    Some(id),
                    next_id,
                    enum_variants,
                    tree,
                );
                scope.pop();
            }
            Item::Option(option) => {
                let path = scoped_path(scope, &option.name.value);
                let value_type = symbols.option_type(&path).cloned();
                let option_variants = value_type
                    .as_ref()
                    .and_then(|ty| match ty {
                        ValueType::Enum(enum_path) => enum_variants
                            .get(enum_path)
                            .or_else(|| {
                                if enum_path.contains("::") {
                                    None
                                } else {
                                    let scoped = scoped_path(scope, enum_path);
                                    enum_variants.get(&scoped)
                                }
                            })
                            .cloned(),
                        _ => None,
                    })
                    .unwrap_or_default();
                push_node(
                    tree,
                    next_id,
                    parent,
                    path,
                    option.name.value.clone(),
                    NodeKind::Option,
                    value_type,
                    option_variants,
                );
            }
            Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                push_node(
                    tree,
                    next_id,
                    parent,
                    path,
                    enum_decl.name.value.clone(),
                    NodeKind::Enum,
                    None,
                    Vec::new(),
                );
            }
            Item::When(when_block) => {
                collect_items(
                    symbols,
                    &when_block.items,
                    scope,
                    parent,
                    next_id,
                    enum_variants,
                    tree,
                );
            }
            Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_items(
                        symbols,
                        &case.items,
                        scope,
                        parent,
                        next_id,
                        enum_variants,
                        tree,
                    );
                }
            }
            Item::Use(_)
            | Item::Require(_)
            | Item::Constraint(_)
            | Item::Patch(_)
            | Item::Export(_) => {}
        }
    }
}

fn push_node(
    tree: &mut ConfigTree,
    next_id: &mut usize,
    parent: Option<usize>,
    path: String,
    name: String,
    kind: NodeKind,
    value_type: Option<ValueType>,
    enum_variants: Vec<String>,
) -> usize {
    let id = *next_id;
    *next_id += 1;

    tree.nodes.push(ConfigNode {
        id,
        parent_id: parent,
        path,
        name,
        kind,
        value_type,
        enum_variants,
        children: Vec::new(),
    });

    if let Some(parent_id) = parent {
        if let Some(parent_node) = tree.nodes.get_mut(parent_id) {
            parent_node.children.push(id);
        }
    } else {
        tree.root_ids.push(id);
    }

    id
}

fn scoped_path(scope: &[String], name: &str) -> String {
    if scope.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", scope.join("::"), name)
    }
}

fn enum_variants_by_enum_path(items: &[Item]) -> BTreeMap<String, Vec<String>> {
    let mut out = BTreeMap::new();
    let mut scope = Vec::new();
    collect_enum_variants(items, &mut scope, &mut out);
    out
}

fn collect_enum_variants(
    items: &[Item],
    scope: &mut Vec<String>,
    out: &mut BTreeMap<String, Vec<String>>,
) {
    for item in items {
        match item {
            Item::Mod(module) => {
                scope.push(module.name.value.clone());
                collect_enum_variants(&module.items, scope, out);
                scope.pop();
            }
            Item::Enum(enum_decl) => {
                let path = scoped_path(scope, &enum_decl.name.value);
                let variants = enum_decl
                    .variants
                    .iter()
                    .map(|variant| variant.name.value.clone())
                    .collect::<Vec<_>>();
                out.insert(path, variants);
            }
            Item::When(when_block) => {
                collect_enum_variants(&when_block.items, scope, out);
            }
            Item::Match(match_block) => {
                for case in &match_block.cases {
                    collect_enum_variants(&case.items, scope, out);
                }
            }
            Item::Option(_)
            | Item::Use(_)
            | Item::Require(_)
            | Item::Constraint(_)
            | Item::Patch(_)
            | Item::Export(_) => {}
        }
    }
}

pub fn kind_from_symbol(symbol_kind: SymbolKind) -> NodeKind {
    match symbol_kind {
        SymbolKind::Mod => NodeKind::Module,
        SymbolKind::Enum => NodeKind::Enum,
        SymbolKind::Option => NodeKind::Option,
    }
}
