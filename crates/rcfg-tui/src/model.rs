use std::collections::BTreeMap;

use rcfg_lang::{IntRange, Item, SymbolKind, SymbolTable, ValueType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Module,
    Option,
    Enum,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GuardClause {
    When(String),
    MatchCase {
        expr: String,
        pattern: String,
        case_guard: Option<String>,
    },
}

#[derive(Debug, Clone)]
pub struct ConfigNode {
    pub id: usize,
    pub parent_id: Option<usize>,
    pub path: String,
    pub name: String,
    pub kind: NodeKind,
    pub depth: usize,
    pub value_type: Option<ValueType>,
    pub enum_variants: Vec<String>,
    pub is_secret: bool,
    pub range: Option<IntRange>,
    pub guard: Vec<GuardClause>,
    pub children: Vec<usize>,
}

#[derive(Debug, Clone, Default)]
pub struct ConfigTree {
    pub nodes: Vec<ConfigNode>,
    pub root_ids: Vec<usize>,
    pub path_to_node: BTreeMap<String, usize>,
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
            &[],
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

    pub fn find_node_by_path(&self, path: &str) -> Option<usize> {
        self.path_to_node.get(path).copied()
    }
}

fn collect_items(
    symbols: &SymbolTable,
    items: &[Item],
    guard: &[GuardClause],
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
                    scope.len(),
                    None,
                    Vec::new(),
                    false,
                    None,
                    guard.to_vec(),
                );

                scope.push(module.name.value.clone());
                collect_items(
                    symbols,
                    &module.items,
                    guard,
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
                let is_secret = symbols.option_is_secret(&path);
                let range = symbols.option_range(&path).cloned();
                push_node(
                    tree,
                    next_id,
                    parent,
                    path,
                    option.name.value.clone(),
                    NodeKind::Option,
                    scope.len(),
                    value_type,
                    option_variants,
                    is_secret,
                    range,
                    guard.to_vec(),
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
                    scope.len(),
                    None,
                    Vec::new(),
                    false,
                    None,
                    guard.to_vec(),
                );
            }
            Item::When(when_block) => {
                let mut next_guard = guard.to_vec();
                next_guard.push(GuardClause::When(when_block.condition.to_string()));
                collect_items(
                    symbols,
                    &when_block.items,
                    &next_guard,
                    scope,
                    parent,
                    next_id,
                    enum_variants,
                    tree,
                );
            }
            Item::Match(match_block) => {
                for case in &match_block.cases {
                    let mut next_guard = guard.to_vec();
                    next_guard.push(GuardClause::MatchCase {
                        expr: match_block.expr.to_string(),
                        pattern: case.pattern.to_string(),
                        case_guard: case.guard.as_ref().map(ToString::to_string),
                    });
                    collect_items(
                        symbols,
                        &case.items,
                        &next_guard,
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
    depth: usize,
    value_type: Option<ValueType>,
    enum_variants: Vec<String>,
    is_secret: bool,
    range: Option<IntRange>,
    guard: Vec<GuardClause>,
) -> usize {
    let id = *next_id;
    *next_id += 1;

    let path_for_index = path.clone();

    tree.nodes.push(ConfigNode {
        id,
        parent_id: parent,
        path,
        name,
        kind,
        depth,
        value_type,
        enum_variants,
        is_secret,
        range,
        guard,
        children: Vec::new(),
    });

    tree.path_to_node.entry(path_for_index).or_insert(id);

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

#[cfg(test)]
mod tests {
    use super::{ConfigTree, GuardClause};
    use rcfg_lang::{analyze_schema, parser::parse_schema_with_diagnostics};

    fn build_tree(schema: &str) -> ConfigTree {
        let (file, parse_diags) = parse_schema_with_diagnostics(schema);
        assert!(
            parse_diags.is_empty(),
            "schema should parse without diagnostics: {parse_diags:?}"
        );

        let report = analyze_schema(&file);
        assert!(
            report
                .diagnostics
                .iter()
                .all(|diag| diag.severity != rcfg_lang::Severity::Error),
            "schema should analyze without errors: {:?}",
            report.diagnostics
        );

        ConfigTree::from_schema(&report.symbols)
    }

    #[test]
    fn tree_records_single_when_guard() {
        let tree = build_tree(
            r#"
mod app {
  option enabled: bool = true;

  when enabled {
    option advanced: bool = false;
  }
}
"#,
        );

        let option_id = tree
            .find_node_by_path("app::advanced")
            .expect("find option node");
        let option = tree.node(option_id).expect("option node");

        assert_eq!(option.guard, vec![GuardClause::When("enabled".to_string())]);
    }

    #[test]
    fn tree_records_nested_when_and_match_case_guards() {
        let tree = build_tree(
            r#"
mod app {
  enum Mode { basic, pro }

  option enabled: bool = true;
  option mode: Mode = Mode::basic;
  option strict_lane: bool = true;

  when enabled {
    match mode {
      case Mode::pro if strict_lane => {
        option advanced: bool = false;
      }
      case Mode::basic => { }
    }
  }
}
"#,
        );

        let option_id = tree
            .find_node_by_path("app::advanced")
            .expect("find option node");
        let option = tree.node(option_id).expect("option node");

        assert_eq!(
            option.guard,
            vec![
                GuardClause::When("enabled".to_string()),
                GuardClause::MatchCase {
                    expr: "mode".to_string(),
                    pattern: "Mode::pro".to_string(),
                    case_guard: Some("strict_lane".to_string()),
                },
            ]
        );
    }
}
