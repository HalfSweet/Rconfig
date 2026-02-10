use rcfg_lang::{ConstValue, SymbolKind, SymbolOccurrenceRole, SymbolTable, ValueType};
use tower_lsp::lsp_types::{Hover, HoverContents, MarkedString, Position, Url};

use crate::document::{AliasBinding, ProjectSnapshot};
use crate::position::lsp_position_to_offset;

pub fn provide(project: &ProjectSnapshot, uri: &Url, position: Position) -> Option<Hover> {
    let analysis = project.analysis.as_ref()?;
    let source = project.schema_docs.get(uri)?;
    let local_offset = lsp_position_to_offset(source, position);

    if let Some(binding) = analysis
        .alias_bindings
        .iter()
        .find(|binding| binding.uri == *uri && binding.contains_local_offset(local_offset))
    {
        return Some(hover_for_alias(&analysis.symbols, binding));
    }

    let base_offset = project.uri_base_offsets.get(uri).copied().unwrap_or(0);
    let global_offset = local_offset + base_offset;

    let occurrence = analysis
        .position_index
        .find_symbol_at_offset(global_offset)?;

    if occurrence.role == SymbolOccurrenceRole::Reference
        && occurrence.path.contains("::")
        && analysis
            .symbols
            .enum_owner_of_variant(&occurrence.path)
            .is_some()
    {
        return Some(hover_for_variant(&analysis.symbols, &occurrence.path));
    }

    match occurrence.kind {
        SymbolKind::Option => hover_for_option(&analysis.symbols, &occurrence.path),
        SymbolKind::Enum => hover_for_enum(&analysis.symbols, &occurrence.path),
        SymbolKind::Mod => hover_for_mod(&analysis.symbols, &occurrence.path),
    }
}

fn hover_for_alias(symbols: &SymbolTable, binding: &AliasBinding) -> Hover {
    let mut lines = vec![
        format!("**use alias** `{}`", binding.alias),
        format!("raw path: `{}`", binding.raw_path),
        format!("target: `{}`", binding.target_path),
    ];

    append_target_symbol_details(symbols, &binding.target_path, &mut lines);

    markdown_hover(lines.join("\n"))
}

fn append_target_symbol_details(symbols: &SymbolTable, target_path: &str, lines: &mut Vec<String>) {
    if let Some(ty) = symbols.option_type(target_path) {
        lines.push("target kind: `option`".to_string());
        lines.push(format!("type: `{}`", type_to_string(ty)));
        if let Some(default_value) = symbols.option_default(target_path) {
            lines.push(format!(
                "default: `{}`",
                const_value_to_string(default_value)
            ));
        }

        let docs = joined_docs(symbols.symbol_docs(target_path));
        if !docs.is_empty() {
            lines.push(String::new());
            lines.push(docs);
        }
        return;
    }

    if let Some(owner) = symbols.enum_owner_of_variant(target_path) {
        lines.push("target kind: `enum variant`".to_string());
        lines.push(format!("enum: `{owner}`"));

        let docs = joined_docs(symbols.enum_variant_docs(target_path));
        if !docs.is_empty() {
            lines.push(String::new());
            lines.push(docs);
        }
        return;
    }

    let Some(info) = symbols.get(target_path) else {
        lines.push("target kind: `unknown`".to_string());
        return;
    };

    lines.push(format!("target kind: `{}`", symbol_kind_label(info.kind)));

    if matches!(info.kind, SymbolKind::Enum) {
        let enum_name = target_path.rsplit("::").next().unwrap_or(target_path);
        let variants = symbols
            .enum_variant_names(target_path)
            .or_else(|| symbols.enum_variant_names(enum_name))
            .unwrap_or_default();
        if !variants.is_empty() {
            lines.push(format!("variants: `{}`", variants.join("`, `")));
        }
    }

    let docs = joined_docs(symbols.symbol_docs(target_path));
    if !docs.is_empty() {
        lines.push(String::new());
        lines.push(docs);
    }
}

fn symbol_kind_label(kind: SymbolKind) -> &'static str {
    match kind {
        SymbolKind::Mod => "mod",
        SymbolKind::Enum => "enum",
        SymbolKind::Option => "option",
    }
}

fn hover_for_option(symbols: &rcfg_lang::SymbolTable, path: &str) -> Option<Hover> {
    let ty = symbols.option_type(path)?;
    let default = symbols.option_default(path);
    let docs = joined_docs(symbols.symbol_docs(path));

    let mut lines = vec![format!("**option** `{path}`")];
    lines.push(format!("type: `{}`", type_to_string(ty)));
    if let Some(default_value) = default {
        lines.push(format!(
            "default: `{}`",
            const_value_to_string(default_value)
        ));
    }
    if !docs.is_empty() {
        lines.push(String::new());
        lines.push(docs);
    }

    Some(markdown_hover(lines.join("\n")))
}

fn hover_for_enum(symbols: &rcfg_lang::SymbolTable, path: &str) -> Option<Hover> {
    let docs = joined_docs(symbols.symbol_docs(path));
    let enum_name = path.rsplit("::").next().unwrap_or(path);
    let variants = symbols
        .enum_variant_names(path)
        .or_else(|| symbols.enum_variant_names(enum_name))
        .unwrap_or_default();

    let mut lines = vec![format!("**enum** `{path}`")];
    if !variants.is_empty() {
        lines.push(format!("variants: `{}`", variants.join("`, `")));
    }
    if !docs.is_empty() {
        lines.push(String::new());
        lines.push(docs);
    }

    Some(markdown_hover(lines.join("\n")))
}

fn hover_for_mod(symbols: &rcfg_lang::SymbolTable, path: &str) -> Option<Hover> {
    let docs = joined_docs(symbols.symbol_docs(path));
    let mut lines = vec![format!("**mod** `{path}`")];
    if !docs.is_empty() {
        lines.push(String::new());
        lines.push(docs);
    }
    Some(markdown_hover(lines.join("\n")))
}

fn hover_for_variant(symbols: &rcfg_lang::SymbolTable, path: &str) -> Hover {
    let docs = joined_docs(symbols.enum_variant_docs(path));
    let owner = symbols
        .enum_owner_of_variant(path)
        .map(str::to_string)
        .unwrap_or_else(|| "<unknown>".to_string());

    let mut lines = vec![
        format!("**enum variant** `{path}`"),
        format!("enum: `{owner}`"),
    ];
    if !docs.is_empty() {
        lines.push(String::new());
        lines.push(docs);
    }

    markdown_hover(lines.join("\n"))
}

fn markdown_hover(text: String) -> Hover {
    Hover {
        contents: HoverContents::Scalar(MarkedString::String(text)),
        range: None,
    }
}

fn joined_docs(docs: Option<&[rcfg_lang::Spanned<String>]>) -> String {
    docs.map(|items| {
        items
            .iter()
            .map(|item| item.value.trim())
            .filter(|line| !line.is_empty())
            .collect::<Vec<_>>()
            .join("\n")
    })
    .unwrap_or_default()
}

fn type_to_string(ty: &ValueType) -> String {
    match ty {
        ValueType::Bool => "bool".to_string(),
        ValueType::Int(int_ty) => match int_ty {
            rcfg_lang::IntType::U8 => "u8".to_string(),
            rcfg_lang::IntType::U16 => "u16".to_string(),
            rcfg_lang::IntType::U32 => "u32".to_string(),
            rcfg_lang::IntType::I32 => "i32".to_string(),
        },
        ValueType::UntypedInt => "int".to_string(),
        ValueType::String => "string".to_string(),
        ValueType::Enum(name) => format!("enum {name}"),
        ValueType::Unknown => "unknown".to_string(),
    }
}

fn const_value_to_string(value: &ConstValue) -> String {
    match value {
        ConstValue::Bool(v, _) => v.to_string(),
        ConstValue::Int(v, _) => v.to_string(),
        ConstValue::String(v, _) => format!("\"{v}\""),
        ConstValue::EnumPath(path) => path.to_string(),
    }
}
