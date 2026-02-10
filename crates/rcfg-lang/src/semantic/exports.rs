use super::*;

pub fn plan_c_header_exports(
    symbols: &SymbolTable,
    include_secrets: bool,
) -> (Vec<PlannedExport>, Vec<Diagnostic>) {
    plan_c_header_exports_with_prefix(symbols, include_secrets, "CONFIG_")
}

pub fn plan_c_header_exports_with_prefix(
    symbols: &SymbolTable,
    include_secrets: bool,
    prefix: &str,
) -> (Vec<PlannedExport>, Vec<Diagnostic>) {
    plan_exports_with_options(
        symbols,
        include_secrets,
        false,
        prefix,
        ExportNameRule::PkgPath,
    )
}

fn plan_exports_with_options(
    symbols: &SymbolTable,
    include_secrets: bool,
    include_context: bool,
    prefix: &str,
    export_name_rule: ExportNameRule,
) -> (Vec<PlannedExport>, Vec<Diagnostic>) {
    let mut diagnostics = Vec::new();
    let mut planned = Vec::new();
    let mut used_names: HashMap<String, String> = HashMap::new();

    let mut option_paths = symbols.option_types.keys().cloned().collect::<Vec<_>>();
    option_paths.sort_by(|left, right| left.as_str().cmp(right.as_str()));

    for option_path in option_paths {
        let option_path_str = option_path.as_str();
        if !include_context && (option_path_str == "ctx" || option_path_str.starts_with("ctx::")) {
            continue;
        }
        if symbols.option_is_secret(option_path_str) && !include_secrets {
            diagnostics.push(
                Diagnostic::warning(
                    "W_SECRET_NOT_EXPORTED",
                    format!(
                        "secret option `{}` is not exported unless `--export-secrets` is enabled",
                        option_path_str
                    ),
                    symbols.option_span(option_path_str).unwrap_or_default(),
                )
                .with_path(option_path_str.to_string()),
            );
            continue;
        }

        let export_name = normalize_export_name_with_prefix(
            &rewrite_export_name_path(option_path_str, export_name_rule),
            prefix,
        );
        if let Some(existing) = used_names.get(&export_name) {
            if existing != option_path_str {
                diagnostics.push(
                    Diagnostic::error(
                        "E_EXPORT_NAME_COLLISION",
                        format!(
                            "export name `{}` collides between `{}` and `{}`",
                            export_name, existing, option_path_str
                        ),
                        symbols.option_span(option_path_str).unwrap_or_default(),
                    )
                    .with_path(option_path_str.to_string()),
                );
                continue;
            }
        }

        used_names.insert(export_name.clone(), option_path_str.to_string());
        planned.push(PlannedExport {
            path: option_path_str.to_string(),
            name: export_name,
        });
    }

    (planned, diagnostics)
}

pub fn generate_exports(
    symbols: &SymbolTable,
    resolved: &ResolvedConfig,
    options: &ExportOptions,
) -> GeneratedExports {
    let (planned, mut diagnostics) = plan_exports_with_options(
        symbols,
        options.include_secrets,
        options.include_context,
        &options.c_prefix,
        options.export_name_rule,
    );

    let resolved_map = resolved
        .options
        .iter()
        .map(|option| (option.path.clone(), option.clone()))
        .collect::<HashMap<_, _>>();

    let mut c_lines = Vec::new();
    let mut cmake_lines = Vec::new();

    for export in planned {
        let Some(option) = resolved_map.get(&export.path) else {
            continue;
        };
        if !option.active {
            continue;
        }
        if symbols.option_is_secret(&export.path) && !options.include_secrets {
            continue;
        }

        let export_name_path = rewrite_export_name_path(&export.path, options.export_name_rule);
        let cmake_name =
            normalize_export_name_with_prefix(&export_name_path, &options.cmake_prefix);
        match option.value.as_ref() {
            Some(ResolvedValue::Bool(value)) => {
                if *value {
                    c_lines.push(format!("#define {} 1", export.name));
                } else if options.bool_false_style == BoolFalseExportStyle::DefineZero {
                    c_lines.push(format!("#define {} 0", export.name));
                }
                cmake_lines.push(format!(
                    "set({} {})",
                    cmake_name,
                    if *value { "ON" } else { "OFF" }
                ));
            }
            Some(ResolvedValue::Int(value)) => {
                let rendered = render_int_export(*value, options.int_export_format);
                c_lines.push(format!("#define {} {}", export.name, rendered));
                cmake_lines.push(format!("set({} {})", cmake_name, rendered));
            }
            Some(ResolvedValue::String(value)) => {
                let escaped = value.replace('"', "\\\"");
                c_lines.push(format!("#define {} \"{}\"", export.name, escaped));
                cmake_lines.push(format!("set({} \"{}\")", cmake_name, escaped));
            }
            Some(ResolvedValue::EnumVariant(selected_variant)) => {
                cmake_lines.push(format!("set({} \"{}\")", cmake_name, selected_variant));

                match options.enum_export_style {
                    EnumExportStyle::String => {
                        c_lines.push(format!("#define {} \"{}\"", export.name, selected_variant));
                    }
                    EnumExportStyle::OneHot => {
                        let enum_owner = symbols.enum_owner_of_variant(selected_variant);
                        let Some(enum_owner) = enum_owner else {
                            diagnostics.push(Diagnostic::error(
                                "E_VALUE_PATH_NOT_ENUM_VARIANT",
                                format!(
                                    "resolved enum variant `{}` for `{}` cannot be resolved",
                                    selected_variant, export.path
                                ),
                                symbols.option_span(&export.path).unwrap_or_default(),
                            ));
                            continue;
                        };

                        let mut variants = symbols
                            .enum_variants
                            .iter()
                            .filter(|(_, owner)| owner.as_str() == enum_owner)
                            .map(|(variant_path, _)| variant_path.as_str().to_string())
                            .collect::<Vec<_>>();
                        variants.sort();

                        for variant in variants {
                            let variant_name = normalize_export_name_with_prefix(
                                &format!(
                                    "{}::{}",
                                    export_name_path,
                                    variant.rsplit("::").next().unwrap_or(&variant)
                                ),
                                &options.c_prefix,
                            );
                            let is_selected = variant == *selected_variant;
                            c_lines.push(format!(
                                "#define {} {}",
                                variant_name,
                                if is_selected { 1 } else { 0 }
                            ));
                        }
                    }
                }
            }
            None => {}
        }
    }

    let mut define_lines = c_lines
        .iter()
        .filter(|line| line.starts_with("#define "))
        .cloned()
        .collect::<Vec<_>>();
    define_lines.sort();

    let include_guard = c_header_include_guard(&options.c_prefix);

    let mut final_c_lines = vec![
        "#pragma once".to_string(),
        format!("#ifndef {}", include_guard),
        format!("#define {}", include_guard),
        "/* Auto-generated by rcfg — do not edit */".to_string(),
        String::new(),
    ];
    final_c_lines.extend(define_lines);
    final_c_lines.push(String::new());
    final_c_lines.push(format!("#endif /* {} */", include_guard));

    cmake_lines.sort();

    let mut final_cmake_lines = vec![format!(
        "# Auto-generated by rcfg {} — do not edit",
        env!("CARGO_PKG_VERSION")
    )];
    if !cmake_lines.is_empty() {
        final_cmake_lines.push(String::new());
        final_cmake_lines.extend(cmake_lines);
    }

    GeneratedExports {
        c_header: final_c_lines.join("\n"),
        cmake: final_cmake_lines.join("\n"),
        diagnostics,
    }
}

fn rewrite_export_name_path(path: &str, rule: ExportNameRule) -> String {
    match rule {
        ExportNameRule::PkgPath => path.to_string(),
        ExportNameRule::PathOnly => path
            .split_once("::")
            .map(|(_, rest)| rest.to_string())
            .unwrap_or_else(|| path.to_string()),
    }
}

fn render_int_export(value: i128, format: IntExportFormat) -> String {
    match format {
        IntExportFormat::Decimal => value.to_string(),
        IntExportFormat::Hex => {
            if value < 0 {
                format!("-0x{:X}", value.unsigned_abs())
            } else {
                format!("0x{:X}", value as u128)
            }
        }
    }
}

fn c_header_include_guard(c_prefix: &str) -> String {
    let normalized = c_prefix
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_uppercase();

    if normalized.is_empty() {
        "RCFG_GENERATED_CONFIG_H".to_string()
    } else {
        format!("RCFG_{}_H", normalized)
    }
}

#[derive(Debug, Clone)]
struct RustEnumSpec {
    type_name: String,
    ordered_variants: Vec<String>,
    variant_by_path: HashMap<String, String>,
}

fn to_rust_pascal_identifier(raw: &str) -> String {
    let mut output = String::new();

    for segment in raw.split(|ch: char| !ch.is_ascii_alphanumeric()) {
        if segment.is_empty() {
            continue;
        }

        let mut chars = segment.chars();
        let Some(first) = chars.next() else {
            continue;
        };

        if output.is_empty() && first.is_ascii_digit() {
            output.push('X');
        }

        output.push(first.to_ascii_uppercase());
        for ch in chars {
            output.push(ch.to_ascii_lowercase());
        }
    }

    if output.is_empty() {
        "X".to_string()
    } else {
        output
    }
}

fn build_rust_enum_specs(symbols: &SymbolTable) -> BTreeMap<String, RustEnumSpec> {
    let mut owners = symbols
        .enum_variants
        .values()
        .map(|owner| owner.as_str().to_string())
        .collect::<Vec<_>>();
    owners.sort();
    owners.dedup();

    let mut type_name_counters: HashMap<String, usize> = HashMap::new();
    let mut enum_specs = BTreeMap::new();

    for owner_path in owners {
        let base_type_name = to_rust_pascal_identifier(&owner_path);
        let next_index = type_name_counters
            .entry(base_type_name.clone())
            .or_insert(0);
        *next_index += 1;

        let type_name = if *next_index == 1 {
            base_type_name
        } else {
            format!("{}{}", base_type_name, next_index)
        };

        let mut variant_paths = symbols
            .enum_variants
            .iter()
            .filter(|(_, owner)| owner.as_str() == owner_path)
            .map(|(variant_path, _)| variant_path.as_str().to_string())
            .collect::<Vec<_>>();
        variant_paths.sort();

        let mut variant_name_counters: HashMap<String, usize> = HashMap::new();
        let mut ordered_variants = Vec::new();
        let mut variant_by_path = HashMap::new();

        for variant_path in variant_paths {
            let raw_variant = variant_path
                .rsplit("::")
                .next()
                .unwrap_or(variant_path.as_str());
            let base_variant_name = to_rust_pascal_identifier(raw_variant);

            let next_variant_index = variant_name_counters
                .entry(base_variant_name.clone())
                .or_insert(0);
            *next_variant_index += 1;

            let variant_name = if *next_variant_index == 1 {
                base_variant_name
            } else {
                format!("{}{}", base_variant_name, next_variant_index)
            };

            ordered_variants.push(variant_name.clone());
            variant_by_path.insert(variant_path, variant_name);
        }

        enum_specs.insert(
            owner_path,
            RustEnumSpec {
                type_name,
                ordered_variants,
                variant_by_path,
            },
        );
    }

    enum_specs
}

fn rust_int_type_name(value_type: Option<&ValueType>) -> &'static str {
    match value_type {
        Some(ValueType::Int(IntType::U8)) => "u8",
        Some(ValueType::Int(IntType::U16)) => "u16",
        Some(ValueType::Int(IntType::U32)) => "u32",
        Some(ValueType::Int(IntType::I32)) => "i32",
        _ => "i128",
    }
}

fn rust_int_fits_type(value: i128, rust_type: &str) -> bool {
    match rust_type {
        "u8" => u8::try_from(value).is_ok(),
        "u16" => u16::try_from(value).is_ok(),
        "u32" => u32::try_from(value).is_ok(),
        "i32" => i32::try_from(value).is_ok(),
        _ => true,
    }
}

fn escape_rust_string_literal(value: &str) -> String {
    value.chars().flat_map(|ch| ch.escape_default()).collect()
}

fn escape_python_string_literal(value: &str) -> String {
    let mut escaped = String::new();
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportRenderResult {
    pub format: String,
    pub content: String,
    pub diagnostics: Vec<Diagnostic>,
}

pub trait ConfigExporter {
    fn format_name(&self) -> &'static str;

    fn render(
        &self,
        symbols: &SymbolTable,
        resolved: &ResolvedConfig,
        options: &ExportOptions,
    ) -> ExportRenderResult;
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CHeaderExporter;

impl ConfigExporter for CHeaderExporter {
    fn format_name(&self) -> &'static str {
        "c-header"
    }

    fn render(
        &self,
        symbols: &SymbolTable,
        resolved: &ResolvedConfig,
        options: &ExportOptions,
    ) -> ExportRenderResult {
        let generated = generate_exports(symbols, resolved, options);
        ExportRenderResult {
            format: self.format_name().to_string(),
            content: generated.c_header,
            diagnostics: generated.diagnostics,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CmakeExporter;

impl ConfigExporter for CmakeExporter {
    fn format_name(&self) -> &'static str {
        "cmake"
    }

    fn render(
        &self,
        symbols: &SymbolTable,
        resolved: &ResolvedConfig,
        options: &ExportOptions,
    ) -> ExportRenderResult {
        let generated = generate_exports(symbols, resolved, options);
        ExportRenderResult {
            format: self.format_name().to_string(),
            content: generated.cmake,
            diagnostics: generated.diagnostics,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RustExporter;

impl ConfigExporter for RustExporter {
    fn format_name(&self) -> &'static str {
        "rust"
    }

    fn render(
        &self,
        symbols: &SymbolTable,
        resolved: &ResolvedConfig,
        options: &ExportOptions,
    ) -> ExportRenderResult {
        let (planned, mut diagnostics) = plan_exports_with_options(
            symbols,
            options.include_secrets,
            options.include_context,
            &options.c_prefix,
            options.export_name_rule,
        );

        let enum_specs = build_rust_enum_specs(symbols);
        let resolved_map = resolved
            .options
            .iter()
            .map(|option| (option.path.clone(), option.clone()))
            .collect::<HashMap<_, _>>();

        let mut const_lines = Vec::new();
        let mut used_enum_owners = BTreeSet::new();

        for export in planned {
            let Some(option) = resolved_map.get(&export.path) else {
                continue;
            };
            if !option.active {
                continue;
            }
            if symbols.option_is_secret(&export.path) && !options.include_secrets {
                continue;
            }

            match option.value.as_ref() {
                Some(ResolvedValue::Bool(value)) => {
                    const_lines.push(format!(
                        "pub const {}: bool = {};",
                        export.name,
                        if *value { "true" } else { "false" }
                    ));
                }
                Some(ResolvedValue::Int(value)) => {
                    let rust_type = rust_int_type_name(symbols.option_type(&export.path));
                    if !rust_int_fits_type(*value, rust_type) {
                        diagnostics.push(
                            Diagnostic::error(
                                "E_EXPORT_VALUE_OUT_OF_RANGE",
                                format!(
                                    "resolved integer value `{}` for `{}` cannot be represented as `{}`",
                                    value, export.path, rust_type
                                ),
                                symbols.option_span(&export.path).unwrap_or_default(),
                            )
                            .with_path(export.path.clone()),
                        );
                        continue;
                    }

                    const_lines.push(format!(
                        "pub const {}: {} = {};",
                        export.name, rust_type, value
                    ));
                }
                Some(ResolvedValue::String(value)) => {
                    let escaped = escape_rust_string_literal(value);
                    const_lines.push(format!(
                        "pub const {}: &str = \"{}\";",
                        export.name, escaped
                    ));
                }
                Some(ResolvedValue::EnumVariant(selected_variant)) => {
                    let Some(enum_owner) = symbols.enum_owner_of_variant(selected_variant) else {
                        diagnostics.push(
                            Diagnostic::error(
                                "E_VALUE_PATH_NOT_ENUM_VARIANT",
                                format!(
                                    "resolved enum variant `{}` for `{}` cannot be resolved",
                                    selected_variant, export.path
                                ),
                                symbols.option_span(&export.path).unwrap_or_default(),
                            )
                            .with_path(export.path.clone()),
                        );
                        continue;
                    };

                    let Some(enum_spec) = enum_specs.get(enum_owner) else {
                        diagnostics.push(
                            Diagnostic::error(
                                "E_VALUE_PATH_NOT_ENUM_VARIANT",
                                format!(
                                    "resolved enum variant `{}` for `{}` cannot be resolved",
                                    selected_variant, export.path
                                ),
                                symbols.option_span(&export.path).unwrap_or_default(),
                            )
                            .with_path(export.path.clone()),
                        );
                        continue;
                    };

                    let Some(variant_name) = enum_spec.variant_by_path.get(selected_variant) else {
                        diagnostics.push(
                            Diagnostic::error(
                                "E_VALUE_PATH_NOT_ENUM_VARIANT",
                                format!(
                                    "resolved enum variant `{}` for `{}` cannot be resolved",
                                    selected_variant, export.path
                                ),
                                symbols.option_span(&export.path).unwrap_or_default(),
                            )
                            .with_path(export.path.clone()),
                        );
                        continue;
                    };

                    used_enum_owners.insert(enum_owner.to_string());
                    const_lines.push(format!(
                        "pub const {}: {} = {}::{};",
                        export.name, enum_spec.type_name, enum_spec.type_name, variant_name
                    ));
                }
                None => {}
            }
        }

        const_lines.sort();

        let mut rust_lines = vec![format!(
            "// Auto-generated by rcfg {} — do not edit",
            env!("CARGO_PKG_VERSION")
        )];

        if !used_enum_owners.is_empty() || !const_lines.is_empty() {
            rust_lines.push(String::new());
        }

        for enum_owner in used_enum_owners {
            let Some(enum_spec) = enum_specs.get(&enum_owner) else {
                continue;
            };

            rust_lines.push("#[derive(Debug, Clone, Copy, PartialEq, Eq)]".to_string());
            rust_lines.push(format!("pub enum {} {{", enum_spec.type_name));
            for variant in &enum_spec.ordered_variants {
                rust_lines.push(format!("    {},", variant));
            }
            rust_lines.push("}".to_string());
            rust_lines.push(String::new());
        }

        rust_lines.extend(const_lines);

        ExportRenderResult {
            format: self.format_name().to_string(),
            content: rust_lines.join("\n"),
            diagnostics,
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct PythonExporter;

impl ConfigExporter for PythonExporter {
    fn format_name(&self) -> &'static str {
        "python"
    }

    fn render(
        &self,
        symbols: &SymbolTable,
        resolved: &ResolvedConfig,
        options: &ExportOptions,
    ) -> ExportRenderResult {
        let (planned, diagnostics) = plan_exports_with_options(
            symbols,
            options.include_secrets,
            options.include_context,
            &options.c_prefix,
            options.export_name_rule,
        );

        let resolved_map = resolved
            .options
            .iter()
            .map(|option| (option.path.clone(), option.clone()))
            .collect::<HashMap<_, _>>();

        let mut python_lines = Vec::new();

        for export in planned {
            let Some(option) = resolved_map.get(&export.path) else {
                continue;
            };
            if !option.active {
                continue;
            }
            if symbols.option_is_secret(&export.path) && !options.include_secrets {
                continue;
            }

            match option.value.as_ref() {
                Some(ResolvedValue::Bool(value)) => {
                    python_lines.push(format!(
                        "{} = {}",
                        export.name,
                        if *value { "True" } else { "False" }
                    ));
                }
                Some(ResolvedValue::Int(value)) => {
                    python_lines.push(format!("{} = {}", export.name, value));
                }
                Some(ResolvedValue::String(value)) => {
                    let escaped = escape_python_string_literal(value);
                    python_lines.push(format!("{} = \"{}\"", export.name, escaped));
                }
                Some(ResolvedValue::EnumVariant(selected_variant)) => {
                    let escaped = escape_python_string_literal(selected_variant);
                    python_lines.push(format!("{} = \"{}\"", export.name, escaped));
                }
                None => {}
            }
        }

        python_lines.sort();

        let mut content_lines = vec![format!(
            "# Auto-generated by rcfg {} — do not edit",
            env!("CARGO_PKG_VERSION")
        )];
        if !python_lines.is_empty() {
            content_lines.push(String::new());
            content_lines.extend(python_lines);
        }

        ExportRenderResult {
            format: self.format_name().to_string(),
            content: content_lines.join("\n"),
            diagnostics,
        }
    }
}

pub fn builtin_exporter_names() -> Vec<&'static str> {
    vec!["c-header", "cmake", "python", "rust"]
}

pub fn create_builtin_exporter(name: &str) -> Option<Box<dyn ConfigExporter>> {
    match name {
        "c-header" => Some(Box::new(CHeaderExporter)),
        "cmake" => Some(Box::new(CmakeExporter)),
        "python" => Some(Box::new(PythonExporter)),
        "rust" => Some(Box::new(RustExporter)),
        _ => None,
    }
}
