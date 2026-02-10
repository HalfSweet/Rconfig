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
    plan_c_header_exports_with_options(symbols, include_secrets, prefix, ExportNameRule::PkgPath)
}

fn plan_c_header_exports_with_options(
    symbols: &SymbolTable,
    include_secrets: bool,
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
    let (planned, mut diagnostics) = plan_c_header_exports_with_options(
        symbols,
        options.include_secrets,
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

pub fn builtin_exporter_names() -> Vec<&'static str> {
    vec!["c-header", "cmake"]
}

pub fn create_builtin_exporter(name: &str) -> Option<Box<dyn ConfigExporter>> {
    match name {
        "c-header" => Some(Box::new(CHeaderExporter)),
        "cmake" => Some(Box::new(CmakeExporter)),
        _ => None,
    }
}
