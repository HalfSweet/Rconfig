use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use glob::glob;
use rcfg_lang::parser::parse_schema_with_diagnostics;
use rcfg_lang::{
    Diagnostic, File, ResolvedConfig, ResolvedValue, Severity, ValuesAnalysisReport,
    analyze_schema, analyze_schema_strict, analyze_values_from_path_report_with_context,
    analyze_values_from_path_report_with_context_and_root,
    analyze_values_from_path_report_with_context_and_root_strict,
    analyze_values_from_path_report_with_context_strict, resolve_values,
    resolve_values_with_context,
};

#[derive(Debug, Clone)]
pub struct I18nCatalog {
    pub strings: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct ManifestModel {
    pub manifest_path: PathBuf,
    pub schema: Vec<PathBuf>,
    pub package_name: String,
    pub package_version: String,
    pub dependencies: BTreeMap<String, PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ManifestGraph {
    pub root: ManifestModel,
    pub packages_depth_first: Vec<ManifestModel>,
}

#[derive(Debug, Clone)]
pub struct AppLoadOptions {
    pub schema: Option<PathBuf>,
    pub manifest: Option<PathBuf>,
    pub context: Option<PathBuf>,
    pub i18n: Option<PathBuf>,
    pub strict: bool,
}

#[derive(Debug, Clone)]
pub struct AppSession {
    schema_file: File,
    parse_diagnostics: Vec<Diagnostic>,
    symbols: rcfg_lang::SymbolTable,
    context: HashMap<String, ResolvedValue>,
    i18n_catalog: Option<I18nCatalog>,
    package_name: Option<String>,
    include_root: Option<PathBuf>,
    strict: bool,
}

impl AppSession {
    pub fn schema_file(&self) -> &File {
        &self.schema_file
    }

    pub fn parse_diagnostics(&self) -> &[Diagnostic] {
        &self.parse_diagnostics
    }

    pub fn symbols(&self) -> &rcfg_lang::SymbolTable {
        &self.symbols
    }

    pub fn context(&self) -> &HashMap<String, ResolvedValue> {
        &self.context
    }

    pub fn i18n(&self) -> Option<&I18nCatalog> {
        self.i18n_catalog.as_ref()
    }

    pub fn package_name(&self) -> Option<&str> {
        self.package_name.as_deref()
    }

    pub fn include_root(&self) -> Option<&Path> {
        self.include_root.as_deref()
    }

    pub fn strict(&self) -> bool {
        self.strict
    }

    pub fn analyze_values_from_path(&self, path: &Path) -> ValuesAnalysisReport {
        analyze_values_report(
            path,
            &self.symbols,
            &self.context,
            self.include_root.as_deref(),
            self.strict,
        )
    }

    pub fn analyze_values(&self, values: &rcfg_lang::ValuesFile) -> Vec<Diagnostic> {
        let mut diagnostics = match (self.strict, self.include_root.as_deref()) {
            (true, Some(root)) => rcfg_lang::analyze_values_with_context_and_root_strict(
                values,
                &self.symbols,
                &self.context,
                root,
            ),
            (false, Some(root)) => rcfg_lang::analyze_values_with_context_and_root(
                values,
                &self.symbols,
                &self.context,
                root,
            ),
            (true, None) => {
                rcfg_lang::analyze_values_with_context_strict(values, &self.symbols, &self.context)
            }
            (false, None) => {
                rcfg_lang::analyze_values_with_context(values, &self.symbols, &self.context)
            }
        };

        diagnostics.extend(self.parse_diagnostics.clone());
        diagnostics
    }

    pub fn resolve(&self, values: &rcfg_lang::ValuesFile) -> ResolvedConfig {
        resolve_with_context(values, &self.symbols, &self.context)
    }

    pub fn localize_diagnostic_message(&self, diag: &Diagnostic) -> String {
        localize_diagnostic_message(diag, self.i18n())
    }
}

pub fn load_session(options: &AppLoadOptions) -> Result<AppSession, String> {
    let i18n_catalog = load_i18n_catalog(options.i18n.as_deref())?;
    let manifest_graph = load_manifest_graph(options.manifest.as_deref())?;
    let root_manifest = manifest_graph.as_ref().map(|graph| &graph.root);
    let schema_path = resolve_schema_path(options.schema.as_deref(), root_manifest)?;

    let (schema_file, mut parse_diagnostics) = if let Some(graph) = manifest_graph.as_ref() {
        load_schema_with_dependencies(graph, options.schema.as_deref())?
    } else {
        let schema_text = fs::read_to_string(&schema_path)
            .map_err(|err| format!("failed to read schema {}: {err}", schema_path.display()))?;
        parse_schema_with_diagnostics(&schema_text)
    };

    if parse_diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
    {
        return Ok(AppSession {
            schema_file,
            parse_diagnostics,
            symbols: rcfg_lang::SymbolTable::default(),
            context: HashMap::new(),
            i18n_catalog,
            package_name: root_manifest.map(|manifest| manifest.package_name.clone()),
            include_root: root_manifest
                .and_then(|manifest| manifest.manifest_path.parent().map(Path::to_path_buf))
                .or_else(|| std::env::current_dir().ok()),
            strict: options.strict,
        });
    }

    let schema_report = if options.strict {
        analyze_schema_strict(&schema_file)
    } else {
        analyze_schema(&schema_file)
    };

    parse_diagnostics.extend(schema_report.diagnostics);

    let context = load_context(options.context.as_deref())?;

    Ok(AppSession {
        schema_file,
        parse_diagnostics,
        symbols: schema_report.symbols,
        context,
        i18n_catalog,
        package_name: root_manifest.map(|manifest| manifest.package_name.clone()),
        include_root: root_manifest
            .and_then(|manifest| manifest.manifest_path.parent().map(Path::to_path_buf))
            .or_else(|| std::env::current_dir().ok()),
        strict: options.strict,
    })
}

pub fn diagnostic_arg_to_json(value: &rcfg_lang::DiagnosticArgValue) -> serde_json::Value {
    match value {
        rcfg_lang::DiagnosticArgValue::Bool(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::Int(raw) => serde_json::json!(raw),
        rcfg_lang::DiagnosticArgValue::String(raw) => serde_json::json!(raw),
    }
}

pub fn diagnostic_to_json(diag: &Diagnostic) -> serde_json::Value {
    let args = diag
        .args
        .iter()
        .map(|(key, value)| (key.clone(), diagnostic_arg_to_json(value)))
        .collect::<serde_json::Map<_, _>>();
    let related = diag
        .related
        .iter()
        .map(|item| {
            serde_json::json!({
                "message": item.message,
                "path": item.path,
                "span": {
                    "start": item.span.start,
                    "end": item.span.end,
                }
            })
        })
        .collect::<Vec<_>>();

    serde_json::json!({
        "severity": match diag.severity { Severity::Error => "error", Severity::Warning => "warning" },
        "code": diag.code,
        "message": diag.message,
        "message_key": diag.message_key,
        "note": diag.note,
        "args": args,
        "related": related,
        "path": diag.path,
        "source": diag.source,
        "include_chain": diag.include_chain,
        "span": {
            "start": diag.span.start,
            "end": diag.span.end,
        }
    })
}

pub fn localize_diagnostic_message(diag: &Diagnostic, i18n: Option<&I18nCatalog>) -> String {
    let Some(i18n) = i18n else {
        return diag.message.clone();
    };

    if let Some(message) = diag
        .message_key
        .as_ref()
        .and_then(|key| i18n.strings.get(key))
        .filter(|text| !text.is_empty())
    {
        return message.clone();
    }

    if let Some(message) = i18n.strings.get(&diag.code).filter(|text| !text.is_empty()) {
        return message.clone();
    }

    diag.message.clone()
}

pub fn load_context(path: Option<&Path>) -> Result<HashMap<String, ResolvedValue>, String> {
    let Some(path) = path else {
        return Ok(HashMap::new());
    };
    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read context {}: {err}", path.display()))?;
    let json = serde_json::from_str::<serde_json::Value>(&text)
        .map_err(|err| format!("failed to parse context json {}: {err}", path.display()))?;

    let mut out = HashMap::new();
    let obj = json
        .as_object()
        .ok_or_else(|| "context json must be a key-value object".to_string())?;
    for (key, value) in obj {
        let resolved = if let Some(raw) = value.as_bool() {
            ResolvedValue::Bool(raw)
        } else if let Some(raw) = value.as_i64() {
            ResolvedValue::Int(raw as i128)
        } else if let Some(raw) = value.as_str() {
            if raw.contains("::") {
                ResolvedValue::EnumVariant(raw.to_string())
            } else {
                ResolvedValue::String(raw.to_string())
            }
        } else {
            return Err(format!("unsupported context value for key `{}`", key));
        };
        out.insert(key.clone(), resolved);
    }
    Ok(out)
}

pub fn load_i18n_catalog(path: Option<&Path>) -> Result<Option<I18nCatalog>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read i18n {}: {err}", path.display()))?;
    let value = text
        .parse::<toml::Value>()
        .map_err(|err| format!("failed to parse i18n {}: {err}", path.display()))?;
    let Some(strings) = value.get("strings").and_then(toml::Value::as_table) else {
        return Ok(Some(I18nCatalog {
            strings: HashMap::new(),
        }));
    };

    let mut out = HashMap::new();
    for (key, value) in strings {
        let text = value
            .as_str()
            .ok_or_else(|| format!("invalid i18n value for key `{}`: expected string", key))?;
        out.insert(key.clone(), text.to_string());
    }

    Ok(Some(I18nCatalog { strings: out }))
}

pub fn load_manifest(path: Option<&Path>) -> Result<Option<ManifestModel>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let manifest_path = fs::canonicalize(path)
        .map_err(|err| format!("failed to resolve manifest {}: {err}", path.display()))?;

    let text = fs::read_to_string(&manifest_path)
        .map_err(|err| format!("failed to read manifest {}: {err}", manifest_path.display()))?;
    let value = text.parse::<toml::Value>().map_err(|err| {
        format!(
            "failed to parse manifest {}: {err}",
            manifest_path.display()
        )
    })?;

    let package = value
        .get("package")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| "manifest missing [package] table".to_string())?;

    let package_name = package
        .get("name")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| "manifest missing package.name".to_string())?
        .to_string();

    let package_version = package
        .get("version")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| "manifest missing package.version".to_string())?
        .to_string();

    let entry = value
        .get("entry")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| "manifest missing [entry] table".to_string())?;

    let base = manifest_path.parent().unwrap_or_else(|| Path::new("."));
    let schema = parse_manifest_schema_paths(entry, base)?;

    let dependencies = parse_manifest_dependencies(&value, base)?;

    Ok(Some(ManifestModel {
        manifest_path,
        schema,
        package_name,
        package_version,
        dependencies,
    }))
}

pub fn load_manifest_graph(path: Option<&Path>) -> Result<Option<ManifestGraph>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let root_manifest = load_manifest(Some(path))?.expect("manifest path was provided");
    let mut visiting = Vec::new();
    let mut visited = BTreeSet::new();
    let mut ordered = Vec::new();

    visit_manifest_depth_first(
        &root_manifest.manifest_path,
        &mut visiting,
        &mut visited,
        &mut ordered,
    )?;

    validate_manifest_package_names(&ordered)?;

    let root = ordered
        .iter()
        .find(|manifest| manifest.manifest_path == root_manifest.manifest_path)
        .cloned()
        .ok_or_else(|| "internal error: root manifest missing in graph".to_string())?;

    Ok(Some(ManifestGraph {
        root,
        packages_depth_first: ordered,
    }))
}

fn validate_manifest_package_names(manifests: &[ManifestModel]) -> Result<(), String> {
    let mut seen = HashMap::new();

    for manifest in manifests {
        if let Some(existing_path) = seen.insert(
            manifest.package_name.clone(),
            manifest.manifest_path.clone(),
        ) {
            return Err(format!(
                "E_PACKAGE_NAME_CONFLICT: duplicate package name `{}` in manifests {} and {}",
                manifest.package_name,
                existing_path.display(),
                manifest.manifest_path.display()
            ));
        }
    }

    Ok(())
}

fn visit_manifest_depth_first(
    manifest_path: &Path,
    visiting: &mut Vec<PathBuf>,
    visited: &mut BTreeSet<PathBuf>,
    ordered: &mut Vec<ManifestModel>,
) -> Result<(), String> {
    let manifest_path = fs::canonicalize(manifest_path).map_err(|err| {
        format!(
            "failed to resolve dependency manifest {}: {err}",
            manifest_path.display()
        )
    })?;

    if let Some(cycle_start) = visiting
        .iter()
        .position(|current| current == &manifest_path)
    {
        let cycle = visiting[cycle_start..]
            .iter()
            .chain(std::iter::once(&manifest_path))
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>();
        return Err(format!(
            "E_PACKAGE_CYCLE: package dependency cycle detected: {}",
            cycle.join(" -> ")
        ));
    }

    if visited.contains(&manifest_path) {
        return Ok(());
    }

    visiting.push(manifest_path.clone());
    let manifest = load_manifest(Some(&manifest_path))?.expect("manifest path was provided");
    for dependency_path in manifest.dependencies.values() {
        let dependency_manifest = resolve_dependency_manifest_path(dependency_path);
        if !dependency_manifest.is_file() {
            return Err(format!(
                "dependency manifest not found: {}",
                dependency_manifest.display()
            ));
        }
        visit_manifest_depth_first(&dependency_manifest, visiting, visited, ordered)?;
    }
    visiting.pop();

    visited.insert(manifest_path);
    ordered.push(manifest);
    Ok(())
}

fn resolve_dependency_manifest_path(path: &Path) -> PathBuf {
    if path
        .file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.eq_ignore_ascii_case("Config.toml"))
        || path
            .extension()
            .and_then(|name| name.to_str())
            .is_some_and(|ext| ext.eq_ignore_ascii_case("toml"))
    {
        path.to_path_buf()
    } else {
        path.join("Config.toml")
    }
}

fn parse_manifest_dependencies(
    manifest: &toml::Value,
    base: &Path,
) -> Result<BTreeMap<String, PathBuf>, String> {
    let Some(dependencies) = manifest.get("dependencies") else {
        return Ok(BTreeMap::new());
    };
    let dependencies = dependencies
        .as_table()
        .ok_or_else(|| "manifest [dependencies] must be a table".to_string())?;

    let mut out = BTreeMap::new();
    for (name, value) in dependencies {
        let dependency_path = value
            .as_str()
            .ok_or_else(|| {
                format!(
                    "manifest dependency `{}` must be a path string in [dependencies]",
                    name
                )
            })
            .map(|path| base.join(path))?;
        out.insert(name.clone(), dependency_path);
    }

    Ok(out)
}

fn parse_manifest_schema_paths(
    entry: &toml::value::Table,
    base: &Path,
) -> Result<Vec<PathBuf>, String> {
    let schema_value = entry
        .get("schema")
        .ok_or_else(|| "manifest missing entry.schema".to_string())?;

    let schema_patterns = match schema_value {
        toml::Value::String(pattern) => vec![pattern.clone()],
        toml::Value::Array(items) => {
            if items.is_empty() {
                return Err("manifest entry.schema array cannot be empty".to_string());
            }

            items
                .iter()
                .map(|item| {
                    item.as_str().map(str::to_string).ok_or_else(|| {
                        "manifest entry.schema array must contain only string patterns".to_string()
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        }
        _ => {
            return Err("manifest entry.schema must be a string or string array".to_string());
        }
    };

    let mut canonical_seen = BTreeSet::new();
    let mut out = Vec::new();

    for pattern in schema_patterns {
        let mut matched_for_pattern = Vec::new();
        let pattern_path = base.join(&pattern);
        let pattern_glob = pattern_path.to_string_lossy().to_string();
        let entries = glob(&pattern_glob)
            .map_err(|err| format!("invalid schema glob pattern `{pattern}`: {err}"))?;

        for entry in entries {
            let path = entry.map_err(|err| {
                format!(
                    "failed to resolve schema glob `{pattern}` entry: {}",
                    err.path().display()
                )
            })?;
            if path.is_file() {
                matched_for_pattern.push(path);
            }
        }

        matched_for_pattern.sort();

        if matched_for_pattern.is_empty() {
            return Err(format!("no schema files matched pattern `{pattern}`"));
        }

        for path in matched_for_pattern {
            let canonical = fs::canonicalize(&path)
                .map_err(|err| format!("failed to resolve schema {}: {err}", path.display()))?;
            if canonical_seen.insert(canonical) {
                out.push(path);
            }
        }
    }

    if out.is_empty() {
        return Err("manifest entry.schema resolved to empty schema list".to_string());
    }

    Ok(out)
}

pub fn resolve_schema_path(
    schema: Option<&Path>,
    manifest: Option<&ManifestModel>,
) -> Result<PathBuf, String> {
    if let Some(schema) = schema {
        return Ok(schema.to_path_buf());
    }

    if let Some(manifest) = manifest {
        return manifest
            .schema
            .first()
            .cloned()
            .ok_or_else(|| "manifest entry.schema resolved to empty schema list".to_string());
    }

    Err("--schema is required (or provide --manifest with entry.schema)".to_string())
}

fn load_schema_with_dependencies(
    graph: &ManifestGraph,
    root_schema_override: Option<&Path>,
) -> Result<(rcfg_lang::File, Vec<rcfg_lang::Diagnostic>), String> {
    let mut all_items = Vec::new();
    let mut all_diags = Vec::new();

    for manifest in &graph.packages_depth_first {
        let mut package_items = Vec::new();
        let schema_paths = if manifest.manifest_path == graph.root.manifest_path {
            root_schema_override
                .map(|path| vec![path.to_path_buf()])
                .unwrap_or_else(|| manifest.schema.clone())
        } else {
            manifest.schema.clone()
        };

        for schema_path in &schema_paths {
            let schema_text = fs::read_to_string(schema_path).map_err(|err| {
                format!(
                    "failed to read schema {} for package `{}`: {err}",
                    schema_path.display(),
                    manifest.package_name
                )
            })?;
            let (schema_file, mut diags) = parse_schema_with_diagnostics(&schema_text);
            for diagnostic in &mut diags {
                if diagnostic.path.is_none() {
                    diagnostic.path = Some(schema_path.display().to_string());
                }
            }
            package_items.extend(schema_file.items);
            all_diags.append(&mut diags);
        }

        let (mut namespaced_items, mut namespace_diags) =
            namespace_schema_items_for_package(package_items, manifest.package_name.as_str());
        for diagnostic in &mut namespace_diags {
            if diagnostic.path.is_none() {
                diagnostic.path = schema_paths.first().map(|path| path.display().to_string());
            }
        }

        all_items.append(&mut namespaced_items);
        all_diags.append(&mut namespace_diags);
    }

    Ok((rcfg_lang::File { items: all_items }, all_diags))
}

fn namespace_schema_items_for_package(
    items: Vec<rcfg_lang::Item>,
    package_name: &str,
) -> (Vec<rcfg_lang::Item>, Vec<rcfg_lang::Diagnostic>) {
    let mut first_namespaced_index = None;

    for (index, item) in items.iter().enumerate() {
        if is_ctx_module(item) {
            continue;
        }

        if first_namespaced_index.is_none() {
            first_namespaced_index = Some(index);
        }
    }

    let mut diagnostics = Vec::new();

    let namespaced_items = if let Some(legacy_module) =
        legacy_package_wrapper_module(&items, package_name)
    {
        diagnostics.push(rcfg_lang::Diagnostic::warning(
            "W_LEGACY_PACKAGE_SCHEMA_LAYOUT",
            format!(
                "package `{}` schema uses deprecated top-level `mod {}` wrapper; move inner items to file root",
                package_name, package_name
            ),
            legacy_module.span,
        ));

        let mut flattened = items
            .iter()
            .filter_map(|item| match item {
                rcfg_lang::Item::Use(use_stmt) => Some(rcfg_lang::Item::Use(use_stmt.clone())),
                _ => None,
            })
            .collect::<Vec<_>>();
        flattened.extend(legacy_module.items.clone());
        flattened
    } else {
        items
            .iter()
            .filter(|item| !is_ctx_module(item))
            .cloned()
            .collect::<Vec<_>>()
    };

    let mut out = Vec::new();
    let mut inserted_namespace = false;
    for (index, item) in items.into_iter().enumerate() {
        let is_ctx_module = is_ctx_module(&item);

        if !inserted_namespace && first_namespaced_index.is_some_and(|value| value == index) {
            out.push(make_package_namespace_module(
                package_name,
                namespaced_items.clone(),
            ));
            inserted_namespace = true;
        }

        if is_ctx_module {
            out.push(item);
        }
    }

    if !inserted_namespace && !namespaced_items.is_empty() {
        out.push(make_package_namespace_module(
            package_name,
            namespaced_items,
        ));
    }

    (out, diagnostics)
}

fn is_ctx_module(item: &rcfg_lang::Item) -> bool {
    matches!(item, rcfg_lang::Item::Mod(module) if module.name.value == "ctx")
}

fn legacy_package_wrapper_module<'a>(
    items: &'a [rcfg_lang::Item],
    package_name: &str,
) -> Option<&'a rcfg_lang::ast::ModDecl> {
    let mut candidate = None;

    for item in items {
        if matches!(item, rcfg_lang::Item::Use(_)) || is_ctx_module(item) {
            continue;
        }

        let rcfg_lang::Item::Mod(module) = item else {
            return None;
        };
        if module.name.value != package_name {
            return None;
        }
        if candidate.is_some() {
            return None;
        }
        candidate = Some(module);
    }

    candidate
}

fn make_package_namespace_module(
    package_name: &str,
    items: Vec<rcfg_lang::Item>,
) -> rcfg_lang::Item {
    rcfg_lang::Item::Mod(rcfg_lang::ast::ModDecl {
        meta: rcfg_lang::ast::ItemMeta::empty(),
        name: rcfg_lang::Spanned::new(package_name.to_string(), rcfg_lang::Span::default()),
        items,
        span: rcfg_lang::Span::default(),
    })
}

pub fn analyze_values_report(
    values: &Path,
    symbols: &rcfg_lang::SymbolTable,
    context: &HashMap<String, ResolvedValue>,
    include_root: Option<&Path>,
    strict: bool,
) -> ValuesAnalysisReport {
    match (strict, include_root) {
        (true, Some(root)) => analyze_values_from_path_report_with_context_and_root_strict(
            values, symbols, context, root,
        ),
        (false, Some(root)) => {
            analyze_values_from_path_report_with_context_and_root(values, symbols, context, root)
        }
        (true, None) => {
            analyze_values_from_path_report_with_context_strict(values, symbols, context)
        }
        (false, None) => analyze_values_from_path_report_with_context(values, symbols, context),
    }
}

pub fn resolve_with_context(
    values: &rcfg_lang::ValuesFile,
    symbols: &rcfg_lang::SymbolTable,
    context: &HashMap<String, ResolvedValue>,
) -> ResolvedConfig {
    if context.is_empty() {
        resolve_values(values, symbols)
    } else {
        resolve_values_with_context(values, symbols, context)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::{load_manifest_graph, resolve_schema_path};
    use std::path::PathBuf;

    fn fixture_root(name: &str) -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("..")
            .join("target")
            .join("tmp-tests")
            .join(name)
    }

    fn write_file(path: &PathBuf, text: &str) {
        if let Some(parent) = path.parent() {
            let _ = fs::create_dir_all(parent);
        }
        fs::write(path, text).expect("write file");
    }

    #[test]
    fn manifest_graph_loads_path_dependencies_depth_first() {
        let root = fixture_root("manifest_graph_depth_first");
        let root_manifest = root.join("app/Config.toml");
        let root_schema = root.join("app/src/schema.rcfg");

        let dep_uart_manifest = root.join("deps/hal_uart/Config.toml");
        let dep_uart_schema = root.join("deps/hal_uart/src/schema.rcfg");

        let dep_gpio_manifest = root.join("deps/hal_gpio/Config.toml");
        let dep_gpio_schema = root.join("deps/hal_gpio/src/schema.rcfg");

        write_file(&root_schema, "mod app { option enabled: bool = false; }");
        write_file(&dep_uart_schema, "mod hal_uart { option on: bool = true; }");
        write_file(&dep_gpio_schema, "mod hal_gpio { option on: bool = true; }");

        write_file(
            &dep_uart_manifest,
            r#"
[package]
name = "hal_uart"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );
        write_file(
            &dep_gpio_manifest,
            r#"
[package]
name = "hal_gpio"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );
        write_file(
            &root_manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
hal_uart = "../deps/hal_uart"
hal_gpio = "../deps/hal_gpio"
"#,
        );

        let graph = load_manifest_graph(Some(&root_manifest))
            .expect("load graph")
            .expect("manifest graph");
        let package_names = graph
            .packages_depth_first
            .iter()
            .map(|package| package.package_name.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            package_names,
            vec![
                "hal_gpio".to_string(),
                "hal_uart".to_string(),
                "app".to_string()
            ]
        );

        let schema = resolve_schema_path(None, Some(&graph.root)).expect("resolve schema");
        assert!(
            schema.ends_with("app/src/schema.rcfg"),
            "{}",
            schema.display()
        );
    }

    #[test]
    fn manifest_graph_reports_dependency_cycle() {
        let root = fixture_root("manifest_graph_cycle");

        let app_manifest = root.join("app/Config.toml");
        let app_schema = root.join("app/src/schema.rcfg");

        let dep_a_manifest = root.join("deps/dep_a/Config.toml");
        let dep_a_schema = root.join("deps/dep_a/src/schema.rcfg");

        let dep_b_manifest = root.join("deps/dep_b/Config.toml");
        let dep_b_schema = root.join("deps/dep_b/src/schema.rcfg");

        write_file(&app_schema, "mod app { option enabled: bool = false; }");
        write_file(&dep_a_schema, "mod dep_a { option enabled: bool = false; }");
        write_file(&dep_b_schema, "mod dep_b { option enabled: bool = false; }");

        write_file(
            &app_manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_a = "../deps/dep_a"
"#,
        );
        write_file(
            &dep_a_manifest,
            r#"
[package]
name = "dep_a"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_b = "../dep_b"
"#,
        );
        write_file(
            &dep_b_manifest,
            r#"
[package]
name = "dep_b"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_a = "../dep_a"
"#,
        );

        let error = load_manifest_graph(Some(&app_manifest)).expect_err("should fail with cycle");
        assert!(error.contains("E_PACKAGE_CYCLE"), "{error}");
    }

    #[test]
    fn manifest_graph_reports_duplicate_package_name_conflict() {
        let root = fixture_root("manifest_graph_duplicate_package_name");

        let app_manifest = root.join("app/Config.toml");
        let app_schema = root.join("app/src/schema.rcfg");

        let dep_a_manifest = root.join("deps/dep_a/Config.toml");
        let dep_a_schema = root.join("deps/dep_a/src/schema.rcfg");

        let dep_b_manifest = root.join("deps/dep_b/Config.toml");
        let dep_b_schema = root.join("deps/dep_b/src/schema.rcfg");

        write_file(&app_schema, "mod app { option enabled: bool = false; }");
        write_file(&dep_a_schema, "mod dep_a { option enabled: bool = false; }");
        write_file(&dep_b_schema, "mod dep_b { option enabled: bool = false; }");

        write_file(
            &app_manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_a = "../deps/dep_a"
dep_b = "../deps/dep_b"
"#,
        );
        write_file(
            &dep_a_manifest,
            r#"
[package]
name = "hal_common"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );
        write_file(
            &dep_b_manifest,
            r#"
[package]
name = "hal_common"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );

        let error = load_manifest_graph(Some(&app_manifest))
            .expect_err("should fail with duplicate package name");
        assert!(error.contains("E_PACKAGE_NAME_CONFLICT"), "{error}");
        assert!(error.contains("hal_common"), "{error}");
    }

    #[test]
    fn load_session_analyze_and_resolve_are_consistent() {
        let root = fixture_root("app_session_consistency");
        let schema = root.join("schema.rcfg");
        let values = root.join("profile.rcfgv");

        write_file(
            &schema,
            r#"
/// App module.
mod app {
  /// Enable app.
  option enabled: bool = false;

  when enabled {
    /// Baud rate.
    option baud: u32 = 115200;
  }
}
"#,
        );
        write_file(
            &values,
            r#"
app::enabled = true;
app::baud = 9600;
"#,
        );

        let session = super::load_session(&super::AppLoadOptions {
            schema: Some(schema.clone()),
            manifest: None,
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session");

        assert!(!session.symbols().is_empty(), "symbols should be loaded");

        let path_report = session.analyze_values_from_path(&values);
        assert!(
            !path_report
                .diagnostics
                .iter()
                .any(|diag| diag.severity == rcfg_lang::Severity::Error),
            "path analysis should have no errors: {:?}",
            path_report.diagnostics
        );

        let memory_diags = session.analyze_values(&path_report.values);
        assert!(
            !memory_diags
                .iter()
                .any(|diag| diag.severity == rcfg_lang::Severity::Error),
            "memory analysis should have no errors: {:?}",
            memory_diags
        );

        let resolved_from_path = path_report.resolved;
        let resolved_from_memory = session.resolve(&path_report.values);
        assert_eq!(resolved_from_path, resolved_from_memory);
    }

    #[test]
    fn analyze_values_expands_includes_with_include_root() {
        let root = fixture_root("app_session_analyze_values_include_root");
        let manifest = root.join("Config.toml");
        let schema = root.join("schema.rcfg");
        let include = root.join("profiles/defaults.rcfgv");
        let values = root.join("profile.rcfgv");

        let _ = std::fs::remove_file(&include);
        let _ = std::fs::remove_file(&values);

        write_file(
            &manifest,
            r#"
[package]
name = "demo"
version = "0.1.0"

[entry]
schema = "schema.rcfg"
"#,
        );
        write_file(
            &schema,
            r#"
mod app {
  option enabled: bool = false;
}
"#,
        );
        write_file(&include, "app::enabled = true;\n");
        write_file(&values, "include \"@root/profiles/defaults.rcfgv\";\n");

        let session = super::load_session(&super::AppLoadOptions {
            schema: None,
            manifest: Some(manifest),
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session");

        let path_report = session.analyze_values_from_path(&values);
        assert!(
            path_report
                .diagnostics
                .iter()
                .all(|diag| diag.code != "E_INCLUDE_NOT_FOUND"),
            "path analysis should resolve include via include_root: {:?}",
            path_report.diagnostics
        );

        std::fs::remove_file(&include).expect("remove include file after path expansion");
        let _ = std::fs::remove_file(&values);

        let include_stmt = rcfg_lang::ast::IncludeStmt {
            path: rcfg_lang::Spanned::new(
                "@root/profiles/defaults.rcfgv".to_string(),
                rcfg_lang::Span::default(),
            ),
            span: rcfg_lang::Span::default(),
        };
        let include_only_values = rcfg_lang::ValuesFile {
            stmts: vec![rcfg_lang::ast::ValuesStmt::Include(include_stmt)],
        };

        let memory_diags = session.analyze_values(&include_only_values);
        assert!(
            memory_diags
                .iter()
                .any(|diag| diag.code == "E_INCLUDE_NOT_FOUND"),
            "memory analysis should detect include in ValuesFile with include_root: {memory_diags:?}"
        );

        let resolved = path_report.resolved;
        let enabled = resolved
            .options
            .iter()
            .find(|option| option.path.ends_with("::app::enabled"))
            .expect("namespaced app::enabled option");
        assert_eq!(enabled.value, Some(rcfg_lang::ResolvedValue::Bool(true)));
    }

    #[test]
    fn manifest_schema_without_legacy_wrapper_uses_single_package_namespace() {
        let root = fixture_root("manifest_schema_without_legacy_wrapper");

        let app_manifest = root.join("app/Config.toml");
        let app_schema = root.join("app/src/schema.rcfg");

        let foo_manifest = root.join("deps/foo/Config.toml");
        let foo_schema = root.join("deps/foo/src/schema.rcfg");

        write_file(
            &app_schema,
            r#"
use foo as foo_cfg;

option enabled: bool = false;

when enabled {
  option baud: u32 = 115200;
}

patch app {
  default baud = 9600;
}
"#,
        );
        write_file(
            &foo_schema,
            r#"
option enabled: bool = true;
"#,
        );

        write_file(
            &app_manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
foo = "../deps/foo"
"#,
        );
        write_file(
            &foo_manifest,
            r#"
[package]
name = "foo"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );

        let session = super::load_session(&super::AppLoadOptions {
            schema: None,
            manifest: Some(app_manifest),
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session with dependencies");

        assert!(
            session
                .parse_diagnostics()
                .iter()
                .all(|diag| diag.code != "W_LEGACY_PACKAGE_SCHEMA_LAYOUT"),
            "new schema layout should not emit legacy warning: {:?}",
            session.parse_diagnostics()
        );

        assert!(session.symbols().option_type("foo::enabled").is_some());
        assert!(session.symbols().option_type("foo::foo::enabled").is_none());
    }

    #[test]
    fn manifest_mode_with_explicit_schema_still_loads_dependencies() {
        let root = fixture_root("manifest_with_explicit_schema_loads_dependencies");

        let app_manifest = root.join("app/Config.toml");
        let app_schema = root.join("app/src/schema.rcfg");

        let dep_manifest = root.join("deps/dep/Config.toml");
        let dep_schema = root.join("deps/dep/src/schema.rcfg");

        write_file(
            &app_schema,
            r#"
use dep as dep_cfg;

option enabled: bool = true;

#[msg("app.dep.enabled")]
require!(dep_cfg::enabled);
"#,
        );
        write_file(
            &dep_schema,
            r#"
option enabled: bool = true;
"#,
        );

        write_file(
            &app_manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep = "../deps/dep"
"#,
        );
        write_file(
            &dep_manifest,
            r#"
[package]
name = "dep"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );

        let session = super::load_session(&super::AppLoadOptions {
            schema: Some(app_schema),
            manifest: Some(app_manifest),
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session with explicit schema and manifest");

        assert!(session.symbols().option_type("dep::enabled").is_some());
        assert!(
            !session
                .parse_diagnostics()
                .iter()
                .any(|diag| diag.code == "E_SYMBOL_NOT_FOUND"),
            "should not emit unresolved symbol diagnostics: {:?}",
            session.parse_diagnostics()
        );
    }

    #[test]
    fn manifest_schema_with_legacy_wrapper_emits_warning_and_still_builds_symbols() {
        let root = fixture_root("manifest_schema_with_legacy_wrapper");

        let manifest = root.join("Config.toml");
        let schema = root.join("src/schema.rcfg");

        write_file(
            &schema,
            r#"
mod app {
  option enabled: bool = false;
}
"#,
        );
        write_file(
            &manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
        );

        let session = super::load_session(&super::AppLoadOptions {
            schema: None,
            manifest: Some(manifest),
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session");

        assert!(
            session.parse_diagnostics().iter().any(|diag| {
                diag.code == "W_LEGACY_PACKAGE_SCHEMA_LAYOUT"
                    && diag.severity == rcfg_lang::Severity::Warning
            }),
            "legacy wrapper should emit warning: {:?}",
            session.parse_diagnostics()
        );

        assert!(
            !session.symbols().is_empty(),
            "warnings should not trigger early return with empty symbols"
        );
        assert!(session.symbols().option_type("app::enabled").is_some());
        assert!(session.symbols().option_type("app::app::enabled").is_none());
    }

    #[test]
    fn manifest_schema_supports_multiple_files() {
        let root = fixture_root("manifest_schema_multiple_files");
        let manifest = root.join("Config.toml");
        let schema_a = root.join("src/a.rcfg");
        let schema_b = root.join("src/b.rcfg");

        write_file(&schema_a, "option enabled: bool = false;\n");
        write_file(
            &schema_b,
            r#"
when enabled {
  option baud: u32 = 9600;
}
"#,
        );
        write_file(
            &manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = ["src/a.rcfg", "src/b.rcfg"]
"#,
        );

        let graph = super::load_manifest_graph(Some(&manifest))
            .expect("load manifest graph")
            .expect("manifest graph should exist");

        assert_eq!(graph.root.schema.len(), 2);
        assert!(
            graph.root.schema[0].ends_with("src/a.rcfg")
                && graph.root.schema[1].ends_with("src/b.rcfg"),
            "entry.schema list should preserve declaration order: {:?}",
            graph.root.schema
        );

        let session = super::load_session(&super::AppLoadOptions {
            schema: None,
            manifest: Some(manifest),
            context: None,
            i18n: None,
            strict: false,
        })
        .expect("load app session");

        assert!(session.symbols().option_type("app::enabled").is_some());
        assert!(session.symbols().option_type("app::baud").is_some());
    }

    #[test]
    fn manifest_schema_glob_is_sorted_and_deduplicated() {
        let root = fixture_root("manifest_schema_glob_sorted");
        let manifest = root.join("Config.toml");
        let schema_a = root.join("src/a.rcfg");
        let schema_b = root.join("src/b.rcfg");

        write_file(&schema_a, "option from_a: bool = true;\n");
        write_file(&schema_b, "option from_b: bool = true;\n");
        write_file(
            &manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = ["src/*.rcfg", "src/a.rcfg"]
"#,
        );

        let loaded = super::load_manifest(Some(&manifest))
            .expect("load manifest")
            .expect("manifest should exist");

        assert_eq!(loaded.schema.len(), 2);
        assert!(
            loaded.schema[0].ends_with("src/a.rcfg") && loaded.schema[1].ends_with("src/b.rcfg"),
            "glob matches should be sorted and duplicated file removed: {:?}",
            loaded.schema
        );
    }

    #[test]
    fn manifest_schema_glob_with_zero_match_returns_error() {
        let root = fixture_root("manifest_schema_glob_zero_match");
        let manifest = root.join("Config.toml");

        write_file(
            &manifest,
            r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/*.rcfg"
"#,
        );

        let error = super::load_manifest(Some(&manifest)).expect_err("should fail");
        assert!(
            error.contains("no schema files matched pattern `src/*.rcfg`"),
            "{error}"
        );
    }
}
