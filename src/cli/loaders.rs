use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use rcfg_lang::ResolvedValue;

#[derive(Debug)]
pub(crate) struct I18nCatalog {
    pub(crate) strings: HashMap<String, String>,
}

pub(crate) fn load_context(path: Option<&Path>) -> Result<HashMap<String, ResolvedValue>, String> {
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

pub(crate) fn load_i18n_catalog(path: Option<&Path>) -> Result<Option<I18nCatalog>, String> {
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

#[derive(Debug, Clone)]
pub(crate) struct ManifestModel {
    pub(crate) manifest_path: PathBuf,
    pub(crate) schema: PathBuf,
    pub(crate) package_name: String,
    pub(crate) package_version: String,
    pub(crate) dependencies: BTreeMap<String, PathBuf>,
}

#[derive(Debug, Clone)]
pub(crate) struct ManifestGraph {
    pub(crate) root: ManifestModel,
    pub(crate) packages_depth_first: Vec<ManifestModel>,
}

pub(crate) fn load_manifest(path: Option<&Path>) -> Result<Option<ManifestModel>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let manifest_path = fs::canonicalize(path)
        .map_err(|err| format!("failed to resolve manifest {}: {err}", path.display()))?;

    let text = fs::read_to_string(&manifest_path)
        .map_err(|err| format!("failed to read manifest {}: {err}", manifest_path.display()))?;
    let value = text
        .parse::<toml::Value>()
        .map_err(|err| format!("failed to parse manifest {}: {err}", manifest_path.display()))?;

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
    let schema = entry
        .get("schema")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| "manifest missing entry.schema".to_string())?;

    let base = manifest_path.parent().unwrap_or_else(|| Path::new("."));
    let schema_path = base.join(schema);
    if !schema_path.is_file() {
        return Err(format!(
            "manifest entry schema does not exist: {}",
            schema_path.display()
        ));
    }

    let dependencies = parse_manifest_dependencies(&value, base)?;

    Ok(Some(ManifestModel {
        manifest_path,
        schema: schema_path,
        package_name,
        package_version,
        dependencies,
    }))
}

pub(crate) fn load_manifest_graph(path: Option<&Path>) -> Result<Option<ManifestGraph>, String> {
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

    if let Some(cycle_start) = visiting.iter().position(|current| current == &manifest_path) {
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

pub(crate) fn resolve_schema_path(
    schema: Option<&Path>,
    manifest: Option<&ManifestModel>,
) -> Result<PathBuf, String> {
    if let Some(schema) = schema {
        return Ok(schema.to_path_buf());
    }

    if let Some(manifest) = manifest {
        return Ok(manifest.schema.clone());
    }

    Err("--schema is required (or provide --manifest with entry.schema)".to_string())
}
