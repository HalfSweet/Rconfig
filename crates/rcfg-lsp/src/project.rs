use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct ManifestModel {
    pub manifest_path: PathBuf,
    pub schema: PathBuf,
    pub package_name: String,
    pub dependencies: BTreeMap<String, PathBuf>,
}

#[derive(Debug, Clone)]
pub struct ManifestGraph {
    pub root: ManifestModel,
    pub packages_depth_first: Vec<ManifestModel>,
}

#[derive(Debug, Clone)]
pub enum ValuesSchemaResolution {
    Manifest { manifest: PathBuf, schemas: Vec<PathBuf> },
    NearbySchema { schema: PathBuf },
    OpenSchemaFallback { schemas: Vec<PathBuf> },
    ParseOnly,
}

pub fn find_nearest_manifest(start: &Path) -> Option<PathBuf> {
    for candidate in start.ancestors() {
        let manifest = candidate.join("Config.toml");
        if manifest.is_file() {
            return fs::canonicalize(&manifest).ok().or(Some(manifest));
        }
    }
    None
}

pub fn load_manifest_graph(manifest_path: &Path) -> Result<ManifestGraph, String> {
    let root_manifest = load_manifest(manifest_path)?;

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

    Ok(ManifestGraph {
        root,
        packages_depth_first: ordered,
    })
}

pub fn manifest_schema_paths(graph: &ManifestGraph) -> Vec<PathBuf> {
    graph
        .packages_depth_first
        .iter()
        .map(|manifest| manifest.schema.clone())
        .collect::<Vec<_>>()
}

pub fn resolve_values_schema(
    values_path: &Path,
    open_schema_paths: &[PathBuf],
) -> ValuesSchemaResolution {
    if let Some(manifest) = find_nearest_manifest(values_path)
        && let Ok(graph) = load_manifest_graph(&manifest)
    {
        return ValuesSchemaResolution::Manifest {
            manifest,
            schemas: manifest_schema_paths(&graph),
        };
    }

    if let Some(schema) = find_nearby_schema(values_path) {
        return ValuesSchemaResolution::NearbySchema { schema };
    }

    if !open_schema_paths.is_empty() {
        let mut schemas = open_schema_paths.to_vec();
        schemas.sort();
        schemas.dedup();
        return ValuesSchemaResolution::OpenSchemaFallback { schemas };
    }

    ValuesSchemaResolution::ParseOnly
}

fn find_nearby_schema(values_path: &Path) -> Option<PathBuf> {
    let mut candidates = Vec::new();

    for dir in values_path.ancestors() {
        let Ok(entries) = fs::read_dir(dir) else {
            continue;
        };

        for entry in entries.flatten() {
            let path = entry.path();
            if path
                .extension()
                .and_then(|ext| ext.to_str())
                .is_some_and(|ext| ext == "rcfg")
            {
                candidates.push(path);
            }
        }

        if !candidates.is_empty() {
            break;
        }
    }

    if candidates.is_empty() {
        return None;
    }

    candidates.sort();

    if let Some(preferred) = candidates.iter().find(|path| {
        path.file_name()
            .and_then(|name| name.to_str())
            .is_some_and(|name| name == "schema.rcfg" || name == "main.rcfg")
    }) {
        return Some(preferred.clone());
    }

    candidates.into_iter().next()
}

fn load_manifest(manifest_path: &Path) -> Result<ManifestModel, String> {
    let manifest_path = fs::canonicalize(manifest_path)
        .map_err(|err| format!("failed to resolve manifest {}: {err}", manifest_path.display()))?;

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

    Ok(ManifestModel {
        manifest_path,
        schema: schema_path,
        package_name,
        dependencies,
    })
}

fn visit_manifest_depth_first(
    manifest_path: &Path,
    visiting: &mut Vec<PathBuf>,
    visited: &mut BTreeSet<PathBuf>,
    ordered: &mut Vec<ManifestModel>,
) -> Result<(), String> {
    let canonical = fs::canonicalize(manifest_path).map_err(|err| {
        format!(
            "failed to resolve dependency manifest {}: {err}",
            manifest_path.display()
        )
    })?;

    if let Some(cycle_start) = visiting.iter().position(|current| current == &canonical) {
        let cycle = visiting[cycle_start..]
            .iter()
            .chain(std::iter::once(&canonical))
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>();
        return Err(format!(
            "E_PACKAGE_CYCLE: package dependency cycle detected: {}",
            cycle.join(" -> ")
        ));
    }

    if visited.contains(&canonical) {
        return Ok(());
    }

    visiting.push(canonical.clone());
    let manifest = load_manifest(&canonical)?;

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
    visited.insert(canonical);
    ordered.push(manifest);
    Ok(())
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
