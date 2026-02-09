use std::collections::HashMap;
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

#[derive(Debug)]
pub(crate) struct ManifestModel {
    pub(crate) schema: PathBuf,
    pub(crate) package_name: Option<String>,
}

pub(crate) fn load_manifest(path: Option<&Path>) -> Result<Option<ManifestModel>, String> {
    let Some(path) = path else {
        return Ok(None);
    };

    let text = fs::read_to_string(path)
        .map_err(|err| format!("failed to read manifest {}: {err}", path.display()))?;
    let value = text
        .parse::<toml::Value>()
        .map_err(|err| format!("failed to parse manifest {}: {err}", path.display()))?;

    let package_name = value
        .get("package")
        .and_then(toml::Value::as_table)
        .and_then(|package| package.get("name"))
        .and_then(toml::Value::as_str)
        .map(str::to_string);

    let entry = value
        .get("entry")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| "manifest missing [entry] table".to_string())?;
    let schema = entry
        .get("schema")
        .and_then(toml::Value::as_str)
        .ok_or_else(|| "manifest missing entry.schema".to_string())?;

    let base = path.parent().unwrap_or_else(|| Path::new("."));
    Ok(Some(ManifestModel {
        schema: base.join(schema),
        package_name,
    }))
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
