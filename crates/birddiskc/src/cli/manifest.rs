use serde::Deserialize;
use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};

const MANIFEST_FILE: &str = "birddisk.json";

#[allow(dead_code)]
#[derive(Debug, Deserialize)]
pub(crate) struct Manifest {
    name: Option<String>,
    version: Option<String>,
    entry: Option<String>,
    deps: Option<HashMap<String, ManifestDep>>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum ManifestDep {
    Path(String),
    Detailed {
        path: String,
        #[allow(dead_code)]
        version: Option<String>,
    },
}

pub(crate) struct ProjectContext {
    pub(crate) entry: String,
    pub(crate) config: birddisk_core::ModuleConfig,
}

fn find_manifest(start: &Path) -> Option<PathBuf> {
    let mut current = if start.is_dir() { Some(start) } else { start.parent() };
    while let Some(dir) = current {
        let candidate = dir.join(MANIFEST_FILE);
        if candidate.is_file() {
            return Some(candidate);
        }
        current = dir.parent();
    }
    None
}

fn load_manifest(path: &Path) -> Result<Manifest, String> {
    let contents = std::fs::read_to_string(path)
        .map_err(|err| format!("unable to read manifest {}: {err}", path.display()))?;
    serde_json::from_str(&contents)
        .map_err(|err| format!("invalid manifest {}: {err}", path.display()))
}

fn manifest_entry_path(root: &Path, manifest: &Manifest) -> Result<PathBuf, String> {
    let entry = manifest.entry.as_deref().unwrap_or("src/main.bd");
    let entry_path = if Path::new(entry).is_absolute() {
        PathBuf::from(entry)
    } else {
        root.join(entry)
    };
    if entry_path.exists() {
        Ok(entry_path)
    } else {
        Err(format!("manifest entry not found: {}", entry_path.display()))
    }
}

fn module_config_from_manifest(
    root: &Path,
    manifest: &Manifest,
) -> Result<birddisk_core::ModuleConfig, String> {
    let mut config = birddisk_core::ModuleConfig::default();
    config.project_root = Some(root.to_path_buf());
    if let Some(deps) = manifest.deps.as_ref() {
        for (name, dep) in deps {
            if name == "std" {
                return Err("manifest deps cannot be named 'std'".to_string());
            }
            let dep_path = PathBuf::from(dep_path(dep));
            let mut full = if dep_path.is_absolute() {
                dep_path
            } else {
                root.join(dep_path)
            };
            if full.exists() {
                if !full.is_dir() {
                    return Err(format!(
                        "dependency '{name}' must be a directory (got {})",
                        full.display()
                    ));
                }
                if let Ok(canonical) = full.canonicalize() {
                    full = canonical;
                }
                config.dep_roots.insert(name.clone(), full);
            } else {
                return Err(format!(
                    "dependency '{name}' path not found: {}",
                    full.display()
                ));
            }
        }
    }
    Ok(config)
}

fn dep_path(dep: &ManifestDep) -> &str {
    match dep {
        ManifestDep::Path(path) => path.as_str(),
        ManifestDep::Detailed { path, .. } => path.as_str(),
    }
}

pub(crate) fn resolve_project_context(path: Option<&str>) -> Result<ProjectContext, String> {
    let cwd = env::current_dir().map_err(|err| format!("unable to read cwd: {err}"))?;
    if let Some(path) = path {
        let path_buf = PathBuf::from(path);
        let start = if path_buf.is_dir() {
            path_buf.clone()
        } else {
            path_buf.parent().unwrap_or(&cwd).to_path_buf()
        };
        if let Some(manifest_path) = find_manifest(&start) {
            let manifest = load_manifest(&manifest_path)?;
            let root = manifest_path
                .parent()
                .ok_or_else(|| "manifest path missing parent directory".to_string())?;
            let entry = if path_buf.is_dir() {
                manifest_entry_path(root, &manifest)?
            } else {
                path_buf
            };
            let config = module_config_from_manifest(root, &manifest)?;
            return Ok(ProjectContext {
                entry: entry.to_string_lossy().to_string(),
                config,
            });
        }
        return Ok(ProjectContext {
            entry: path.to_string(),
            config: birddisk_core::ModuleConfig::default(),
        });
    }

    let manifest_path = find_manifest(&cwd)
        .ok_or_else(|| "missing path and no birddisk.json found".to_string())?;
    let manifest = load_manifest(&manifest_path)?;
    let root = manifest_path
        .parent()
        .ok_or_else(|| "manifest path missing parent directory".to_string())?;
    let entry = manifest_entry_path(root, &manifest)?;
    let config = module_config_from_manifest(root, &manifest)?;
    Ok(ProjectContext {
        entry: entry.to_string_lossy().to_string(),
        config,
    })
}

#[cfg(test)]
pub(crate) fn manifest_filename() -> &'static str {
    MANIFEST_FILE
}
