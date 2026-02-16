use super::diagnostics::test_harness_diagnostic;
use std::collections::HashSet;

pub(crate) fn collect_test_paths(
    dirs: &[String],
    tags: &[String],
    filters: &[String],
) -> Result<Vec<String>, birddisk_core::Diagnostic> {
    let roots = if dirs.is_empty() {
        default_test_dirs()
    } else {
        dirs.to_vec()
    };

    if roots.is_empty() {
        return Err(test_harness_diagnostic(
            "No default test directories found (expected examples/ or tests/)",
        ));
    }

    let mut paths = Vec::new();
    for dir in roots {
        let root = std::path::Path::new(&dir);
        if !root.exists() {
            return Err(test_harness_diagnostic(format!(
                "Test directory not found: {dir}"
            )));
        }
        collect_bd_files(root, &mut paths).map_err(test_harness_diagnostic)?;
    }

    let mut paths: Vec<String> = paths
        .into_iter()
        .filter(|path| matches_tags(path, tags))
        .filter(|path| matches_filters(path, filters))
        .collect();
    paths.sort();
    if paths.is_empty() {
        let message = if tags.is_empty() && filters.is_empty() {
            "No .bd files found in test directories".to_string()
        } else {
            let mut parts = Vec::new();
            if !tags.is_empty() {
                parts.push(format!("tags: {}", tags.join(", ")));
            }
            if !filters.is_empty() {
                parts.push(format!("filters: {}", filters.join(", ")));
            }
            format!("No .bd files matched {}", parts.join("; "))
        };
        return Err(test_harness_diagnostic(message));
    }
    Ok(paths)
}

pub(crate) fn default_test_dirs() -> Vec<String> {
    let mut roots = Vec::new();
    for candidate in ["examples", "tests"] {
        if std::path::Path::new(candidate).exists() {
            roots.push(candidate.to_string());
        }
    }
    roots
}

fn collect_bd_files(dir: &std::path::Path, paths: &mut Vec<String>) -> Result<(), String> {
    let entries = std::fs::read_dir(dir).map_err(|err| err.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            collect_bd_files(&path, paths)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("bd") {
            paths.push(path.to_string_lossy().to_string());
        }
    }
    Ok(())
}

pub(crate) fn companion_path(path: &str, extension: &str) -> String {
    let mut output = std::path::PathBuf::from(path);
    output.set_extension(extension);
    output.to_string_lossy().to_string()
}

fn read_optional_file(path: &str) -> Result<Option<String>, String> {
    if std::path::Path::new(path).exists() {
        std::fs::read_to_string(path)
            .map(Some)
            .map_err(|err| err.to_string())
    } else {
        Ok(None)
    }
}

pub(crate) fn read_test_input(path: &str) -> Result<String, String> {
    let stdin_path = companion_path(path, "stdin");
    Ok(read_optional_file(&stdin_path)?.unwrap_or_default())
}

pub(crate) fn read_test_args(path: &str) -> Result<Vec<String>, String> {
    let args_path = companion_path(path, "args");
    let Some(contents) = read_optional_file(&args_path)? else {
        return Ok(Vec::new());
    };
    Ok(contents
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| line.to_string())
        .collect())
}

pub(crate) fn read_expected_output(path: &str) -> Result<Option<String>, String> {
    let stdout_path = companion_path(path, "stdout");
    read_optional_file(&stdout_path)
}

pub(crate) fn read_expected_error(path: &str) -> Result<Option<Vec<String>>, String> {
    let error_path = companion_path(path, "error");
    match read_optional_file(&error_path)? {
        Some(contents) => Ok(Some(parse_expected_error(&contents)?)),
        None => Ok(None),
    }
}

fn parse_expected_error(contents: &str) -> Result<Vec<String>, String> {
    let mut codes = Vec::new();
    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        for token in trimmed.split_whitespace() {
            if token.starts_with('#') {
                break;
            }
            codes.push(token.to_string());
        }
    }
    if codes.is_empty() {
        Err("expected error file did not contain any codes".to_string())
    } else {
        Ok(codes)
    }
}

fn matches_tags(path: &str, tags: &[String]) -> bool {
    if tags.is_empty() {
        return true;
    }
    let tokens = tag_tokens(std::path::Path::new(path));
    tags.iter().all(|tag| tokens.contains(&tag.to_lowercase()))
}

fn matches_filters(path: &str, filters: &[String]) -> bool {
    if filters.is_empty() {
        return true;
    }
    let lowered = path.to_lowercase();
    filters
        .iter()
        .all(|filter| lowered.contains(&filter.to_lowercase()))
}

fn tag_tokens(path: &std::path::Path) -> HashSet<String> {
    let mut tokens = HashSet::new();
    for component in path.components() {
        if let std::path::Component::Normal(name) = component {
            if let Some(name) = name.to_str() {
                tokens.insert(name.to_lowercase());
            }
        }
    }
    if let Some(stem) = path.file_stem().and_then(|name| name.to_str()) {
        for token in stem.split(|ch: char| !ch.is_ascii_alphanumeric()) {
            if !token.is_empty() {
                tokens.insert(token.to_lowercase());
            }
        }
    }
    tokens
}
