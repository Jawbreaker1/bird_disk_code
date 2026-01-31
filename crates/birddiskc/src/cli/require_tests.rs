use super::diagnostics::{require_tests_config_diagnostic, require_tests_diagnostic};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

pub(crate) fn enforce_require_tests(
    entry: &str,
    config: &birddisk_core::ModuleConfig,
    exclude: &[String],
) -> Vec<birddisk_core::Diagnostic> {
    let program = match birddisk_core::parse_and_typecheck_with_config(entry, config) {
        Ok(program) => program,
        Err(diags) => return diags,
    };
    let root = config
        .project_root
        .clone()
        .or_else(|| Path::new(entry).parent().map(|p| p.to_path_buf()))
        .unwrap_or_else(|| PathBuf::from("."));
    let root = root.canonicalize().unwrap_or(root);
    let stdlib_root = root.join("stdlib");
    let exclude_rules = resolve_excludes(exclude, &root);

    let mut files = HashSet::new();
    files.insert(PathBuf::from(entry));
    for func in &program.functions {
        files.insert(PathBuf::from(&func.file));
    }
    for book in &program.books {
        for method in &book.methods {
            files.insert(PathBuf::from(&method.file));
        }
    }

    let mut diagnostics = Vec::new();
    for file in files {
        let file = file.canonicalize().unwrap_or(file);
        if is_excluded(&file, &stdlib_root, &exclude_rules) {
            continue;
        }
        let expected = expected_test_path(&file, &root);
        if !expected.exists() {
            diagnostics.push(require_tests_diagnostic(
                &file.to_string_lossy(),
                &expected.to_string_lossy(),
            ));
        }
    }
    diagnostics
}

pub(crate) fn enforce_require_tests_from_cwd() -> Vec<birddisk_core::Diagnostic> {
    let cwd = match std::env::current_dir() {
        Ok(cwd) => cwd,
        Err(_) => return vec![require_tests_config_diagnostic("Unable to read current directory.")],
    };
    let manifest_path = cwd.join("birddisk.json");
    if manifest_path.exists() {
        return match super::manifest::resolve_project_context(None) {
            Ok(context) => enforce_require_tests(
                &context.entry,
                &context.config,
                &context.test_exclude,
            ),
            Err(err) => vec![require_tests_config_diagnostic(err)],
        };
    }
    let entry = cwd.join("src").join("main.bd");
    if entry.exists() {
        let mut config = birddisk_core::ModuleConfig::default();
        config.project_root = Some(cwd);
        return enforce_require_tests(entry.to_string_lossy().as_ref(), &config, &[]);
    }
    let entry = cwd.join("main.bd");
    if entry.exists() {
        let mut config = birddisk_core::ModuleConfig::default();
        config.project_root = Some(cwd);
        return enforce_require_tests(entry.to_string_lossy().as_ref(), &config, &[]);
    }
    vec![require_tests_config_diagnostic(
        "Require-tests needs birddisk.json or src/main.bd.",
    )]
}

fn is_excluded(path: &Path, stdlib_root: &Path, exclude: &[ExcludeRule]) -> bool {
    if path.starts_with(stdlib_root) {
        return true;
    }
    for rule in exclude {
        if rule.is_dir {
            if path.starts_with(&rule.path) {
                return true;
            }
        } else if path == rule.path {
            return true;
        }
    }
    for component in path.components() {
        if let std::path::Component::Normal(name) = component {
            if let Some(name) = name.to_str() {
                if matches!(
                    name,
                    "tests"
                        | "examples"
                        | "stdlib"
                        | "eval"
                        | "vm_tests"
                        | "vm_error_tests"
                        | "testlib"
                        | "target"
                ) {
                    return true;
                }
            }
        }
    }
    false
}

fn expected_test_path(source: &Path, root: &Path) -> PathBuf {
    let relative = source.strip_prefix(root).unwrap_or(source);
    let mut out = root.join("tests").join(relative);
    let stem = out
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("module");
    out.set_file_name(format!("{stem}_test.bd"));
    out
}

#[derive(Clone)]
struct ExcludeRule {
    path: PathBuf,
    is_dir: bool,
}

fn resolve_excludes(exclude: &[String], root: &Path) -> Vec<ExcludeRule> {
    let mut rules = Vec::new();
    for item in exclude {
        let raw = PathBuf::from(item);
        let mut full = if raw.is_absolute() {
            raw
        } else {
            root.join(raw)
        };
        let mut is_dir = item.ends_with('/') || item.ends_with(std::path::MAIN_SEPARATOR);
        if full.exists() {
            is_dir = full.is_dir();
        }
        full = full.canonicalize().unwrap_or(full);
        rules.push(ExcludeRule { path: full, is_dir });
    }
    rules
}

#[cfg(test)]
mod tests {
    use super::enforce_require_tests;
    use birddisk_core::ModuleConfig;
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    fn setup_project(name: &str, source: &str) -> (PathBuf, String, ModuleConfig) {
        let mut root = env::temp_dir();
        root.push(format!("birddisk_require_tests_{name}_{}", std::process::id()));
        let src_dir = root.join("src");
        let tests_dir = root.join("tests").join("src");
        fs::create_dir_all(&src_dir).expect("create src");
        fs::create_dir_all(&tests_dir).expect("create tests");
        let entry = src_dir.join("main.bd");
        fs::write(&entry, source).expect("write entry");
        let mut config = ModuleConfig::default();
        config.project_root = Some(root.clone());
        (root, entry.to_string_lossy().to_string(), config)
    }

    #[test]
    fn require_tests_accepts_expected_file() {
        let (root, entry, config) = setup_project(
            "ok",
            "rule main() -> i64:\n  yield 0.\nend\n",
        );
        let test_path = root.join("tests").join("src").join("main_test.bd");
        fs::write(&test_path, "rule test_main() -> void:\nend\n").expect("write test");
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_reports_missing_file() {
        let (root, entry, config) = setup_project(
            "missing",
            "rule main() -> i64:\n  yield 0.\nend\n",
        );
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.iter().any(|diag| diag.code == "L2000"));
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_respects_exclude_list() {
        let (root, entry, config) = setup_project(
            "exclude",
            "rule main() -> i64:\n  yield 0.\nend\n",
        );
        let diagnostics = enforce_require_tests(&entry, &config, &["src/main.bd".to_string()]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }
}
