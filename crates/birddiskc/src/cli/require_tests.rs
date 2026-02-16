use super::diagnostics::{
    require_tests_config_diagnostic, require_tests_diagnostic, require_tests_rule_diagnostic,
};
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
    let rule_targets = collect_rule_targets(&program);
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
    for target in &rule_targets {
        files.insert(target.file.clone());
    }

    let mut diagnostics = Vec::new();
    for file in files {
        let file = file.canonicalize().unwrap_or(file);
        if is_excluded(&file, &stdlib_root, &exclude_rules) {
            continue;
        }
        let required: Vec<&RuleTarget> = rule_targets
            .iter()
            .filter(|target| {
                target
                    .file
                    .canonicalize()
                    .unwrap_or_else(|_| target.file.clone())
                    == file
            })
            .collect();
        if required.is_empty() {
            continue;
        }
        let expected = expected_test_path(&file, &root);
        if !expected.exists() {
            diagnostics.push(require_tests_diagnostic(
                &file.to_string_lossy(),
                &expected.to_string_lossy(),
            ));
            continue;
        }
        let test_rules = match parse_test_rules(&expected) {
            Ok(names) => names,
            Err(diags) => {
                diagnostics.extend(diags);
                continue;
            }
        };
        for target in required {
            if !test_rules.contains(&target.test_rule) {
                diagnostics.push(require_tests_rule_diagnostic(
                    &file.to_string_lossy(),
                    &target.rule_name,
                    &target.test_rule,
                ));
            }
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
struct RuleTarget {
    file: PathBuf,
    rule_name: String,
    test_rule: String,
}

fn collect_rule_targets(program: &birddisk_core::ast::Program) -> Vec<RuleTarget> {
    let mut targets = Vec::new();
    for func in &program.functions {
        let base = base_rule_name(&func.name);
        if is_exempt_rule(base) || base.starts_with("test_") {
            continue;
        }
        targets.push(RuleTarget {
            file: PathBuf::from(&func.file),
            rule_name: base.to_string(),
            test_rule: format!("test_{base}"),
        });
    }
    for book in &program.books {
        for method in &book.methods {
            if is_exempt_rule(&method.name) {
                continue;
            }
            if method.name.starts_with("test_") {
                continue;
            }
            targets.push(RuleTarget {
                file: PathBuf::from(&method.file),
                rule_name: format!("{}::{}", book.name, method.name),
                test_rule: format!("test_{}_{}", book.name, method.name),
            });
        }
    }
    targets
}

fn base_rule_name(name: &str) -> &str {
    name.split("::").last().unwrap_or(name)
}

fn is_exempt_rule(name: &str) -> bool {
    matches!(name, "main" | "init")
}

fn parse_test_rules(path: &Path) -> Result<HashSet<String>, Vec<birddisk_core::Diagnostic>> {
    let source = std::fs::read_to_string(path).map_err(|err| {
        vec![birddisk_core::Diagnostic {
            code: "E0001",
            severity: "error",
            message: format!("Unable to read file: {err}"),
            file: path.to_string_lossy().to_string(),
            span: default_span(),
            trace: Vec::new(),
            notes: vec!["IO error".to_string()],
            spec_refs: Vec::new(),
            fixits: Vec::new(),
            help: Some("Ensure the path exists and is readable.".to_string()),
        }]
    })?;
    let tokens = birddisk_core::lexer::lex(&source)
        .map_err(|err| vec![lex_error_diagnostic(path, err)])?;
    let program = birddisk_core::parser::parse_with_recovery(&tokens).map_err(|errs| {
        errs.into_iter()
            .map(|err| parse_error_diagnostic(path, err))
            .collect::<Vec<_>>()
    })?;
    let names = program
        .functions
        .iter()
        .map(|func| func.name.clone())
        .collect::<HashSet<_>>();
    Ok(names)
}

fn lex_error_diagnostic(
    path: &Path,
    err: birddisk_core::lexer::LexError,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: err.code,
        severity: "error",
        message: err.message,
        file: path.to_string_lossy().to_string(),
        span: err.span,
        trace: Vec::new(),
        notes: vec!["Lexer error".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

fn parse_error_diagnostic(
    path: &Path,
    err: birddisk_core::parser::ParseError,
) -> birddisk_core::Diagnostic {
    let fixits = err
        .fixit
        .map(|hint| {
            vec![birddisk_core::FixIt {
                title: hint.title.to_string(),
                edits: vec![birddisk_core::Edit {
                    file: path.to_string_lossy().to_string(),
                    span: hint.span,
                    replacement: hint.replacement,
                }],
            }]
        })
        .unwrap_or_default();
    birddisk_core::Diagnostic {
        code: err.code,
        severity: "error",
        message: err.message,
        file: path.to_string_lossy().to_string(),
        span: err.span,
        trace: Vec::new(),
        notes: vec!["Parser error".to_string()],
        spec_refs: Vec::new(),
        fixits,
        help: None,
    }
}

fn default_span() -> birddisk_core::Span {
    birddisk_core::Span::new(
        birddisk_core::Position::new(1, 1),
        birddisk_core::Position::new(1, 1),
    )
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
            "rule helper() -> i64:\n  yield 1.\nend\n\nrule main() -> i64:\n  yield helper().\nend\n",
        );
        let test_path = root.join("tests").join("src").join("main_test.bd");
        fs::write(&test_path, "rule test_helper() -> void:\nend\n").expect("write test");
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_reports_missing_file() {
        let (root, entry, config) = setup_project(
            "missing",
            "rule helper() -> i64:\n  yield 1.\nend\n\nrule main() -> i64:\n  yield helper().\nend\n",
        );
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.iter().any(|diag| diag.code == "L2000"));
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_reports_missing_rule() {
        let (root, entry, config) = setup_project(
            "missing_rule",
            "rule helper() -> i64:\n  yield 1.\nend\n\nrule main() -> i64:\n  yield helper().\nend\n",
        );
        let test_path = root.join("tests").join("src").join("main_test.bd");
        fs::write(&test_path, "rule test_main() -> void:\nend\n").expect("write test");
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.iter().any(|diag| diag.code == "L2002"));
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_respects_exclude_list() {
        let (root, entry, config) = setup_project(
            "exclude",
            "rule helper() -> i64:\n  yield 1.\nend\n\nrule main() -> i64:\n  yield helper().\nend\n",
        );
        let diagnostics = enforce_require_tests(&entry, &config, &["src/main.bd".to_string()]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_skips_main_only() {
        let (root, entry, config) = setup_project(
            "main_only",
            "rule main() -> i64:\n  yield 0.\nend\n",
        );
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn require_tests_accepts_book_method() {
        let (root, entry, config) = setup_project(
            "book_method",
            "book Counter:\n  field value: i64.\n  rule init(self: Counter, start: i64) -> Counter:\n    put self::value = start.\n    yield self.\n  end\n  rule inc(self: Counter) -> Counter:\n    put self::value = self::value + 1.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set c: Counter = new Counter(1).\n  yield c::value.\nend\n",
        );
        let test_path = root.join("tests").join("src").join("main_test.bd");
        fs::write(&test_path, "rule test_Counter_inc() -> void:\nend\n").expect("write test");
        let diagnostics = enforce_require_tests(&entry, &config, &[]);
        assert!(diagnostics.is_empty());
        let _ = fs::remove_dir_all(&root);
    }
}
