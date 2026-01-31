//! BirdDisk core library (placeholder).

pub mod ast;
mod diagnostics;
mod fmt;
pub mod lexer;
pub mod parser;
pub mod runtime;
mod typecheck;

use diagnostics::diagnostic;
use lexer::LexError;
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

pub const TOOL_NAME: &str = "birddisk";
pub const VERSION: &str = "0.1";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Engine {
    Vm,
    Wasm,
    Native,
}

#[derive(Debug, Clone, Default)]
pub struct ModuleConfig {
    pub project_root: Option<PathBuf>,
    pub dep_roots: HashMap<String, PathBuf>,
}

#[derive(Serialize)]
struct CheckReport {
    tool: &'static str,
    version: &'static str,
    ok: bool,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Serialize)]
pub struct RunReport {
    pub tool: &'static str,
    pub version: &'static str,
    pub ok: bool,
    pub result: Option<i64>,
    pub stdout: Option<String>,
    pub diagnostics: Vec<Diagnostic>,
}

pub use diagnostics::{Diagnostic, Edit, FixIt, Position, Span, TraceFrame};

pub fn check_json(path: &str) -> String {
    let report = match parse_and_typecheck(path) {
        Ok(_) => CheckReport {
            tool: TOOL_NAME,
            version: VERSION,
            ok: true,
            diagnostics: Vec::new(),
        },
        Err(diagnostics) => CheckReport {
            tool: TOOL_NAME,
            version: VERSION,
            ok: false,
            diagnostics,
        },
    };

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

pub fn run_json(path: &str, _engine: Engine) -> String {
    let report = match parse_and_typecheck(path) {
        Ok(_) => RunReport {
            tool: TOOL_NAME,
            version: VERSION,
            ok: false,
            result: None,
            stdout: None,
            diagnostics: vec![not_implemented_diagnostic(path)],
        },
        Err(diagnostics) => RunReport {
            tool: TOOL_NAME,
            version: VERSION,
            ok: false,
            result: None,
            stdout: None,
            diagnostics,
        },
    };

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

pub fn fmt(path: &str) -> Result<(), String> {
    fmt::format_path(path)
}

pub fn test_json() -> String {
    let report = CheckReport {
        tool: TOOL_NAME,
        version: VERSION,
        ok: false,
        diagnostics: vec![not_implemented_diagnostic("<tests>")],
    };

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

fn not_implemented_diagnostic(path: &str) -> Diagnostic {
    diagnostic(
        "E0000",
        "error",
        "Not implemented".to_string(),
        path,
        default_span(),
        vec!["Compiler stub".to_string()],
        Vec::new(),
        Vec::new(),
        Some("BirdDisk compiler is not implemented yet.".to_string()),
    )
}

fn load_source(path: &str) -> Result<String, Diagnostic> {
    fs::read_to_string(path).map_err(|err| {
        diagnostic(
            "E0001",
            "error",
            format!("Unable to read file: {err}"),
            path,
            default_span(),
            vec!["IO error".to_string()],
            Vec::new(),
            Vec::new(),
            Some("Ensure the path exists and is readable.".to_string()),
        )
    })
}

fn diagnostic_from_lex_error(path: &str, err: LexError) -> Diagnostic {
    diagnostic(
        err.code,
        "error",
        err.message,
        path,
        err.span,
        vec!["Lexer error".to_string()],
        Vec::new(),
        Vec::new(),
        None,
    )
}

fn diagnostic_from_parse_error(path: &str, err: parser::ParseError) -> Diagnostic {
    let fixits = err
        .fixit
        .map(|hint| vec![FixIt {
            title: hint.title.to_string(),
            edits: vec![Edit {
                file: path.to_string(),
                span: hint.span,
                replacement: hint.replacement,
            }],
        }])
        .unwrap_or_default();
    diagnostic(
        err.code,
        "error",
        err.message,
        path,
        err.span,
        vec!["Parser error".to_string()],
        Vec::new(),
        fixits,
        None,
    )
}

fn default_span() -> Span {
    Span::new(Position::new(1, 1), Position::new(1, 1))
}

fn is_std_import(path: &[String]) -> bool {
    path.first().map(|part| part == "std").unwrap_or(false)
}

fn is_builtin_std_module(path: &[String]) -> bool {
    matches!(
        path,
        [root, module]
            if root == "std"
                && (module == "string"
                    || module == "bytes"
                    || module == "io"
                    || module == "time"
                    || module == "rand"
                    || module == "fs"
                    || module == "path"
                    || module == "env"
                    || module == "json")
    )
}

fn stdlib_root_from(start: &Path) -> Option<PathBuf> {
    let mut current = if start.is_dir() {
        Some(start)
    } else {
        start.parent()
    };
    while let Some(dir) = current {
        let candidate = dir.join("stdlib");
        if candidate.is_dir() && candidate.join("std").is_dir() {
            return Some(candidate);
        }
        current = dir.parent();
    }
    None
}

fn stdlib_import_diagnostic(file: &str, span: Span, message: impl Into<String>) -> Diagnostic {
    diagnostic(
        "E0003",
        "error",
        message.into(),
        file,
        span,
        vec!["Stdlib module resolution failed.".to_string()],
        vec!["SPEC.md#14-stdlib-layout".to_string()],
        Vec::new(),
        None,
    )
}

fn module_import_diagnostic(file: &str, span: Span, message: impl Into<String>) -> Diagnostic {
    diagnostic(
        "E0004",
        "error",
        message.into(),
        file,
        span,
        vec!["Module resolution failed.".to_string()],
        vec!["SPEC.md#1-1-imports".to_string()],
        Vec::new(),
        None,
    )
}

fn module_path_from_base(base: &Path, module_path: &[String]) -> PathBuf {
    let mut path = base.to_path_buf();
    for part in module_path {
        path.push(part);
    }
    path.set_extension("bd");
    path
}

fn project_root_from_entry(entry_path: &Path) -> Option<PathBuf> {
    stdlib_root_from(entry_path).and_then(|root| root.parent().map(|dir| dir.to_path_buf()))
}

fn user_module_candidates(
    entry_path: &Path,
    project_root: Option<&Path>,
    module_path: &[String],
) -> Vec<PathBuf> {
    let mut candidates = Vec::new();
    if let Some(entry_dir) = entry_path.parent() {
        candidates.push(module_path_from_base(entry_dir, module_path));
    }
    if let Some(root) = project_root {
        let candidate = module_path_from_base(&root, module_path);
        if !candidates.iter().any(|existing| *existing == candidate) {
            candidates.push(candidate);
        }
    }
    candidates
}

fn resolve_user_module_path(
    entry_path: &Path,
    project_root: Option<&Path>,
    module_path: &[String],
) -> Option<PathBuf> {
    for candidate in user_module_candidates(entry_path, project_root, module_path) {
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

fn dep_module_candidates(dep_root: &Path, module_path: &[String]) -> Vec<PathBuf> {
    let mut candidates = Vec::new();
    if module_path.len() <= 1 {
        if dep_root.is_file() {
            candidates.push(dep_root.to_path_buf());
        }
        if dep_root.is_dir() {
            candidates.push(dep_root.join("mod.bd"));
        }
    } else if dep_root.is_dir() {
        candidates.push(module_path_from_base(dep_root, &module_path[1..]));
    }
    candidates
}

fn line_snippet(lines: &[&str], line: u32) -> String {
    if line == 0 {
        return String::new();
    }
    let idx = (line - 1) as usize;
    lines
        .get(idx)
        .map(|value| value.trim_end().to_string())
        .unwrap_or_default()
}

pub fn attach_sources(program: &mut ast::Program, file: &str, source: &str) {
    let lines: Vec<&str> = source.lines().collect();
    let apply = |func: &mut ast::Function| {
        func.file = file.to_string();
        func.source = line_snippet(&lines, func.span.start.line);
    };
    for func in &mut program.functions {
        apply(func);
    }
    for book in &mut program.books {
        for method in &mut book.methods {
            apply(method);
        }
    }
}

enum ModuleKind {
    Stdlib,
    User,
}

struct ModuleLoader<'a> {
    entry_file: &'a str,
    entry_path: &'a Path,
    project_root: Option<PathBuf>,
    stdlib_root: Option<PathBuf>,
    dep_roots: HashMap<String, PathBuf>,
    loaded: HashSet<PathBuf>,
    enums: Vec<ast::EnumDecl>,
    books: Vec<ast::Book>,
    functions: Vec<ast::Function>,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> ModuleLoader<'a> {
    fn new(entry_file: &'a str, entry_path: &'a Path, config: &ModuleConfig) -> Self {
        let project_root = config
            .project_root
            .clone()
            .or_else(|| project_root_from_entry(entry_path));
        Self {
            entry_file,
            entry_path,
            project_root: project_root.clone(),
            stdlib_root: project_root
                .as_deref()
                .and_then(stdlib_root_from)
                .or_else(|| stdlib_root_from(entry_path)),
            dep_roots: config.dep_roots.clone(),
            loaded: HashSet::new(),
            enums: Vec::new(),
            books: Vec::new(),
            functions: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn load_imports(&mut self, imports: &[ast::Import]) {
        for import in imports {
            if is_std_import(&import.path) {
                if is_builtin_std_module(&import.path) {
                    continue;
                }
                let key = import.path.join("::");
                let Some(root) = self.stdlib_root.as_ref() else {
                    self.diagnostics.push(stdlib_import_diagnostic(
                        self.entry_file,
                        import.span,
                        format!(
                            "Unable to resolve stdlib module '{key}' (stdlib directory not found)."
                        ),
                    ));
                    continue;
                };
                let module_path = module_path_from_base(root, &import.path);
                if !module_path.exists() {
                    self.diagnostics.push(stdlib_import_diagnostic(
                        self.entry_file,
                        import.span,
                        format!(
                            "Unable to resolve stdlib module '{key}' (expected {}).",
                            module_path.display()
                        ),
                    ));
                    continue;
                }
                self.load_module(module_path, &import.path, ModuleKind::Stdlib);
                continue;
            }

            if let Some(candidates) = import
                .path
                .first()
                .and_then(|name| self.dep_roots.get(name))
                .map(|root| dep_module_candidates(root, &import.path))
            {
                if let Some(module_path) = candidates.iter().find(|path| path.exists()) {
                    self.load_module(module_path.clone(), &import.path, ModuleKind::User);
                } else {
                    let key = import.path.join("::");
                    let expected: Vec<String> = candidates
                        .iter()
                        .map(|path| path.display().to_string())
                        .collect();
                    self.diagnostics.push(module_import_diagnostic(
                        self.entry_file,
                        import.span,
                        format!(
                            "Unable to resolve module '{key}' from dependency (expected {}).",
                            expected.join(" or ")
                        ),
                    ));
                }
                continue;
            }

            let key = import.path.join("::");
            let Some(module_path) =
                resolve_user_module_path(self.entry_path, self.project_root.as_deref(), &import.path)
            else {
                let expected: Vec<String> = user_module_candidates(
                    self.entry_path,
                    self.project_root.as_deref(),
                    &import.path,
                )
                .into_iter()
                .map(|path| path.display().to_string())
                .collect();
                self.diagnostics.push(module_import_diagnostic(
                    self.entry_file,
                    import.span,
                    format!(
                        "Unable to resolve module '{key}' (expected {}).",
                        expected.join(" or ")
                    ),
                ));
                continue;
            };
            self.load_module(module_path, &import.path, ModuleKind::User);
        }
    }

    fn load_module(
        &mut self,
        path: PathBuf,
        module_path: &[String],
        _kind: ModuleKind,
    ) {
        let path = path.canonicalize().unwrap_or(path);
        if !self.loaded.insert(path.clone()) {
            return;
        }
        let path_str = path.to_string_lossy();
        let source = match load_source(path_str.as_ref()) {
            Ok(source) => source,
            Err(diag) => {
                self.diagnostics.push(diag);
                return;
            }
        };
        let tokens = match lexer::lex(&source) {
            Ok(tokens) => tokens,
            Err(err) => {
                self.diagnostics
                    .push(diagnostic_from_lex_error(path_str.as_ref(), err));
                return;
            }
        };
        let mut module_program = match parser::parse_with_recovery(&tokens) {
            Ok(program) => program,
            Err(errs) => {
                self.diagnostics.extend(
                    errs.into_iter()
                        .map(|err| diagnostic_from_parse_error(path_str.as_ref(), err)),
                );
                return;
            }
        };
        attach_sources(&mut module_program, path_str.as_ref(), &source);
        self.load_imports(&module_program.imports);
        qualify_module_program(&mut module_program, module_path);
        self.functions.extend(module_program.functions);
        self.books.extend(module_program.books);
        self.enums.extend(module_program.enums);
    }
}

fn qualify_module_program(program: &mut ast::Program, module_path: &[String]) {
    let prefix = module_path.join("::");
    let local_names: HashSet<String> = program
        .functions
        .iter()
        .map(|func| func.name.clone())
        .collect();
    for func in &mut program.functions {
        for stmt in &mut func.body {
            qualify_stmt(stmt, &local_names, &prefix);
        }
        func.name = format!("{prefix}::{}", func.name);
    }
    for book in &mut program.books {
        for method in &mut book.methods {
            for stmt in &mut method.body {
                qualify_stmt(stmt, &local_names, &prefix);
            }
        }
    }
}

fn qualify_stmt(stmt: &mut ast::Stmt, local_names: &HashSet<String>, prefix: &str) {
    match stmt {
        ast::Stmt::Set { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Expr { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Put { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::PutIndex { index, expr, .. } => {
            qualify_expr(index, local_names, prefix);
            qualify_expr(expr, local_names, prefix);
        }
        ast::Stmt::PutField { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Yield { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Throw { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Try {
            try_body,
            catch_body,
            ..
        } => {
            for stmt in try_body {
                qualify_stmt(stmt, local_names, prefix);
            }
            for stmt in catch_body {
                qualify_stmt(stmt, local_names, prefix);
            }
        }
        ast::Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            qualify_expr(cond, local_names, prefix);
            for stmt in then_body {
                qualify_stmt(stmt, local_names, prefix);
            }
            for stmt in else_body {
                qualify_stmt(stmt, local_names, prefix);
            }
        }
        ast::Stmt::Repeat { cond, body, .. } => {
            qualify_expr(cond, local_names, prefix);
            for stmt in body {
                qualify_stmt(stmt, local_names, prefix);
            }
        }
        ast::Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => {
            qualify_expr(expr, local_names, prefix);
            for case in cases {
                for stmt in &mut case.body {
                    qualify_stmt(stmt, local_names, prefix);
                }
            }
            for stmt in otherwise {
                qualify_stmt(stmt, local_names, prefix);
            }
        }
    }
}

fn qualify_expr(expr: &mut ast::Expr, local_names: &HashSet<String>, prefix: &str) {
    match &mut expr.kind {
        ast::ExprKind::Call { name, args } => {
            for arg in args {
                qualify_expr(arg, local_names, prefix);
            }
            if !name.contains("::") && local_names.contains(name) {
                *name = format!("{prefix}::{name}");
            }
        }
        ast::ExprKind::New { args, .. } => {
            for arg in args {
                qualify_expr(arg, local_names, prefix);
            }
        }
        ast::ExprKind::MemberAccess { .. } => {}
        ast::ExprKind::ArrayLit(elements) => {
            for element in elements {
                qualify_expr(element, local_names, prefix);
            }
        }
        ast::ExprKind::ArrayNew { len } => qualify_expr(len, local_names, prefix),
        ast::ExprKind::Index { base, index } => {
            qualify_expr(base, local_names, prefix);
            qualify_expr(index, local_names, prefix);
        }
        ast::ExprKind::Unary { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::ExprKind::Binary { left, right, .. } => {
            qualify_expr(left, local_names, prefix);
            qualify_expr(right, local_names, prefix);
        }
        _ => {}
    }
}

pub fn parse_and_typecheck(path: &str) -> Result<ast::Program, Vec<Diagnostic>> {
    parse_and_typecheck_with_config(path, &ModuleConfig::default())
}

pub fn parse_and_typecheck_with_config(
    path: &str,
    config: &ModuleConfig,
) -> Result<ast::Program, Vec<Diagnostic>> {
    let source = load_source(path).map_err(|diag| vec![diag])?;
    let tokens = lexer::lex(&source).map_err(|err| vec![diagnostic_from_lex_error(path, err)])?;
    let mut program = parser::parse_with_recovery(&tokens).map_err(|errs| {
        errs.into_iter()
            .map(|err| diagnostic_from_parse_error(path, err))
            .collect::<Vec<_>>()
    })?;
    attach_sources(&mut program, path, &source);
    let entry_path = PathBuf::from(path);
    let entry_path = entry_path.canonicalize().unwrap_or(entry_path);
    let mut loader = ModuleLoader::new(path, &entry_path, config);
    loader.load_imports(&program.imports);
    if !loader.diagnostics.is_empty() {
        return Err(loader.diagnostics);
    }
    program.functions.extend(loader.functions);
    program.books.extend(loader.books);
    program.enums.extend(loader.enums);
    let diagnostics = typecheck::typecheck(&program, path);
    if diagnostics.is_empty() {
        Ok(program)
    } else {
        Err(diagnostics)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;
    use std::path::PathBuf;

    fn write_temp(contents: &str, name: &str) -> PathBuf {
        let mut path = env::temp_dir();
        let file_name = format!("birddisk_{name}_{}.bd", std::process::id());
        path.push(file_name);
        fs::write(&path, contents).expect("write temp file");
        path
    }

    #[test]
    fn check_json_ok_when_rule_present() {
        let path = write_temp("rule main() -> i64:\n  yield 0.\nend\n", "ok");
        let output = check_json(path.to_str().unwrap());
        let value: serde_json::Value = serde_json::from_str(&output).unwrap();
        assert_eq!(value["ok"], true);
        fs::remove_file(path).ok();
    }

    #[test]
    fn check_json_error_when_no_rule() {
        let path = write_temp("set x = 1.\n", "missing_rule");
        let output = check_json(path.to_str().unwrap());
        let value: serde_json::Value = serde_json::from_str(&output).unwrap();
        assert_eq!(value["ok"], false);
        fs::remove_file(path).ok();
    }

    #[test]
    fn parse_with_dep_roots_resolves_modules() {
        let mut root = env::temp_dir();
        root.push(format!("birddisk_dep_root_{}", std::process::id()));
        let src_dir = root.join("src");
        let dep_dir = root.join("deps").join("util");
        fs::create_dir_all(&src_dir).expect("create src");
        fs::create_dir_all(&dep_dir).expect("create dep");
        fs::write(
            src_dir.join("main.bd"),
            "import util::math.\nrule main() -> i64:\n  yield util::math::add(1, 2).\nend\n",
        )
        .expect("write main");
        fs::write(
            dep_dir.join("math.bd"),
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n",
        )
        .expect("write module");
        let mut config = ModuleConfig::default();
        config.project_root = Some(root.clone());
        config
            .dep_roots
            .insert("util".to_string(), dep_dir.clone());

        let entry = src_dir.join("main.bd");
        let program =
            parse_and_typecheck_with_config(entry.to_str().unwrap(), &config).unwrap();
        assert!(program
            .functions
            .iter()
            .any(|func| func.name == "util::math::add"));

        let _ = fs::remove_dir_all(&root);
    }
}
