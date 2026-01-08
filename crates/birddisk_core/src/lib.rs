//! BirdDisk core library (placeholder).

pub mod ast;
mod diagnostics;
mod fmt;
pub mod lexer;
pub mod parser;
mod typecheck;

use diagnostics::diagnostic;
use lexer::LexError;
use serde::Serialize;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

pub const TOOL_NAME: &str = "birddisk";
pub const VERSION: &str = "0.1";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Engine {
    Vm,
    Wasm,
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
                && (module == "string" || module == "bytes" || module == "io")
    )
}

fn stdlib_root(entry_path: &Path) -> Option<PathBuf> {
    let mut current = entry_path.parent();
    while let Some(dir) = current {
        let candidate = dir.join("stdlib");
        if candidate.is_dir() {
            return Some(candidate);
        }
        current = dir.parent();
    }
    None
}

fn stdlib_module_path(root: &Path, module_path: &[String]) -> PathBuf {
    let mut path = root.to_path_buf();
    for part in module_path {
        path.push(part);
    }
    path.set_extension("bd");
    path
}

fn stdlib_import_diagnostic(file: &str, span: Span, message: impl Into<String>) -> Diagnostic {
    diagnostic(
        "E0003",
        "error",
        message.into(),
        file,
        span,
        vec!["Stdlib module resolution failed.".to_string()],
        vec!["SPEC.md#12-stdlib-layout".to_string()],
        Vec::new(),
        None,
    )
}

fn load_stdlib_modules(
    imports: &[ast::Import],
    entry_file: &str,
    entry_path: &Path,
) -> Result<Vec<ast::Function>, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut functions = Vec::new();
    let mut loaded = HashSet::new();
    let root = stdlib_root(entry_path);

    for import in imports {
        if !is_std_import(&import.path) || is_builtin_std_module(&import.path) {
            continue;
        }
        let key = import.path.join("::");
        if !loaded.insert(key.clone()) {
            continue;
        }
        let Some(root) = root.as_ref() else {
            diagnostics.push(stdlib_import_diagnostic(
                entry_file,
                import.span,
                format!(
                    "Unable to resolve stdlib module '{key}' (stdlib directory not found)."
                ),
            ));
            continue;
        };
        let module_path = stdlib_module_path(root, &import.path);
        if !module_path.exists() {
            diagnostics.push(stdlib_import_diagnostic(
                entry_file,
                import.span,
                format!(
                    "Unable to resolve stdlib module '{key}' (expected {}).",
                    module_path.display()
                ),
            ));
            continue;
        }
        let path_str = module_path.to_string_lossy();
        let source = match load_source(path_str.as_ref()) {
            Ok(source) => source,
            Err(diag) => {
                diagnostics.push(diag);
                continue;
            }
        };
        let tokens = match lexer::lex(&source) {
            Ok(tokens) => tokens,
            Err(err) => {
                diagnostics.push(diagnostic_from_lex_error(path_str.as_ref(), err));
                continue;
            }
        };
        let mut module_program = match parser::parse_with_recovery(&tokens) {
            Ok(program) => program,
            Err(errs) => {
                diagnostics.extend(
                    errs.into_iter()
                        .map(|err| diagnostic_from_parse_error(path_str.as_ref(), err)),
                );
                continue;
            }
        };
        qualify_module_program(&mut module_program, &import.path);
        functions.extend(module_program.functions);
    }

    if diagnostics.is_empty() {
        Ok(functions)
    } else {
        Err(diagnostics)
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
}

fn qualify_stmt(stmt: &mut ast::Stmt, local_names: &HashSet<String>, prefix: &str) {
    match stmt {
        ast::Stmt::Set { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Put { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::PutIndex { index, expr, .. } => {
            qualify_expr(index, local_names, prefix);
            qualify_expr(expr, local_names, prefix);
        }
        ast::Stmt::PutField { expr, .. } => qualify_expr(expr, local_names, prefix),
        ast::Stmt::Yield { expr, .. } => qualify_expr(expr, local_names, prefix),
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
    let source = load_source(path).map_err(|diag| vec![diag])?;
    let tokens = lexer::lex(&source).map_err(|err| vec![diagnostic_from_lex_error(path, err)])?;
    let mut program = parser::parse_with_recovery(&tokens).map_err(|errs| {
        errs.into_iter()
            .map(|err| diagnostic_from_parse_error(path, err))
            .collect::<Vec<_>>()
    })?;
    let entry_path = PathBuf::from(path);
    let entry_path = entry_path.canonicalize().unwrap_or(entry_path);
    let stdlib_functions = load_stdlib_modules(&program.imports, path, &entry_path)?;
    program.functions.extend(stdlib_functions);
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
}
