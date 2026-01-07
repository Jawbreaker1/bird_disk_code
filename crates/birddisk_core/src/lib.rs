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
use std::fs;

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
    pub diagnostics: Vec<Diagnostic>,
}

pub use diagnostics::{Diagnostic, Edit, FixIt, Position, Span};

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
            diagnostics: vec![not_implemented_diagnostic(path)],
        },
        Err(diagnostics) => RunReport {
            tool: TOOL_NAME,
            version: VERSION,
            ok: false,
            result: None,
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

pub fn parse_and_typecheck(path: &str) -> Result<ast::Program, Vec<Diagnostic>> {
    let source = load_source(path).map_err(|diag| vec![diag])?;
    let tokens = lexer::lex(&source).map_err(|err| vec![diagnostic_from_lex_error(path, err)])?;
    let program = parser::parse_with_recovery(&tokens).map_err(|errs| {
        errs.into_iter()
            .map(|err| diagnostic_from_parse_error(path, err))
            .collect::<Vec<_>>()
    })?;
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
