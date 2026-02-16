use super::diagnostics::{native_threading_diagnostic, wasm_threading_diagnostic};
use birddisk_core::ast::{Expr, ExprKind, Function, Program, Stmt};

pub(crate) fn wasm_threading_guard(
    path: &str,
    config: &birddisk_core::ModuleConfig,
) -> Option<birddisk_core::Diagnostic> {
    let program = birddisk_core::load_program_with_config(path, config).ok()?;
    if program_uses_threading(&program) {
        Some(wasm_threading_diagnostic(path))
    } else {
        None
    }
}

pub(crate) fn native_threading_guard(
    path: &str,
    config: &birddisk_core::ModuleConfig,
) -> Option<birddisk_core::Diagnostic> {
    let program = birddisk_core::load_program_with_config(path, config).ok()?;
    if program_uses_threading(&program) {
        Some(native_threading_diagnostic(path))
    } else {
        None
    }
}

fn program_uses_threading(program: &Program) -> bool {
    let mut functions: Vec<&Function> = program.functions.iter().collect();
    for book in &program.books {
        functions.extend(book.methods.iter());
    }
    for func in functions {
        for stmt in &func.body {
            if stmt_uses_threading(stmt) {
                return true;
            }
        }
    }
    false
}

fn stmt_uses_threading(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_uses_threading(expr),
        Stmt::Expr { expr, .. } => expr_uses_threading(expr),
        Stmt::Put { expr, .. } => expr_uses_threading(expr),
        Stmt::PutIndex { index, expr, .. } => {
            expr_uses_threading(index) || expr_uses_threading(expr)
        }
        Stmt::PutField { expr, .. } => expr_uses_threading(expr),
        Stmt::Yield { expr, .. } => expr_uses_threading(expr),
        Stmt::Throw { expr, .. } => expr_uses_threading(expr),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => try_body.iter().any(stmt_uses_threading) || catch_body.iter().any(stmt_uses_threading),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_uses_threading(cond)
                || then_body.iter().any(stmt_uses_threading)
                || else_body.iter().any(stmt_uses_threading)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_uses_threading(cond) || body.iter().any(stmt_uses_threading)
        }
        Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => {
            if expr_uses_threading(expr) {
                return true;
            }
            for case in cases {
                if case.body.iter().any(stmt_uses_threading) {
                    return true;
                }
            }
            otherwise.iter().any(stmt_uses_threading)
        }
    }
}

fn expr_uses_threading(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            if name.starts_with("std::thread::") {
                return true;
            }
            args.iter().any(expr_uses_threading)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_uses_threading),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_uses_threading),
        ExprKind::ArrayNew { len } => expr_uses_threading(len),
        ExprKind::Index { base, index } => {
            expr_uses_threading(base) || expr_uses_threading(index)
        }
        ExprKind::Unary { expr, .. } => expr_uses_threading(expr),
        ExprKind::Cast { expr, .. } => expr_uses_threading(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_uses_threading(left) || expr_uses_threading(right)
        }
        _ => false,
    }
}
