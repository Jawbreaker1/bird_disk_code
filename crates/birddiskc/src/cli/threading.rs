use super::diagnostics::{wasm_net_diagnostic, wasm_threading_diagnostic};
use birddisk_core::ast::{Expr, ExprKind, Function, Program, Stmt};

pub(crate) fn wasm_threading_guard(
    path: &str,
    config: &birddisk_core::ModuleConfig,
) -> Option<birddisk_core::Diagnostic> {
    if let Ok(source) = std::fs::read_to_string(path) {
        if source.contains("import std::thread.") || source.contains("std::thread::") {
            return Some(wasm_threading_diagnostic(path));
        }
        if source.contains("import std::net.") || source.contains("std::net::") {
            return Some(wasm_net_diagnostic(path));
        }
    }
    let program = birddisk_core::load_program_with_config(path, config).ok()?;
    if program_uses_threading(&program) {
        Some(wasm_threading_diagnostic(path))
    } else if program_uses_net(&program) {
        Some(wasm_net_diagnostic(path))
    } else {
        None
    }
}

fn program_uses_threading(program: &Program) -> bool {
    program_uses_std_prefix(program, "std::thread::")
}

fn program_uses_net(program: &Program) -> bool {
    if program
        .imports
        .iter()
        .any(|import| import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "net")
    {
        return true;
    }
    program_uses_std_prefix(program, "std::net::")
}

fn program_uses_std_prefix(program: &Program, prefix: &str) -> bool {
    let mut functions: Vec<&Function> = program.functions.iter().collect();
    for book in &program.books {
        functions.extend(book.methods.iter());
    }
    for func in functions {
        for stmt in &func.body {
            if stmt_uses_std_prefix(stmt, prefix) {
                return true;
            }
        }
    }
    false
}

fn stmt_uses_std_prefix(stmt: &Stmt, prefix: &str) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::Expr { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::Put { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::PutIndex { index, expr, .. } => {
            expr_uses_std_prefix(index, prefix) || expr_uses_std_prefix(expr, prefix)
        }
        Stmt::PutField { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::Yield { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::Throw { expr, .. } => expr_uses_std_prefix(expr, prefix),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => {
            try_body
                .iter()
                .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
                || catch_body
                    .iter()
                    .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_uses_std_prefix(cond, prefix)
                || then_body
                    .iter()
                    .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
                || else_body
                    .iter()
                    .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_uses_std_prefix(cond, prefix)
                || body.iter().any(|stmt| stmt_uses_std_prefix(stmt, prefix))
        }
        Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => {
            if expr_uses_std_prefix(expr, prefix) {
                return true;
            }
            for case in cases {
                if case
                    .body
                    .iter()
                    .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
                {
                    return true;
                }
            }
            otherwise
                .iter()
                .any(|stmt| stmt_uses_std_prefix(stmt, prefix))
        }
    }
}

fn expr_uses_std_prefix(expr: &Expr, prefix: &str) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            if name.starts_with(prefix) {
                return true;
            }
            args.iter().any(|expr| expr_uses_std_prefix(expr, prefix))
        }
        ExprKind::New { args, .. } => args.iter().any(|expr| expr_uses_std_prefix(expr, prefix)),
        ExprKind::ArrayLit(elements) => elements
            .iter()
            .any(|expr| expr_uses_std_prefix(expr, prefix)),
        ExprKind::ArrayNew { len } => expr_uses_std_prefix(len, prefix),
        ExprKind::Index { base, index } => {
            expr_uses_std_prefix(base, prefix) || expr_uses_std_prefix(index, prefix)
        }
        ExprKind::Unary { expr, .. } => expr_uses_std_prefix(expr, prefix),
        ExprKind::Cast { expr, .. } => expr_uses_std_prefix(expr, prefix),
        ExprKind::Binary { left, right, .. } => {
            expr_uses_std_prefix(left, prefix) || expr_uses_std_prefix(right, prefix)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::wasm_threading_guard;
    use std::env;
    use std::fs;

    #[test]
    fn wasm_guard_rejects_threading_usage() {
        let source = "import std::thread.\nrule worker() -> i64:\n  yield 1.\nend\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\").\n  yield std::thread::join(t).\nend\n";
        let mut path = env::temp_dir();
        path.push(format!(
            "birddisk_wasm_guard_thread_{}.bd",
            std::process::id()
        ));
        fs::write(&path, source).expect("write temp source");
        let path_str = path.to_string_lossy().to_string();

        let diag = wasm_threading_guard(&path_str, &birddisk_core::ModuleConfig::default())
            .expect("expected wasm guard diagnostic");
        assert_eq!(diag.code, "E0325");

        let _ = fs::remove_file(&path);
    }

    #[test]
    fn wasm_guard_rejects_std_net_usage() {
        let source = "import std::net.\nrule main() -> i64:\n  yield 0.\nend\n";
        let mut path = env::temp_dir();
        path.push(format!("birddisk_wasm_guard_net_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp source");
        let path_str = path.to_string_lossy().to_string();

        let diag = wasm_threading_guard(&path_str, &birddisk_core::ModuleConfig::default())
            .expect("expected wasm guard diagnostic");
        assert_eq!(diag.code, "E0326");

        let _ = fs::remove_file(&path);
    }
}
