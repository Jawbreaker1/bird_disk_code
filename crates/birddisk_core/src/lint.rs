use crate::ast::{Expr, ExprKind, Function, MatchCase, Program, Stmt};
use crate::diagnostics::{diagnostic, Edit, FixIt};
use crate::{Diagnostic, Span};

const SHORT_NAME_ALLOWLIST: &[&str] = &["i", "j", "k"];

pub fn lint_program(program: &Program) -> Vec<Diagnostic> {
    let mut import_usage = program
        .imports
        .iter()
        .map(|import| ImportUsage {
            path: import.path.clone(),
            span: import.span,
            used: false,
        })
        .collect::<Vec<_>>();
    let mut diagnostics = Vec::new();
    for func in &program.functions {
        lint_function(func, &mut import_usage, &mut diagnostics);
    }
    for book in &program.books {
        for method in &book.methods {
            lint_function(method, &mut import_usage, &mut diagnostics);
        }
    }
    let module_file = program
        .functions
        .first()
        .map(|func| func.file.as_str())
        .or_else(|| {
            program
                .books
                .first()
                .and_then(|book| book.methods.first().map(|m| m.file.as_str()))
        })
        .unwrap_or("<module>");
    for import in &import_usage {
        if !import.used {
            diagnostics.push(warn_with_fix(
                "L1007",
                format!("Unused import '{}'.", import.path.join("::")),
                module_file,
                import.span,
                vec!["Unused imports add noise for LLMs.".to_string()],
                remove_span_fix(module_file, import.span, "Remove unused import."),
                Some("Remove the import or use it.".to_string()),
            ));
        }
    }
    diagnostics
}

fn lint_function(
    function: &Function,
    import_usage: &mut [ImportUsage],
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut context = LintContext::new(function.file.clone());
    for param in &function.params {
        context.declare(&param.name, param.span, true, diagnostics);
    }
    for param in &function.params {
        lint_name(&param.name, &function.file, param.span, diagnostics);
    }
    for stmt in &function.body {
        lint_stmt(stmt, &mut context, import_usage, diagnostics);
    }
    context.emit_unused(diagnostics);
    let depth = max_nesting(&function.body, 0);
    if depth > 3 {
        diagnostics.push(warn(
            "L1003",
            format!(
                "Deep nesting in '{}' (depth {}). Consider extracting helper rules.",
                function.name, depth
            ),
            &function.file,
            function.span,
            vec!["LLM-friendly code benefits from shallow control flow.".to_string()],
            Some("Split nested blocks into named helper rules.".to_string()),
        ));
    }
}

fn lint_stmt(
    stmt: &Stmt,
    context: &mut LintContext,
    import_usage: &mut [ImportUsage],
    diagnostics: &mut Vec<Diagnostic>,
) {
    match stmt {
        Stmt::Set {
            name,
            ty,
            span,
            expr,
        } => {
            lint_name(name, &context.file, *span, diagnostics);
            if ty.is_none() {
                diagnostics.push(warn(
                    "L1001",
                    format!("Prefer explicit type annotation for '{name}'."),
                    &context.file,
                    *span,
                    vec!["Explicit types reduce ambiguity for LLMs.".to_string()],
                    Some("Add `: <type>` to the binding.".to_string()),
                ));
            }
            lint_expr(context, import_usage, diagnostics, expr);
            context.declare(name, *span, false, diagnostics);
        }
        Stmt::Try {
            catch_name, span, ..
        } => {
            lint_name(catch_name, &context.file, *span, diagnostics);
            if let Stmt::Try {
                try_body,
                catch_body,
                ..
            } = stmt
            {
                context.with_scope(|ctx| {
                    for stmt in try_body {
                        lint_stmt(stmt, ctx, import_usage, diagnostics);
                    }
                });
                context.with_scope(|ctx| {
                    ctx.declare(catch_name, *span, false, diagnostics);
                    for stmt in catch_body {
                        lint_stmt(stmt, ctx, import_usage, diagnostics);
                    }
                });
            }
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            if let ExprKind::Bool(value) = cond.kind {
                diagnostics.push(warn(
                    "L1008",
                    format!("Constant condition ({value}) in 'when'."),
                    &context.file,
                    cond.span,
                    vec!["Constant conditions hide intent for LLMs.".to_string()],
                    Some("Use a variable or comparison for the condition.".to_string()),
                ));
            }
            context.with_scope(|ctx| {
                for stmt in then_body {
                    lint_stmt(stmt, ctx, import_usage, diagnostics);
                }
            });
            context.with_scope(|ctx| {
                for stmt in else_body {
                    lint_stmt(stmt, ctx, import_usage, diagnostics);
                }
            });
        }
        Stmt::Repeat { cond, body, .. } => {
            if let ExprKind::Bool(value) = cond.kind {
                diagnostics.push(warn(
                    "L1008",
                    format!("Constant condition ({value}) in 'repeat'."),
                    &context.file,
                    cond.span,
                    vec!["Constant conditions hide intent for LLMs.".to_string()],
                    Some("Use a variable or comparison for the condition.".to_string()),
                ));
            }
            context.with_scope(|ctx| {
                for stmt in body {
                    lint_stmt(stmt, ctx, import_usage, diagnostics);
                }
            });
        }
        Stmt::Match {
            cases, otherwise, ..
        } => {
            for case in cases {
                lint_match_case(case, context, import_usage, diagnostics);
            }
            context.with_scope(|ctx| {
                for stmt in otherwise {
                    lint_stmt(stmt, ctx, import_usage, diagnostics);
                }
            });
        }
        Stmt::Expr { expr, .. } => lint_expr(context, import_usage, diagnostics, expr),
        Stmt::Put { name, expr, span } => {
            if let ExprKind::Ident(ident) = &expr.kind {
                if ident == name {
                    diagnostics.push(warn_with_fix(
                        "L1009",
                        format!("Self-assignment to '{name}' has no effect."),
                        &context.file,
                        *span,
                        vec!["Self-assignment adds noise for LLMs.".to_string()],
                        remove_span_fix(&context.file, *span, "Remove redundant assignment."),
                        Some("Remove the assignment or change the value.".to_string()),
                    ));
                }
            }
            lint_expr(context, import_usage, diagnostics, expr);
        }
        Stmt::PutIndex {
            name, index, expr, ..
        } => {
            context.mark_used(name);
            lint_expr(context, import_usage, diagnostics, index);
            lint_expr(context, import_usage, diagnostics, expr);
        }
        Stmt::PutField { base, expr, .. } => {
            context.mark_used(base);
            lint_expr(context, import_usage, diagnostics, expr);
        }
        Stmt::Yield { expr, .. } => lint_expr(context, import_usage, diagnostics, expr),
        Stmt::Throw { expr, .. } => lint_expr(context, import_usage, diagnostics, expr),
    }
}

fn lint_name(name: &str, file: &str, span: Span, diagnostics: &mut Vec<Diagnostic>) {
    if name.len() < 2 && !SHORT_NAME_ALLOWLIST.contains(&name) {
        diagnostics.push(warn(
            "L1002",
            format!("Name '{name}' is too short for LLM-friendly code."),
            file,
            span,
            vec!["Prefer descriptive names (min 2 characters).".to_string()],
            Some("Rename the binding to a descriptive name.".to_string()),
        ));
    }
}

fn lint_match_case(
    case: &MatchCase,
    context: &mut LintContext,
    import_usage: &mut [ImportUsage],
    diagnostics: &mut Vec<Diagnostic>,
) {
    context.with_scope(|ctx| {
        if let Some(binding) = &case.binding {
            ctx.declare(binding, case.span, false, diagnostics);
        }
        for stmt in &case.body {
            lint_stmt(stmt, ctx, import_usage, diagnostics);
        }
    });
}

fn lint_expr(
    context: &mut LintContext,
    import_usage: &mut [ImportUsage],
    diagnostics: &mut Vec<Diagnostic>,
    expr: &Expr,
) {
    match &expr.kind {
        ExprKind::Ident(name) => {
            context.mark_used(name);
        }
        ExprKind::Call { name, args } => {
            if name.contains("::") {
                let base = name.split("::").next().unwrap_or("");
                if context.find_binding(base).is_some() {
                    context.mark_used(base);
                } else {
                    mark_import_used(name, import_usage);
                }
            }
            for arg in args {
                lint_expr(context, import_usage, diagnostics, arg);
            }
        }
        ExprKind::MemberAccess { base, .. } => {
            context.mark_used(base);
        }
        ExprKind::ArrayLit(elements) => {
            for elem in elements {
                lint_expr(context, import_usage, diagnostics, elem);
            }
        }
        ExprKind::ArrayNew { len } => lint_expr(context, import_usage, diagnostics, len),
        ExprKind::Index { base, index } => {
            lint_expr(context, import_usage, diagnostics, base);
            lint_expr(context, import_usage, diagnostics, index);
        }
        ExprKind::Unary { expr, .. } => lint_expr(context, import_usage, diagnostics, expr),
        ExprKind::Cast { expr, .. } => lint_expr(context, import_usage, diagnostics, expr),
        ExprKind::Binary { left, right, .. } => {
            lint_expr(context, import_usage, diagnostics, left);
            lint_expr(context, import_usage, diagnostics, right);
        }
        ExprKind::New { .. }
        | ExprKind::Int(_)
        | ExprKind::Float(_)
        | ExprKind::Bool(_)
        | ExprKind::String(_) => {}
    }
}

fn max_nesting(stmts: &[Stmt], depth: u32) -> u32 {
    let mut max_depth = depth;
    for stmt in stmts {
        match stmt {
            Stmt::When {
                then_body,
                else_body,
                ..
            } => {
                max_depth = max_depth.max(max_nesting(then_body, depth + 1));
                max_depth = max_depth.max(max_nesting(else_body, depth + 1));
            }
            Stmt::Try {
                try_body,
                catch_body,
                ..
            } => {
                max_depth = max_depth.max(max_nesting(try_body, depth + 1));
                max_depth = max_depth.max(max_nesting(catch_body, depth + 1));
            }
            Stmt::Repeat { body, .. } => {
                max_depth = max_depth.max(max_nesting(body, depth + 1));
            }
            Stmt::Match {
                cases, otherwise, ..
            } => {
                for case in cases {
                    max_depth = max_depth.max(max_nesting(&case.body, depth + 1));
                }
                max_depth = max_depth.max(max_nesting(otherwise, depth + 1));
            }
            _ => {}
        }
    }
    max_depth
}

struct LintContext {
    file: String,
    bindings: Vec<Binding>,
    scopes: Vec<Scope>,
}

#[derive(Clone)]
struct Binding {
    name: String,
    span: Span,
    used: bool,
    is_param: bool,
}

#[derive(Default)]
struct Scope {
    names: std::collections::HashMap<String, usize>,
}

impl LintContext {
    fn new(file: String) -> Self {
        Self {
            file,
            bindings: Vec::new(),
            scopes: vec![Scope::default()],
        }
    }

    fn declare(
        &mut self,
        name: &str,
        span: Span,
        is_param: bool,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        if self.find_binding(name).is_some() {
            diagnostics.push(warn(
                "L1006",
                format!("Name '{name}' shadows an existing binding."),
                &self.file,
                span,
                vec!["Shadowing reduces clarity for LLMs.".to_string()],
                Some("Rename the binding to a distinct name.".to_string()),
            ));
        }
        let index = self.bindings.len();
        self.bindings.push(Binding {
            name: name.to_string(),
            span,
            used: false,
            is_param,
        });
        if let Some(scope) = self.scopes.last_mut() {
            scope.names.insert(name.to_string(), index);
        }
    }

    fn mark_used(&mut self, name: &str) {
        if let Some(index) = self.find_binding(name) {
            self.bindings[index].used = true;
        }
    }

    fn find_binding(&self, name: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(index) = scope.names.get(name) {
                return Some(*index);
            }
        }
        None
    }

    fn with_scope<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.scopes.push(Scope::default());
        f(self);
        self.scopes.pop();
    }

    fn emit_unused(&self, diagnostics: &mut Vec<Diagnostic>) {
        for binding in &self.bindings {
            if binding.used || binding.name.starts_with('_') {
                continue;
            }
            if binding.is_param {
                diagnostics.push(warn(
                    "L1005",
                    format!("Parameter '{}' is never used.", binding.name),
                    &self.file,
                    binding.span,
                    vec!["Unused parameters add noise for LLMs.".to_string()],
                    Some("Remove the parameter or use it.".to_string()),
                ));
            } else {
                diagnostics.push(warn(
                    "L1004",
                    format!("Binding '{}' is never used.", binding.name),
                    &self.file,
                    binding.span,
                    vec!["Unused bindings add noise for LLMs.".to_string()],
                    Some("Remove the binding or use it.".to_string()),
                ));
            }
        }
    }
}

struct ImportUsage {
    path: Vec<String>,
    span: Span,
    used: bool,
}

fn mark_import_used(call_name: &str, import_usage: &mut [ImportUsage]) {
    let parts: Vec<&str> = call_name.split("::").collect();
    if parts.len() < 2 {
        return;
    }
    for usage in import_usage {
        if usage.path.len() > parts.len() {
            continue;
        }
        if usage
            .path
            .iter()
            .zip(parts.iter())
            .all(|(left, right)| left == right)
        {
            usage.used = true;
        }
    }
}

fn warn(
    code: &'static str,
    message: String,
    file: &str,
    span: Span,
    notes: Vec<String>,
    help: Option<String>,
) -> Diagnostic {
    diagnostic(
        code,
        "warning",
        message,
        file,
        span,
        notes,
        Vec::new(),
        Vec::new(),
        help,
    )
}

fn warn_with_fix(
    code: &'static str,
    message: String,
    file: &str,
    span: Span,
    notes: Vec<String>,
    fixits: Vec<FixIt>,
    help: Option<String>,
) -> Diagnostic {
    diagnostic(
        code,
        "warning",
        message,
        file,
        span,
        notes,
        Vec::new(),
        fixits,
        help,
    )
}

fn remove_span_fix(file: &str, span: Span, title: &str) -> Vec<FixIt> {
    vec![FixIt {
        title: title.to_string(),
        edits: vec![Edit {
            file: file.to_string(),
            span,
            replacement: String::new(),
        }],
    }]
}

#[cfg(test)]
mod tests {
    use super::lint_program;
    use crate::parse_and_typecheck;
    use std::env;
    use std::fs;

    fn write_temp(source: &str, name: &str) -> String {
        let mut path = env::temp_dir();
        path.push(format!("birddisk_lint_{name}_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp file");
        path.to_string_lossy().to_string()
    }

    #[test]
    fn lint_warns_missing_set_type() {
        let path = write_temp(
            "rule main() -> i64:\n  set value = 1.\n  yield value.\nend\n",
            "set_type",
        );
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1001"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_short_names() {
        let path = write_temp("rule main(x: i64) -> i64:\n  yield x.\nend\n", "short_name");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1002"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_unused_binding() {
        let path = write_temp(
            "rule main() -> i64:\n  set value: i64 = 1.\n  yield 0.\nend\n",
            "unused_binding",
        );
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1004"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_unused_param() {
        let path = write_temp(
            "rule main(value: i64) -> i64:\n  yield 0.\nend\n",
            "unused_param",
        );
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1005"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_shadowing() {
        let source = "rule main() -> i64:\n  set count: i64 = 1.\n  when true:\n    set count: i64 = 2.\n    yield count.\n  otherwise:\n    yield count.\n  end\nend\n";
        let path = write_temp(source, "shadowing");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1006"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_unused_import() {
        let source = "import std::io.\n\nrule main() -> i64:\n  yield 0.\nend\n";
        let path = write_temp(source, "unused_import");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1007"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_constant_condition() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 0.\n  end\nend\n";
        let path = write_temp(source, "constant_condition");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1008"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_self_assignment() {
        let source = "rule main() -> i64:\n  set value: i64 = 1.\n  put value = value.\n  yield value.\nend\n";
        let path = write_temp(source, "self_assign");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1009"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_accepts_used_import() {
        let source =
            "import std::io.\n\nrule main() -> i64:\n  std::io::print(\"hi\").\n  yield 0.\nend\n";
        let path = write_temp(source, "used_import");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(!diagnostics.iter().any(|diag| diag.code == "L1007"));
        let _ = fs::remove_file(&path);
    }

    #[test]
    fn lint_warns_deep_nesting() {
        let source = "rule main() -> i64:\n  when true:\n    when true:\n      when true:\n        when true:\n          yield 0.\n        otherwise:\n          yield 0.\n        end\n      otherwise:\n        yield 0.\n      end\n    otherwise:\n      yield 0.\n    end\n  otherwise:\n    yield 0.\n  end\nend\n";
        let path = write_temp(source, "nesting");
        let program = parse_and_typecheck(&path).expect("parse");
        let diagnostics = lint_program(&program);
        assert!(diagnostics.iter().any(|diag| diag.code == "L1003"));
        let _ = fs::remove_file(&path);
    }
}
