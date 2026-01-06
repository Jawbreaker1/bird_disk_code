use crate::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, Type, UnaryOp};
use crate::diagnostics::{diagnostic, Diagnostic, Edit, FixIt, Position, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Ty {
    I64,
    Bool,
    Unknown,
}

impl Ty {
    fn from_ast(ty: Type) -> Self {
        match ty {
            Type::I64 => Ty::I64,
            Type::Bool => Ty::Bool,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Ty::I64 => "i64",
            Ty::Bool => "bool",
            Ty::Unknown => "unknown",
        }
    }
}

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Ty>,
    return_type: Ty,
}

pub fn typecheck(program: &Program, file: &str) -> Vec<Diagnostic> {
    let mut checker = Checker::new(file);
    checker.collect_signatures(program);
    checker.check_program(program);
    checker.diagnostics
}

struct Checker<'a> {
    file: &'a str,
    diagnostics: Vec<Diagnostic>,
    functions: HashMap<String, FunctionSig>,
    scopes: Vec<HashMap<String, Ty>>,
    current_return: Ty,
}

impl<'a> Checker<'a> {
    fn new(file: &'a str) -> Self {
        Self {
            file,
            diagnostics: Vec::new(),
            functions: HashMap::new(),
            scopes: Vec::new(),
            current_return: Ty::Unknown,
        }
    }

    fn collect_signatures(&mut self, program: &Program) {
        for function in &program.functions {
            if self.functions.contains_key(&function.name) {
                self.diagnostics.push(diagnostic(
                    "E0307",
                    "error",
                    format!("Duplicate function '{}'.", function.name),
                    self.file,
                    function.span,
                    vec!["Function names must be unique.".to_string()],
                    vec!["SPEC.md#4-functions".to_string()],
                    Vec::new(),
                    None,
                ));
                continue;
            }
            let params = function.params.iter().map(|p| Ty::from_ast(p.ty)).collect();
            let sig = FunctionSig {
                params,
                return_type: Ty::from_ast(function.return_type),
            };
            self.functions.insert(function.name.clone(), sig);
        }
    }

    fn check_program(&mut self, program: &Program) {
        if !self.functions.contains_key("main") {
            let default_pos = Position::new(1, 1);
            let span = program
                .functions
                .first()
                .map(|func| func.span)
                .unwrap_or_else(|| Span::new(default_pos, default_pos));
            let fixits = vec![FixIt {
                title: "Add a default main function".to_string(),
                edits: vec![Edit {
                    file: self.file.to_string(),
                    span: Span::new(default_pos, default_pos),
                    replacement: "rule main() -> i64:\n  yield 0.\nend\n\n".to_string(),
                }],
            }];
            self.diagnostics.push(diagnostic(
                "E0309",
                "error",
                "Missing 'main' entry point.".to_string(),
                self.file,
                span,
                vec!["Add `rule main() -> i64:` to define the entry point.".to_string()],
                vec!["SPEC.md#1-program-structure".to_string()],
                fixits,
                None,
            ));
        }
        for function in &program.functions {
            self.check_function(function);
        }
    }

    fn check_function(&mut self, function: &crate::ast::Function) {
        self.scopes.clear();
        self.push_scope();
        self.current_return = Ty::from_ast(function.return_type);

        for param in &function.params {
            if self.scopes[0].contains_key(&param.name) {
                self.diagnostics.push(diagnostic(
                    "E0308",
                    "error",
                    format!("Duplicate parameter '{}'.", param.name),
                    self.file,
                    param.span,
                    vec!["Parameter names must be unique.".to_string()],
                    vec!["SPEC.md#4-functions".to_string()],
                    Vec::new(),
                    None,
                ));
            } else {
                self.scopes[0].insert(param.name.clone(), Ty::from_ast(param.ty));
            }
        }

        for stmt in &function.body {
            self.check_stmt(stmt);
        }

        if !block_always_yields(&function.body) {
            self.diagnostics.push(diagnostic(
                "E0306",
                "error",
                "Not all control-flow paths yield a value.".to_string(),
                self.file,
                function.span,
                vec!["Ensure every path ends with `yield`.".to_string()],
                vec!["SPEC.md#4-functions".to_string()],
                Vec::new(),
                None,
            ));
        }

        self.pop_scope();
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Set { name, ty, expr, span } => {
                let expr_ty = self.check_expr(expr);
                let bound_ty = if let Some(ty) = ty {
                    let annotated = Ty::from_ast(*ty);
                    if expr_ty != Ty::Unknown && expr_ty != annotated {
                        self.diagnostics.push(type_mismatch(
                            self.file,
                            *span,
                            annotated,
                            expr_ty,
                        ));
                    }
                    annotated
                } else if expr_ty == Ty::Unknown {
                    self.diagnostics.push(diagnostic(
                        "E0305",
                        "error",
                        format!("Cannot infer type for '{name}'."),
                        self.file,
                        *span,
                        vec!["Add an explicit type annotation.".to_string()],
                        vec!["SPEC.md#7-type-inference-v0-1".to_string()],
                        Vec::new(),
                        None,
                    ));
                    Ty::Unknown
                } else {
                    expr_ty
                };
                self.current_scope_mut().insert(name.clone(), bound_ty);
            }
            Stmt::Put { name, expr, span } => {
                let expr_ty = self.check_expr(expr);
                match self.lookup(name) {
                    Some(expected) => {
                        if expr_ty != Ty::Unknown && expected != expr_ty {
                            self.diagnostics.push(type_mismatch(
                                self.file,
                                *span,
                                expected,
                                expr_ty,
                            ));
                        }
                    }
                    None => {
                        let suggestion = self.suggest_name(name);
                        let notes = notes_with_suggestion(
                            vec!["Declare it with `set` before use.".to_string()],
                            suggestion,
                        );
                        self.diagnostics.push(diagnostic(
                            "E0301",
                            "error",
                            format!("Unknown name '{name}'."),
                            self.file,
                            *span,
                            notes,
                            vec!["SPEC.md#3-names-scope-and-shadowing".to_string()],
                            Vec::new(),
                            None,
                        ));
                    }
                }
            }
            Stmt::Yield { expr, span } => {
                let expr_ty = self.check_expr(expr);
                if expr_ty != Ty::Unknown && self.current_return != expr_ty {
                    self.diagnostics.push(type_mismatch(
                        self.file,
                        *span,
                        self.current_return,
                        expr_ty,
                    ));
                }
            }
            Stmt::When {
                cond,
                span,
                then_body,
                else_body,
            } => {
                let cond_ty = self.check_expr(cond);
                if cond_ty != Ty::Unknown && cond_ty != Ty::Bool {
                    self.diagnostics.push(diagnostic(
                        "E0304",
                        "error",
                        "Condition must be bool.".to_string(),
                        self.file,
                        cond.span,
                        vec!["when conditions require bool.".to_string()],
                        vec!["SPEC.md#5-4-conditional-when-otherwise-end".to_string()],
                        Vec::new(),
                        None,
                    ));
                }

                self.push_scope();
                for stmt in then_body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();

                self.push_scope();
                for stmt in else_body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();

                let _ = span;
            }
            Stmt::Repeat { cond, body, .. } => {
                let cond_ty = self.check_expr(cond);
                if cond_ty != Ty::Unknown && cond_ty != Ty::Bool {
                    self.diagnostics.push(diagnostic(
                        "E0304",
                        "error",
                        "Condition must be bool.".to_string(),
                        self.file,
                        cond.span,
                        vec!["repeat while conditions require bool.".to_string()],
                        vec!["SPEC.md#5-5-loop-repeat-while-end".to_string()],
                        Vec::new(),
                        None,
                    ));
                }
                self.push_scope();
                for stmt in body {
                    self.check_stmt(stmt);
                }
                self.pop_scope();
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Int(_) => Ty::I64,
            ExprKind::Bool(_) => Ty::Bool,
            ExprKind::Ident(name) => self.lookup(name).unwrap_or_else(|| {
                let suggestion = self.suggest_name(name);
                let notes = notes_with_suggestion(
                    vec!["Declare it with `set` before use.".to_string()],
                    suggestion,
                );
                self.diagnostics.push(diagnostic(
                    "E0301",
                    "error",
                    format!("Unknown name '{name}'."),
                    self.file,
                    expr.span,
                    notes,
                    vec!["SPEC.md#3-names-scope-and-shadowing".to_string()],
                    Vec::new(),
                    None,
                ));
                Ty::Unknown
            }),
            ExprKind::Call { name, args } => self.check_call(expr.span, name, args),
            ExprKind::Unary { op, expr: inner } => {
                let inner_ty = self.check_expr(inner);
                match op {
                    UnaryOp::Neg => match inner_ty {
                        Ty::I64 => Ty::I64,
                        Ty::Unknown => Ty::Unknown,
                        other => {
                            self.diagnostics.push(type_mismatch(
                                self.file,
                                expr.span,
                                Ty::I64,
                                other,
                            ));
                            Ty::Unknown
                        }
                    },
                    UnaryOp::Not => match inner_ty {
                        Ty::Bool => Ty::Bool,
                        Ty::Unknown => Ty::Unknown,
                        other => {
                            self.diagnostics.push(type_mismatch(
                                self.file,
                                expr.span,
                                Ty::Bool,
                                other,
                            ));
                            Ty::Unknown
                        }
                    },
                }
            }
            ExprKind::Binary { left, op, right } => self.check_binary(expr.span, *op, left, right),
        }
    }

    fn check_call(&mut self, span: Span, name: &str, args: &[Expr]) -> Ty {
        let Some(sig) = self.functions.get(name).cloned() else {
            let suggestion = self.suggest_function(name);
            let notes = notes_with_suggestion(
                vec!["Define the function before calling it.".to_string()],
                suggestion,
            );
            self.diagnostics.push(diagnostic(
                "E0303",
                "error",
                format!("Unknown function '{name}'."),
                self.file,
                span,
                notes,
                vec!["SPEC.md#6-5-function-calls".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };

        if sig.params.len() != args.len() {
            self.diagnostics.push(diagnostic(
                "E0302",
                "error",
                format!(
                    "Wrong number of arguments: expected {}, got {}.",
                    sig.params.len(),
                    args.len()
                ),
                self.file,
                span,
                vec!["Argument count must match the function signature.".to_string()],
                vec!["SPEC.md#6-5-function-calls".to_string()],
                Vec::new(),
                None,
            ));
            return sig.return_type;
        }

        for (arg, expected) in args.iter().zip(sig.params.iter()) {
            let actual = self.check_expr(arg);
            if actual != Ty::Unknown && *expected != actual {
                self.diagnostics.push(type_mismatch(self.file, arg.span, *expected, actual));
            }
        }

        sig.return_type
    }

    fn check_binary(&mut self, _span: Span, op: BinaryOp, left: &Expr, right: &Expr) -> Ty {
        let left_ty = self.check_expr(left);
        let right_ty = self.check_expr(right);

        if left_ty == Ty::Unknown || right_ty == Ty::Unknown {
            return Ty::Unknown;
        }

        match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod => {
                if !self.check_binary_operands(Ty::I64, left, left_ty, right, right_ty) {
                    return Ty::Unknown;
                }
                Ty::I64
            }
            BinaryOp::EqEq
            | BinaryOp::NotEq
            | BinaryOp::Lt
            | BinaryOp::LtEq
            | BinaryOp::Gt
            | BinaryOp::GtEq => {
                if !self.check_binary_operands(Ty::I64, left, left_ty, right, right_ty) {
                    return Ty::Unknown;
                }
                Ty::Bool
            }
            BinaryOp::AndAnd | BinaryOp::OrOr => {
                if !self.check_binary_operands(Ty::Bool, left, left_ty, right, right_ty) {
                    return Ty::Unknown;
                }
                Ty::Bool
            }
        }
    }

    fn check_binary_operands(
        &mut self,
        expected: Ty,
        left: &Expr,
        left_ty: Ty,
        right: &Expr,
        right_ty: Ty,
    ) -> bool {
        let mut ok = true;
        if left_ty != expected {
            self.diagnostics
                .push(type_mismatch(self.file, left.span, expected, left_ty));
            ok = false;
        }
        if right_ty != expected {
            self.diagnostics
                .push(type_mismatch(self.file, right.span, expected, right_ty));
            ok = false;
        }
        ok
    }

    fn lookup(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(*ty);
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Ty> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn suggest_name(&self, name: &str) -> Option<String> {
        let mut candidates = HashSet::new();
        for scope in &self.scopes {
            for key in scope.keys() {
                candidates.insert(key.as_str());
            }
        }
        best_suggestion(name, candidates.into_iter())
    }

    fn suggest_function(&self, name: &str) -> Option<String> {
        best_suggestion(name, self.functions.keys().map(|key| key.as_str()))
    }
}

fn type_mismatch(file: &str, span: Span, expected: Ty, actual: Ty) -> Diagnostic {
    diagnostic(
        "E0300",
        "error",
        format!("Type mismatch: expected {}, got {}.", expected.name(), actual.name()),
        file,
        span,
        vec!["Expression type must match the expected type.".to_string()],
        vec!["SPEC.md#2-types".to_string()],
        Vec::new(),
        None,
    )
}

fn notes_with_suggestion(mut notes: Vec<String>, suggestion: Option<String>) -> Vec<String> {
    if let Some(suggestion) = suggestion {
        notes.push(format!("Did you mean `{suggestion}`?"));
    }
    notes
}

fn best_suggestion<'a>(name: &str, candidates: impl Iterator<Item = &'a str>) -> Option<String> {
    let mut best: Option<(&str, usize)> = None;
    for candidate in candidates {
        if candidate == name {
            return Some(candidate.to_string());
        }
        let distance = edit_distance(name, candidate);
        match best {
            Some((_, best_distance)) if distance >= best_distance => {}
            _ => best = Some((candidate, distance)),
        }
    }

    let (candidate, distance) = best?;
    if distance <= max_suggestion_distance(name.len()) {
        Some(candidate.to_string())
    } else {
        None
    }
}

fn max_suggestion_distance(len: usize) -> usize {
    if len <= 4 {
        1
    } else if len <= 8 {
        2
    } else {
        3
    }
}

fn edit_distance(a: &str, b: &str) -> usize {
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut curr = vec![0; b.len() + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1)
                .min(curr[j] + 1)
                .min(prev[j] + cost);
        }
        prev.clone_from_slice(&curr);
    }

    prev[b.len()]
}

fn block_always_yields(stmts: &[Stmt]) -> bool {
    for stmt in stmts {
        if stmt_always_yields(stmt) {
            return true;
        }
    }
    false
}

fn stmt_always_yields(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Yield { .. } => true,
        Stmt::When {
            then_body,
            else_body,
            ..
        } => block_always_yields(then_body) && block_always_yields(else_body),
        Stmt::Repeat { .. } => false,
        Stmt::Set { .. } | Stmt::Put { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser};

    fn check(source: &str) -> Vec<Diagnostic> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        typecheck(&program, "test.bd")
    }

    #[test]
    fn typecheck_simple_ok() {
        let diags = check("rule main() -> i64:\n  set x = 1.\n  yield x.\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_non_bool_condition() {
        let diags = check("rule main() -> i64:\n  when 1:\n    yield 0.\n  otherwise:\n    yield 1.\n  end\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0304"));
    }

    #[test]
    fn typecheck_rejects_wrong_arity() {
        let diags = check(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0302"));
    }

    #[test]
    fn typecheck_suggests_unknown_name() {
        let diags = check(
            "rule main() -> i64:\n  set count = 1.\n  yield coun.\nend\n",
        );
        let diag = diags.iter().find(|d| d.code == "E0301").unwrap();
        assert!(diag.notes.iter().any(|note| note.contains("count")));
    }

    #[test]
    fn typecheck_suggests_unknown_function() {
        let diags = check(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield ad(1, 2).\nend\n",
        );
        let diag = diags.iter().find(|d| d.code == "E0303").unwrap();
        assert!(diag.notes.iter().any(|note| note.contains("add")));
    }

    #[test]
    fn typecheck_rejects_missing_main() {
        let diags = check("rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n");
        let diag = diags.iter().find(|d| d.code == "E0309");
        assert!(diag.is_some());
        assert!(!diag.unwrap().fixits.is_empty());
    }

    #[test]
    fn typecheck_rejects_missing_yield() {
        let diags = check("rule main() -> i64:\n  set x = 1.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0306"));
    }

    #[test]
    fn typecheck_accepts_yield_after_when() {
        let diags = check(
            "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    set x = 2.\n  end\n  yield 3.\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_reports_rhs_mismatch_in_bool_op() {
        let diags = check("rule main() -> i64:\n  yield true && 1.\nend\n");
        assert!(diags.iter().any(|d| {
            d.code == "E0300"
                && d.message.contains("expected bool")
                && d.message.contains("got i64")
        }));
    }

    #[test]
    fn typecheck_reports_rhs_mismatch_in_arithmetic_op() {
        let diags = check("rule main() -> i64:\n  yield 1 + true.\nend\n");
        assert!(diags.iter().any(|d| {
            d.code == "E0300"
                && d.message.contains("expected i64")
                && d.message.contains("got bool")
        }));
    }
}
