use crate::ast::{BinaryOp, Expr, ExprKind, Function, Program, Stmt, Type, UnaryOp};
use std::collections::HashMap;

#[derive(Clone)]
struct InlineFn {
    params: Vec<String>,
    expr: Expr,
}

pub fn optimize_program(program: &mut Program) {
    let inline_map = collect_inline_candidates(program);
    for func in program.functions.iter_mut() {
        optimize_function(func, &inline_map);
    }
    for book in program.books.iter_mut() {
        for method in book.methods.iter_mut() {
            optimize_function(method, &inline_map);
        }
    }
}

fn optimize_function(func: &mut Function, inline_map: &HashMap<String, InlineFn>) {
    optimize_block(&mut func.body, inline_map);
}

fn optimize_block(body: &mut Vec<Stmt>, inline_map: &HashMap<String, InlineFn>) {
    let mut out = Vec::with_capacity(body.len());
    let mut terminated = false;
    for stmt in body.drain(..) {
        if terminated {
            break;
        }
        let mut optimized = optimize_stmt(stmt, inline_map);
        for stmt in optimized.drain(..) {
            let is_terminal = matches!(stmt, Stmt::Yield { .. } | Stmt::Throw { .. });
            out.push(stmt);
            if is_terminal {
                terminated = true;
                break;
            }
        }
    }
    *body = out;
}

fn optimize_stmt(stmt: Stmt, inline_map: &HashMap<String, InlineFn>) -> Vec<Stmt> {
    match stmt {
        Stmt::Set {
            name,
            ty,
            mut expr,
            span,
        } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::Set { name, ty, expr, span }]
        }
        Stmt::Expr { mut expr, span } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::Expr { expr, span }]
        }
        Stmt::Put { name, mut expr, span } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::Put { name, expr, span }]
        }
        Stmt::PutIndex {
            name,
            mut index,
            mut expr,
            span,
        } => {
            optimize_expr(&mut index, inline_map);
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::PutIndex {
                name,
                index,
                expr,
                span,
            }]
        }
        Stmt::PutField {
            base,
            field,
            mut expr,
            span,
        } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::PutField {
                base,
                field,
                expr,
                span,
            }]
        }
        Stmt::Yield { mut expr, span } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::Yield { expr, span }]
        }
        Stmt::Throw { mut expr, span } => {
            optimize_expr(&mut expr, inline_map);
            vec![Stmt::Throw { expr, span }]
        }
        Stmt::Try {
            mut try_body,
            catch_name,
            mut catch_body,
            span,
        } => {
            optimize_block(&mut try_body, inline_map);
            optimize_block(&mut catch_body, inline_map);
            vec![Stmt::Try {
                try_body,
                catch_name,
                catch_body,
                span,
            }]
        }
        Stmt::When {
            mut cond,
            span,
            mut then_body,
            mut else_body,
        } => {
            optimize_expr(&mut cond, inline_map);
            optimize_block(&mut then_body, inline_map);
            optimize_block(&mut else_body, inline_map);
            if let ExprKind::Bool(value) = cond.kind {
                if value {
                    else_body.clear();
                } else {
                    then_body.clear();
                }
            }
            vec![Stmt::When {
                cond,
                span,
                then_body,
                else_body,
            }]
        }
        Stmt::Repeat {
            mut cond,
            span,
            mut body,
        } => {
            optimize_expr(&mut cond, inline_map);
            optimize_block(&mut body, inline_map);
            if matches!(cond.kind, ExprKind::Bool(false)) {
                Vec::new()
            } else {
                vec![Stmt::Repeat { cond, span, body }]
            }
        }
        Stmt::Match {
            mut expr,
            mut cases,
            mut otherwise,
            span,
        } => {
            optimize_expr(&mut expr, inline_map);
            for case in &mut cases {
                optimize_block(&mut case.body, inline_map);
            }
            optimize_block(&mut otherwise, inline_map);
            vec![Stmt::Match {
                expr,
                cases,
                otherwise,
                span,
            }]
        }
    }
}

fn optimize_expr(expr: &mut Expr, inline_map: &HashMap<String, InlineFn>) {
    match &mut expr.kind {
        ExprKind::Call { name, args } => {
            for arg in args.iter_mut() {
                optimize_expr(arg, inline_map);
            }
            if let Some(inline) = inline_map.get(name) {
                if inline.params.len() == args.len() {
                    let mut bindings = HashMap::new();
                    for (param, arg) in inline.params.iter().zip(args.iter()) {
                        bindings.insert(param.clone(), arg.clone());
                    }
                    if let Some(mut inlined) = inline_expr(&inline.expr, &bindings) {
                        optimize_expr(&mut inlined, inline_map);
                        *expr = inlined;
                    }
                }
            }
        }
        ExprKind::New { args, .. } => {
            for arg in args {
                optimize_expr(arg, inline_map);
            }
        }
        ExprKind::ArrayLit(elements) => {
            for element in elements {
                optimize_expr(element, inline_map);
            }
        }
        ExprKind::ArrayNew { len } => optimize_expr(len, inline_map),
        ExprKind::Index { base, index } => {
            optimize_expr(base, inline_map);
            optimize_expr(index, inline_map);
        }
        ExprKind::Unary { op, expr: inner } => {
            optimize_expr(inner, inline_map);
            if let Some(kind) = fold_unary(*op, inner) {
                expr.kind = kind;
            }
        }
        ExprKind::Cast { expr: inner, ty } => {
            optimize_expr(inner, inline_map);
            if let Some(kind) = fold_cast(ty, inner) {
                expr.kind = kind;
            }
        }
        ExprKind::Binary { left, op, right } => {
            optimize_expr(left, inline_map);
            optimize_expr(right, inline_map);
            if let Some(kind) = fold_binary(*op, left, right) {
                expr.kind = kind;
            }
        }
        _ => {}
    }
}

fn collect_inline_candidates(program: &Program) -> HashMap<String, InlineFn> {
    let mut map = HashMap::new();
    for func in &program.functions {
        if matches!(func.return_type, Type::Void) {
            continue;
        }
        if func.body.len() != 1 {
            continue;
        }
        let Stmt::Yield { expr, .. } = &func.body[0] else {
            continue;
        };
        if !is_inline_expr(expr) {
            continue;
        }
        let params = func.params.iter().map(|param| param.name.clone()).collect();
        map.insert(
            func.name.clone(),
            InlineFn {
                params,
                expr: expr.clone(),
            },
        );
    }
    map
}

fn is_inline_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { .. } => false,
        ExprKind::MemberAccess { .. } => false,
        ExprKind::New { args, .. } => args.iter().all(is_inline_expr),
        ExprKind::ArrayLit(elements) => elements.iter().all(is_inline_expr),
        ExprKind::ArrayNew { len } => is_inline_expr(len),
        ExprKind::Index { base, index } => is_inline_expr(base) && is_inline_expr(index),
        ExprKind::Unary { expr, .. } => is_inline_expr(expr),
        ExprKind::Cast { expr, .. } => is_inline_expr(expr),
        ExprKind::Binary { left, right, .. } => {
            is_inline_expr(left) && is_inline_expr(right)
        }
        _ => true,
    }
}

fn inline_expr(template: &Expr, bindings: &HashMap<String, Expr>) -> Option<Expr> {
    let mut out = template.clone();
    if replace_idents(&mut out, bindings) {
        Some(out)
    } else {
        None
    }
}

fn replace_idents(expr: &mut Expr, bindings: &HashMap<String, Expr>) -> bool {
    match &mut expr.kind {
        ExprKind::Ident(name) => {
            if let Some(value) = bindings.get(name) {
                *expr = value.clone();
            }
            true
        }
        ExprKind::MemberAccess { base, .. } => {
            if let Some(value) = bindings.get(base) {
                if let ExprKind::Ident(name) = &value.kind {
                    *base = name.clone();
                    true
                } else {
                    false
                }
            } else {
                true
            }
        }
        ExprKind::Call { args, .. } => args.iter_mut().all(|arg| replace_idents(arg, bindings)),
        ExprKind::New { args, .. } => args.iter_mut().all(|arg| replace_idents(arg, bindings)),
        ExprKind::ArrayLit(elements) => elements
            .iter_mut()
            .all(|element| replace_idents(element, bindings)),
        ExprKind::ArrayNew { len } => replace_idents(len, bindings),
        ExprKind::Index { base, index } => {
            replace_idents(base, bindings) && replace_idents(index, bindings)
        }
        ExprKind::Unary { expr, .. } => replace_idents(expr, bindings),
        ExprKind::Cast { expr, .. } => replace_idents(expr, bindings),
        ExprKind::Binary { left, right, .. } => {
            replace_idents(left, bindings) && replace_idents(right, bindings)
        }
        _ => true,
    }
}

fn fold_unary(op: UnaryOp, expr: &Expr) -> Option<ExprKind> {
    match (op, &expr.kind) {
        (UnaryOp::Neg, ExprKind::Int(value)) => value.checked_neg().map(ExprKind::Int),
        (UnaryOp::Neg, ExprKind::Float(value)) => Some(ExprKind::Float(-value)),
        (UnaryOp::Not, ExprKind::Bool(value)) => Some(ExprKind::Bool(!value)),
        _ => None,
    }
}

fn fold_cast(target: &Type, expr: &Expr) -> Option<ExprKind> {
    match (target, &expr.kind) {
        (Type::F64, ExprKind::Int(value)) => Some(ExprKind::Float(*value as f64)),
        (Type::I64, ExprKind::Float(value)) => {
            if !value.is_finite() {
                return None;
            }
            let min = i64::MIN as f64;
            let max = i64::MAX as f64;
            if *value < min || *value > max {
                return None;
            }
            Some(ExprKind::Int(value.trunc() as i64))
        }
        _ => None,
    }
}

fn fold_binary(op: BinaryOp, left: &Expr, right: &Expr) -> Option<ExprKind> {
    match (&left.kind, &right.kind) {
        (ExprKind::Int(left), ExprKind::Int(right)) => fold_i64(op, *left, *right),
        (ExprKind::Float(left), ExprKind::Float(right)) => fold_f64(op, *left, *right),
        (ExprKind::Bool(left), ExprKind::Bool(right)) => fold_bool(op, *left, *right),
        _ => None,
    }
}

fn fold_i64(op: BinaryOp, left: i64, right: i64) -> Option<ExprKind> {
    match op {
        BinaryOp::Add => left.checked_add(right).map(ExprKind::Int),
        BinaryOp::Sub => left.checked_sub(right).map(ExprKind::Int),
        BinaryOp::Mul => left.checked_mul(right).map(ExprKind::Int),
        BinaryOp::Div => {
            if right == 0 {
                None
            } else {
                left.checked_div(right).map(ExprKind::Int)
            }
        }
        BinaryOp::Mod => {
            if right == 0 {
                None
            } else {
                left.checked_rem(right).map(ExprKind::Int)
            }
        }
        BinaryOp::EqEq => Some(ExprKind::Bool(left == right)),
        BinaryOp::NotEq => Some(ExprKind::Bool(left != right)),
        BinaryOp::Lt => Some(ExprKind::Bool(left < right)),
        BinaryOp::LtEq => Some(ExprKind::Bool(left <= right)),
        BinaryOp::Gt => Some(ExprKind::Bool(left > right)),
        BinaryOp::GtEq => Some(ExprKind::Bool(left >= right)),
        _ => None,
    }
}

fn fold_f64(op: BinaryOp, left: f64, right: f64) -> Option<ExprKind> {
    match op {
        BinaryOp::Add => Some(ExprKind::Float(left + right)),
        BinaryOp::Sub => Some(ExprKind::Float(left - right)),
        BinaryOp::Mul => Some(ExprKind::Float(left * right)),
        BinaryOp::Div => {
            if right == 0.0 {
                None
            } else {
                Some(ExprKind::Float(left / right))
            }
        }
        BinaryOp::Mod => {
            if right == 0.0 {
                None
            } else {
                Some(ExprKind::Float(left % right))
            }
        }
        BinaryOp::EqEq => Some(ExprKind::Bool(left == right)),
        BinaryOp::NotEq => Some(ExprKind::Bool(left != right)),
        BinaryOp::Lt => Some(ExprKind::Bool(left < right)),
        BinaryOp::LtEq => Some(ExprKind::Bool(left <= right)),
        BinaryOp::Gt => Some(ExprKind::Bool(left > right)),
        BinaryOp::GtEq => Some(ExprKind::Bool(left >= right)),
        _ => None,
    }
}

fn fold_bool(op: BinaryOp, left: bool, right: bool) -> Option<ExprKind> {
    match op {
        BinaryOp::EqEq => Some(ExprKind::Bool(left == right)),
        BinaryOp::NotEq => Some(ExprKind::Bool(left != right)),
        BinaryOp::AndAnd => Some(ExprKind::Bool(left && right)),
        BinaryOp::OrOr => Some(ExprKind::Bool(left || right)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, ExprKind};
    use crate::diagnostics::{Position, Span};

    fn span() -> Span {
        Span::new(Position::new(0, 0), Position::new(0, 0))
    }

    #[test]
    fn folds_simple_i64_binary() {
        let mut expr = Expr {
            kind: ExprKind::Binary {
                left: Box::new(Expr {
                    kind: ExprKind::Int(2),
                    span: span(),
                }),
                op: BinaryOp::Add,
                right: Box::new(Expr {
                    kind: ExprKind::Int(3),
                    span: span(),
                }),
            },
            span: span(),
        };
        let inline_map = HashMap::new();
        optimize_expr(&mut expr, &inline_map);
        assert!(matches!(expr.kind, ExprKind::Int(5)));
    }

    #[test]
    fn avoids_div_by_zero_fold() {
        let mut expr = Expr {
            kind: ExprKind::Binary {
                left: Box::new(Expr {
                    kind: ExprKind::Int(10),
                    span: span(),
                }),
                op: BinaryOp::Div,
                right: Box::new(Expr {
                    kind: ExprKind::Int(0),
                    span: span(),
                }),
            },
            span: span(),
        };
        let inline_map = HashMap::new();
        optimize_expr(&mut expr, &inline_map);
        match expr.kind {
            ExprKind::Binary { .. } => {}
            _ => panic!("division by zero should not fold"),
        }
    }

    #[test]
    fn folds_bool_and() {
        let mut expr = Expr {
            kind: ExprKind::Binary {
                left: Box::new(Expr {
                    kind: ExprKind::Bool(true),
                    span: span(),
                }),
                op: BinaryOp::AndAnd,
                right: Box::new(Expr {
                    kind: ExprKind::Bool(false),
                    span: span(),
                }),
            },
            span: span(),
        };
        let inline_map = HashMap::new();
        optimize_expr(&mut expr, &inline_map);
        assert!(matches!(expr.kind, ExprKind::Bool(false)));
    }

    #[test]
    fn inlines_simple_call() {
        let inc = Function {
            name: "inc".to_string(),
            params: vec![crate::ast::Param {
                name: "x".to_string(),
                ty: Type::I64,
                span: span(),
            }],
            return_type: Type::I64,
            body: vec![Stmt::Yield {
                expr: Expr {
                    kind: ExprKind::Binary {
                        left: Box::new(Expr {
                            kind: ExprKind::Ident("x".to_string()),
                            span: span(),
                        }),
                        op: BinaryOp::Add,
                        right: Box::new(Expr {
                            kind: ExprKind::Int(1),
                            span: span(),
                        }),
                    },
                    span: span(),
                },
                span: span(),
            }],
            span: span(),
            file: "main.bd".to_string(),
            source: String::new(),
        };
        let mut main = Function {
            name: "main".to_string(),
            params: Vec::new(),
            return_type: Type::I64,
            body: vec![Stmt::Yield {
                expr: Expr {
                    kind: ExprKind::Call {
                        name: "inc".to_string(),
                        args: vec![Expr {
                            kind: ExprKind::Int(2),
                            span: span(),
                        }],
                    },
                    span: span(),
                },
                span: span(),
            }],
            span: span(),
            file: "main.bd".to_string(),
            source: String::new(),
        };
        let mut program = Program {
            imports: Vec::new(),
            enums: Vec::new(),
            books: Vec::new(),
            functions: vec![inc, main.clone()],
        };
        optimize_program(&mut program);
        main = program.functions[1].clone();
        let Stmt::Yield { expr, .. } = &main.body[0] else {
            panic!("expected yield");
        };
        assert!(matches!(expr.kind, ExprKind::Int(3)));
    }

    #[test]
    fn drops_statements_after_yield() {
        let mut func = Function {
            name: "main".to_string(),
            params: Vec::new(),
            return_type: Type::I64,
            body: vec![
                Stmt::Yield {
                    expr: Expr {
                        kind: ExprKind::Int(1),
                        span: span(),
                    },
                    span: span(),
                },
                Stmt::Set {
                    name: "x".to_string(),
                    ty: None,
                    expr: Expr {
                        kind: ExprKind::Int(2),
                        span: span(),
                    },
                    span: span(),
                },
            ],
            span: span(),
            file: "main.bd".to_string(),
            source: String::new(),
        };
        let inline_map = HashMap::new();
        optimize_function(&mut func, &inline_map);
        assert_eq!(func.body.len(), 1);
        assert!(matches!(func.body[0], Stmt::Yield { .. }));
    }
}
