use super::definitions::{build_symbol_index, SymbolIndex};
use super::imports::find_stdlib_root;
use super::server::{
    extract_range, extract_uri_and_position, position_leq, position_to_lsp, span_contains,
    uri_to_path, Server,
};
use super::stdlib::{stdlib_signatures, CallSignature};
use birddisk_core::ast::{Expr, ExprKind, Function, Program, Stmt, Type};
use birddisk_core::Position;
use serde_json::{json, Value};
use std::collections::HashMap;

impl Server {
    pub(crate) fn handle_signature_help(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return Value::Null;
        };
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match birddisk_core::lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Value::Null,
        };
        let program = match birddisk_core::parser::parse(&tokens) {
            Ok(program) => program,
            Err(_) => return Value::Null,
        };
        let stdlib_root = uri_to_path(&uri).and_then(|path| find_stdlib_root(&path));
        let stdlib = stdlib_signatures(&program, stdlib_root.as_deref());
        let index = build_symbol_index(&uri, &program);
        signature_help_at_position(&program, pos, &index, &stdlib).unwrap_or(Value::Null)
    }

    pub(crate) fn handle_inlay_hints(&self, params: Value) -> Value {
        let Some(uri) = super::server::extract_uri(&params) else {
            return Value::Null;
        };
        let Some(range) = extract_range(&params) else {
            return Value::Null;
        };
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match birddisk_core::lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Value::Null,
        };
        let program = match birddisk_core::parser::parse(&tokens) {
            Ok(program) => program,
            Err(_) => return Value::Null,
        };
        let hints = inlay_hints(&program, range, &uri);
        Value::Array(hints)
    }
}

#[derive(Clone)]
pub(crate) struct TypeEnv {
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub(crate) fn insert(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub(crate) fn get(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }
}

pub(crate) fn env_for_position(
    program: &Program,
    pos: Position,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> TypeEnv {
    for func in &program.functions {
        if span_contains(func.span, pos) {
            return env_for_function(func, index, stdlib);
        }
    }
    for book in &program.books {
        for method in &book.methods {
            if span_contains(method.span, pos) {
                return env_for_function(method, index, stdlib);
            }
        }
    }
    TypeEnv::new()
}

fn env_for_function(
    func: &Function,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> TypeEnv {
    let mut env = TypeEnv::new();
    for param in &func.params {
        env.insert(param.name.clone(), param.ty.clone());
    }
    for stmt in &func.body {
        update_env_from_stmt(stmt, &mut env, index, stdlib);
    }
    env
}

fn update_env_from_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) {
    match stmt {
        Stmt::Set { name, ty, expr, .. } => {
            let inferred = infer_expr_type(expr, env, index, stdlib);
            if let Some(ty) = ty {
                env.insert(name.clone(), ty.clone());
            } else if let Some(inferred) = inferred {
                env.insert(name.clone(), inferred);
            }
        }
        Stmt::Expr { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Put { name, expr, .. } => {
            let inferred = infer_expr_type(expr, env, index, stdlib);
            if env.get(name).is_none() {
                if let Some(inferred) = inferred {
                    env.insert(name.clone(), inferred);
                }
            }
        }
        Stmt::PutIndex {
            index: idx_expr,
            expr,
            ..
        } => {
            infer_expr_type(idx_expr, env, index, stdlib);
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::PutField { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Yield { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Throw { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Try {
            try_body,
            catch_name,
            catch_body,
            ..
        } => {
            let mut try_env = env.clone();
            try_env.push();
            for stmt in try_body {
                update_env_from_stmt(stmt, &mut try_env, index, stdlib);
            }
            let mut catch_env = env.clone();
            catch_env.push();
            catch_env.insert(catch_name.clone(), Type::String);
            for stmt in catch_body {
                update_env_from_stmt(stmt, &mut catch_env, index, stdlib);
            }
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            infer_expr_type(cond, env, index, stdlib);
            let mut then_env = env.clone();
            then_env.push();
            for stmt in then_body {
                update_env_from_stmt(stmt, &mut then_env, index, stdlib);
            }
            let mut else_env = env.clone();
            else_env.push();
            for stmt in else_body {
                update_env_from_stmt(stmt, &mut else_env, index, stdlib);
            }
        }
        Stmt::Repeat { cond, body, .. } => {
            infer_expr_type(cond, env, index, stdlib);
            let mut loop_env = env.clone();
            loop_env.push();
            for stmt in body {
                update_env_from_stmt(stmt, &mut loop_env, index, stdlib);
            }
        }
        Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => {
            infer_expr_type(expr, env, index, stdlib);
            for case in cases {
                let mut case_env = env.clone();
                case_env.push();
                for stmt in &case.body {
                    update_env_from_stmt(stmt, &mut case_env, index, stdlib);
                }
            }
            let mut else_env = env.clone();
            else_env.push();
            for stmt in otherwise {
                update_env_from_stmt(stmt, &mut else_env, index, stdlib);
            }
        }
    }
}

fn infer_expr_type(
    expr: &Expr,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<Type> {
    let mut hints = Vec::new();
    let range = birddisk_core::Span::new(Position::new(0, 0), Position::new(0, 0));
    collect_inlay_hints_in_expr(expr, env, index, stdlib, range, &mut hints)
}

pub(crate) fn inlay_hints(program: &Program, range: birddisk_core::Span, uri: &str) -> Vec<Value> {
    let index = SymbolIndex::new(program, uri);
    let stdlib_root = uri_to_path(uri).and_then(|path| find_stdlib_root(&path));
    let stdlib = stdlib_signatures(program, stdlib_root.as_deref());
    let mut hints = Vec::new();
    for func in &program.functions {
        let mut env = TypeEnv::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty.clone());
        }
        collect_inlay_hints_in_stmts(&func.body, &mut env, &index, &stdlib, range, &mut hints);
    }
    for book in &program.books {
        for method in &book.methods {
            let mut env = TypeEnv::new();
            for param in &method.params {
                env.insert(param.name.clone(), param.ty.clone());
            }
            collect_inlay_hints_in_stmts(
                &method.body,
                &mut env,
                &index,
                &stdlib,
                range,
                &mut hints,
            );
        }
    }
    hints
}

fn collect_inlay_hints_in_stmts(
    stmts: &[Stmt],
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: birddisk_core::Span,
    hints: &mut Vec<Value>,
) {
    for stmt in stmts {
        collect_inlay_hints_in_stmt(stmt, env, index, stdlib, range, hints);
    }
}

fn collect_inlay_hints_in_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: birddisk_core::Span,
    hints: &mut Vec<Value>,
) {
    match stmt {
        Stmt::Set { name, ty, expr, .. } => {
            let inferred = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            if let Some(ty) = ty {
                env.insert(name.clone(), ty.clone());
            } else if let Some(inferred) = inferred {
                env.insert(name.clone(), inferred);
            }
        }
        Stmt::Expr { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Put { name, expr, .. } => {
            let inferred = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            if env.get(name).is_none() {
                if let Some(inferred) = inferred {
                    env.insert(name.clone(), inferred);
                }
            }
        }
        Stmt::PutIndex {
            index: idx_expr,
            expr,
            ..
        } => {
            collect_inlay_hints_in_expr(idx_expr, env, index, stdlib, range, hints);
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::PutField { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Yield { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Throw { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Try {
            try_body,
            catch_name,
            catch_body,
            ..
        } => {
            let mut try_env = env.clone();
            try_env.push();
            collect_inlay_hints_in_stmts(try_body, &mut try_env, index, stdlib, range, hints);
            let mut catch_env = env.clone();
            catch_env.push();
            catch_env.insert(catch_name.clone(), Type::String);
            collect_inlay_hints_in_stmts(catch_body, &mut catch_env, index, stdlib, range, hints);
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            collect_inlay_hints_in_expr(cond, env, index, stdlib, range, hints);
            let mut then_env = env.clone();
            then_env.push();
            collect_inlay_hints_in_stmts(then_body, &mut then_env, index, stdlib, range, hints);
            let mut else_env = env.clone();
            else_env.push();
            collect_inlay_hints_in_stmts(else_body, &mut else_env, index, stdlib, range, hints);
        }
        Stmt::Repeat { cond, body, .. } => {
            collect_inlay_hints_in_expr(cond, env, index, stdlib, range, hints);
            let mut loop_env = env.clone();
            loop_env.push();
            collect_inlay_hints_in_stmts(body, &mut loop_env, index, stdlib, range, hints);
        }
        Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            for case in cases {
                let mut case_env = env.clone();
                case_env.push();
                collect_inlay_hints_in_stmts(
                    &case.body,
                    &mut case_env,
                    index,
                    stdlib,
                    range,
                    hints,
                );
            }
            let mut else_env = env.clone();
            else_env.push();
            collect_inlay_hints_in_stmts(otherwise, &mut else_env, index, stdlib, range, hints);
        }
    }
}

fn collect_inlay_hints_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: birddisk_core::Span,
    hints: &mut Vec<Value>,
) -> Option<Type> {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            for arg in args {
                collect_inlay_hints_in_expr(arg, env, index, stdlib, range, hints);
            }
            if let Some(signature) = resolve_call_signature(name, env, index, stdlib) {
                let count = std::cmp::min(args.len(), signature.params.len());
                for idx in 0..count {
                    let span = args[idx].span;
                    if !span_in_range(span, range) {
                        continue;
                    }
                    let label = format!("{}:", signature.params[idx]);
                    let hint = json!({
                        "position": position_to_lsp(span.start),
                        "label": label,
                        "kind": 2,
                        "paddingRight": true
                    });
                    hints.push(hint);
                }
                return Some(signature.return_type);
            }
            None
        }
        ExprKind::New { book, args } => {
            for arg in args {
                collect_inlay_hints_in_expr(arg, env, index, stdlib, range, hints);
            }
            Some(Type::Book(book.clone()))
        }
        ExprKind::ArrayLit(elements) => {
            let mut element_type = None;
            for element in elements {
                let ty = collect_inlay_hints_in_expr(element, env, index, stdlib, range, hints);
                match (element_type.as_ref(), ty) {
                    (None, Some(found)) => element_type = Some(found),
                    (Some(existing), Some(found)) if existing == &found => {}
                    _ => element_type = None,
                }
            }
            element_type.map(|inner| Type::Array(Box::new(inner)))
        }
        ExprKind::ArrayNew { len } => {
            collect_inlay_hints_in_expr(len, env, index, stdlib, range, hints);
            None
        }
        ExprKind::Index { base, index: idx } => {
            let base_ty = collect_inlay_hints_in_expr(base, env, index, stdlib, range, hints);
            collect_inlay_hints_in_expr(idx, env, index, stdlib, range, hints);
            match base_ty {
                Some(Type::Array(inner)) => Some(*inner),
                _ => None,
            }
        }
        ExprKind::MemberAccess { base, field } => {
            if let Some(Type::Book(book)) = env.get(base) {
                if let Some(book_info) = index.books.get(&book) {
                    if let Some(field_info) = book_info.fields.get(field) {
                        return Some(field_info.ty.clone());
                    }
                }
            }
            None
        }
        ExprKind::Cast { expr, ty } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            Some(ty.clone())
        }
        ExprKind::Unary { op, expr } => {
            let inner = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            match op {
                birddisk_core::ast::UnaryOp::Neg => {
                    if inner == Some(Type::F64) {
                        Some(Type::F64)
                    } else {
                        Some(Type::I64)
                    }
                }
                birddisk_core::ast::UnaryOp::Not => Some(Type::Bool),
            }
            .or(inner)
        }
        ExprKind::Binary { op, left, right } => {
            let left_ty = collect_inlay_hints_in_expr(left, env, index, stdlib, range, hints);
            let right_ty = collect_inlay_hints_in_expr(right, env, index, stdlib, range, hints);
            match op {
                birddisk_core::ast::BinaryOp::EqEq
                | birddisk_core::ast::BinaryOp::NotEq
                | birddisk_core::ast::BinaryOp::Lt
                | birddisk_core::ast::BinaryOp::LtEq
                | birddisk_core::ast::BinaryOp::Gt
                | birddisk_core::ast::BinaryOp::GtEq
                | birddisk_core::ast::BinaryOp::AndAnd
                | birddisk_core::ast::BinaryOp::OrOr => Some(Type::Bool),
                birddisk_core::ast::BinaryOp::Add
                | birddisk_core::ast::BinaryOp::Sub
                | birddisk_core::ast::BinaryOp::Mul
                | birddisk_core::ast::BinaryOp::Div
                | birddisk_core::ast::BinaryOp::Mod => {
                    if left_ty == Some(Type::F64) || right_ty == Some(Type::F64) {
                        Some(Type::F64)
                    } else if left_ty == Some(Type::I64) || right_ty == Some(Type::I64) {
                        Some(Type::I64)
                    } else {
                        None
                    }
                }
            }
        }
        ExprKind::Ident(name) => env.get(name),
        ExprKind::Int(_) => Some(Type::I64),
        ExprKind::Float(_) => Some(Type::F64),
        ExprKind::Bool(_) => Some(Type::Bool),
        ExprKind::String(_) => Some(Type::String),
    }
}

fn resolve_call_signature(
    name: &str,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<CallSignature> {
    if let Some(signature) = stdlib.get(name) {
        return Some(signature.clone());
    }
    if let Some(info) = index.functions.get(name) {
        return Some(CallSignature {
            params: info.params.iter().map(|(param, _)| param.clone()).collect(),
            return_type: info.return_type.clone(),
        });
    }
    if let Some((base, method)) = name.split_once("::") {
        if let Some(Type::Book(book_name)) = env.get(base) {
            if let Some(book_info) = index.books.get(&book_name) {
                if let Some(method_info) = book_info.methods.get(method) {
                    return Some(method_signature(method_info));
                }
            }
        }
        if let Some(book_info) = index.books.get(base) {
            if let Some(method_info) = book_info.methods.get(method) {
                return Some(method_signature(method_info));
            }
        }
    }
    None
}

fn method_signature(info: &super::definitions::FunctionInfo) -> CallSignature {
    let mut params: Vec<String> = info.params.iter().map(|(param, _)| param.clone()).collect();
    if params.first().map(|param| param == "self").unwrap_or(false) {
        params.remove(0);
    }
    CallSignature {
        params,
        return_type: info.return_type.clone(),
    }
}

fn span_in_range(span: birddisk_core::Span, range: birddisk_core::Span) -> bool {
    position_leq(range.start, span.start) && position_leq(span.start, range.end)
}

struct CallInfo {
    kind: CallKind,
    args: Vec<Expr>,
}

enum CallKind {
    Rule(String),
    New(String),
}

struct SignatureInfo {
    label: String,
    params: Vec<String>,
}

pub(crate) fn signature_help_at_position(
    program: &Program,
    pos: Position,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<Value> {
    let call = find_call_in_program(program, pos)?;
    let env = env_for_position(program, pos, index, stdlib);
    let signature = match &call.kind {
        CallKind::Rule(name) => resolve_signature_info(name, &env, index, stdlib)?,
        CallKind::New(book) => resolve_constructor_signature(book, index),
    };
    let active = active_param_index(&call.args, pos);
    let params: Vec<Value> = signature
        .params
        .iter()
        .map(|label| json!({"label": label}))
        .collect();
    Some(json!({
        "signatures": [{
            "label": signature.label,
            "parameters": params
        }],
        "activeSignature": 0,
        "activeParameter": active
    }))
}

fn find_call_in_program(program: &Program, pos: Position) -> Option<CallInfo> {
    for func in &program.functions {
        if span_contains(func.span, pos) {
            if let Some(call) = find_call_in_stmts(&func.body, pos) {
                return Some(call);
            }
        }
    }
    for book in &program.books {
        for method in &book.methods {
            if span_contains(method.span, pos) {
                if let Some(call) = find_call_in_stmts(&method.body, pos) {
                    return Some(call);
                }
            }
        }
    }
    None
}

fn find_call_in_stmts(stmts: &[Stmt], pos: Position) -> Option<CallInfo> {
    for stmt in stmts {
        if let Some(call) = find_call_in_stmt(stmt, pos) {
            return Some(call);
        }
    }
    None
}

fn find_call_in_stmt(stmt: &Stmt, pos: Position) -> Option<CallInfo> {
    match stmt {
        Stmt::Set { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Expr { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Put { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::PutIndex { index, expr, .. } => {
            find_call_in_expr(index, pos).or_else(|| find_call_in_expr(expr, pos))
        }
        Stmt::PutField { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Yield { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Throw { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => find_call_in_stmts(try_body, pos).or_else(|| find_call_in_stmts(catch_body, pos)),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => find_call_in_expr(cond, pos)
            .or_else(|| find_call_in_stmts(then_body, pos))
            .or_else(|| find_call_in_stmts(else_body, pos)),
        Stmt::Repeat { cond, body, .. } => {
            find_call_in_expr(cond, pos).or_else(|| find_call_in_stmts(body, pos))
        }
        Stmt::Match {
            expr,
            cases,
            otherwise,
            ..
        } => find_call_in_expr(expr, pos)
            .or_else(|| {
                cases
                    .iter()
                    .find_map(|case| find_call_in_stmts(&case.body, pos))
            })
            .or_else(|| find_call_in_stmts(otherwise, pos)),
    }
}

fn find_call_in_expr(expr: &Expr, pos: Position) -> Option<CallInfo> {
    if !span_contains(expr.span, pos) {
        return None;
    }
    match &expr.kind {
        ExprKind::Call { name, args } => {
            for arg in args {
                if let Some(call) = find_call_in_expr(arg, pos) {
                    return Some(call);
                }
            }
            Some(CallInfo {
                kind: CallKind::Rule(name.clone()),
                args: args.clone(),
            })
        }
        ExprKind::New { book, args } => {
            for arg in args {
                if let Some(call) = find_call_in_expr(arg, pos) {
                    return Some(call);
                }
            }
            Some(CallInfo {
                kind: CallKind::New(book.clone()),
                args: args.clone(),
            })
        }
        ExprKind::ArrayLit(elements) => elements
            .iter()
            .find_map(|element| find_call_in_expr(element, pos)),
        ExprKind::ArrayNew { len } => find_call_in_expr(len, pos),
        ExprKind::Index { base, index } => {
            find_call_in_expr(base, pos).or_else(|| find_call_in_expr(index, pos))
        }
        ExprKind::Unary { expr, .. } => find_call_in_expr(expr, pos),
        ExprKind::Binary { left, right, .. } => {
            find_call_in_expr(left, pos).or_else(|| find_call_in_expr(right, pos))
        }
        _ => None,
    }
}

fn active_param_index(args: &[Expr], pos: Position) -> usize {
    if args.is_empty() {
        return 0;
    }
    let mut count = 0;
    for arg in args {
        if position_leq(arg.span.end, pos) {
            count += 1;
        } else {
            break;
        }
    }
    if count >= args.len() {
        args.len() - 1
    } else {
        count
    }
}

fn resolve_signature_info(
    name: &str,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<SignatureInfo> {
    if let Some(signature) = stdlib.get(name) {
        let params = signature.params.clone();
        let label = format!(
            "rule {}({}) -> {}",
            name,
            params.join(", "),
            super::server::type_name(&signature.return_type)
        );
        return Some(SignatureInfo { label, params });
    }
    if let Some(info) = index.functions.get(name) {
        return Some(signature_from_function(info));
    }
    if let Some((base, method)) = name.split_once("::") {
        if let Some(Type::Book(book_name)) = env.get(base) {
            if let Some(book_info) = index.books.get(&book_name) {
                if let Some(method_info) = book_info.methods.get(method) {
                    return Some(signature_from_method(&book_info.name, method_info));
                }
            }
        }
        if let Some(book_info) = index.books.get(base) {
            if let Some(method_info) = book_info.methods.get(method) {
                return Some(signature_from_method(&book_info.name, method_info));
            }
        }
    }
    None
}

fn resolve_constructor_signature(book: &str, index: &SymbolIndex) -> SignatureInfo {
    let mut params = Vec::new();
    let mut return_type = Type::Book(book.to_string());
    if let Some(book_info) = index.books.get(book) {
        if let Some(method_info) = book_info.methods.get("init") {
            params = method_info
                .params
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, super::server::type_name(ty)))
                .collect();
            if params
                .first()
                .map(|param| param.starts_with("self:"))
                .unwrap_or(false)
            {
                params.remove(0);
            }
            return_type = method_info.return_type.clone();
        }
    }
    let label = format!(
        "new {}({}) -> {}",
        book,
        params.join(", "),
        super::server::type_name(&return_type)
    );
    SignatureInfo { label, params }
}

fn signature_from_function(info: &super::definitions::FunctionInfo) -> SignatureInfo {
    let params: Vec<String> = info
        .params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, super::server::type_name(ty)))
        .collect();
    let label = format!(
        "rule {}({}) -> {}",
        info.name,
        params.join(", "),
        super::server::type_name(&info.return_type)
    );
    SignatureInfo { label, params }
}

fn signature_from_method(
    book_name: &str,
    info: &super::definitions::FunctionInfo,
) -> SignatureInfo {
    let mut params: Vec<String> = info
        .params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, super::server::type_name(ty)))
        .collect();
    if params
        .first()
        .map(|param| param.starts_with("self:"))
        .unwrap_or(false)
    {
        params.remove(0);
    }
    let label = format!(
        "rule {}::{}({}) -> {}",
        book_name,
        info.name,
        params.join(", "),
        super::server::type_name(&info.return_type)
    );
    SignatureInfo { label, params }
}
