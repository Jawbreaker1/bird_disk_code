use crate::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, Type, UnaryOp};
use crate::diagnostics::{diagnostic, Diagnostic, Edit, FixIt, Position, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    I64,
    Bool,
    String,
    U8,
    Array(Box<Ty>),
    Book(String),
    Unknown,
}

impl Ty {
    fn from_ast(ty: Type) -> Self {
        match ty {
            Type::I64 => Ty::I64,
            Type::Bool => Ty::Bool,
            Type::String => Ty::String,
            Type::U8 => Ty::U8,
            Type::Array(inner) => Ty::Array(Box::new(Ty::from_ast(*inner))),
            Type::Book(name) => Ty::Book(name),
        }
    }

    fn name(&self) -> String {
        match self {
            Ty::I64 => "i64".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::String => "string".to_string(),
            Ty::U8 => "u8".to_string(),
            Ty::Array(inner) => format!("{}[]", inner.name()),
            Ty::Book(name) => name.clone(),
            Ty::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Ty>,
    return_type: Ty,
}

#[derive(Clone)]
struct BookInfo {
    fields: HashMap<String, Ty>,
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
    books: HashMap<String, BookInfo>,
    scopes: Vec<HashMap<String, Ty>>,
    current_return: Ty,
}

impl<'a> Checker<'a> {
    fn new(file: &'a str) -> Self {
        Self {
            file,
            diagnostics: Vec::new(),
            functions: HashMap::new(),
            books: HashMap::new(),
            scopes: Vec::new(),
            current_return: Ty::Unknown,
        }
    }

    fn collect_signatures(&mut self, program: &Program) {
        self.collect_books(program);
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
            let params = function
                .params
                .iter()
                .map(|p| {
                    let ty = Ty::from_ast(p.ty.clone());
                    self.validate_type(&ty, p.span);
                    ty
                })
                .collect();
            let sig = FunctionSig {
                params,
                return_type: {
                    let ty = Ty::from_ast(function.return_type.clone());
                    self.validate_type(&ty, function.span);
                    ty
                },
            };
            self.functions.insert(function.name.clone(), sig);
        }
        self.register_stdlib(program);
    }

    fn collect_books(&mut self, program: &Program) {
        let mut seen = HashSet::new();
        for book in &program.books {
            if !seen.insert(book.name.clone()) {
                self.diagnostics.push(diagnostic(
                    "E0307",
                    "error",
                    format!("Duplicate book '{}'.", book.name),
                    self.file,
                    book.span,
                    vec!["Book names must be unique.".to_string()],
                    vec!["SPEC.md#13-objects".to_string()],
                    Vec::new(),
                    None,
                ));
                continue;
            }
            self.books
                .insert(book.name.clone(), BookInfo { fields: HashMap::new() });
        }

        let mut processed = HashSet::new();
        for book in &program.books {
            if !processed.insert(book.name.clone()) {
                continue;
            }
            let mut fields = HashMap::new();
            for field in &book.fields {
                if fields.contains_key(&field.name) {
                    self.diagnostics.push(diagnostic(
                        "E0307",
                        "error",
                        format!("Duplicate field '{}' in book '{}'.", field.name, book.name),
                        self.file,
                        field.span,
                        vec!["Field names must be unique within a book.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                    continue;
                }
                let ty = Ty::from_ast(field.ty.clone());
                if matches!(ty, Ty::Book(_)) {
                    self.diagnostics.push(diagnostic(
                        "E0300",
                        "error",
                        format!(
                            "Field '{}' in book '{}' cannot be a book type yet.",
                            field.name, book.name
                        ),
                        self.file,
                        field.span,
                        vec!["Book-typed fields are not supported in v0.1.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                }
                fields.insert(field.name.clone(), ty);
            }

            for method in &book.methods {
                let full_name = format!("{}::{}", book.name, method.name);
                if self.functions.contains_key(&full_name) {
                    self.diagnostics.push(diagnostic(
                        "E0307",
                        "error",
                        format!("Duplicate method '{}'.", full_name),
                        self.file,
                        method.span,
                        vec!["Method names must be unique within a book.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                    continue;
                }
                if let Some(first) = method.params.first() {
                    if first.name != "self" {
                        self.diagnostics.push(diagnostic(
                            "E0300",
                            "error",
                            format!(
                                "First parameter of method '{}' must be 'self'.",
                                full_name
                            ),
                            self.file,
                            first.span,
                            vec!["Methods must take self as the first parameter.".to_string()],
                            vec!["SPEC.md#13-objects".to_string()],
                            Vec::new(),
                            None,
                        ));
                    }
                    let expected = Ty::Book(book.name.clone());
                    let actual = Ty::from_ast(first.ty.clone());
                    if actual != expected {
                        self.diagnostics.push(type_mismatch(
                            self.file,
                            first.span,
                            expected,
                            actual,
                        ));
                    }
                } else {
                    self.diagnostics.push(diagnostic(
                        "E0300",
                        "error",
                        format!(
                            "Method '{}' must declare self as the first parameter.",
                            full_name
                        ),
                        self.file,
                        method.span,
                        vec!["Methods must take self as the first parameter.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                }
                let params = method
                    .params
                    .iter()
                    .map(|p| {
                        let ty = Ty::from_ast(p.ty.clone());
                        self.validate_type(&ty, p.span);
                        ty
                    })
                    .collect();
                let sig = FunctionSig {
                    params,
                    return_type: {
                        let ty = Ty::from_ast(method.return_type.clone());
                        self.validate_type(&ty, method.span);
                        ty
                    },
                };
                self.functions.insert(full_name, sig);
            }

            self.books.insert(book.name.clone(), BookInfo { fields });
        }
    }

    fn register_stdlib(&mut self, program: &Program) {
        let has_std_string = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "string"
        });
        let has_std_bytes = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "bytes"
        });
        let has_std_io = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "io"
        });
        if has_std_string {
            self.insert_function(
                "std::string::len",
                vec![Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::string::concat",
                vec![Ty::String, Ty::String],
                Ty::String,
            );
            self.insert_function(
                "std::string::eq",
                vec![Ty::String, Ty::String],
                Ty::Bool,
            );
            self.insert_function(
                "std::string::bytes",
                vec![Ty::String],
                Ty::Array(Box::new(Ty::U8)),
            );
            self.insert_function(
                "std::string::from_bytes",
                vec![Ty::Array(Box::new(Ty::U8))],
                Ty::String,
            );
            self.insert_function(
                "std::string::to_i64",
                vec![Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::string::from_i64",
                vec![Ty::I64],
                Ty::String,
            );
        }
        if has_std_bytes {
            let bytes = Ty::Array(Box::new(Ty::U8));
            self.insert_function("std::bytes::len", vec![bytes.clone()], Ty::I64);
            self.insert_function(
                "std::bytes::eq",
                vec![bytes.clone(), bytes],
                Ty::Bool,
            );
        }
        if has_std_io {
            self.insert_function("std::io::print", vec![Ty::String], Ty::I64);
            self.insert_function("std::io::read_line", Vec::new(), Ty::String);
        }
    }

    fn insert_function(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.functions.entry(name.to_string()).or_insert(FunctionSig {
            params,
            return_type,
        });
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
        for book in &program.books {
            for method in &book.methods {
                self.check_function(method);
            }
        }
    }

    fn check_function(&mut self, function: &crate::ast::Function) {
        self.scopes.clear();
        self.push_scope();
        self.current_return = Ty::from_ast(function.return_type.clone());

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
                self.scopes[0].insert(param.name.clone(), Ty::from_ast(param.ty.clone()));
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
                let mut skip_infer_error = false;
                let expr_ty = match (&expr.kind, ty.as_ref()) {
                    (ExprKind::ArrayNew { len }, _) => {
                        if ty.is_none() {
                            skip_infer_error = true;
                        }
                        let expected = ty.as_ref().map(|ty| Ty::from_ast(ty.clone()));
                        self.check_array_new(len, expected.as_ref(), *span)
                    }
                    (ExprKind::ArrayLit(elements), Some(ty)) => {
                        self.check_array_literal_expected(elements, expr.span, &Ty::from_ast(ty.clone()))
                    }
                    (ExprKind::Int(value), Some(ty)) => {
                        self.check_int_literal_expected(*value, expr.span, &Ty::from_ast(ty.clone()))
                    }
                    _ => self.check_expr(expr),
                };
                let bound_ty = if let Some(ty) = ty {
                    let annotated = Ty::from_ast(ty.clone());
                    self.validate_type(&annotated, *span);
                    if expr_ty != Ty::Unknown && expr_ty != annotated {
                        self.diagnostics.push(type_mismatch(
                            self.file,
                            *span,
                            annotated.clone(),
                            expr_ty.clone(),
                        ));
                    }
                    annotated
                } else if expr_ty == Ty::Unknown {
                    if skip_infer_error {
                        self.current_scope_mut().insert(name.clone(), Ty::Unknown);
                        return;
                    }
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
                let expr_ty = match &expr.kind {
                    ExprKind::ArrayNew { len } => {
                        let expected = self.lookup(name);
                        self.check_array_new(len, expected.as_ref(), *span)
                    }
                    ExprKind::ArrayLit(elements) => match self.lookup(name) {
                        Some(expected) => {
                            self.check_array_literal_expected(elements, expr.span, &expected)
                        }
                        None => self.check_expr(expr),
                    },
                    ExprKind::Int(value) => match self.lookup(name) {
                        Some(expected) => {
                            self.check_int_literal_expected(*value, expr.span, &expected)
                        }
                        None => self.check_expr(expr),
                    },
                    _ => self.check_expr(expr),
                };
                match self.lookup(name) {
                    Some(expected) => {
                        if expr_ty != Ty::Unknown && expected != expr_ty {
                            self.diagnostics.push(type_mismatch(
                                self.file,
                                *span,
                                expected.clone(),
                                expr_ty.clone(),
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
            Stmt::PutIndex {
                name,
                index,
                expr,
                span,
            } => {
                let expected = match self.lookup(name) {
                    Some(expected) => expected,
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
                        return;
                    }
                };

                let elem_ty = match &expected {
                    Ty::Array(elem) => *elem.clone(),
                    _ => {
                        self.diagnostics.push(diagnostic(
                            "E0300",
                            "error",
                            "Index assignment requires array type.".to_string(),
                            self.file,
                            *span,
                            vec!["Use `name[index] = value` only on arrays.".to_string()],
                            vec!["SPEC.md#8-arrays".to_string()],
                            Vec::new(),
                            None,
                        ));
                        return;
                    }
                };

                let index_ty = self.check_expr(index);
                if index_ty != Ty::Unknown && index_ty != Ty::I64 {
                    self.diagnostics.push(type_mismatch(
                        self.file,
                        index.span,
                        Ty::I64,
                        index_ty,
                    ));
                }

                let expr_ty = match (&expr.kind, &elem_ty) {
                    (ExprKind::Int(value), Ty::U8) => {
                        self.check_int_literal_expected(*value, expr.span, &elem_ty)
                    }
                    _ => self.check_expr(expr),
                };
                if expr_ty != Ty::Unknown && expr_ty != elem_ty {
                    self.diagnostics.push(type_mismatch(
                        self.file,
                        expr.span,
                        elem_ty,
                        expr_ty,
                    ));
                }
            }
            Stmt::PutField {
                base,
                field,
                expr,
                span,
            } => {
                let expr_ty = self.check_expr(expr);
                let Some(base_ty) = self.lookup(base) else {
                    let suggestion = self.suggest_name(base);
                    let notes = notes_with_suggestion(
                        vec!["Declare it with `set` before use.".to_string()],
                        suggestion,
                    );
                    self.diagnostics.push(diagnostic(
                        "E0301",
                        "error",
                        format!("Unknown name '{base}'."),
                        self.file,
                        *span,
                        notes,
                        vec!["SPEC.md#3-names-scope-and-shadowing".to_string()],
                        Vec::new(),
                        None,
                    ));
                    return;
                };
                let Ty::Book(book_name) = base_ty else {
                    self.diagnostics.push(diagnostic(
                        "E0300",
                        "error",
                        "Field assignment requires a book instance.".to_string(),
                        self.file,
                        *span,
                        vec!["Use `book` types with `obj::field`.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                    return;
                };
                let Some(book) = self.books.get(&book_name) else {
                    self.diagnostics.push(diagnostic(
                        "E0300",
                        "error",
                        format!("Unknown book '{book_name}'."),
                        self.file,
                        *span,
                        vec!["Define the book before use.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                    return;
                };
                let Some(field_ty) = book.fields.get(field) else {
                    self.diagnostics.push(diagnostic(
                        "E0301",
                        "error",
                        format!("Unknown field '{field}' on '{book_name}'."),
                        self.file,
                        *span,
                        vec!["Check the field name.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                    return;
                };
                if expr_ty != Ty::Unknown && *field_ty != expr_ty {
                    self.diagnostics.push(type_mismatch(
                        self.file,
                        *span,
                        field_ty.clone(),
                        expr_ty,
                    ));
                }
            }
            Stmt::Yield { expr, span } => {
                let expected = self.current_return.clone();
                let expr_ty = match (&expr.kind, &expected) {
                    (ExprKind::Int(value), Ty::U8) => {
                        self.check_int_literal_expected(*value, expr.span, &expected)
                    }
                    (ExprKind::ArrayLit(elements), Ty::Array(_)) => {
                        self.check_array_literal_expected(elements, expr.span, &expected)
                    }
                    _ => self.check_expr(expr),
                };
                if expr_ty != Ty::Unknown && self.current_return != expr_ty {
                    self.diagnostics.push(type_mismatch(
                        self.file,
                        *span,
                        self.current_return.clone(),
                        expr_ty.clone(),
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
            ExprKind::String(_) => Ty::String,
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
            ExprKind::New { book, args } => self.check_new(expr.span, book, args),
            ExprKind::MemberAccess { base, field } => {
                self.check_member_access(expr.span, base, field)
            }
            ExprKind::ArrayLit(elements) => self.check_array_literal(elements, expr.span),
            ExprKind::ArrayNew { .. } => {
                self.diagnostics.push(diagnostic(
                    "E0310",
                    "error",
                    "Array constructor requires explicit array type.".to_string(),
                    self.file,
                    expr.span,
                    vec!["Use `set name: i64[] = array(len).`".to_string()],
                    vec!["SPEC.md#8-arrays".to_string()],
                    Vec::new(),
                    None,
                ));
                Ty::Unknown
            }
            ExprKind::Index { base, index } => self.check_index(expr.span, base, index),
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
                                other.clone(),
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
                                other.clone(),
                            ));
                            Ty::Unknown
                        }
                    },
                }
            }
            ExprKind::Binary { left, op, right } => self.check_binary(expr.span, *op, left, right),
        }
    }

    fn check_int_literal_expected(&mut self, value: i64, span: Span, expected: &Ty) -> Ty {
        if matches!(expected, Ty::U8) {
            if (0..=u8::MAX as i64).contains(&value) {
                Ty::U8
            } else {
                self.diagnostics.push(diagnostic(
                    "E0311",
                    "error",
                    "u8 literal out of range (0..255).".to_string(),
                    self.file,
                    span,
                    vec!["Use a value between 0 and 255.".to_string()],
                    vec!["SPEC.md#2-types".to_string()],
                    Vec::new(),
                    None,
                ));
                Ty::Unknown
            }
        } else {
            Ty::I64
        }
    }

    fn check_array_literal_expected(&mut self, elements: &[Expr], span: Span, expected: &Ty) -> Ty {
        let expected_elem = match expected {
            Ty::Array(elem) => elem.as_ref().clone(),
            _ => return self.check_array_literal(elements, span),
        };

        if elements.is_empty() {
            return Ty::Array(Box::new(expected_elem));
        }

        for element in elements {
            let actual = match (&element.kind, &expected_elem) {
                (ExprKind::Int(value), Ty::U8) => {
                    self.check_int_literal_expected(*value, element.span, &expected_elem)
                }
                (ExprKind::ArrayLit(inner), Ty::Array(_)) => {
                    self.check_array_literal_expected(inner, element.span, &expected_elem)
                }
                _ => self.check_expr(element),
            };
            if actual != Ty::Unknown && actual != expected_elem {
                self.diagnostics.push(type_mismatch(
                    self.file,
                    element.span,
                    expected_elem.clone(),
                    actual,
                ));
            }
        }

        Ty::Array(Box::new(expected_elem))
    }

    fn check_array_new(&mut self, len: &Expr, expected: Option<&Ty>, span: Span) -> Ty {
        let expected = match expected {
            Some(Ty::Array(elem)) => Some(elem.as_ref()),
            Some(_) => {
                self.diagnostics.push(diagnostic(
                    "E0310",
                    "error",
                    "Array constructor requires array type.".to_string(),
                    self.file,
                    span,
                    vec!["Use `set name: i64[] = array(len).`".to_string()],
                    vec!["SPEC.md#8-arrays".to_string()],
                    Vec::new(),
                    None,
                ));
                return Ty::Unknown;
            }
            None => {
                self.diagnostics.push(diagnostic(
                    "E0310",
                    "error",
                    "Array constructor requires explicit array type.".to_string(),
                    self.file,
                    span,
                    vec!["Use `set name: i64[] = array(len).`".to_string()],
                    vec!["SPEC.md#8-arrays".to_string()],
                    Vec::new(),
                    None,
                ));
                return Ty::Unknown;
            }
        };

        let len_ty = self.check_expr(len);
        if len_ty != Ty::Unknown && len_ty != Ty::I64 {
            self.diagnostics
                .push(type_mismatch(self.file, len.span, Ty::I64, len_ty));
        }

        match expected {
            Some(elem) => Ty::Array(Box::new(elem.clone())),
            None => Ty::Unknown,
        }
    }

    fn check_array_literal(&mut self, elements: &[Expr], span: Span) -> Ty {
        if elements.is_empty() {
            return Ty::Unknown;
        }

        let first_ty = self.check_expr(&elements[0]);
        if first_ty == Ty::Unknown {
            for element in elements.iter().skip(1) {
                self.check_expr(element);
            }
            return Ty::Unknown;
        }

        let mut ok = true;
        for element in elements.iter().skip(1) {
            let ty = self.check_expr(element);
            if ty != Ty::Unknown && ty != first_ty {
                ok = false;
                self.diagnostics
                    .push(type_mismatch(self.file, element.span, first_ty.clone(), ty));
            }
        }

        if ok {
            Ty::Array(Box::new(first_ty))
        } else {
            self.diagnostics.push(diagnostic(
                "E0300",
                "error",
                "Array literal elements must have the same type.".to_string(),
                self.file,
                span,
                vec!["Ensure all elements have the same type.".to_string()],
                vec!["SPEC.md#8-arrays".to_string()],
                Vec::new(),
                None,
            ));
            Ty::Unknown
        }
    }

    fn check_index(&mut self, span: Span, base: &Expr, index: &Expr) -> Ty {
        let base_ty = self.check_expr(base);
        let index_ty = self.check_expr(index);

        if index_ty != Ty::Unknown && index_ty != Ty::I64 {
            self.diagnostics
                .push(type_mismatch(self.file, index.span, Ty::I64, index_ty));
        }

        match base_ty {
            Ty::Array(elem) => *elem,
            Ty::Unknown => Ty::Unknown,
            _ => {
                self.diagnostics.push(diagnostic(
                    "E0300",
                    "error",
                    "Indexing requires array type.".to_string(),
                    self.file,
                    span,
                    vec!["Use `name[index]` only on arrays.".to_string()],
                    vec!["SPEC.md#8-arrays".to_string()],
                    Vec::new(),
                    None,
                ));
                Ty::Unknown
            }
        }
    }

    fn check_member_access(&mut self, span: Span, base: &str, field: &str) -> Ty {
        let Some(base_ty) = self.lookup(base) else {
            let suggestion = self.suggest_name(base);
            let notes = notes_with_suggestion(
                vec!["Declare it with `set` before use.".to_string()],
                suggestion,
            );
            self.diagnostics.push(diagnostic(
                "E0301",
                "error",
                format!("Unknown name '{base}'."),
                self.file,
                span,
                notes,
                vec!["SPEC.md#3-names-scope-and-shadowing".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };
        let Ty::Book(book_name) = base_ty else {
            self.diagnostics.push(diagnostic(
                "E0300",
                "error",
                "Field access requires a book instance.".to_string(),
                self.file,
                span,
                vec!["Use `obj::field` with a book value.".to_string()],
                vec!["SPEC.md#13-objects".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };
        let Some(book) = self.books.get(&book_name) else {
            self.diagnostics.push(diagnostic(
                "E0300",
                "error",
                format!("Unknown book '{book_name}'."),
                self.file,
                span,
                vec!["Define the book before use.".to_string()],
                vec!["SPEC.md#13-objects".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };
        let Some(field_ty) = book.fields.get(field) else {
            self.diagnostics.push(diagnostic(
                "E0301",
                "error",
                format!("Unknown field '{field}' on '{book_name}'."),
                self.file,
                span,
                vec!["Check the field name.".to_string()],
                vec!["SPEC.md#13-objects".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };
        field_ty.clone()
    }

    fn check_new(&mut self, span: Span, book: &str, args: &[Expr]) -> Ty {
        let Some(book_info) = self.books.get(book) else {
            let suggestion = self.suggest_book(book);
            let notes = notes_with_suggestion(
                vec!["Define the book before using `new`.".to_string()],
                suggestion,
            );
            self.diagnostics.push(diagnostic(
                "E0301",
                "error",
                format!("Unknown book '{book}'."),
                self.file,
                span,
                notes,
                vec!["SPEC.md#13-objects".to_string()],
                Vec::new(),
                None,
            ));
            return Ty::Unknown;
        };
        let _ = book_info;
        let init_name = format!("{book}::init");
        if let Some(sig) = self.functions.get(&init_name).cloned() {
            if sig.params.is_empty() {
                self.diagnostics.push(diagnostic(
                    "E0300",
                    "error",
                    format!("Constructor '{init_name}' must take self."),
                    self.file,
                    span,
                    vec!["Add a self parameter to the init method.".to_string()],
                    vec!["SPEC.md#13-objects".to_string()],
                    Vec::new(),
                    None,
                ));
            } else {
                let expected_args = sig.params.len() - 1;
                if expected_args != args.len() {
                    self.diagnostics.push(diagnostic(
                        "E0302",
                        "error",
                        format!(
                            "Wrong number of arguments: expected {}, got {}.",
                            expected_args,
                            args.len()
                        ),
                        self.file,
                        span,
                        vec!["Argument count must match the constructor signature.".to_string()],
                        vec!["SPEC.md#13-objects".to_string()],
                        Vec::new(),
                        None,
                    ));
                }
                for (arg, expected) in args.iter().zip(sig.params.iter().skip(1)) {
                    let actual = self.check_expr(arg);
                    if actual != Ty::Unknown && expected != &actual {
                        self.diagnostics.push(type_mismatch(
                            self.file,
                            arg.span,
                            expected.clone(),
                            actual,
                        ));
                    }
                }
            }
            let expected = Ty::Book(book.to_string());
            if sig.return_type != expected {
                self.diagnostics.push(type_mismatch(
                    self.file,
                    span,
                    expected.clone(),
                    sig.return_type,
                ));
            }
            expected
        } else {
            if !args.is_empty() {
                self.diagnostics.push(diagnostic(
                    "E0303",
                    "error",
                    format!("Missing constructor '{init_name}'."),
                    self.file,
                    span,
                    vec!["Define `rule init(self: Book, ...) -> Book`.".to_string()],
                    vec!["SPEC.md#13-objects".to_string()],
                    Vec::new(),
                    None,
                ));
                return Ty::Unknown;
            }
            Ty::Book(book.to_string())
        }
    }

    fn check_call(&mut self, span: Span, name: &str, args: &[Expr]) -> Ty {
        if let Some((base, method)) = name.split_once("::") {
            if base != "std" {
                if let Some(Ty::Book(book_name)) = self.lookup(base) {
                    let full_name = format!("{book_name}::{method}");
                    let Some(sig) = self.functions.get(&full_name).cloned() else {
                        self.diagnostics.push(diagnostic(
                            "E0303",
                            "error",
                            format!("Unknown method '{full_name}'."),
                            self.file,
                            span,
                            vec!["Define the method on the book.".to_string()],
                            vec!["SPEC.md#13-objects".to_string()],
                            Vec::new(),
                            None,
                        ));
                        return Ty::Unknown;
                    };
                    if sig.params.is_empty() {
                        self.diagnostics.push(diagnostic(
                            "E0300",
                            "error",
                            format!("Method '{full_name}' must take self."),
                            self.file,
                            span,
                            vec!["Add a self parameter to the method.".to_string()],
                            vec!["SPEC.md#13-objects".to_string()],
                            Vec::new(),
                            None,
                        ));
                    }
                    let expected_args = sig.params.len().saturating_sub(1);
                    if expected_args != args.len() {
                        self.diagnostics.push(diagnostic(
                            "E0302",
                            "error",
                            format!(
                                "Wrong number of arguments: expected {}, got {}.",
                                expected_args,
                                args.len()
                            ),
                            self.file,
                            span,
                            vec!["Argument count must match the method signature.".to_string()],
                            vec!["SPEC.md#13-objects".to_string()],
                            Vec::new(),
                            None,
                        ));
                        return sig.return_type;
                    }
                    for (arg, expected) in args.iter().zip(sig.params.iter().skip(1)) {
                        let actual = match (&arg.kind, expected) {
                            (ExprKind::Int(value), Ty::U8) => {
                                self.check_int_literal_expected(*value, arg.span, expected)
                            }
                            (ExprKind::ArrayLit(elements), Ty::Array(_)) => {
                                self.check_array_literal_expected(elements, arg.span, expected)
                            }
                            _ => self.check_expr(arg),
                        };
                        if actual != Ty::Unknown && expected != &actual {
                            self.diagnostics.push(type_mismatch(
                                self.file,
                                arg.span,
                                expected.clone(),
                                actual,
                            ));
                        }
                    }
                    return sig.return_type;
                }
            }
        }
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
            let actual = match (&arg.kind, expected) {
                (ExprKind::Int(value), Ty::U8) => {
                    self.check_int_literal_expected(*value, arg.span, expected)
                }
                (ExprKind::ArrayLit(elements), Ty::Array(_)) => {
                    self.check_array_literal_expected(elements, arg.span, expected)
                }
                _ => self.check_expr(arg),
            };
            if actual != Ty::Unknown && expected != &actual {
                self.diagnostics.push(type_mismatch(
                    self.file,
                    arg.span,
                    expected.clone(),
                    actual,
                ));
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
                .push(type_mismatch(self.file, left.span, expected.clone(), left_ty));
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
                return Some(ty.clone());
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

    fn suggest_book(&self, name: &str) -> Option<String> {
        best_suggestion(name, self.books.keys().map(|key| key.as_str()))
    }

    fn validate_type(&mut self, ty: &Ty, span: Span) {
        match ty {
            Ty::Array(inner) => self.validate_type(inner, span),
            Ty::Book(name) => {
                if self.books.contains_key(name) {
                    return;
                }
                let suggestion = self.suggest_book(name);
                let notes = notes_with_suggestion(
                    vec!["Define the book before using it as a type.".to_string()],
                    suggestion,
                );
                self.diagnostics.push(diagnostic(
                    "E0301",
                    "error",
                    format!("Unknown book type '{name}'."),
                    self.file,
                    span,
                    notes,
                    vec!["SPEC.md#13-objects".to_string()],
                    Vec::new(),
                    None,
                ));
            }
            _ => {}
        }
    }
}

fn type_mismatch(file: &str, span: Span, expected: Ty, actual: Ty) -> Diagnostic {
    diagnostic(
        "E0300",
        "error",
        format!(
            "Type mismatch: expected {}, got {}.",
            expected.name(),
            actual.name()
        ),
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
        Stmt::Set { .. }
        | Stmt::Put { .. }
        | Stmt::PutIndex { .. }
        | Stmt::PutField { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parser, parse_and_typecheck};
    use std::path::PathBuf;

    fn check(source: &str) -> Vec<Diagnostic> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        typecheck(&program, "test.bd")
    }

    fn fixture_path(rel: &str) -> PathBuf {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.pop();
        root.pop();
        root.push(rel);
        root
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

    #[test]
    fn typecheck_infers_array_literal() {
        let diags = check("rule main() -> i64:\n  set xs = [1, 2].\n  yield xs[1].\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_empty_array_without_annotation() {
        let diags = check("rule main() -> i64:\n  set xs = [].\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0305"));
    }

    #[test]
    fn typecheck_rejects_array_constructor_without_annotation() {
        let diags = check("rule main() -> i64:\n  set xs = array(3).\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0310"));
    }

    #[test]
    fn typecheck_rejects_array_constructor_non_i64_len() {
        let diags = check("rule main() -> i64:\n  set xs: i64[] = array(true).\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_std_string_without_import() {
        let diags = check(
            "rule main() -> i64:\n  set s: string = \"hi\".\n  yield std::string::len(s).\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_std_string_import() {
        let diags = check(
            "import std::string.\nrule main() -> i64:\n  set s: string = \"hi\".\n  yield std::string::len(s).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_u8_literal_in_context() {
        let diags = check("rule main() -> i64:\n  set b: u8 = 255.\n  set xs: u8[] = [0, 1, 2].\n  yield 0.\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_u8_out_of_range() {
        let diags = check("rule main() -> i64:\n  set b: u8 = 256.\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0311"));
    }

    #[test]
    fn typecheck_accepts_bytes_module() {
        let diags = check(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set bs: u8[] = std::string::bytes(\"hi\").\n  yield std::bytes::len(bs).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_string_from_bytes() {
        let diags = check(
            "import std::string.\nrule main() -> i64:\n  set bs: u8[] = [104, 105].\n  set text: string = std::string::from_bytes(bs).\n  yield std::string::len(text).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_std_io_without_import() {
        let diags = check(
            "rule main() -> i64:\n  yield std::io::print(\"hi\").\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_std_io_import() {
        let diags = check(
            "import std::io.\nrule main() -> i64:\n  yield std::io::print(\"hi\").\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_index_type_fixture() {
        let path = fixture_path("vm_error_tests/arrays/array_index_type_error.bd");
        let diags = parse_and_typecheck(path.to_str().unwrap()).unwrap_err();
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_unknown_book_type() {
        let diags = check("rule main(b: Ghost) -> i64:\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0301"));
    }

    #[test]
    fn typecheck_rejects_method_without_self() {
        let diags = check(
            "book Thing:\n  rule inc(x: i64) -> i64:\n    yield x.\n  end\nend\n\nrule main() -> i64:\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_method_wrong_self_type() {
        let diags = check(
            "book Thing:\n  rule inc(self: i64) -> i64:\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_unknown_field_access() {
        let diags = check(
            "book Thing:\n  field x: i64.\nend\n\nrule main() -> i64:\n  set t: Thing = new Thing().\n  yield t::y.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0301"));
    }

    #[test]
    fn typecheck_rejects_missing_constructor_args() {
        let diags = check(
            "book Thing:\n  field x: i64.\nend\n\nrule main() -> i64:\n  set t: Thing = new Thing(1).\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }
}
