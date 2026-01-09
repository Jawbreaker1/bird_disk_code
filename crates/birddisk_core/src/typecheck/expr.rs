use super::{notes_with_suggestion, type_mismatch, Checker, Ty};
use crate::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use crate::diagnostics::{diagnostic, Span};

impl<'a> Checker<'a> {
    pub(super) fn check_expr(&mut self, expr: &Expr) -> Ty {
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

    pub(super) fn check_int_literal_expected(&mut self, value: i64, span: Span, expected: &Ty) -> Ty {
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

    pub(super) fn check_array_literal_expected(&mut self, elements: &[Expr], span: Span, expected: &Ty) -> Ty {
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

    pub(super) fn check_array_new(&mut self, len: &Expr, expected: Option<&Ty>, span: Span) -> Ty {
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

    pub(super) fn check_array_literal(&mut self, elements: &[Expr], span: Span) -> Ty {
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

    pub(super) fn check_index(&mut self, span: Span, base: &Expr, index: &Expr) -> Ty {
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

    pub(super) fn check_member_access(&mut self, span: Span, base: &str, field: &str) -> Ty {
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

    pub(super) fn check_new(&mut self, span: Span, book: &str, args: &[Expr]) -> Ty {
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

    pub(super) fn check_call(&mut self, span: Span, name: &str, args: &[Expr]) -> Ty {
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

    pub(super) fn check_binary(&mut self, _span: Span, op: BinaryOp, left: &Expr, right: &Expr) -> Ty {
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

    pub(super) fn check_binary_operands(
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
}
