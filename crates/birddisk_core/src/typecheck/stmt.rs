use super::{notes_with_suggestion, type_mismatch, Checker, Ty};
use crate::ast::{ExprKind, Stmt};
use crate::diagnostics::diagnostic;

impl<'a> Checker<'a> {
    pub(super) fn check_stmt(&mut self, stmt: &Stmt) {
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
}
