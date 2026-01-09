use super::{type_mismatch, BookInfo, Checker, FunctionSig, Ty};
use crate::ast::Program;
use crate::diagnostics::diagnostic;
use std::collections::{HashMap, HashSet};

impl<'a> Checker<'a> {
    pub(super) fn collect_books(&mut self, program: &Program) {
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
}
