use super::{EnumInfo, EnumVariantInfo, Checker};
use crate::ast::Program;
use crate::diagnostics::diagnostic;
use std::collections::{HashMap, HashSet};

impl<'a> Checker<'a> {
    pub(super) fn collect_enums(&mut self, program: &Program) {
        let book_names: HashSet<String> =
            program.books.iter().map(|book| book.name.clone()).collect();
        let mut seen = HashSet::new();

        for enum_decl in &program.enums {
            if book_names.contains(&enum_decl.name) {
                self.diagnostics.push(diagnostic(
                    "E0307",
                    "error",
                    format!("Enum name '{}' conflicts with a book.", enum_decl.name),
                    self.file,
                    enum_decl.span,
                    vec!["Enum names must be unique across books.".to_string()],
                    vec!["SPEC.md#2-1-enums".to_string()],
                    Vec::new(),
                    None,
                ));
                continue;
            }
            if !seen.insert(enum_decl.name.clone()) {
                self.diagnostics.push(diagnostic(
                    "E0307",
                    "error",
                    format!("Duplicate enum '{}'.", enum_decl.name),
                    self.file,
                    enum_decl.span,
                    vec!["Enum names must be unique.".to_string()],
                    vec!["SPEC.md#2-1-enums".to_string()],
                    Vec::new(),
                    None,
                ));
                continue;
            }
            self.enums
                .insert(enum_decl.name.clone(), EnumInfo { variants: HashMap::new() });
        }

        for enum_decl in &program.enums {
            if !self.enums.contains_key(&enum_decl.name) {
                continue;
            }
            let mut variants = HashMap::new();
            for variant in &enum_decl.variants {
                if variants.contains_key(&variant.name) {
                    self.diagnostics.push(diagnostic(
                        "E0307",
                        "error",
                        format!(
                            "Duplicate case '{}' in enum '{}'.",
                            variant.name, enum_decl.name
                        ),
                        self.file,
                        variant.span,
                        vec!["Case names must be unique within an enum.".to_string()],
                        vec!["SPEC.md#2-1-enums".to_string()],
                        Vec::new(),
                        None,
                    ));
                    continue;
                }
                let payload_ty = variant.payload.as_ref().map(|payload| {
                    let ty = self.type_from_ast(payload.ty.clone());
                    self.validate_value_type(&ty, payload.span);
                    ty
                });
                variants.insert(
                    variant.name.clone(),
                    EnumVariantInfo { payload: payload_ty },
                );
            }
            if let Some(info) = self.enums.get_mut(&enum_decl.name) {
                info.variants = variants;
            }
        }
    }
}
