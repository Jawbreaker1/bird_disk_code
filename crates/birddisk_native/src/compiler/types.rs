use super::{clif_type, NativeCompiler, VarInfo};
use crate::error::{native_error, NativeError};
use crate::program::{stdlib_signature, type_name};
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Type, UnaryOp};
use cranelift_frontend::Variable;
use cranelift_module::Module;

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(super) fn lookup_local_type(&self, name: &str) -> Option<Type> {
        self.vars
            .get(name)
            .map(|info| info.ty.clone())
            .or_else(|| self.locals.get(name).cloned())
    }

    pub(super) fn bind_or_assign_local(
        &mut self,
        name: &str,
        ty: Type,
        value: cranelift_codegen::ir::Value,
    ) -> Result<(), NativeError> {
        if let Some(info) = self.vars.get(name).cloned() {
            if info.ty != ty {
                return Err(native_error(format!(
                    "type mismatch for '{name}': expected {}, got {}.",
                    type_name(&info.ty),
                    type_name(&ty)
                )));
            }
            self.builder.def_var(info.var, value);
            self.update_root(name, value);
            return Ok(());
        }
        if let Some(expected) = self.locals.get(name) {
            if expected != &ty {
                return Err(native_error(format!(
                    "type mismatch for '{name}': expected {}, got {}.",
                    type_name(expected),
                    type_name(&ty)
                )));
            }
        }
        let var = self.new_var(&ty);
        self.builder.def_var(var, value);
        self.vars.insert(
            name.to_string(),
            VarInfo {
                var,
                ty: ty.clone(),
            },
        );
        self.update_root(name, value);
        Ok(())
    }

    pub(super) fn infer_expr_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Int(_) => Some(Type::I64),
            ExprKind::Float(_) => Some(Type::F64),
            ExprKind::Bool(_) => Some(Type::Bool),
            ExprKind::String(_) => Some(Type::String),
            ExprKind::Ident(name) => self
                .vars
                .get(name)
                .map(|info| info.ty.clone())
                .or_else(|| self.locals.get(name).cloned()),
            ExprKind::Call { name, .. } => {
                if let Some(return_type) = stdlib_signature(name)
                    .map(|sig| sig.return_type)
                    .or_else(|| self.functions.get(name).map(|sig| sig.return_type.clone()))
                {
                    return Some(return_type);
                }
                if let Some((enum_name, variant_name)) = name.split_once("::") {
                    if enum_name != "std"
                        && !variant_name.contains("::")
                        && self.lookup_local_type(enum_name).is_none()
                    {
                        if let Some(enum_info) = self.enums.get(enum_name) {
                            if enum_info.variants.contains_key(variant_name) {
                                return Some(Type::Book(enum_name.to_string()));
                            }
                            return None;
                        }
                    }
                }
                if let Some((base, method)) = name.split_once("::") {
                    if base != "std" {
                        if let Some(Type::Book(book)) = self.lookup_local_type(base) {
                            let full_name = format!("{book}::{method}");
                            return self
                                .functions
                                .get(&full_name)
                                .map(|sig| sig.return_type.clone());
                        }
                    }
                }
                None
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => self.infer_expr_type(expr),
                UnaryOp::Not => Some(Type::Bool),
            },
            ExprKind::Binary { op, left, right } => {
                let left_ty = self.infer_expr_type(left);
                let right_ty = self.infer_expr_type(right);
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        if matches!(
                            (left_ty.as_ref(), right_ty.as_ref()),
                            (Some(Type::F64), Some(Type::F64))
                        ) {
                            Some(Type::F64)
                        } else {
                            Some(Type::I64)
                        }
                    }
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq
                    | BinaryOp::AndAnd
                    | BinaryOp::OrOr => Some(Type::Bool),
                }
            }
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    None
                } else {
                    let first = self.infer_expr_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let ty = self.infer_expr_type(elem)?;
                        if ty != first {
                            return None;
                        }
                    }
                    Some(Type::Array(Box::new(first)))
                }
            }
            ExprKind::Index { base, .. } => {
                let base_ty = self.infer_expr_type(base)?;
                match base_ty {
                    Type::Array(inner) => Some(*inner),
                    _ => None,
                }
            }
            ExprKind::New { book, .. } => Some(Type::Book(book.clone())),
            ExprKind::MemberAccess { base, field } => {
                let Type::Book(book) = self.lookup_local_type(base)? else {
                    return None;
                };
                let layout = self.books.get(&book)?;
                let index = layout.field_index.get(field)?;
                layout.fields.get(*index).cloned()
            }
            _ => None,
        }
    }

    pub(super) fn new_var(&mut self, ty: &Type) -> Variable {
        let var = Variable::from_u32(self.next_var);
        self.next_var += 1;
        self.builder.declare_var(var, clif_type(ty));
        var
    }
}
