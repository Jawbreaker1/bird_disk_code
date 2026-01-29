use crate::error::{native_error, NativeError};
use crate::program::{stdlib_signature, BookLayout, EnumInfo, FunctionSig};
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Stmt, Type, UnaryOp};
use birddisk_core::runtime as abi;
use std::collections::HashMap;

pub(crate) fn collect_local_types(
    function: &birddisk_core::ast::Function,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
    enums: &HashMap<String, EnumInfo>,
) -> Result<HashMap<String, Type>, NativeError> {
    let mut locals = HashMap::new();
    for param in &function.params {
        if locals.contains_key(&param.name) {
            return Err(native_error(format!(
                "native backend does not support shadowing '{}'.",
                param.name
            )));
        }
        locals.insert(param.name.clone(), param.ty.clone());
    }
    collect_local_types_in_block(&function.body, &mut locals, functions, books, enums)?;
    Ok(locals)
}

fn collect_local_types_in_block(
    stmts: &[Stmt],
    locals: &mut HashMap<String, Type>,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
    enums: &HashMap<String, EnumInfo>,
) -> Result<(), NativeError> {
    for stmt in stmts {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                if locals.contains_key(name) {
                    return Err(native_error(format!(
                        "native backend does not support shadowing '{name}'."
                    )));
                }
                let var_ty = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    infer_expr_type(expr, locals, functions, books, enums).ok_or_else(|| {
                        native_error(format!(
                            "native backend requires explicit type for '{name}'."
                        ))
                    })?
                };
                locals.insert(name.clone(), var_ty);
            }
            Stmt::When {
                then_body,
                else_body,
                ..
            } => {
                collect_local_types_in_block(then_body, locals, functions, books, enums)?;
                collect_local_types_in_block(else_body, locals, functions, books, enums)?;
            }
            Stmt::Try {
                try_body,
                catch_name,
                catch_body,
                ..
            } => {
                collect_local_types_in_block(try_body, locals, functions, books, enums)?;
                if locals.contains_key(catch_name) {
                    return Err(native_error(format!(
                        "native backend does not support shadowing '{catch_name}'."
                    )));
                }
                locals.insert(catch_name.clone(), Type::String);
                collect_local_types_in_block(catch_body, locals, functions, books, enums)?;
            }
            Stmt::Repeat { body, .. } => {
                collect_local_types_in_block(body, locals, functions, books, enums)?;
            }
            Stmt::Match { cases, otherwise, .. } => {
                for case in cases {
                    if let Some(binding) = &case.binding {
                        let enum_info = enums.get(&case.enum_name).ok_or_else(|| {
                            native_error(format!("unknown enum '{}'.", case.enum_name))
                        })?;
                        let variant = enum_info.variants.get(&case.variant_name).ok_or_else(|| {
                            native_error(format!(
                                "unknown enum variant '{}::{}'.",
                                case.enum_name, case.variant_name
                            ))
                        })?;
                        let Some(payload_ty) = variant.payload.as_ref() else {
                            return Err(native_error(format!(
                                "variant '{}::{}' has no payload.",
                                case.enum_name, case.variant_name
                            )));
                        };
                        if locals.contains_key(binding) {
                            return Err(native_error(format!(
                                "native backend does not support shadowing '{binding}'."
                            )));
                        }
                        locals.insert(binding.clone(), payload_ty.clone());
                    }
                    collect_local_types_in_block(&case.body, locals, functions, books, enums)?;
                }
                collect_local_types_in_block(otherwise, locals, functions, books, enums)?;
            }
            _ => {}
        }
    }
    Ok(())
}

pub(crate) fn infer_expr_type(
    expr: &Expr,
    locals: &HashMap<String, Type>,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
    enums: &HashMap<String, EnumInfo>,
) -> Option<Type> {
    match &expr.kind {
        ExprKind::Int(_) => Some(Type::I64),
        ExprKind::Float(_) => Some(Type::F64),
        ExprKind::Bool(_) => Some(Type::Bool),
        ExprKind::String(_) => Some(Type::String),
        ExprKind::Ident(name) => locals.get(name).cloned(),
        ExprKind::Call { name, .. } => {
            if let Some(return_type) = stdlib_signature(name)
                .map(|sig| sig.return_type)
                .or_else(|| functions.get(name).map(|sig| sig.return_type.clone()))
            {
                return Some(return_type);
            }
            if let Some((enum_name, variant_name)) = name.split_once("::") {
                if enum_name != "std"
                    && !variant_name.contains("::")
                    && !locals.contains_key(enum_name)
                {
                    if let Some(enum_info) = enums.get(enum_name) {
                        if enum_info.variants.contains_key(variant_name) {
                            return Some(Type::Book(enum_name.to_string()));
                        }
                    }
                }
            }
            if let Some((base, method)) = name.split_once("::") {
                if base != "std" {
                    if let Some(Type::Book(book)) = locals.get(base) {
                        let full_name = format!("{book}::{method}");
                        return functions.get(&full_name).map(|sig| sig.return_type.clone());
                    }
                }
            }
            None
        }
        ExprKind::Unary { op, expr } => match op {
            UnaryOp::Neg => infer_expr_type(expr, locals, functions, books, enums),
            UnaryOp::Not => Some(Type::Bool),
        },
        ExprKind::Cast { ty, .. } => Some(ty.clone()),
        ExprKind::Binary { op, left, right } => {
            let left_ty = infer_expr_type(left, locals, functions, books, enums);
            let right_ty = infer_expr_type(right, locals, functions, books, enums);
            match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod => {
                    if matches!((left_ty.as_ref(), right_ty.as_ref()), (Some(Type::F64), Some(Type::F64))) {
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
                let first = infer_expr_type(&elements[0], locals, functions, books, enums)?;
                for elem in &elements[1..] {
                    let ty = infer_expr_type(elem, locals, functions, books, enums)?;
                    if ty != first {
                        return None;
                    }
                }
                Some(Type::Array(Box::new(first)))
            }
        }
        ExprKind::Index { base, .. } => {
            let base_ty = infer_expr_type(base, locals, functions, books, enums)?;
            match base_ty {
                Type::Array(inner) => Some(*inner),
                _ => None,
            }
        }
        ExprKind::New { book, .. } => Some(Type::Book(book.clone())),
        ExprKind::MemberAccess { base, field } => {
            let Type::Book(book) = locals.get(base)?.clone() else {
                return None;
            };
            let layout = books.get(&book)?;
            let index = layout.field_index.get(field)?;
            layout.fields.get(*index).cloned()
        }
        _ => None,
    }
}

pub(crate) fn build_root_slots(locals: &HashMap<String, Type>) -> HashMap<String, u32> {
    let mut slots = HashMap::new();
    let mut names: Vec<String> = locals
        .iter()
        .filter_map(|(name, ty)| if is_ref_type(ty) { Some(name.clone()) } else { None })
        .collect();
    names.sort();
    for (index, name) in names.into_iter().enumerate() {
        slots.insert(name, index as u32);
    }
    slots
}

pub(crate) fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}

pub(crate) fn elem_kind_for_type(ty: &Type) -> Result<u32, NativeError> {
    match ty {
        Type::I64 => Ok(abi::ARRAY_KIND_I64),
        Type::F64 => Ok(abi::ARRAY_KIND_F64),
        Type::Bool => Ok(abi::ARRAY_KIND_BOOL),
        Type::U8 => Ok(abi::ARRAY_KIND_U8),
        Type::String | Type::Array(_) | Type::Book(_) => Ok(abi::ARRAY_KIND_REF),
        Type::Void => Err(native_error("void is not a valid array element type.")),
    }
}

pub(crate) fn elem_size_for_kind(kind: u32) -> Result<u32, NativeError> {
    match kind {
        value if value == abi::ARRAY_KIND_I64 => Ok(8),
        value if value == abi::ARRAY_KIND_F64 => Ok(8),
        value if value == abi::ARRAY_KIND_BOOL => Ok(1),
        value if value == abi::ARRAY_KIND_U8 => Ok(1),
        value if value == abi::ARRAY_KIND_REF => Ok(8),
        _ => Err(native_error("unknown array element kind.")),
    }
}
