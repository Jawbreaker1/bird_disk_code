use super::{ARRAY_KIND_BOOL, ARRAY_KIND_I64, ARRAY_KIND_REF, ARRAY_KIND_U8, WasmError};
use birddisk_core::ast::Type;

pub(super) fn wat_type(ty: &Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::Bool => "i32",
        Type::String => "i32",
        Type::U8 => "i32",
        Type::Array(_) => "i32",
        Type::Book(_) => "i32",
    }
}

pub(super) fn array_elem_size(ty: &Type) -> Result<i32, WasmError> {
    match ty {
        Type::I64 => Ok(8),
        Type::Bool => Ok(4),
        Type::String => Ok(4),
        Type::U8 => Ok(1),
        Type::Array(_) => Ok(4),
        Type::Book(_) => Ok(4),
    }
}

pub(super) fn array_elem_kind(ty: &Type) -> i32 {
    match ty {
        Type::I64 => ARRAY_KIND_I64,
        Type::Bool => ARRAY_KIND_BOOL,
        Type::U8 => ARRAY_KIND_U8,
        Type::String | Type::Array(_) | Type::Book(_) => ARRAY_KIND_REF,
    }
}
