use crate::heap::HeapHandle;
use crate::runtime_error::{runtime_error, RuntimeError};
use birddisk_core::ast::Type;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    I64(i64),
    Bool(bool),
    String(HeapHandle),
    U8(u8),
    Array { handle: HeapHandle, elem_type: Type },
    Object { handle: HeapHandle, book: String },
}

impl Value {
    pub(crate) fn heap_handle(&self) -> Option<HeapHandle> {
        match self {
            Value::String(handle)
            | Value::Array { handle, .. }
            | Value::Object { handle, .. } => Some(*handle),
            _ => None,
        }
    }
}

pub(crate) fn value_type(value: &Value) -> Result<Type, RuntimeError> {
    match value {
        Value::I64(_) => Ok(Type::I64),
        Value::Bool(_) => Ok(Type::Bool),
        Value::String(_) => Ok(Type::String),
        Value::U8(_) => Ok(Type::U8),
        Value::Array { elem_type, .. } => Ok(Type::Array(Box::new(elem_type.clone()))),
        Value::Object { book, .. } => Ok(Type::Book(book.clone())),
    }
}

pub(crate) fn coerce_value(value: Value, expected: &Type) -> Result<Value, RuntimeError> {
    match expected {
        Type::I64 => match value {
            Value::I64(value) => Ok(Value::I64(value)),
            _ => Err(runtime_error("E0400", "Expected i64 value.")),
        },
        Type::Bool => match value {
            Value::Bool(value) => Ok(Value::Bool(value)),
            _ => Err(runtime_error("E0400", "Expected bool value.")),
        },
        Type::String => match value {
            Value::String(handle) => Ok(Value::String(handle)),
            _ => Err(runtime_error("E0400", "Expected string value.")),
        },
        Type::U8 => match value {
            Value::U8(value) => Ok(Value::U8(value)),
            Value::I64(value) => {
                if (0..=u8::MAX as i64).contains(&value) {
                    Ok(Value::U8(value as u8))
                } else {
                    Err(runtime_error("E0400", "u8 value out of range."))
                }
            }
            _ => Err(runtime_error("E0400", "Expected u8 value.")),
        },
        Type::Array(expected_elem) => match value {
            Value::Array { handle, elem_type } => {
                if elem_type == *expected_elem.as_ref() {
                    Ok(Value::Array { handle, elem_type })
                } else {
                    Err(runtime_error("E0400", "Expected array value."))
                }
            }
            _ => Err(runtime_error("E0400", "Expected array value.")),
        },
        Type::Book(expected) => match value {
            Value::Object { handle, book } => {
                if &book == expected {
                    Ok(Value::Object { handle, book })
                } else {
                    Err(runtime_error("E0400", "Expected book value."))
                }
            }
            _ => Err(runtime_error("E0400", "Expected book value.")),
        },
    }
}
