use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(super) fn eval_bytes_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::bytes::len" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::len expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::Array { handle, elem_type } if *elem_type == Type::U8 => {
                        let len = self.array_len(*handle)?;
                        Ok(Some(Value::I64(len as i64)))
                    }
                    _ => Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                }
            }
            "std::bytes::eq" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::eq expects 2 arguments",
                    ));
                }
                let left = match &args[0] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                let right = match &args[1] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                Ok(Some(Value::Bool(left == right)))
            }
            "std::bytes::slice" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::slice expects 3 arguments",
                    ));
                }
                let bytes = match &args[0] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                let (Value::I64(start), Value::I64(len)) = (&args[1], &args[2]) else {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::slice expects (u8[], i64, i64).",
                    ));
                };
                if *start < 0 || *len < 0 {
                    return Err(runtime_error("E0400", "std::bytes::slice out of bounds."));
                }
                let start = *start as usize;
                let len = *len as usize;
                let end = start.saturating_add(len);
                if start > bytes.len() || end > bytes.len() {
                    return Err(runtime_error("E0400", "std::bytes::slice out of bounds."));
                }
                Ok(Some(self.alloc_u8_array(&bytes[start..end])))
            }
            "std::bytes::index_of" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::index_of expects 2 arguments",
                    ));
                }
                let bytes = match &args[0] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                let Value::U8(needle) = &args[1] else {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::index_of expects u8 needle.",
                    ));
                };
                for (idx, value) in bytes.iter().enumerate() {
                    if value == needle {
                        return Ok(Some(Value::I64(idx as i64)));
                    }
                }
                Ok(Some(Value::I64(-1)))
            }
            "std::bytes::contains" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::contains expects 2 arguments",
                    ));
                }
                let bytes = match &args[0] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                let Value::U8(needle) = &args[1] else {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::contains expects u8 needle.",
                    ));
                };
                Ok(Some(Value::Bool(bytes.iter().any(|value| value == needle))))
            }
            _ => Ok(None),
        }
    }
}
