use crate::runtime_error::{runtime_error, RuntimeError};
use crate::vm::Vm;
use crate::value::Value;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(crate) fn eval_builtin_call(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::string::len" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::len expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let len = self.string_len(*handle)?;
                        Ok(Some(Value::I64(len as i64)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::len expects string argument",
                    )),
                }
            }
            "std::string::concat" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::concat expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(left), Value::String(right)) => {
                        let mut bytes = self.string_bytes(*left)?;
                        bytes.extend(self.string_bytes(*right)?);
                        Ok(Some(self.alloc_string_from_bytes(&bytes)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::concat expects string arguments",
                    )),
                }
            }
            "std::string::eq" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::eq expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(left), Value::String(right)) => {
                        Ok(Some(Value::Bool(
                            self.string_bytes(*left)? == self.string_bytes(*right)?,
                        )))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::eq expects string arguments",
                    )),
                }
            }
            "std::string::bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::bytes expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let bytes = self.string_bytes(*handle)?;
                        Ok(Some(self.alloc_u8_array(&bytes)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::bytes expects string argument",
                    )),
                }
            }
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
            "std::string::from_bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::from_bytes expects 1 argument",
                    ));
                }
                let bytes = match &args[0] {
                    Value::Array { handle, elem_type } => self.read_u8_array(*handle, elem_type)?,
                    _ => return Err(runtime_error("E0400", "std::bytes expects u8 array.")),
                };
                let text = String::from_utf8(bytes).map_err(|_| {
                    runtime_error(
                        "E0400",
                        "Invalid UTF-8 in std::string::from_bytes.",
                    )
                })?;
                Ok(Some(self.alloc_string(&text)))
            }
            "std::string::to_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::to_i64 expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        let parsed = parse_string_i64(&text).ok_or_else(|| {
                            runtime_error("E0400", "Invalid integer in std::string::to_i64.")
                        })?;
                        Ok(Some(Value::I64(parsed)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::to_i64 expects string argument",
                    )),
                }
            }
            "std::string::from_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::from_i64 expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::I64(number) => Ok(Some(self.alloc_string(&number.to_string()))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::from_i64 expects i64 argument",
                    )),
                }
            }
            "std::io::print" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::io::print expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        self.push_output(&text);
                        Ok(Some(Value::I64(text.len() as i64)))
                    }
                    _ => Err(runtime_error("E0400", "std::io::print expects string argument")),
                }
            }
            "std::io::read_line" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::io::read_line expects 0 arguments",
                    ));
                }
                let line = self.read_input_line();
                Ok(Some(self.alloc_string(&line)))
            }
            _ => Ok(None),
        }
    }
}

fn parse_string_i64(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = 0;
    let mut sign: i128 = 1;
    if bytes[0] == b'-' {
        sign = -1;
        idx = 1;
        if idx == bytes.len() {
            return None;
        }
    }
    let mut value: i128 = 0;
    while idx < bytes.len() {
        let ch = bytes[idx];
        if !(b'0'..=b'9').contains(&ch) {
            return None;
        }
        value = value * 10 + (ch - b'0') as i128;
        idx += 1;
    }
    value *= sign;
    if value < i64::MIN as i128 || value > i64::MAX as i128 {
        return None;
    }
    Some(value as i64)
}
