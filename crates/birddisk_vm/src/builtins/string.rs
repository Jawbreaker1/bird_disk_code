use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_string_builtin(
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
                    (Value::String(left), Value::String(right)) => Ok(Some(Value::Bool(
                        self.string_bytes(*left)? == self.string_bytes(*right)?,
                    ))),
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
            "std::string::slice" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::slice expects 3 arguments",
                    ));
                }
                match (&args[0], &args[1], &args[2]) {
                    (Value::String(handle), Value::I64(start), Value::I64(len)) => {
                        if *start < 0 || *len < 0 {
                            return Err(runtime_error(
                                "E0400",
                                "std::string::slice out of bounds.",
                            ));
                        }
                        let bytes = self.string_bytes(*handle)?;
                        let start = *start as usize;
                        let len = *len as usize;
                        let end = start.saturating_add(len);
                        if start > bytes.len() || end > bytes.len() {
                            return Err(runtime_error(
                                "E0400",
                                "std::string::slice out of bounds.",
                            ));
                        }
                        let slice = bytes[start..end].to_vec();
                        if String::from_utf8(slice.clone()).is_err() {
                            return Err(runtime_error(
                                "E0400",
                                "std::string::slice produced invalid UTF-8.",
                            ));
                        }
                        Ok(Some(self.alloc_string_from_bytes(&slice)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::slice expects (string, i64, i64).",
                    )),
                }
            }
            "std::string::index_of" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::index_of expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(text), Value::String(needle)) => {
                        let hay = self.string_bytes(*text)?;
                        let needle = self.string_bytes(*needle)?;
                        if needle.is_empty() {
                            return Ok(Some(Value::I64(0)));
                        }
                        if needle.len() > hay.len() {
                            return Ok(Some(Value::I64(-1)));
                        }
                        for idx in 0..=hay.len() - needle.len() {
                            if hay[idx..idx + needle.len()] == needle[..] {
                                return Ok(Some(Value::I64(idx as i64)));
                            }
                        }
                        Ok(Some(Value::I64(-1)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::index_of expects string arguments",
                    )),
                }
            }
            "std::string::contains" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::contains expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(text), Value::String(needle)) => {
                        let hay = self.string_bytes(*text)?;
                        let needle = self.string_bytes(*needle)?;
                        if needle.is_empty() {
                            return Ok(Some(Value::Bool(true)));
                        }
                        if needle.len() > hay.len() {
                            return Ok(Some(Value::Bool(false)));
                        }
                        for idx in 0..=hay.len() - needle.len() {
                            if hay[idx..idx + needle.len()] == needle[..] {
                                return Ok(Some(Value::Bool(true)));
                            }
                        }
                        Ok(Some(Value::Bool(false)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::contains expects string arguments",
                    )),
                }
            }
            "std::string::replace" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::replace expects 3 arguments",
                    ));
                }
                match (&args[0], &args[1], &args[2]) {
                    (Value::String(text), Value::String(needle), Value::String(repl)) => {
                        let hay = self.string_bytes(*text)?;
                        let needle = self.string_bytes(*needle)?;
                        if needle.is_empty() || needle.len() > hay.len() {
                            return Ok(Some(Value::String(*text)));
                        }
                        let replacement = self.string_bytes(*repl)?;
                        let mut out = Vec::new();
                        let mut idx = 0;
                        while idx + needle.len() <= hay.len() {
                            if hay[idx..idx + needle.len()] == needle[..] {
                                out.extend_from_slice(&replacement);
                                idx += needle.len();
                            } else {
                                out.push(hay[idx]);
                                idx += 1;
                            }
                        }
                        if idx < hay.len() {
                            out.extend_from_slice(&hay[idx..]);
                        }
                        if String::from_utf8(out.clone()).is_err() {
                            return Err(runtime_error(
                                "E0400",
                                "std::string::replace produced invalid UTF-8.",
                            ));
                        }
                        Ok(Some(self.alloc_string_from_bytes(&out)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::replace expects string arguments",
                    )),
                }
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
            _ => Ok(None),
        }
    }
}

pub(super) fn parse_string_i64(text: &str) -> Option<i64> {
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
