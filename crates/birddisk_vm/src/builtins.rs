use crate::runtime_error::{runtime_error, RuntimeError};
use crate::vm::Vm;
use crate::value::Value;
use birddisk_core::ast::Type;
use std::path::{Component, Path, PathBuf};

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
            "std::json::encode_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::encode_i64 expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::I64(number) => Ok(Some(self.alloc_string(&number.to_string()))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::encode_i64 expects i64 argument",
                    )),
                }
            }
            "std::json::encode_bool" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::encode_bool expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::Bool(value) => {
                        let text = if *value { "true" } else { "false" };
                        Ok(Some(self.alloc_string(text)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::encode_bool expects bool argument",
                    )),
                }
            }
            "std::json::encode_string" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::encode_string expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        let encoded = json_encode_string(&text).ok_or_else(|| {
                            runtime_error(
                                "E0400",
                                "std::json::encode_string does not support control characters.",
                            )
                        })?;
                        Ok(Some(self.alloc_string(&encoded)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::encode_string expects string argument",
                    )),
                }
            }
            "std::json::decode_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::decode_i64 expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        let trimmed = trim_ascii_whitespace(&text);
                        let parsed = parse_string_i64(trimmed).ok_or_else(|| {
                            runtime_error("E0400", "Invalid JSON in std::json::decode_i64.")
                        })?;
                        Ok(Some(Value::I64(parsed)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::decode_i64 expects string argument",
                    )),
                }
            }
            "std::json::decode_bool" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::decode_bool expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        let parsed = json_decode_bool(&text).ok_or_else(|| {
                            runtime_error("E0400", "Invalid JSON in std::json::decode_bool.")
                        })?;
                        Ok(Some(Value::Bool(parsed)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::decode_bool expects string argument",
                    )),
                }
            }
            "std::json::decode_string" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::json::decode_string expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        let decoded = json_decode_string(&text).ok_or_else(|| {
                            runtime_error("E0400", "Invalid JSON in std::json::decode_string.")
                        })?;
                        Ok(Some(self.alloc_string(&decoded)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::json::decode_string expects string argument",
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
                        Ok(Some(Value::Void))
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
            "std::time::now_ms" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::time::now_ms expects 0 arguments",
                    ));
                }
                Ok(Some(Value::I64(self.now_ms())))
            }
            "std::time::sleep_ms" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::time::sleep_ms expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::I64(millis) => Ok(Some(Value::I64(self.sleep_ms(*millis)?))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::time::sleep_ms expects i64 argument",
                    )),
                }
            }
            "std::fs::read_text" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::read_text expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let text = std::fs::read_to_string(path)
                            .map_err(|_| runtime_error("E0400", "std::fs::read_text failed."))?;
                        Ok(Some(self.alloc_string(&text)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::read_text expects string argument",
                    )),
                }
            }
            "std::fs::write_text" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::write_text expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(path_handle), Value::String(text_handle)) => {
                        let path = self.string_text(*path_handle)?;
                        let text = self.string_text(*text_handle)?;
                        std::fs::write(path, text.as_bytes())
                            .map_err(|_| runtime_error("E0400", "std::fs::write_text failed."))?;
                        Ok(Some(Value::I64(text.as_bytes().len() as i64)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::write_text expects string arguments",
                    )),
                }
            }
            "std::fs::read_bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::read_bytes expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let bytes = std::fs::read(path)
                            .map_err(|_| runtime_error("E0400", "std::fs::read_bytes failed."))?;
                        Ok(Some(self.alloc_u8_array(&bytes)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::read_bytes expects string argument",
                    )),
                }
            }
            "std::fs::write_bytes" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::write_bytes expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(path_handle), Value::Array { handle, elem_type }) => {
                        let path = self.string_text(*path_handle)?;
                        let bytes = self.read_u8_array(*handle, elem_type)?;
                        std::fs::write(path, &bytes)
                            .map_err(|_| runtime_error("E0400", "std::fs::write_bytes failed."))?;
                        Ok(Some(Value::I64(bytes.len() as i64)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::write_bytes expects string and u8[] arguments",
                    )),
                }
            }
            "std::path::join" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::join expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(left), Value::String(right)) => {
                        let left = self.string_text(*left)?;
                        let right = self.string_text(*right)?;
                        let joined = path_join(&left, &right)?;
                        Ok(Some(self.alloc_string(&joined)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::join expects string arguments",
                    )),
                }
            }
            "std::path::normalize" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::normalize expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let normalized = path_normalize(&path)?;
                        Ok(Some(self.alloc_string(&normalized)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::normalize expects string argument",
                    )),
                }
            }
            "std::path::basename" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::basename expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let name = path_basename(&path)?;
                        Ok(Some(self.alloc_string(&name)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::basename expects string argument",
                    )),
                }
            }
            "std::path::dirname" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::dirname expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let name = path_dirname(&path)?;
                        Ok(Some(self.alloc_string(&name)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::dirname expects string argument",
                    )),
                }
            }
            "std::env::args" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::args expects 0 arguments",
                    ));
                }
                let values = self.alloc_env_args()?;
                Ok(Some(values))
            }
            "std::env::get" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::get expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let name = self.string_text(*handle)?;
                        let value = match std::env::var_os(&name) {
                            Some(value) => value,
                            None => return Ok(Some(self.alloc_string(""))),
                        };
                        let value = value.into_string().map_err(|_| {
                            runtime_error("E0400", "std::env::get returned invalid UTF-8.")
                        })?;
                        Ok(Some(self.alloc_string(&value)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::get expects string argument",
                    )),
                }
            }
            "std::env::set_var" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::set_var expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(name_handle), Value::String(value_handle)) => {
                        let name = self.string_text(*name_handle)?;
                        let value = self.string_text(*value_handle)?;
                        if name.contains('\0') || value.contains('\0') {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_var expects strings without NUL.",
                            ));
                        }
                        std::env::set_var(name, value);
                        Ok(Some(Value::I64(1)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::set_var expects string arguments",
                    )),
                }
            }
            "std::env::cwd" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::cwd expects 0 arguments",
                    ));
                }
                let cwd = std::env::current_dir().map_err(|_| {
                    runtime_error("E0400", "std::env::cwd failed.")
                })?;
                let cwd = cwd.to_str().ok_or_else(|| {
                    runtime_error("E0400", "std::env::cwd returned invalid UTF-8.")
                })?;
                Ok(Some(self.alloc_string(cwd)))
            }
            "std::env::set_cwd" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::set_cwd expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        if path.contains('\0') {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_cwd expects string without NUL.",
                            ));
                        }
                        if std::env::set_current_dir(path).is_err() {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_cwd failed.",
                            ));
                        }
                        Ok(Some(Value::I64(1)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::set_cwd expects string argument",
                    )),
                }
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

fn trim_ascii_whitespace(text: &str) -> &str {
    let bytes = text.as_bytes();
    let mut start = 0;
    let mut end = bytes.len();
    while start < end && is_ascii_whitespace(bytes[start]) {
        start += 1;
    }
    while end > start && is_ascii_whitespace(bytes[end - 1]) {
        end -= 1;
    }
    &text[start..end]
}

fn is_ascii_whitespace(byte: u8) -> bool {
    matches!(byte, b' ' | b'\n' | b'\r' | b'\t')
}

fn json_encode_string(text: &str) -> Option<String> {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000c}' => out.push_str("\\f"),
            ch if (ch as u32) < 0x20 => return None,
            _ => out.push(ch),
        }
    }
    out.push('"');
    Some(out)
}

fn json_decode_bool(text: &str) -> Option<bool> {
    match trim_ascii_whitespace(text) {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

fn json_decode_string(text: &str) -> Option<String> {
    let trimmed = trim_ascii_whitespace(text);
    let bytes = trimmed.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'"' || bytes[bytes.len() - 1] != b'"' {
        return None;
    }
    let mut out = Vec::with_capacity(bytes.len().saturating_sub(2));
    let mut idx = 1;
    let end = bytes.len() - 1;
    while idx < end {
        let byte = bytes[idx];
        if byte == b'\\' {
            idx += 1;
            if idx >= end {
                return None;
            }
            let escaped = match bytes[idx] {
                b'"' => b'"',
                b'\\' => b'\\',
                b'/' => b'/',
                b'b' => 0x08,
                b'f' => 0x0c,
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'u' => return None,
                _ => return None,
            };
            out.push(escaped);
        } else {
            if byte < 0x20 {
                return None;
            }
            out.push(byte);
        }
        idx += 1;
    }
    String::from_utf8(out).ok()
}

fn path_join(left: &str, right: &str) -> Result<String, RuntimeError> {
    let joined = Path::new(left).join(right);
    path_to_string(&joined, "std::path::join")
}

fn path_normalize(path: &str) -> Result<String, RuntimeError> {
    let mut out = PathBuf::new();
    let mut parts: Vec<std::ffi::OsString> = Vec::new();
    let mut has_root = false;
    for component in Path::new(path).components() {
        match component {
            Component::Prefix(prefix) => out.push(prefix.as_os_str()),
            Component::RootDir => {
                out.push(component.as_os_str());
                has_root = true;
            }
            Component::CurDir => {}
            Component::ParentDir => {
                if let Some(last) = parts.last() {
                    if last != std::ffi::OsStr::new("..") {
                        parts.pop();
                    } else {
                        parts.push(std::ffi::OsString::from(".."));
                    }
                } else if !has_root {
                    parts.push(std::ffi::OsString::from(".."));
                }
            }
            Component::Normal(part) => parts.push(part.to_os_string()),
        }
    }
    for part in parts {
        out.push(part);
    }
    if out.as_os_str().is_empty() {
        return Ok(".".to_string());
    }
    path_to_string(&out, "std::path::normalize")
}

fn path_basename(path: &str) -> Result<String, RuntimeError> {
    let name = Path::new(path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    Ok(name.to_string())
}

fn path_dirname(path: &str) -> Result<String, RuntimeError> {
    let path = Path::new(path);
    if let Some(parent) = path.parent() {
        if parent.as_os_str().is_empty() {
            return Ok(".".to_string());
        }
        return path_to_string(parent, "std::path::dirname");
    }
    if path.has_root() {
        return path_to_string(path, "std::path::dirname");
    }
    Ok(".".to_string())
}

fn path_to_string(path: &Path, op: &str) -> Result<String, RuntimeError> {
    path.to_str()
        .map(|value| value.to_string())
        .ok_or_else(|| runtime_error("E0400", format!("{op} produced invalid UTF-8.")))
}
