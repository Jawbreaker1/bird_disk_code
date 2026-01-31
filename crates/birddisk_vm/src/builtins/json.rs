use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

use super::string::parse_string_i64;

impl<'a> Vm<'a> {
    pub(super) fn eval_json_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
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
            _ => Ok(None),
        }
    }
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
