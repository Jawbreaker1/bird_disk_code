use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_json_encode_i64(rt: *mut Runtime, value: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let text = value.to_string();
    let handle = match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_json_encode_bool(rt: *mut Runtime, value: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let text = if value != 0 { "true" } else { "false" };
    let handle = match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_json_encode_string(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match std::str::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let encoded = match super::json_encode_string(text) {
        Some(value) => value,
        None => {
            runtime_error(rt, "std::json::encode_string does not support control characters.");
            return 0;
        }
    };
    let handle = match alloc_string_from_bytes(rt, encoded.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_json_decode_i64(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match std::str::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let trimmed = super::trim_ascii_whitespace(text);
    match super::parse_string_i64(trimmed) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Invalid JSON in std::json::decode_i64.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_json_decode_bool(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match std::str::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    match super::json_decode_bool(text) {
        Some(value) => i64::from(value),
        None => {
            runtime_error(rt, "Invalid JSON in std::json::decode_bool.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_json_decode_string(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match std::str::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let decoded = match super::json_decode_string(text) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Invalid JSON in std::json::decode_string.");
            return 0;
        }
    };
    let handle = match alloc_string_from_bytes(rt, decoded.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}
