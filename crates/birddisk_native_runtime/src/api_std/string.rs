use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_string_len(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match string_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    header.len_or_size as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_concat(
    rt: *mut Runtime,
    left: u64,
    right: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return 0,
    };
    let combined = {
        let left_bytes = match string_bytes_slice(rt, left_handle) {
            Some(value) => value,
            None => return 0,
        };
        let right_bytes = match string_bytes_slice(rt, right_handle) {
            Some(value) => value,
            None => return 0,
        };
        let total_len = match left_bytes.len().checked_add(right_bytes.len()) {
            Some(value) => value,
            None => {
                oom_error(rt);
                return 0;
            }
        };
        let mut combined = Vec::new();
        if combined.try_reserve_exact(total_len).is_err() {
            oom_error(rt);
            return 0;
        }
        combined.extend_from_slice(left_bytes);
        combined.extend_from_slice(right_bytes);
        combined
    };
    let handle = match alloc_string_from_bytes(rt, &combined) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_eq(rt: *mut Runtime, left: u64, right: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return 0,
    };
    let left_bytes = match string_bytes_slice(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right_bytes = match string_bytes_slice(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    if left_bytes == right_bytes { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_bytes(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value.to_vec(),
        None => return 0,
    };
    let array = match rt.heap_mut().alloc_array(ElemKind::U8, bytes.len(), 1) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    let payload = match heap_payload_mut(rt, array) {
        Some(value) => value,
        None => return 0,
    };
    payload[..bytes.len()].copy_from_slice(&bytes);
    array.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_slice(
    rt: *mut Runtime,
    handle: u64,
    start: i64,
    len: i64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if start < 0 || len < 0 {
        runtime_error(rt, "std::string::slice out of bounds.");
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
    let start = start as usize;
    let len = len as usize;
    let end = start.saturating_add(len);
    if start > bytes.len() || end > bytes.len() {
        runtime_error(rt, "std::string::slice out of bounds.");
        return 0;
    }
    let slice = bytes[start..end].to_vec();
    if std::str::from_utf8(&slice).is_err() {
        runtime_error(rt, "std::string::slice produced invalid UTF-8.");
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, &slice) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_index_of(
    rt: *mut Runtime,
    text: u64,
    needle: u64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let text = match heap_handle(rt, text) {
        Some(value) => value,
        None => return 0,
    };
    let needle = match heap_handle(rt, needle) {
        Some(value) => value,
        None => return 0,
    };
    let hay = match string_bytes_slice(rt, text) {
        Some(value) => value,
        None => return 0,
    };
    let needle = match string_bytes_slice(rt, needle) {
        Some(value) => value,
        None => return 0,
    };
    if needle.is_empty() {
        return 0;
    }
    if needle.len() > hay.len() {
        return -1;
    }
    for idx in 0..=hay.len() - needle.len() {
        if hay[idx..idx + needle.len()] == needle[..] {
            return idx as i64;
        }
    }
    -1
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_contains(
    rt: *mut Runtime,
    text: u64,
    needle: u64,
) -> i64 {
    let index = bd_string_index_of(rt, text, needle);
    if index >= 0 { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_replace(
    rt: *mut Runtime,
    text: u64,
    needle: u64,
    replacement: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let text = match heap_handle(rt, text) {
        Some(value) => value,
        None => return 0,
    };
    let needle = match heap_handle(rt, needle) {
        Some(value) => value,
        None => return 0,
    };
    let replacement = match heap_handle(rt, replacement) {
        Some(value) => value,
        None => return 0,
    };
    let hay = match string_bytes_slice(rt, text) {
        Some(value) => value,
        None => return 0,
    };
    let needle_bytes = match string_bytes_slice(rt, needle) {
        Some(value) => value,
        None => return 0,
    };
    if needle_bytes.is_empty() || needle_bytes.len() > hay.len() {
        return text.as_u32() as u64;
    }
    let replacement_bytes = match string_bytes_slice(rt, replacement) {
        Some(value) => value,
        None => return 0,
    };
    let mut out = Vec::new();
    let mut idx = 0;
    while idx + needle_bytes.len() <= hay.len() {
        if hay[idx..idx + needle_bytes.len()] == needle_bytes[..] {
            out.extend_from_slice(replacement_bytes);
            idx += needle_bytes.len();
        } else {
            out.push(hay[idx]);
            idx += 1;
        }
    }
    if idx < hay.len() {
        out.extend_from_slice(&hay[idx..]);
    }
    if std::str::from_utf8(&out).is_err() {
        runtime_error(rt, "std::string::replace produced invalid UTF-8.");
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, &out) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_from_bytes(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match bytes_slice(rt, handle) {
        Some(value) => value.to_vec(),
        None => return 0,
    };
    if std::str::from_utf8(&bytes).is_err() {
        runtime_error(rt, "Invalid UTF-8 in std::string::from_bytes.");
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, &bytes) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_to_i64(rt: *mut Runtime, handle: u64) -> i64 {
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
    match super::parse_string_i64(text) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Invalid integer in std::string::to_i64.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_from_i64(rt: *mut Runtime, value: i64) -> u64 {
    let text = value.to_string();
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}
