use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_len(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match bytes_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    header.len_or_size as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_eq(rt: *mut Runtime, left: u64, right: u64) -> i64 {
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
    let left_bytes = match bytes_slice(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right_bytes = match bytes_slice(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    if left_bytes == right_bytes {
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_slice(
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
        runtime_error(rt, "std::bytes::slice out of bounds.");
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let start = start as usize;
    let len = len as usize;
    let end = start.saturating_add(len);
    if start > bytes.len() || end > bytes.len() {
        runtime_error(rt, "std::bytes::slice out of bounds.");
        return 0;
    }
    let slice = bytes[start..end].to_vec();
    let array = match rt.heap_mut().alloc_array(ElemKind::U8, slice.len(), 1) {
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
    payload[..slice.len()].copy_from_slice(&slice);
    array.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_index_of(rt: *mut Runtime, handle: u64, needle: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if needle < 0 || needle > u8::MAX as i64 {
        runtime_error(rt, "std::bytes::index_of expects u8 needle.");
        return 0;
    }
    let needle = needle as u8;
    for (idx, value) in bytes.iter().enumerate() {
        if *value == needle {
            return idx as i64;
        }
    }
    -1
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_contains(rt: *mut Runtime, handle: u64, needle: i64) -> i64 {
    let index = bd_bytes_index_of(rt, handle, needle);
    if index >= 0 {
        1
    } else {
        0
    }
}
