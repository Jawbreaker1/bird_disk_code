use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_fs_read_text(rt: *mut Runtime, path_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path_bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match std::str::from_utf8(path_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let text = match std::fs::read_to_string(path) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "std::fs::read_text failed.");
            return 0;
        }
    };
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
pub extern "C-unwind" fn bd_fs_write_text(
    rt: *mut Runtime,
    path_handle: u64,
    text_handle: u64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let path_handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let text_handle = match heap_handle(rt, text_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path_bytes = match string_bytes_slice(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let text_bytes = match string_bytes_slice(rt, text_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match std::str::from_utf8(path_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    if std::fs::write(path, text_bytes).is_err() {
        runtime_error(rt, "std::fs::write_text failed.");
        return 0;
    }
    text_bytes.len() as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_fs_read_bytes(rt: *mut Runtime, path_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path_bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match std::str::from_utf8(path_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let bytes = match std::fs::read(path) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "std::fs::read_bytes failed.");
            return 0;
        }
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
pub extern "C-unwind" fn bd_fs_write_bytes(
    rt: *mut Runtime,
    path_handle: u64,
    bytes_handle: u64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let path_handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes_handle = match heap_handle(rt, bytes_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path_bytes = match string_bytes_slice(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match bytes_slice(rt, bytes_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match std::str::from_utf8(path_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    if std::fs::write(path, bytes).is_err() {
        runtime_error(rt, "std::fs::write_bytes failed.");
        return 0;
    }
    bytes.len() as i64
}
