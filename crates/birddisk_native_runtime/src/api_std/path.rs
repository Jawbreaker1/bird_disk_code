use crate::rt_core::*;
use std::path::Path;

#[no_mangle]
pub extern "C-unwind" fn bd_path_join(
    rt: *mut Runtime,
    left_handle: u64,
    right_handle: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    let left = match super::path_from_handle(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right = match super::path_from_handle(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    let joined = Path::new(&left).join(&right);
    let handle = match super::alloc_string_from_path(
        rt,
        "std::path::join produced invalid UTF-8.",
        &joined,
    ) {
        Some(value) => value,
        None => return 0,
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_path_normalize(rt: *mut Runtime, path_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match super::path_from_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let normalized = super::normalize_path(&path);
    let handle = match super::alloc_string_from_path(
        rt,
        "std::path::normalize produced invalid UTF-8.",
        &normalized,
    ) {
        Some(value) => value,
        None => return 0,
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_path_basename(rt: *mut Runtime, path_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match super::path_from_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let name = Path::new(&path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    let handle = match alloc_string_from_bytes(rt, name.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_path_dirname(rt: *mut Runtime, path_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, path_handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = match super::path_from_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let path = Path::new(&path);
    let output = if let Some(parent) = path.parent() {
        if parent.as_os_str().is_empty() {
            ".".to_string()
        } else {
            match parent.to_str() {
                Some(value) => value.to_string(),
                None => {
                    runtime_error(rt, "std::path::dirname produced invalid UTF-8.");
                    return 0;
                }
            }
        }
    } else if path.has_root() {
        match path.to_str() {
            Some(value) => value.to_string(),
            None => {
                runtime_error(rt, "std::path::dirname produced invalid UTF-8.");
                return 0;
            }
        }
    } else {
        ".".to_string()
    };
    let handle = match alloc_string_from_bytes(rt, output.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}
