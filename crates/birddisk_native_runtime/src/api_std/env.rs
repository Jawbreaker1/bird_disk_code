use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_env_args(rt: *mut Runtime) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let count = rt.args.len();
    let handle = match rt
        .heap_mut()
        .alloc_array(ElemKind::Ref, count, elem_size(ElemKind::Ref))
    {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    if count == 0 {
        return handle.as_u32() as u64;
    }
    let args = rt.args.clone();
    let mut handles = Vec::with_capacity(args.len());
    for arg in &args {
        let string = match alloc_string_from_bytes(rt, arg.as_bytes()) {
            Some(value) => value,
            None => {
                oom_error(rt);
                return 0;
            }
        };
        handles.push(string.as_u32() as u64);
    }
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let elem_size = elem_size(ElemKind::Ref);
    for (idx, raw) in handles.iter().enumerate() {
        let offset = idx * elem_size;
        let target = match payload.get_mut(offset..offset + elem_size) {
            Some(value) => value,
            None => {
                runtime_error(rt, "Array payload out of bounds.");
                return 0;
            }
        };
        target.copy_from_slice(&raw.to_le_bytes());
    }
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_env_get(rt: *mut Runtime, name_handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, name_handle) {
        Some(value) => value,
        None => return 0,
    };
    let name_bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let name = match std::str::from_utf8(name_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let value = match std::env::var_os(name) {
        Some(value) => value,
        None => {
            let handle = match alloc_string_from_bytes(rt, &[]) {
                Some(value) => value,
                None => {
                    oom_error(rt);
                    return 0;
                }
            };
            return handle.as_u32() as u64;
        }
    };
    let value = match value.into_string() {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "std::env::get returned invalid UTF-8.");
            return 0;
        }
    };
    let handle = match alloc_string_from_bytes(rt, value.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_env_set(rt: *mut Runtime, name_handle: u64, value_handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let name_handle = match heap_handle(rt, name_handle) {
        Some(value) => value,
        None => return 0,
    };
    let value_handle = match heap_handle(rt, value_handle) {
        Some(value) => value,
        None => return 0,
    };
    let name_bytes = match string_bytes_slice(rt, name_handle) {
        Some(value) => value,
        None => return 0,
    };
    let value_bytes = match string_bytes_slice(rt, value_handle) {
        Some(value) => value,
        None => return 0,
    };
    if name_bytes.contains(&0) || value_bytes.contains(&0) {
        runtime_error(rt, "std::env::set_var expects strings without NUL.");
        return 0;
    }
    let name = match std::str::from_utf8(name_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    let value = match std::str::from_utf8(value_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    std::env::set_var(name, value);
    1
}

#[no_mangle]
pub extern "C-unwind" fn bd_env_cwd(rt: *mut Runtime) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let cwd = match std::env::current_dir() {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "std::env::cwd failed.");
            return 0;
        }
    };
    let cwd = match cwd.to_str() {
        Some(value) => value,
        None => {
            runtime_error(rt, "std::env::cwd returned invalid UTF-8.");
            return 0;
        }
    };
    let handle = match alloc_string_from_bytes(rt, cwd.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_env_set_cwd(rt: *mut Runtime, path_handle: u64) -> i64 {
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
    if path_bytes.contains(&0) {
        runtime_error(rt, "std::env::set_cwd expects string without NUL.");
        return 0;
    }
    let path = match std::str::from_utf8(path_bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    if std::env::set_current_dir(path).is_err() {
        runtime_error(rt, "std::env::set_cwd failed.");
        return 0;
    }
    1
}
