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
    match parse_string_i64(text) {
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
    if left_bytes == right_bytes { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_io_print(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let text = match {
        let bytes = match string_bytes_slice(rt, handle) {
            Some(value) => value,
            None => return,
        };
        std::str::from_utf8(bytes)
    } {
        Ok(value) => value.to_string(),
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return;
        }
    };
    rt.push_output(&text);
}

#[no_mangle]
pub extern "C-unwind" fn bd_io_read_line(rt: *mut Runtime) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let line = rt.read_line();
    let handle = match alloc_string_from_bytes(rt, line.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

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
    let left = match path_from_handle(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right = match path_from_handle(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    let joined = Path::new(&left).join(&right);
    let handle = match alloc_string_from_path(
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
    let path = match path_from_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let normalized = normalize_path(&path);
    let handle = match alloc_string_from_path(
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
    let path = match path_from_handle(rt, handle) {
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
    let path = match path_from_handle(rt, handle) {
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
pub extern "C-unwind" fn bd_env_set(
    rt: *mut Runtime,
    name_handle: u64,
    value_handle: u64,
) -> i64 {
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
    let encoded = match json_encode_string(text) {
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
    let trimmed = trim_ascii_whitespace(text);
    match parse_string_i64(trimmed) {
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
    match json_decode_bool(text) {
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
    let decoded = match json_decode_string(text) {
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

fn path_from_handle(rt: &Runtime, handle: HeapHandle) -> Option<String> {
    let path_bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return None,
    };
    match std::str::from_utf8(path_bytes) {
        Ok(value) => Some(value.to_string()),
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            None
        }
    }
}

fn alloc_string_from_path(
    rt: &mut Runtime,
    message: &'static str,
    path: &Path,
) -> Option<HeapHandle> {
    let text = match path.to_str() {
        Some(value) => value,
        None => {
            runtime_error(rt, message);
            return None;
        }
    };
    match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => Some(value),
        None => {
            oom_error(rt);
            None
        }
    }
}

fn normalize_path(path: &str) -> PathBuf {
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
        out.push(".");
    }
    out
}

#[no_mangle]
pub extern "C-unwind" fn bd_time_now_ms(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.now_ms()
}

#[no_mangle]
pub extern "C-unwind" fn bd_time_sleep_ms(rt: *mut Runtime, millis: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if millis < 0 {
        runtime_error(rt, "Sleep duration must be >= 0.");
        return 0;
    }
    std::thread::sleep(Duration::from_millis(millis as u64));
    millis
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
use crate::rt_core::*;
use std::path::{Component, Path, PathBuf};
use std::time::Duration;
