use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_root_push(rt: *mut Runtime, slots: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let slots = match usize::try_from(slots) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    match rt.roots.push_frame(slots) {
        Some(base) => base as u64,
        None => {
            oom_error(rt);
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_root_pop(rt: *mut Runtime, slots: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let slots = match usize::try_from(slots) {
        Ok(value) => value,
        Err(_) => {
            invalid_heap_error(rt);
            return;
        }
    };
    rt.roots.pop_frame(slots);
}

#[no_mangle]
pub extern "C-unwind" fn bd_root_set(rt: *mut Runtime, slot: u64, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match usize::try_from(slot) {
        Ok(value) => value,
        Err(_) => {
            invalid_heap_error(rt);
            return;
        }
    };
    rt.roots.set_slot(slot, RootValue::Ptr(handle));
}

#[no_mangle]
pub extern "C-unwind" fn bd_trace_push(rt: *mut Runtime, id: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let id = match usize::try_from(id) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid trace frame.");
            return;
        }
    };
    if id >= rt.trace_frames.len() {
        runtime_error(rt, "Invalid trace frame.");
        return;
    }
    if !rt.trace.push(id) {
        oom_error(rt);
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_trace_pop(rt: *mut Runtime) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    rt.trace.pop();
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_string(rt: *mut Runtime, ptr: *const u8, len: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let len = match usize::try_from(len) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    let handle = match rt.heap_mut().alloc_string(len) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if len == 0 {
        return handle.as_u32() as u64;
    }
    if ptr.is_null() {
        runtime_error(rt, "String data pointer was null.");
        return 0;
    }
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
    payload[..len].copy_from_slice(bytes);
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_array(
    rt: *mut Runtime,
    elem_kind: u64,
    elem_size_arg: u64,
    len: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let kind = match parse_elem_kind(rt, elem_kind) {
        Some(value) => value,
        None => return 0,
    };
    let expected_size = elem_size(kind);
    if elem_size_arg as usize != expected_size {
        invalid_heap_error(rt);
        return 0;
    }
    let len_i64 = len as i64;
    if len_i64 < 0 {
        runtime_error(rt, "Array length must be >= 0.");
        return 0;
    }
    let len = match usize::try_from(len_i64) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    let handle = match rt.heap_mut().alloc_array(kind, len, expected_size) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_object(
    rt: *mut Runtime,
    book_id: u64,
    field_count: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let field_count = match usize::try_from(field_count) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    if book_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    let handle = rt
        .heap_mut()
        .alloc_object(book_id as u32, field_count);
    let handle = match handle {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_enum(
    rt: *mut Runtime,
    enum_id: u64,
    variant_id: u64,
    payload_kind: u64,
    payload_len: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    if enum_id > u32::MAX as u64 || variant_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    let payload_len = match usize::try_from(payload_len) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    let payload_kind = if payload_len == 0 {
        if payload_kind != 0 {
            invalid_heap_error(rt);
            return 0;
        }
        0
    } else {
        let kind = match parse_elem_kind(rt, payload_kind) {
            Some(value) => value,
            None => return 0,
        };
        kind as u32
    };
    let handle = match rt.heap_mut().alloc_enum(
        enum_id as u32,
        variant_id as u32,
        payload_kind,
        payload_len,
    ) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}
