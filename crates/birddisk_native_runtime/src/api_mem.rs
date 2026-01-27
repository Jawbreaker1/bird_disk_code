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
pub extern "C-unwind" fn bd_has_error(rt: *mut Runtime) -> i64 {
    let rt = runtime_ref(rt);
    if rt.has_error() { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_error_is_throw(rt: *mut Runtime) -> i64 {
    let rt = runtime_ref(rt);
    if rt.error_is_throw() { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_error_message(rt: *mut Runtime) -> u64 {
    let rt = runtime_ref(rt);
    rt.error_message_handle()
        .map(|handle| handle.as_u32() as u64)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C-unwind" fn bd_clear_error(rt: *mut Runtime) {
    let rt = runtime_ref(rt);
    rt.clear_error();
}

#[no_mangle]
pub extern "C-unwind" fn bd_throw(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let bytes = match crate::rt_core::string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Expected string handle.");
            return;
        }
    };
    let message = String::from_utf8_lossy(bytes).to_string();
    crate::rt_core::throw_error(rt, handle, message);
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

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_i64(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::I64) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::I64);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    i64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_i64(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::I64) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::I64);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_bool(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::Bool) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::Bool);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(offset) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    if byte == 0 { 0 } else { 1 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_bool(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::Bool) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::Bool);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *slot = if value == 0 { 0 } else { 1 };
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_u8(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::U8) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::U8);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(offset) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    byte as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_u8(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::U8) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::U8);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *slot = value as u8;
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_ref(rt: *mut Runtime, handle: u64, index: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::Ref) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::Ref);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    raw
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_ref(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::Ref) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::Ref);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

fn object_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Object {
        runtime_error(rt, "Expected book handle.");
        return None;
    }
    Some(header)
}

fn enum_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Enum {
        runtime_error(rt, "Expected enum handle.");
        return None;
    }
    Some(header)
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_variant(
    rt: *mut Runtime,
    handle: u64,
    expected_enum_id: u64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if expected_enum_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if header.type_id() != expected_enum_id as u32 {
        runtime_error(rt, "Enum type mismatch.");
        return 0;
    }
    header.len_or_size as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_payload_i64(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if header.aux != ElemKind::I64 as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return 0;
    }
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(0..8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return 0;
        }
    };
    i64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_payload_bool(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if header.aux != ElemKind::Bool as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return 0;
    }
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(0) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return 0;
        }
    };
    if byte == 0 { 0 } else { 1 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_payload_u8(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if header.aux != ElemKind::U8 as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return 0;
    }
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(0) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return 0;
        }
    };
    byte as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_payload_ref(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    if header.aux != ElemKind::Ref as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return 0;
    }
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(0..8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    if raw > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    raw
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_set_payload_i64(
    rt: *mut Runtime,
    handle: u64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    if header.aux != ElemKind::I64 as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return;
    }
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(0..8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return;
        }
    };
    slot.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_set_payload_bool(
    rt: *mut Runtime,
    handle: u64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    if header.aux != ElemKind::Bool as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return;
    }
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(0) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return;
        }
    };
    *slot = if value == 0 { 0 } else { 1 };
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_set_payload_u8(
    rt: *mut Runtime,
    handle: u64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    if header.aux != ElemKind::U8 as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return;
    }
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(0) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return;
        }
    };
    *slot = value as u8;
}

#[no_mangle]
pub extern "C-unwind" fn bd_enum_set_payload_ref(
    rt: *mut Runtime,
    handle: u64,
    value: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    if header.aux != ElemKind::Ref as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return;
    }
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(0..8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return;
        }
    };
    slot.copy_from_slice(&value.to_le_bytes());
}

fn object_index(rt: &Runtime, field_count: usize, index: i64) -> Option<usize> {
    if index < 0 {
        runtime_error(rt, "Field index out of bounds.");
        return None;
    }
    let index = index as usize;
    if index >= field_count {
        runtime_error(rt, "Field index out of bounds.");
        return None;
    }
    Some(index)
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_i64(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    i64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_i64(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_bool(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    if raw == 0 { 0 } else { 1 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_bool(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    let raw = if value == 0 { 0u64 } else { 1u64 };
    target.copy_from_slice(&raw.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_u8(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    raw as u8 as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_u8(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&(value as u64).to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_ref(rt: *mut Runtime, handle: u64, index: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    u64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_ref(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}
use crate::rt_core::*;
use birddisk_core::runtime as abi;
