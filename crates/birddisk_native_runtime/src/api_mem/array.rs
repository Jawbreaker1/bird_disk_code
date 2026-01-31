use crate::rt_core::*;

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
pub extern "C-unwind" fn bd_array_get_f64(rt: *mut Runtime, handle: u64, index: i64) -> f64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0.0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let header = match array_header(rt, handle, ElemKind::F64) {
        Some(value) => value,
        None => return 0.0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0.0,
    };
    let offset = idx * elem_size(ElemKind::F64);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0.0;
        }
    };
    f64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_f64(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: f64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::F64) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::F64);
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
    let target = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *target = if value == 0 { 0 } else { 1 };
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
    let target = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *target = value as u8;
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
    u64::from_le_bytes(bytes.try_into().unwrap())
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
