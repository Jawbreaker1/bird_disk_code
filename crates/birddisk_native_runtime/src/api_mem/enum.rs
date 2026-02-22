use crate::rt_core::*;

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
pub extern "C-unwind" fn bd_enum_payload_f64(rt: *mut Runtime, handle: u64) -> f64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0.0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let header = match enum_header(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    if header.aux != ElemKind::F64 as u32 {
        runtime_error(rt, "Enum payload type mismatch.");
        return 0.0;
    }
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let bytes = match payload.get(0..8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Enum payload missing.");
            return 0.0;
        }
    };
    f64::from_le_bytes(bytes.try_into().unwrap())
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
    if byte == 0 {
        0
    } else {
        1
    }
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
pub extern "C-unwind" fn bd_enum_set_payload_i64(rt: *mut Runtime, handle: u64, value: i64) {
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
pub extern "C-unwind" fn bd_enum_set_payload_f64(rt: *mut Runtime, handle: u64, value: f64) {
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
    if header.aux != ElemKind::F64 as u32 {
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
pub extern "C-unwind" fn bd_enum_set_payload_bool(rt: *mut Runtime, handle: u64, value: i64) {
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
pub extern "C-unwind" fn bd_enum_set_payload_u8(rt: *mut Runtime, handle: u64, value: i64) {
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
pub extern "C-unwind" fn bd_enum_set_payload_ref(rt: *mut Runtime, handle: u64, value: u64) {
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
