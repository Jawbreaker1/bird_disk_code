use crate::rt_core::*;
use birddisk_core::runtime as abi;

fn object_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Object {
        runtime_error(rt, "Expected book handle.");
        return None;
    }
    Some(header)
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
pub extern "C-unwind" fn bd_object_get_f64(rt: *mut Runtime, handle: u64, index: i64) -> f64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0.0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0.0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0.0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0.0;
        }
    };
    f64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_f64(
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
