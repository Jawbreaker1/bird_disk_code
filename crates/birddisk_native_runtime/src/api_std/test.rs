use crate::rt_core::*;

fn throw_if_false(rt: &mut Runtime, cond: bool, message_handle: u64) {
    if cond || rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, message_handle) {
        Some(value) => value,
        None => return,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let message = String::from_utf8_lossy(bytes).to_string();
    throw_error(rt, handle, message);
}

#[no_mangle]
pub extern "C-unwind" fn bd_test_assert(rt: *mut Runtime, cond: i64, message: u64) {
    let rt = runtime_mut(rt);
    throw_if_false(rt, cond != 0, message);
}

#[no_mangle]
pub extern "C-unwind" fn bd_test_assert_eq_i64(
    rt: *mut Runtime,
    left: i64,
    right: i64,
    message: u64,
) {
    let rt = runtime_mut(rt);
    throw_if_false(rt, left == right, message);
}

#[no_mangle]
pub extern "C-unwind" fn bd_test_assert_eq_bool(
    rt: *mut Runtime,
    left: i64,
    right: i64,
    message: u64,
) {
    let rt = runtime_mut(rt);
    throw_if_false(rt, left == right, message);
}

#[no_mangle]
pub extern "C-unwind" fn bd_test_assert_eq_string(
    rt: *mut Runtime,
    left: u64,
    right: u64,
    message: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return,
    };
    let left_bytes = match string_bytes_slice(rt, left_handle) {
        Some(value) => value,
        None => return,
    };
    let right_bytes = match string_bytes_slice(rt, right_handle) {
        Some(value) => value,
        None => return,
    };
    let equal = left_bytes == right_bytes;
    throw_if_false(rt, equal, message);
}
