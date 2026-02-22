use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_has_error(rt: *mut Runtime) -> i64 {
    let rt = runtime_ref(rt);
    if rt.has_error() {
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_error_is_throw(rt: *mut Runtime) -> i64 {
    let rt = runtime_ref(rt);
    if rt.error_is_throw() {
        1
    } else {
        0
    }
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
