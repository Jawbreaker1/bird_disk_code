use crate::rt_core::*;

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
