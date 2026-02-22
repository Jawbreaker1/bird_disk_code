use crate::rt_core::*;

fn alloc_channel(rt: &mut Runtime, book_id: u64, kind: ChannelKind) -> u64 {
    if rt.has_error() {
        return 0;
    }
    if book_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    maybe_collect(rt);
    let handle = match rt.heap_mut().alloc_object(book_id as u32, 0) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    rt.register_channel(handle, kind);
    handle.as_u32() as u64
}

fn channel_state<'a>(
    rt: &'a mut Runtime,
    handle: u64,
    kind: ChannelKind,
) -> Option<&'a mut ChannelState> {
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return None,
    };
    let key = handle.as_u32();
    let mismatch = {
        let state = match rt.channels.get(&key) {
            Some(value) => value,
            None => {
                channel_error(rt, "Channel state missing at runtime.");
                return None;
            }
        };
        state.kind != kind
    };
    if mismatch {
        channel_error(rt, "Channel kind mismatch at runtime.");
        return None;
    }
    rt.channels.get_mut(&key)
}

fn channel_send_i64(rt: &mut Runtime, handle: u64, value: i64, kind: ChannelKind) -> i64 {
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::I64(value));
    1
}

fn channel_send_bool(rt: &mut Runtime, handle: u64, value: i64, kind: ChannelKind) -> i64 {
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::Bool(value != 0));
    1
}

fn channel_send_f64(rt: &mut Runtime, handle: u64, value: f64, kind: ChannelKind) -> i64 {
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::F64(value));
    1
}

fn channel_send_u8(rt: &mut Runtime, handle: u64, value: i64, kind: ChannelKind) -> i64 {
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return 0;
    }
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::U8(value as u8));
    1
}

fn channel_send_string(rt: &mut Runtime, handle: u64, value: u64, kind: ChannelKind) -> i64 {
    let value_handle = match heap_handle(rt, value) {
        Some(value) => value,
        None => return 0,
    };
    if string_header(rt, value_handle).is_none() {
        return 0;
    }
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::Ref(value_handle));
    1
}

fn channel_send_bytes(rt: &mut Runtime, handle: u64, value: u64, kind: ChannelKind) -> i64 {
    let value_handle = match heap_handle(rt, value) {
        Some(value) => value,
        None => return 0,
    };
    if bytes_header(rt, value_handle).is_none() {
        return 0;
    }
    let state = match channel_state(rt, handle, kind) {
        Some(value) => value,
        None => return 0,
    };
    if state.closed {
        return 0;
    }
    state.queue.push_back(ChannelValue::Ref(value_handle));
    1
}

fn channel_recv(
    rt: &mut Runtime,
    handle: u64,
    kind: ChannelKind,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    if enum_id > u32::MAX as u64 || ok_variant > u32::MAX as u64 || closed_variant > u32::MAX as u64
    {
        invalid_heap_error(rt);
        return 0;
    }
    maybe_collect(rt);
    let (closed, value) = {
        let state = match channel_state(rt, handle, kind) {
            Some(value) => value,
            None => return 0,
        };
        let value = state.queue.pop_front();
        (state.closed, value)
    };
    if let Some(value) = value {
        let (payload_kind, payload_len) = match kind {
            ChannelKind::I64 => (ElemKind::I64, 8),
            ChannelKind::Bool => (ElemKind::Bool, 1),
            ChannelKind::F64 => (ElemKind::F64, 8),
            ChannelKind::U8 => (ElemKind::U8, 1),
            ChannelKind::String | ChannelKind::Bytes => (ElemKind::Ref, 8),
        };
        let handle = match rt.heap_mut().alloc_enum(
            enum_id as u32,
            ok_variant as u32,
            payload_kind as u32,
            payload_len,
        ) {
            Some(value) => value,
            None => {
                oom_error(rt);
                return 0;
            }
        };
        let payload = rt.heap_mut().payload_mut(handle);
        match value {
            ChannelValue::I64(value) => {
                if let Some(bytes) = payload.get_mut(0..8) {
                    bytes.copy_from_slice(&value.to_le_bytes());
                } else {
                    runtime_error(rt, "Enum payload missing.");
                    return 0;
                }
            }
            ChannelValue::Bool(value) => {
                if let Some(byte) = payload.get_mut(0) {
                    *byte = if value { 1 } else { 0 };
                } else {
                    runtime_error(rt, "Enum payload missing.");
                    return 0;
                }
            }
            ChannelValue::F64(value) => {
                if let Some(bytes) = payload.get_mut(0..8) {
                    bytes.copy_from_slice(&value.to_le_bytes());
                } else {
                    runtime_error(rt, "Enum payload missing.");
                    return 0;
                }
            }
            ChannelValue::U8(value) => {
                if let Some(byte) = payload.get_mut(0) {
                    *byte = value;
                } else {
                    runtime_error(rt, "Enum payload missing.");
                    return 0;
                }
            }
            ChannelValue::Ref(value) => {
                if let Some(bytes) = payload.get_mut(0..8) {
                    bytes.copy_from_slice(&(value.as_u32() as u64).to_le_bytes());
                } else {
                    runtime_error(rt, "Enum payload missing.");
                    return 0;
                }
            }
        }
        return handle.as_u32() as u64;
    }
    if closed {
        let handle = match rt
            .heap_mut()
            .alloc_enum(enum_id as u32, closed_variant as u32, 0, 0)
        {
            Some(value) => value,
            None => {
                oom_error(rt);
                return 0;
            }
        };
        return handle.as_u32() as u64;
    }
    channel_would_block_error(rt);
    0
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_i64(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::I64)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_bool(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::Bool)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_f64(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::F64)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_u8(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::U8)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_string(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::String)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_bytes(rt: *mut Runtime, book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    alloc_channel(rt, book_id, ChannelKind::Bytes)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_i64(rt: *mut Runtime, handle: u64, value: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_i64(rt, handle, value, ChannelKind::I64)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_bool(rt: *mut Runtime, handle: u64, value: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_bool(rt, handle, value, ChannelKind::Bool)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_f64(rt: *mut Runtime, handle: u64, value: f64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_f64(rt, handle, value, ChannelKind::F64)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_u8(rt: *mut Runtime, handle: u64, value: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_u8(rt, handle, value, ChannelKind::U8)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_string(rt: *mut Runtime, handle: u64, value: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_string(rt, handle, value, ChannelKind::String)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_send_bytes(rt: *mut Runtime, handle: u64, value: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_send_bytes(rt, handle, value, ChannelKind::Bytes)
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_i64(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::I64,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_bool(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::Bool,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_f64(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::F64,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_u8(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::U8,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_string(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::String,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_recv_bytes(
    rt: *mut Runtime,
    handle: u64,
    enum_id: u64,
    ok_variant: u64,
    closed_variant: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    channel_recv(
        rt,
        handle,
        ChannelKind::Bytes,
        enum_id,
        ok_variant,
        closed_variant,
    )
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_i64(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::I64) {
        state.closed = true;
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_bool(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::Bool) {
        state.closed = true;
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_f64(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::F64) {
        state.closed = true;
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_u8(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::U8) {
        state.closed = true;
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_string(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::String) {
        state.closed = true;
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_channel_close_bytes(rt: *mut Runtime, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if let Some(state) = channel_state(rt, handle, ChannelKind::Bytes) {
        state.closed = true;
    }
}
