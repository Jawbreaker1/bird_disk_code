use crate::rt_core::*;
use std::io::{Read, Write};
use std::io::ErrorKind;
use std::net::Shutdown;
use std::time::Duration;

fn net_string_arg(rt: &Runtime, handle: HeapHandle, op: &'static str) -> Option<String> {
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return None,
    };
    match std::str::from_utf8(bytes) {
        Ok(value) => Some(value.to_string()),
        Err(_) => {
            net_error(rt, format!("{op} received invalid UTF-8 address."));
            None
        }
    }
}

fn alloc_net_handle(rt: &mut Runtime, book_id: u64, op: &'static str) -> Option<HeapHandle> {
    if book_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return None;
    }
    maybe_collect(rt);
    match rt.heap_mut().alloc_object(book_id as u32, 0) {
        Some(value) => Some(value),
        None => {
            net_error(rt, format!("{op} failed: out of memory."));
            None
        }
    }
}

fn tcp_stream_handle(rt: &Runtime, value: u64, op: &'static str) -> Option<HeapHandle> {
    let handle = heap_handle(rt, value)?;
    if rt.tcp_streams.contains_key(&handle.as_u32()) {
        Some(handle)
    } else {
        net_error(rt, format!("{op} failed: TcpStream handle is invalid."));
        None
    }
}

fn tcp_listener_handle(rt: &Runtime, value: u64, op: &'static str) -> Option<HeapHandle> {
    let handle = heap_handle(rt, value)?;
    if rt.tcp_listeners.contains_key(&handle.as_u32()) {
        Some(handle)
    } else {
        net_error(rt, format!("{op} failed: TcpListener handle is invalid."));
        None
    }
}

fn tcp_pool_handle(rt: &Runtime, value: u64, op: &'static str) -> Option<HeapHandle> {
    let handle = heap_handle(rt, value)?;
    if rt.tcp_pools.contains_key(&handle.as_u32()) {
        Some(handle)
    } else {
        net_error(rt, format!("{op} failed: TcpPool handle is invalid."));
        None
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_connect(rt: *mut Runtime, addr: u64, stream_book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let addr_handle = match heap_handle(rt, addr) {
        Some(value) => value,
        None => return 0,
    };
    let addr_text = match net_string_arg(rt, addr_handle, "std::net::connect") {
        Some(value) => value,
        None => return 0,
    };
    let stream = match std::net::TcpStream::connect(addr_text.as_str()) {
        Ok(value) => value,
        Err(err) => {
            net_error(rt, format!("std::net::connect failed: {err}"));
            return 0;
        }
    };
    let handle = match alloc_net_handle(rt, stream_book_id, "std::net::connect") {
        Some(value) => value,
        None => return 0,
    };
    rt.register_tcp_stream(handle, stream);
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_listen(rt: *mut Runtime, addr: u64, listener_book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let addr_handle = match heap_handle(rt, addr) {
        Some(value) => value,
        None => return 0,
    };
    let addr_text = match net_string_arg(rt, addr_handle, "std::net::listen") {
        Some(value) => value,
        None => return 0,
    };
    let listener = match std::net::TcpListener::bind(addr_text.as_str()) {
        Ok(value) => value,
        Err(err) => {
            net_error(rt, format!("std::net::listen failed: {err}"));
            return 0;
        }
    };
    let handle = match alloc_net_handle(rt, listener_book_id, "std::net::listen") {
        Some(value) => value,
        None => return 0,
    };
    rt.register_tcp_listener(handle, listener);
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_listener_addr(rt: *mut Runtime, listener: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let listener_handle = match tcp_listener_handle(rt, listener, "std::net::listener_addr") {
        Some(value) => value,
        None => return 0,
    };
    let addr = match rt.tcp_listener_mut(listener_handle) {
        Some(value) => match value.local_addr() {
            Ok(addr) => addr.to_string(),
            Err(err) => {
                net_error(rt, format!("std::net::listener_addr failed: {err}"));
                return 0;
            }
        },
        None => {
            net_error(
                rt,
                "std::net::listener_addr failed: TcpListener handle is invalid.",
            );
            return 0;
        }
    };
    match alloc_string_from_bytes(rt, addr.as_bytes()) {
        Some(handle) => handle.as_u32() as u64,
        None => {
            net_error(rt, "std::net::listener_addr failed: out of memory.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_accept(
    rt: *mut Runtime,
    listener: u64,
    stream_book_id: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let listener_handle = match tcp_listener_handle(rt, listener, "std::net::accept") {
        Some(value) => value,
        None => return 0,
    };
    let (stream, _) = match rt.tcp_listener_mut(listener_handle) {
        Some(value) => match value.accept() {
            Ok(pair) => pair,
            Err(err) => {
                net_error(rt, format!("std::net::accept failed: {err}"));
                return 0;
            }
        },
        None => {
            net_error(
                rt,
                "std::net::accept failed: TcpListener handle is invalid.",
            );
            return 0;
        }
    };
    let stream_handle = match alloc_net_handle(rt, stream_book_id, "std::net::accept") {
        Some(value) => value,
        None => return 0,
    };
    rt.register_tcp_stream(stream_handle, stream);
    stream_handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_write_text(rt: *mut Runtime, stream: u64, text: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::write_text") {
        Some(value) => value,
        None => return 0,
    };
    let text_handle = match heap_handle(rt, text) {
        Some(value) => value,
        None => return 0,
    };
    let text_value = match net_string_arg(rt, text_handle, "std::net::write_text") {
        Some(value) => value,
        None => return 0,
    };
    match rt.tcp_stream_mut(stream_handle) {
        Some(value) => {
            if let Err(err) = value.write_all(text_value.as_bytes()) {
                net_error(rt, format!("std::net::write_text failed: {err}"));
                return 0;
            }
        }
        None => {
            net_error(
                rt,
                "std::net::write_text failed: TcpStream handle is invalid.",
            );
            return 0;
        }
    }
    text_value.len() as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_read_line(rt: *mut Runtime, stream: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::read_line") {
        Some(value) => value,
        None => return 0,
    };
    let mut bytes = Vec::new();
    let mut byte = [0u8; 1];
    loop {
        let read_res = match rt.tcp_stream_mut(stream_handle) {
            Some(value) => value.read(&mut byte),
            None => {
                net_error(
                    rt,
                    "std::net::read_line failed: TcpStream handle is invalid.",
                );
                return 0;
            }
        };
        match read_res {
            Ok(0) => break,
            Ok(_) => {
                if byte[0] == b'\n' {
                    break;
                }
                bytes.push(byte[0]);
            }
            Err(err) => match err.kind() {
                // Peer closed/reset mid-read: treat as EOF for line-oriented callers.
                ErrorKind::ConnectionReset
                | ErrorKind::ConnectionAborted
                | ErrorKind::BrokenPipe
                | ErrorKind::UnexpectedEof
                | ErrorKind::NotConnected => break,
                ErrorKind::Interrupted => continue,
                _ => {
                    net_error(rt, format!("std::net::read_line failed: {err}"));
                    return 0;
                }
            }
        }
    }
    if bytes.last() == Some(&b'\r') {
        bytes.pop();
    }
    let value = match String::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            net_error(rt, "std::net::read_line received invalid UTF-8.");
            return 0;
        }
    };
    match alloc_string_from_bytes(rt, value.as_bytes()) {
        Some(handle) => handle.as_u32() as u64,
        None => {
            net_error(rt, "std::net::read_line failed: out of memory.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_read_exact(rt: *mut Runtime, stream: u64, len: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if len < 0 {
        net_error(rt, "std::net::read_exact expects len >= 0.");
        return 0;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::read_exact") {
        Some(value) => value,
        None => return 0,
    };
    let target_len = len as usize;
    let mut bytes = vec![0u8; target_len];
    let mut offset = 0usize;
    while offset < target_len {
        let read_res = match rt.tcp_stream_mut(stream_handle) {
            Some(value) => value.read(&mut bytes[offset..]),
            None => {
                net_error(
                    rt,
                    "std::net::read_exact failed: TcpStream handle is invalid.",
                );
                return 0;
            }
        };
        match read_res {
            Ok(0) => {
                net_error(
                    rt,
                    "std::net::read_exact reached EOF before reading requested length.",
                );
                return 0;
            }
            Ok(n) => offset += n,
            Err(err) => {
                net_error(rt, format!("std::net::read_exact failed: {err}"));
                return 0;
            }
        }
    }
    match String::from_utf8(bytes) {
        Ok(value) => match alloc_string_from_bytes(rt, value.as_bytes()) {
            Some(handle) => handle.as_u32() as u64,
            None => {
                net_error(rt, "std::net::read_exact failed: out of memory.");
                0
            }
        },
        Err(_) => {
            net_error(rt, "std::net::read_exact received invalid UTF-8 bytes.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_read_to_end(rt: *mut Runtime, stream: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::read_to_end") {
        Some(value) => value,
        None => return 0,
    };
    let mut bytes = Vec::new();
    let mut chunk = [0u8; 4096];
    loop {
        let read_res = match rt.tcp_stream_mut(stream_handle) {
            Some(value) => value.read(&mut chunk),
            None => {
                net_error(
                    rt,
                    "std::net::read_to_end failed: TcpStream handle is invalid.",
                );
                return 0;
            }
        };
        match read_res {
            Ok(0) => break,
            Ok(n) => bytes.extend_from_slice(&chunk[..n]),
            Err(err) => {
                net_error(rt, format!("std::net::read_to_end failed: {err}"));
                return 0;
            }
        }
    }
    match String::from_utf8(bytes) {
        Ok(value) => match alloc_string_from_bytes(rt, value.as_bytes()) {
            Some(handle) => handle.as_u32() as u64,
            None => {
                net_error(rt, "std::net::read_to_end failed: out of memory.");
                0
            }
        },
        Err(_) => {
            net_error(rt, "std::net::read_to_end received invalid UTF-8 bytes.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_set_read_timeout_ms(
    rt: *mut Runtime,
    stream: u64,
    timeout_ms: i64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::set_read_timeout_ms") {
        Some(value) => value,
        None => return 0,
    };
    if timeout_ms < 0 {
        net_error(rt, "std::net::set_read_timeout_ms expects timeout >= 0.");
        return 0;
    }
    let timeout = if timeout_ms == 0 {
        None
    } else {
        Some(Duration::from_millis(timeout_ms as u64))
    };
    match rt.tcp_stream_mut(stream_handle) {
        Some(value) => {
            if let Err(err) = value.set_read_timeout(timeout) {
                net_error(rt, format!("std::net::set_read_timeout_ms failed: {err}"));
                return 0;
            }
        }
        None => {
            net_error(
                rt,
                "std::net::set_read_timeout_ms failed: TcpStream handle is invalid.",
            );
            return 0;
        }
    }
    timeout_ms
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_pool(
    rt: *mut Runtime,
    addr: u64,
    max_idle: i64,
    pool_book_id: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if max_idle < 0 {
        net_error(rt, "std::net::pool expects max_idle >= 0.");
        return 0;
    }
    let addr_handle = match heap_handle(rt, addr) {
        Some(value) => value,
        None => return 0,
    };
    let addr_text = match net_string_arg(rt, addr_handle, "std::net::pool") {
        Some(value) => value,
        None => return 0,
    };
    let pool_handle = match alloc_net_handle(rt, pool_book_id, "std::net::pool") {
        Some(value) => value,
        None => return 0,
    };
    rt.register_tcp_pool(
        pool_handle,
        TcpPoolState {
            addr: addr_text,
            max_idle: max_idle as usize,
            idle: Vec::new(),
        },
    );
    pool_handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_pool_get(rt: *mut Runtime, pool: u64, stream_book_id: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let pool_handle = match tcp_pool_handle(rt, pool, "std::net::pool_get") {
        Some(value) => value,
        None => return 0,
    };
    let stream = if let Some(stream) = match rt.tcp_pool_mut(pool_handle) {
        Some(state) => Some(state.idle.pop()),
        None => {
            net_error(rt, "std::net::pool_get failed: TcpPool handle is invalid.");
            return 0;
        }
    }
    .flatten()
    {
        stream
    } else {
        let addr = match rt.tcp_pool_mut(pool_handle) {
            Some(state) => state.addr.clone(),
            None => {
                net_error(rt, "std::net::pool_get failed: TcpPool handle is invalid.");
                return 0;
            }
        };
        match std::net::TcpStream::connect(addr.as_str()) {
            Ok(stream) => stream,
            Err(err) => {
                net_error(rt, format!("std::net::pool_get failed: {err}"));
                return 0;
            }
        }
    };
    let stream_handle = match alloc_net_handle(rt, stream_book_id, "std::net::pool_get") {
        Some(value) => value,
        None => return 0,
    };
    rt.register_tcp_stream(stream_handle, stream);
    stream_handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_pool_put(rt: *mut Runtime, pool: u64, stream: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let pool_handle = match tcp_pool_handle(rt, pool, "std::net::pool_put") {
        Some(value) => value,
        None => return 0,
    };
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::pool_put") {
        Some(value) => value,
        None => return 0,
    };
    let Some(stream_value) = rt.take_tcp_stream(stream_handle) else {
        net_error(
            rt,
            "std::net::pool_put failed: TcpStream handle is invalid.",
        );
        return 0;
    };
    let mut stream_slot = Some(stream_value);
    let keep = {
        let pool_state = match rt.tcp_pool_mut(pool_handle) {
            Some(value) => value,
            None => {
                net_error(rt, "std::net::pool_put failed: TcpPool handle is invalid.");
                return 0;
            }
        };
        if pool_state.idle.len() < pool_state.max_idle {
            let stream = match stream_slot.take() {
                Some(value) => value,
                None => {
                    net_error(
                        rt,
                        "std::net::pool_put failed: TcpStream handle is invalid.",
                    );
                    return 0;
                }
            };
            pool_state.idle.push(stream);
            true
        } else {
            false
        }
    };
    if !keep {
        if let Some(stream) = stream_slot.take() {
            let _ = stream.shutdown(Shutdown::Both);
        }
    }
    if keep {
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_pool_close(rt: *mut Runtime, pool: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let pool_handle = match tcp_pool_handle(rt, pool, "std::net::pool_close") {
        Some(value) => value,
        None => return,
    };
    let Some(state) = rt.close_tcp_pool(pool_handle) else {
        net_error(
            rt,
            "std::net::pool_close failed: TcpPool handle is invalid.",
        );
        return;
    };
    for stream in state.idle {
        let _ = stream.shutdown(Shutdown::Both);
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_close_stream(rt: *mut Runtime, stream: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let stream_handle = match tcp_stream_handle(rt, stream, "std::net::close_stream") {
        Some(value) => value,
        None => return,
    };
    let Some(stream) = rt.take_tcp_stream(stream_handle) else {
        net_error(
            rt,
            "std::net::close_stream failed: TcpStream handle is invalid.",
        );
        return;
    };
    let _ = stream.shutdown(Shutdown::Both);
}

#[no_mangle]
pub extern "C-unwind" fn bd_net_close_listener(rt: *mut Runtime, listener: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let listener_handle = match tcp_listener_handle(rt, listener, "std::net::close_listener") {
        Some(value) => value,
        None => return,
    };
    if !rt.close_tcp_listener(listener_handle) {
        net_error(
            rt,
            "std::net::close_listener failed: TcpListener handle is invalid.",
        );
    }
}
