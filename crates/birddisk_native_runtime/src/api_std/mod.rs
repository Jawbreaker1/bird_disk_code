mod bytes;
mod channel;
mod env;
mod fs;
mod io;
mod json;
mod net;
mod path;
mod profiler;
mod rand;
mod string;
mod test;
mod thread;
mod time;

pub use bytes::{bd_bytes_contains, bd_bytes_eq, bd_bytes_index_of, bd_bytes_len, bd_bytes_slice};
pub use channel::{
    bd_channel_bool, bd_channel_bytes, bd_channel_close_bool, bd_channel_close_bytes,
    bd_channel_close_f64, bd_channel_close_i64, bd_channel_close_string, bd_channel_close_u8,
    bd_channel_f64, bd_channel_i64, bd_channel_recv_bool, bd_channel_recv_bytes,
    bd_channel_recv_f64, bd_channel_recv_i64, bd_channel_recv_string, bd_channel_recv_u8,
    bd_channel_send_bool, bd_channel_send_bytes, bd_channel_send_f64, bd_channel_send_i64,
    bd_channel_send_string, bd_channel_send_u8, bd_channel_string, bd_channel_u8,
};
pub use env::{bd_env_args, bd_env_cwd, bd_env_get, bd_env_set, bd_env_set_cwd};
pub use fs::{bd_fs_read_bytes, bd_fs_read_text, bd_fs_write_bytes, bd_fs_write_text};
pub use io::{bd_io_print, bd_io_read_line};
pub use json::{
    bd_json_decode_bool, bd_json_decode_i64, bd_json_decode_string, bd_json_encode_bool,
    bd_json_encode_i64, bd_json_encode_string,
};
pub use net::{
    bd_net_accept, bd_net_close_listener, bd_net_close_stream, bd_net_connect,
    bd_net_listener_addr, bd_net_listen, bd_net_pool, bd_net_pool_close, bd_net_pool_get,
    bd_net_pool_put, bd_net_read_exact, bd_net_read_line, bd_net_read_to_end,
    bd_net_set_read_timeout_ms, bd_net_write_text,
};
pub use path::{bd_path_basename, bd_path_dirname, bd_path_join, bd_path_normalize};
pub use profiler::{
    bd_profiler_alloc_count, bd_profiler_bytes_allocated, bd_profiler_bytes_in_use,
    bd_profiler_gc_runs, bd_profiler_last_freed, bd_profiler_last_freed_bytes,
    bd_profiler_last_live, bd_profiler_last_live_bytes, bd_profiler_peak_bytes_in_use,
    bd_profiler_uptime_ms,
};
pub use rand::{bd_rand_range, bd_rand_seed};
pub use string::{
    bd_string_bytes, bd_string_concat, bd_string_contains, bd_string_eq, bd_string_from_bytes,
    bd_string_from_i64, bd_string_index_of, bd_string_len, bd_string_replace, bd_string_slice,
    bd_string_to_i64,
};
pub use test::{
    bd_test_assert, bd_test_assert_eq_bool, bd_test_assert_eq_i64, bd_test_assert_eq_string,
};
pub use thread::{
    bd_thread_join, bd_thread_spawn_i64_0, bd_thread_spawn_i64_1,
    bd_thread_spawn_i64_stream_i64_2, bd_thread_store,
};
pub use time::{bd_time_now_ms, bd_time_sleep_ms};

use crate::rt_core::*;
use std::path::{Component, Path, PathBuf};

fn path_from_handle(rt: &Runtime, handle: HeapHandle) -> Option<String> {
    let path_bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return None,
    };
    match std::str::from_utf8(path_bytes) {
        Ok(value) => Some(value.to_string()),
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            None
        }
    }
}

fn alloc_string_from_path(
    rt: &mut Runtime,
    message: &'static str,
    path: &Path,
) -> Option<HeapHandle> {
    let text = match path.to_str() {
        Some(value) => value,
        None => {
            runtime_error(rt, message);
            return None;
        }
    };
    match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => Some(value),
        None => {
            oom_error(rt);
            None
        }
    }
}

fn normalize_path(path: &str) -> PathBuf {
    let mut out = PathBuf::new();
    let mut parts: Vec<std::ffi::OsString> = Vec::new();
    let mut has_root = false;
    for component in Path::new(path).components() {
        match component {
            Component::Prefix(prefix) => out.push(prefix.as_os_str()),
            Component::RootDir => {
                out.push(component.as_os_str());
                has_root = true;
            }
            Component::CurDir => {}
            Component::ParentDir => {
                if let Some(last) = parts.last() {
                    if last != std::ffi::OsStr::new("..") {
                        parts.pop();
                    } else {
                        parts.push(std::ffi::OsString::from(".."));
                    }
                } else if !has_root {
                    parts.push(std::ffi::OsString::from(".."));
                }
            }
            Component::Normal(part) => parts.push(part.to_os_string()),
        }
    }
    for part in parts {
        out.push(part);
    }
    if out.as_os_str().is_empty() {
        out.push(".");
    }
    out
}

fn parse_string_i64(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = 0;
    let mut sign: i128 = 1;
    if bytes[0] == b'-' {
        sign = -1;
        idx = 1;
        if idx == bytes.len() {
            return None;
        }
    }
    let mut value: i128 = 0;
    while idx < bytes.len() {
        let ch = bytes[idx];
        if !(b'0'..=b'9').contains(&ch) {
            return None;
        }
        value = value * 10 + (ch - b'0') as i128;
        idx += 1;
    }
    value *= sign;
    if value < i64::MIN as i128 || value > i64::MAX as i128 {
        return None;
    }
    Some(value as i64)
}

fn trim_ascii_whitespace(text: &str) -> &str {
    let bytes = text.as_bytes();
    let mut start = 0;
    let mut end = bytes.len();
    while start < end && is_ascii_whitespace(bytes[start]) {
        start += 1;
    }
    while end > start && is_ascii_whitespace(bytes[end - 1]) {
        end -= 1;
    }
    &text[start..end]
}

fn is_ascii_whitespace(byte: u8) -> bool {
    matches!(byte, b' ' | b'\n' | b'\r' | b'\t')
}

fn json_encode_string(text: &str) -> Option<String> {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{0008}' => out.push_str("\\b"),
            '\u{000c}' => out.push_str("\\f"),
            ch if (ch as u32) < 0x20 => return None,
            _ => out.push(ch),
        }
    }
    out.push('"');
    Some(out)
}

fn json_decode_bool(text: &str) -> Option<bool> {
    match trim_ascii_whitespace(text) {
        "true" => Some(true),
        "false" => Some(false),
        _ => None,
    }
}

fn json_decode_string(text: &str) -> Option<String> {
    let trimmed = trim_ascii_whitespace(text);
    let bytes = trimmed.as_bytes();
    if bytes.len() < 2 || bytes[0] != b'"' || bytes[bytes.len() - 1] != b'"' {
        return None;
    }
    let mut out = Vec::with_capacity(bytes.len().saturating_sub(2));
    let mut idx = 1;
    let end = bytes.len() - 1;
    while idx < end {
        let byte = bytes[idx];
        if byte == b'\\' {
            idx += 1;
            if idx >= end {
                return None;
            }
            let escaped = match bytes[idx] {
                b'"' => b'"',
                b'\\' => b'\\',
                b'/' => b'/',
                b'b' => 0x08,
                b'f' => 0x0c,
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'u' => return None,
                _ => return None,
            };
            out.push(escaped);
        } else {
            if byte < 0x20 {
                return None;
            }
            out.push(byte);
        }
        idx += 1;
    }
    String::from_utf8(out).ok()
}
