#![allow(dead_code)]

mod api_mem;
mod api_std;
mod rt_core;

#[cfg(test)]
mod tests;

pub use birddisk_core::{Position, Span, TraceFrame};
pub use rt_core::{NativeTrap, Runtime};

pub use api_mem::{
    bd_alloc_array, bd_alloc_enum, bd_alloc_object, bd_alloc_string, bd_array_get_bool,
    bd_array_get_f64, bd_array_get_i64, bd_array_get_ref, bd_array_get_u8, bd_array_set_bool,
    bd_array_set_f64, bd_array_set_i64, bd_array_set_ref, bd_array_set_u8, bd_clear_error,
    bd_enum_payload_bool, bd_enum_payload_f64, bd_enum_payload_i64, bd_enum_payload_ref,
    bd_enum_payload_u8, bd_enum_set_payload_bool, bd_enum_set_payload_f64,
    bd_enum_set_payload_i64, bd_enum_set_payload_ref, bd_enum_set_payload_u8, bd_enum_variant,
    bd_error_is_throw, bd_error_message, bd_has_error, bd_object_get_bool, bd_object_get_f64,
    bd_object_get_i64, bd_object_get_ref, bd_object_get_u8, bd_object_set_bool,
    bd_object_set_f64, bd_object_set_i64, bd_object_set_ref, bd_object_set_u8, bd_root_pop,
    bd_root_push, bd_root_set, bd_throw, bd_trace_pop, bd_trace_push,
};

pub use api_std::{
    bd_bytes_contains, bd_bytes_eq, bd_bytes_index_of, bd_bytes_len, bd_bytes_slice, bd_env_args,
    bd_env_cwd, bd_env_get, bd_env_set, bd_env_set_cwd, bd_fs_read_bytes, bd_fs_read_text,
    bd_fs_write_bytes, bd_fs_write_text, bd_io_print, bd_io_read_line, bd_json_decode_bool,
    bd_json_decode_i64, bd_json_decode_string, bd_json_encode_bool, bd_json_encode_i64,
    bd_json_encode_string, bd_path_basename, bd_path_dirname, bd_path_join, bd_path_normalize,
    bd_rand_range, bd_rand_seed, bd_string_bytes, bd_string_concat, bd_string_contains,
    bd_string_eq, bd_string_from_bytes, bd_string_from_i64, bd_string_index_of, bd_string_len,
    bd_string_replace, bd_string_slice, bd_string_to_i64, bd_time_now_ms, bd_time_sleep_ms,
};
