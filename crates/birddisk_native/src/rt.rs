use crate::error::{native_error, NativeError};
use cranelift_codegen::ir::{types, AbiParam};
use cranelift_module::{FuncId, Linkage, Module};

#[derive(Clone, Copy)]
pub(crate) struct RuntimeFuncs {
    pub(crate) root_push: FuncId,
    pub(crate) root_pop: FuncId,
    pub(crate) root_set: FuncId,
    pub(crate) trace_push: FuncId,
    pub(crate) trace_pop: FuncId,
    pub(crate) has_error: FuncId,
    #[allow(dead_code)]
    pub(crate) error_is_throw: FuncId,
    pub(crate) error_message: FuncId,
    pub(crate) clear_error: FuncId,
    pub(crate) throw_error: FuncId,
    pub(crate) alloc_string: FuncId,
    pub(crate) alloc_array: FuncId,
    pub(crate) alloc_enum: FuncId,
    pub(crate) array_get_i64: FuncId,
    pub(crate) array_set_i64: FuncId,
    pub(crate) array_get_f64: FuncId,
    pub(crate) array_set_f64: FuncId,
    pub(crate) array_get_bool: FuncId,
    pub(crate) array_set_bool: FuncId,
    pub(crate) array_get_u8: FuncId,
    pub(crate) array_set_u8: FuncId,
    pub(crate) array_get_ref: FuncId,
    pub(crate) array_set_ref: FuncId,
    pub(crate) alloc_object: FuncId,
    pub(crate) object_get_i64: FuncId,
    pub(crate) object_set_i64: FuncId,
    pub(crate) object_get_f64: FuncId,
    pub(crate) object_set_f64: FuncId,
    pub(crate) object_get_bool: FuncId,
    pub(crate) object_set_bool: FuncId,
    pub(crate) object_get_u8: FuncId,
    pub(crate) object_set_u8: FuncId,
    pub(crate) object_get_ref: FuncId,
    pub(crate) object_set_ref: FuncId,
    pub(crate) enum_variant: FuncId,
    pub(crate) enum_payload_i64: FuncId,
    pub(crate) enum_payload_f64: FuncId,
    pub(crate) enum_payload_bool: FuncId,
    pub(crate) enum_payload_u8: FuncId,
    pub(crate) enum_payload_ref: FuncId,
    pub(crate) enum_set_payload_i64: FuncId,
    pub(crate) enum_set_payload_f64: FuncId,
    pub(crate) enum_set_payload_bool: FuncId,
    pub(crate) enum_set_payload_u8: FuncId,
    pub(crate) enum_set_payload_ref: FuncId,
    pub(crate) string_len: FuncId,
    pub(crate) string_concat: FuncId,
    pub(crate) string_eq: FuncId,
    pub(crate) string_bytes: FuncId,
    pub(crate) string_slice: FuncId,
    pub(crate) string_index_of: FuncId,
    pub(crate) string_contains: FuncId,
    pub(crate) string_replace: FuncId,
    pub(crate) string_from_bytes: FuncId,
    pub(crate) string_to_i64: FuncId,
    pub(crate) string_from_i64: FuncId,
    pub(crate) bytes_len: FuncId,
    pub(crate) bytes_eq: FuncId,
    pub(crate) bytes_slice: FuncId,
    pub(crate) bytes_index_of: FuncId,
    pub(crate) bytes_contains: FuncId,
    pub(crate) io_print: FuncId,
    pub(crate) io_read_line: FuncId,
    pub(crate) time_now_ms: FuncId,
    pub(crate) time_sleep_ms: FuncId,
    pub(crate) profiler_uptime_ms: FuncId,
    pub(crate) profiler_alloc_count: FuncId,
    pub(crate) profiler_bytes_allocated: FuncId,
    pub(crate) profiler_bytes_in_use: FuncId,
    pub(crate) profiler_peak_bytes_in_use: FuncId,
    pub(crate) profiler_gc_runs: FuncId,
    pub(crate) profiler_last_freed: FuncId,
    pub(crate) profiler_last_live: FuncId,
    pub(crate) profiler_last_freed_bytes: FuncId,
    pub(crate) profiler_last_live_bytes: FuncId,
    pub(crate) rand_seed: FuncId,
    pub(crate) rand_range: FuncId,
    pub(crate) test_assert: FuncId,
    pub(crate) test_assert_eq_i64: FuncId,
    pub(crate) test_assert_eq_bool: FuncId,
    pub(crate) test_assert_eq_string: FuncId,
    pub(crate) fs_read_text: FuncId,
    pub(crate) fs_write_text: FuncId,
    pub(crate) fs_read_bytes: FuncId,
    pub(crate) fs_write_bytes: FuncId,
    pub(crate) path_join: FuncId,
    pub(crate) path_normalize: FuncId,
    pub(crate) path_basename: FuncId,
    pub(crate) path_dirname: FuncId,
    pub(crate) env_args: FuncId,
    pub(crate) env_get: FuncId,
    pub(crate) env_set: FuncId,
    pub(crate) env_cwd: FuncId,
    pub(crate) env_set_cwd: FuncId,
    pub(crate) json_encode_i64: FuncId,
    pub(crate) json_encode_bool: FuncId,
    pub(crate) json_encode_string: FuncId,
    pub(crate) json_decode_i64: FuncId,
    pub(crate) json_decode_bool: FuncId,
    pub(crate) json_decode_string: FuncId,
    pub(crate) channel_i64: FuncId,
    pub(crate) channel_bool: FuncId,
    pub(crate) channel_f64: FuncId,
    pub(crate) channel_u8: FuncId,
    pub(crate) channel_string: FuncId,
    pub(crate) channel_bytes: FuncId,
    pub(crate) channel_send_i64: FuncId,
    pub(crate) channel_send_bool: FuncId,
    pub(crate) channel_send_f64: FuncId,
    pub(crate) channel_send_u8: FuncId,
    pub(crate) channel_send_string: FuncId,
    pub(crate) channel_send_bytes: FuncId,
    pub(crate) channel_recv_i64: FuncId,
    pub(crate) channel_recv_bool: FuncId,
    pub(crate) channel_recv_f64: FuncId,
    pub(crate) channel_recv_u8: FuncId,
    pub(crate) channel_recv_string: FuncId,
    pub(crate) channel_recv_bytes: FuncId,
    pub(crate) channel_close_i64: FuncId,
    pub(crate) channel_close_bool: FuncId,
    pub(crate) channel_close_f64: FuncId,
    pub(crate) channel_close_u8: FuncId,
    pub(crate) channel_close_string: FuncId,
    pub(crate) channel_close_bytes: FuncId,
    pub(crate) thread_store: FuncId,
    pub(crate) thread_join: FuncId,
    pub(crate) thread_spawn_i64_0: FuncId,
    pub(crate) thread_spawn_i64_1: FuncId,
    pub(crate) net_connect: FuncId,
    pub(crate) net_listen: FuncId,
    pub(crate) net_listener_addr: FuncId,
    pub(crate) net_accept: FuncId,
    pub(crate) net_write_text: FuncId,
    pub(crate) net_read_line: FuncId,
    pub(crate) net_read_exact: FuncId,
    pub(crate) net_read_to_end: FuncId,
    pub(crate) net_set_read_timeout_ms: FuncId,
    pub(crate) net_close_stream: FuncId,
    pub(crate) net_close_listener: FuncId,
    pub(crate) net_pool: FuncId,
    pub(crate) net_pool_get: FuncId,
    pub(crate) net_pool_put: FuncId,
    pub(crate) net_pool_close: FuncId,
}

impl RuntimeFuncs {
    pub(crate) fn declare<M: Module>(module: &mut M) -> Result<Self, NativeError> {
        let root_push = declare_runtime_func(
            module,
            "bd_root_push",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let root_pop = declare_runtime_func(module, "bd_root_pop", &[types::I64, types::I64], &[])?;
        let root_set = declare_runtime_func(
            module,
            "bd_root_set",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let trace_push =
            declare_runtime_func(module, "bd_trace_push", &[types::I64, types::I64], &[])?;
        let trace_pop = declare_runtime_func(module, "bd_trace_pop", &[types::I64], &[])?;
        let has_error = declare_runtime_func(module, "bd_has_error", &[types::I64], &[types::I64])?;
        let error_is_throw =
            declare_runtime_func(module, "bd_error_is_throw", &[types::I64], &[types::I64])?;
        let error_message =
            declare_runtime_func(module, "bd_error_message", &[types::I64], &[types::I64])?;
        let clear_error = declare_runtime_func(module, "bd_clear_error", &[types::I64], &[])?;
        let throw_error = declare_runtime_func(module, "bd_throw", &[types::I64, types::I64], &[])?;
        let alloc_string = declare_runtime_func(
            module,
            "bd_alloc_string",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let alloc_array = declare_runtime_func(
            module,
            "bd_alloc_array",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let alloc_enum = declare_runtime_func(
            module,
            "bd_alloc_enum",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let array_get_i64 = declare_runtime_func(
            module,
            "bd_array_get_i64",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let array_set_i64 = declare_runtime_func(
            module,
            "bd_array_set_i64",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let array_get_f64 = declare_runtime_func(
            module,
            "bd_array_get_f64",
            &[types::I64, types::I64, types::I64],
            &[types::F64],
        )?;
        let array_set_f64 = declare_runtime_func(
            module,
            "bd_array_set_f64",
            &[types::I64, types::I64, types::I64, types::F64],
            &[],
        )?;
        let array_get_bool = declare_runtime_func(
            module,
            "bd_array_get_bool",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let array_set_bool = declare_runtime_func(
            module,
            "bd_array_set_bool",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let array_get_u8 = declare_runtime_func(
            module,
            "bd_array_get_u8",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let array_set_u8 = declare_runtime_func(
            module,
            "bd_array_set_u8",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let array_get_ref = declare_runtime_func(
            module,
            "bd_array_get_ref",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let array_set_ref = declare_runtime_func(
            module,
            "bd_array_set_ref",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let alloc_object = declare_runtime_func(
            module,
            "bd_alloc_object",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let object_get_i64 = declare_runtime_func(
            module,
            "bd_object_get_i64",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let object_set_i64 = declare_runtime_func(
            module,
            "bd_object_set_i64",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let object_get_f64 = declare_runtime_func(
            module,
            "bd_object_get_f64",
            &[types::I64, types::I64, types::I64],
            &[types::F64],
        )?;
        let object_set_f64 = declare_runtime_func(
            module,
            "bd_object_set_f64",
            &[types::I64, types::I64, types::I64, types::F64],
            &[],
        )?;
        let object_get_bool = declare_runtime_func(
            module,
            "bd_object_get_bool",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let object_set_bool = declare_runtime_func(
            module,
            "bd_object_set_bool",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let object_get_u8 = declare_runtime_func(
            module,
            "bd_object_get_u8",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let object_set_u8 = declare_runtime_func(
            module,
            "bd_object_set_u8",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let object_get_ref = declare_runtime_func(
            module,
            "bd_object_get_ref",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let object_set_ref = declare_runtime_func(
            module,
            "bd_object_set_ref",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let enum_variant = declare_runtime_func(
            module,
            "bd_enum_variant",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let enum_payload_i64 = declare_runtime_func(
            module,
            "bd_enum_payload_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let enum_payload_f64 = declare_runtime_func(
            module,
            "bd_enum_payload_f64",
            &[types::I64, types::I64],
            &[types::F64],
        )?;
        let enum_payload_bool = declare_runtime_func(
            module,
            "bd_enum_payload_bool",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let enum_payload_u8 = declare_runtime_func(
            module,
            "bd_enum_payload_u8",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let enum_payload_ref = declare_runtime_func(
            module,
            "bd_enum_payload_ref",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let enum_set_payload_i64 = declare_runtime_func(
            module,
            "bd_enum_set_payload_i64",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let enum_set_payload_f64 = declare_runtime_func(
            module,
            "bd_enum_set_payload_f64",
            &[types::I64, types::I64, types::F64],
            &[],
        )?;
        let enum_set_payload_bool = declare_runtime_func(
            module,
            "bd_enum_set_payload_bool",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let enum_set_payload_u8 = declare_runtime_func(
            module,
            "bd_enum_set_payload_u8",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let enum_set_payload_ref = declare_runtime_func(
            module,
            "bd_enum_set_payload_ref",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let string_len = declare_runtime_func(
            module,
            "bd_string_len",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let string_concat = declare_runtime_func(
            module,
            "bd_string_concat",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_eq = declare_runtime_func(
            module,
            "bd_string_eq",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_bytes = declare_runtime_func(
            module,
            "bd_string_bytes",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let string_slice = declare_runtime_func(
            module,
            "bd_string_slice",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_index_of = declare_runtime_func(
            module,
            "bd_string_index_of",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_contains = declare_runtime_func(
            module,
            "bd_string_contains",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_replace = declare_runtime_func(
            module,
            "bd_string_replace",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let string_from_bytes = declare_runtime_func(
            module,
            "bd_string_from_bytes",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let string_to_i64 = declare_runtime_func(
            module,
            "bd_string_to_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let string_from_i64 = declare_runtime_func(
            module,
            "bd_string_from_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let bytes_len = declare_runtime_func(
            module,
            "bd_bytes_len",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let bytes_eq = declare_runtime_func(
            module,
            "bd_bytes_eq",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let bytes_slice = declare_runtime_func(
            module,
            "bd_bytes_slice",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let bytes_index_of = declare_runtime_func(
            module,
            "bd_bytes_index_of",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let bytes_contains = declare_runtime_func(
            module,
            "bd_bytes_contains",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let io_print = declare_runtime_func(module, "bd_io_print", &[types::I64, types::I64], &[])?;
        let io_read_line =
            declare_runtime_func(module, "bd_io_read_line", &[types::I64], &[types::I64])?;
        let time_now_ms =
            declare_runtime_func(module, "bd_time_now_ms", &[types::I64], &[types::I64])?;
        let time_sleep_ms = declare_runtime_func(
            module,
            "bd_time_sleep_ms",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let profiler_uptime_ms = declare_runtime_func(
            module,
            "bd_profiler_uptime_ms",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_alloc_count = declare_runtime_func(
            module,
            "bd_profiler_alloc_count",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_bytes_allocated = declare_runtime_func(
            module,
            "bd_profiler_bytes_allocated",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_bytes_in_use = declare_runtime_func(
            module,
            "bd_profiler_bytes_in_use",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_peak_bytes_in_use = declare_runtime_func(
            module,
            "bd_profiler_peak_bytes_in_use",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_gc_runs =
            declare_runtime_func(module, "bd_profiler_gc_runs", &[types::I64], &[types::I64])?;
        let profiler_last_freed = declare_runtime_func(
            module,
            "bd_profiler_last_freed",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_last_live = declare_runtime_func(
            module,
            "bd_profiler_last_live",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_last_freed_bytes = declare_runtime_func(
            module,
            "bd_profiler_last_freed_bytes",
            &[types::I64],
            &[types::I64],
        )?;
        let profiler_last_live_bytes = declare_runtime_func(
            module,
            "bd_profiler_last_live_bytes",
            &[types::I64],
            &[types::I64],
        )?;
        let rand_seed =
            declare_runtime_func(module, "bd_rand_seed", &[types::I64, types::I64], &[])?;
        let rand_range = declare_runtime_func(
            module,
            "bd_rand_range",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let test_assert = declare_runtime_func(
            module,
            "bd_test_assert",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let test_assert_eq_i64 = declare_runtime_func(
            module,
            "bd_test_assert_eq_i64",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let test_assert_eq_bool = declare_runtime_func(
            module,
            "bd_test_assert_eq_bool",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let test_assert_eq_string = declare_runtime_func(
            module,
            "bd_test_assert_eq_string",
            &[types::I64, types::I64, types::I64, types::I64],
            &[],
        )?;
        let fs_read_text = declare_runtime_func(
            module,
            "bd_fs_read_text",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let fs_write_text = declare_runtime_func(
            module,
            "bd_fs_write_text",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let fs_read_bytes = declare_runtime_func(
            module,
            "bd_fs_read_bytes",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let fs_write_bytes = declare_runtime_func(
            module,
            "bd_fs_write_bytes",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let path_join = declare_runtime_func(
            module,
            "bd_path_join",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let path_normalize = declare_runtime_func(
            module,
            "bd_path_normalize",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let path_basename = declare_runtime_func(
            module,
            "bd_path_basename",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let path_dirname = declare_runtime_func(
            module,
            "bd_path_dirname",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let env_args = declare_runtime_func(module, "bd_env_args", &[types::I64], &[types::I64])?;
        let env_get = declare_runtime_func(
            module,
            "bd_env_get",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let env_set = declare_runtime_func(
            module,
            "bd_env_set",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let env_cwd = declare_runtime_func(module, "bd_env_cwd", &[types::I64], &[types::I64])?;
        let env_set_cwd = declare_runtime_func(
            module,
            "bd_env_set_cwd",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_encode_i64 = declare_runtime_func(
            module,
            "bd_json_encode_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_encode_bool = declare_runtime_func(
            module,
            "bd_json_encode_bool",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_encode_string = declare_runtime_func(
            module,
            "bd_json_encode_string",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_decode_i64 = declare_runtime_func(
            module,
            "bd_json_decode_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_decode_bool = declare_runtime_func(
            module,
            "bd_json_decode_bool",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let json_decode_string = declare_runtime_func(
            module,
            "bd_json_decode_string",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_i64 = declare_runtime_func(
            module,
            "bd_channel_i64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_bool = declare_runtime_func(
            module,
            "bd_channel_bool",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_f64 = declare_runtime_func(
            module,
            "bd_channel_f64",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_u8 = declare_runtime_func(
            module,
            "bd_channel_u8",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_string = declare_runtime_func(
            module,
            "bd_channel_string",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_bytes = declare_runtime_func(
            module,
            "bd_channel_bytes",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_send_i64 = declare_runtime_func(
            module,
            "bd_channel_send_i64",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_send_bool = declare_runtime_func(
            module,
            "bd_channel_send_bool",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_send_f64 = declare_runtime_func(
            module,
            "bd_channel_send_f64",
            &[types::I64, types::I64, types::F64],
            &[types::I64],
        )?;
        let channel_send_u8 = declare_runtime_func(
            module,
            "bd_channel_send_u8",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_send_string = declare_runtime_func(
            module,
            "bd_channel_send_string",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_send_bytes = declare_runtime_func(
            module,
            "bd_channel_send_bytes",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_i64 = declare_runtime_func(
            module,
            "bd_channel_recv_i64",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_bool = declare_runtime_func(
            module,
            "bd_channel_recv_bool",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_f64 = declare_runtime_func(
            module,
            "bd_channel_recv_f64",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_u8 = declare_runtime_func(
            module,
            "bd_channel_recv_u8",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_string = declare_runtime_func(
            module,
            "bd_channel_recv_string",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_recv_bytes = declare_runtime_func(
            module,
            "bd_channel_recv_bytes",
            &[types::I64, types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let channel_close_i64 = declare_runtime_func(
            module,
            "bd_channel_close_i64",
            &[types::I64, types::I64],
            &[],
        )?;
        let channel_close_bool = declare_runtime_func(
            module,
            "bd_channel_close_bool",
            &[types::I64, types::I64],
            &[],
        )?;
        let channel_close_f64 = declare_runtime_func(
            module,
            "bd_channel_close_f64",
            &[types::I64, types::I64],
            &[],
        )?;
        let channel_close_u8 = declare_runtime_func(
            module,
            "bd_channel_close_u8",
            &[types::I64, types::I64],
            &[],
        )?;
        let channel_close_string = declare_runtime_func(
            module,
            "bd_channel_close_string",
            &[types::I64, types::I64],
            &[],
        )?;
        let channel_close_bytes = declare_runtime_func(
            module,
            "bd_channel_close_bytes",
            &[types::I64, types::I64],
            &[],
        )?;
        let thread_store = declare_runtime_func(
            module,
            "bd_thread_store",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let thread_join = declare_runtime_func(
            module,
            "bd_thread_join",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let thread_spawn_i64_0 = declare_runtime_func(
            module,
            "bd_thread_spawn_i64_0",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let thread_spawn_i64_1 = declare_runtime_func(
            module,
            "bd_thread_spawn_i64_1",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_connect = declare_runtime_func(
            module,
            "bd_net_connect",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_listen = declare_runtime_func(
            module,
            "bd_net_listen",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_listener_addr = declare_runtime_func(
            module,
            "bd_net_listener_addr",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let net_accept = declare_runtime_func(
            module,
            "bd_net_accept",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_write_text = declare_runtime_func(
            module,
            "bd_net_write_text",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_read_line = declare_runtime_func(
            module,
            "bd_net_read_line",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let net_read_exact = declare_runtime_func(
            module,
            "bd_net_read_exact",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_read_to_end = declare_runtime_func(
            module,
            "bd_net_read_to_end",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let net_set_read_timeout_ms = declare_runtime_func(
            module,
            "bd_net_set_read_timeout_ms",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_close_stream = declare_runtime_func(
            module,
            "bd_net_close_stream",
            &[types::I64, types::I64],
            &[],
        )?;
        let net_close_listener = declare_runtime_func(
            module,
            "bd_net_close_listener",
            &[types::I64, types::I64],
            &[],
        )?;
        let net_pool = declare_runtime_func(
            module,
            "bd_net_pool",
            &[types::I64, types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_pool_get = declare_runtime_func(
            module,
            "bd_net_pool_get",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_pool_put = declare_runtime_func(
            module,
            "bd_net_pool_put",
            &[types::I64, types::I64, types::I64],
            &[types::I64],
        )?;
        let net_pool_close =
            declare_runtime_func(module, "bd_net_pool_close", &[types::I64, types::I64], &[])?;
        Ok(Self {
            root_push,
            root_pop,
            root_set,
            trace_push,
            trace_pop,
            has_error,
            error_is_throw,
            error_message,
            clear_error,
            throw_error,
            alloc_string,
            alloc_array,
            alloc_enum,
            array_get_i64,
            array_set_i64,
            array_get_f64,
            array_set_f64,
            array_get_bool,
            array_set_bool,
            array_get_u8,
            array_set_u8,
            array_get_ref,
            array_set_ref,
            alloc_object,
            object_get_i64,
            object_set_i64,
            object_get_f64,
            object_set_f64,
            object_get_bool,
            object_set_bool,
            object_get_u8,
            object_set_u8,
            object_get_ref,
            object_set_ref,
            enum_variant,
            enum_payload_i64,
            enum_payload_f64,
            enum_payload_bool,
            enum_payload_u8,
            enum_payload_ref,
            enum_set_payload_i64,
            enum_set_payload_f64,
            enum_set_payload_bool,
            enum_set_payload_u8,
            enum_set_payload_ref,
            string_len,
            string_concat,
            string_eq,
            string_bytes,
            string_slice,
            string_index_of,
            string_contains,
            string_replace,
            string_from_bytes,
            string_to_i64,
            string_from_i64,
            bytes_len,
            bytes_eq,
            bytes_slice,
            bytes_index_of,
            bytes_contains,
            io_print,
            io_read_line,
            time_now_ms,
            time_sleep_ms,
            profiler_uptime_ms,
            profiler_alloc_count,
            profiler_bytes_allocated,
            profiler_bytes_in_use,
            profiler_peak_bytes_in_use,
            profiler_gc_runs,
            profiler_last_freed,
            profiler_last_live,
            profiler_last_freed_bytes,
            profiler_last_live_bytes,
            rand_seed,
            rand_range,
            test_assert,
            test_assert_eq_i64,
            test_assert_eq_bool,
            test_assert_eq_string,
            fs_read_text,
            fs_write_text,
            fs_read_bytes,
            fs_write_bytes,
            path_join,
            path_normalize,
            path_basename,
            path_dirname,
            env_args,
            env_get,
            env_set,
            env_cwd,
            env_set_cwd,
            json_encode_i64,
            json_encode_bool,
            json_encode_string,
            json_decode_i64,
            json_decode_bool,
            json_decode_string,
            channel_i64,
            channel_bool,
            channel_f64,
            channel_u8,
            channel_string,
            channel_bytes,
            channel_send_i64,
            channel_send_bool,
            channel_send_f64,
            channel_send_u8,
            channel_send_string,
            channel_send_bytes,
            channel_recv_i64,
            channel_recv_bool,
            channel_recv_f64,
            channel_recv_u8,
            channel_recv_string,
            channel_recv_bytes,
            channel_close_i64,
            channel_close_bool,
            channel_close_f64,
            channel_close_u8,
            channel_close_string,
            channel_close_bytes,
            thread_store,
            thread_join,
            thread_spawn_i64_0,
            thread_spawn_i64_1,
            net_connect,
            net_listen,
            net_listener_addr,
            net_accept,
            net_write_text,
            net_read_line,
            net_read_exact,
            net_read_to_end,
            net_set_read_timeout_ms,
            net_close_stream,
            net_close_listener,
            net_pool,
            net_pool_get,
            net_pool_put,
            net_pool_close,
        })
    }
}

fn declare_runtime_func(
    module: &mut dyn Module,
    name: &str,
    params: &[types::Type],
    returns: &[types::Type],
) -> Result<FuncId, NativeError> {
    let mut sig = module.make_signature();
    for param in params {
        sig.params.push(AbiParam::new(*param));
    }
    for ret in returns {
        sig.returns.push(AbiParam::new(*ret));
    }
    module
        .declare_function(name, Linkage::Import, &sig)
        .map_err(|err| native_error(format!("native declare failed: {err}")))
}
