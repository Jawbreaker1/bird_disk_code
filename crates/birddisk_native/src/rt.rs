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
    pub(crate) string_from_bytes: FuncId,
    pub(crate) string_to_i64: FuncId,
    pub(crate) string_from_i64: FuncId,
    pub(crate) bytes_len: FuncId,
    pub(crate) bytes_eq: FuncId,
    pub(crate) io_print: FuncId,
    pub(crate) io_read_line: FuncId,
    pub(crate) time_now_ms: FuncId,
    pub(crate) time_sleep_ms: FuncId,
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
}

impl RuntimeFuncs {
    pub(crate) fn declare<M: Module>(module: &mut M) -> Result<Self, NativeError> {
        let root_push = declare_runtime_func(
            module,
            "bd_root_push",
            &[types::I64, types::I64],
            &[types::I64],
        )?;
        let root_pop =
            declare_runtime_func(module, "bd_root_pop", &[types::I64, types::I64], &[])?;
        let root_set = declare_runtime_func(
            module,
            "bd_root_set",
            &[types::I64, types::I64, types::I64],
            &[],
        )?;
        let trace_push =
            declare_runtime_func(module, "bd_trace_push", &[types::I64, types::I64], &[])?;
        let trace_pop = declare_runtime_func(module, "bd_trace_pop", &[types::I64], &[])?;
        let has_error =
            declare_runtime_func(module, "bd_has_error", &[types::I64], &[types::I64])?;
        let error_is_throw =
            declare_runtime_func(module, "bd_error_is_throw", &[types::I64], &[types::I64])?;
        let error_message =
            declare_runtime_func(module, "bd_error_message", &[types::I64], &[types::I64])?;
        let clear_error =
            declare_runtime_func(module, "bd_clear_error", &[types::I64], &[])?;
        let throw_error =
            declare_runtime_func(module, "bd_throw", &[types::I64, types::I64], &[])?;
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
        let io_print = declare_runtime_func(
            module,
            "bd_io_print",
            &[types::I64, types::I64],
            &[],
        )?;
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
        let env_args = declare_runtime_func(
            module,
            "bd_env_args",
            &[types::I64],
            &[types::I64],
        )?;
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
        let env_cwd = declare_runtime_func(
            module,
            "bd_env_cwd",
            &[types::I64],
            &[types::I64],
        )?;
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
            string_from_bytes,
            string_to_i64,
            string_from_i64,
            bytes_len,
            bytes_eq,
            io_print,
            io_read_line,
            time_now_ms,
            time_sleep_ms,
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
