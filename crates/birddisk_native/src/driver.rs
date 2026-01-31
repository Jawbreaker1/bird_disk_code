use crate::analysis::{build_root_slots, collect_local_types};
use crate::compiler::NativeCompiler;
use crate::error::{native_error, native_error_with_code_and_trace, NativeError};
use crate::program::{
    build_book_layouts, build_enum_layouts, build_trace_table, collect_function_sigs,
    collect_functions, declare_functions, make_signature, mangle_symbol,
};
use crate::rt::RuntimeFuncs;
use crate::runtime;
use birddisk_core::ast::Type;
use birddisk_core::ast::Program;
use birddisk_core::TraceFrame;
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_codegen::settings;
use cranelift_codegen::settings::Configurable;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
use cranelift_native::builder as native_builder;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

pub fn run(program: &Program) -> Result<i64, NativeError> {
    let (result, _) = run_with_io(program, "", &[])?;
    Ok(result)
}

pub fn run_with_io(
    program: &Program,
    input: &str,
    args: &[String],
) -> Result<(i64, String), NativeError> {
    if program.functions.is_empty() {
        return Err(native_error("missing main function."));
    }
    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| native_error("missing main function."))?;
    if !main.params.is_empty() {
        return Err(native_error("native main must take no parameters."));
    }
    if main.return_type != Type::I64 {
        return Err(native_error("native main must return i64."));
    }

    let mut flag_builder = settings::builder();
    flag_builder
        .set("is_pic", "false")
        .map_err(|err| native_error(format!("native isa flag error: {err}")))?;
    flag_builder
        .set("use_colocated_libcalls", "true")
        .map_err(|err| native_error(format!("native isa flag error: {err}")))?;
    let isa_builder = native_builder()
        .map_err(|err| native_error(format!("native isa builder failed: {err}")))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|err| native_error(format!("native isa finish failed: {err}")))?;
    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    builder.symbol("bd_root_push", runtime::bd_root_push as *const u8);
    builder.symbol("bd_root_pop", runtime::bd_root_pop as *const u8);
    builder.symbol("bd_root_set", runtime::bd_root_set as *const u8);
    builder.symbol("bd_trace_push", runtime::bd_trace_push as *const u8);
    builder.symbol("bd_trace_pop", runtime::bd_trace_pop as *const u8);
    builder.symbol("bd_has_error", runtime::bd_has_error as *const u8);
    builder.symbol("bd_error_is_throw", runtime::bd_error_is_throw as *const u8);
    builder.symbol("bd_error_message", runtime::bd_error_message as *const u8);
    builder.symbol("bd_clear_error", runtime::bd_clear_error as *const u8);
    builder.symbol("bd_throw", runtime::bd_throw as *const u8);
    builder.symbol("bd_alloc_string", runtime::bd_alloc_string as *const u8);
    builder.symbol("bd_alloc_array", runtime::bd_alloc_array as *const u8);
    builder.symbol("bd_alloc_enum", runtime::bd_alloc_enum as *const u8);
    builder.symbol("bd_array_get_i64", runtime::bd_array_get_i64 as *const u8);
    builder.symbol("bd_array_set_i64", runtime::bd_array_set_i64 as *const u8);
    builder.symbol("bd_array_get_f64", runtime::bd_array_get_f64 as *const u8);
    builder.symbol("bd_array_set_f64", runtime::bd_array_set_f64 as *const u8);
    builder.symbol("bd_array_get_bool", runtime::bd_array_get_bool as *const u8);
    builder.symbol("bd_array_set_bool", runtime::bd_array_set_bool as *const u8);
    builder.symbol("bd_array_get_u8", runtime::bd_array_get_u8 as *const u8);
    builder.symbol("bd_array_set_u8", runtime::bd_array_set_u8 as *const u8);
    builder.symbol("bd_array_get_ref", runtime::bd_array_get_ref as *const u8);
    builder.symbol("bd_array_set_ref", runtime::bd_array_set_ref as *const u8);
    builder.symbol("bd_alloc_object", runtime::bd_alloc_object as *const u8);
    builder.symbol("bd_object_get_i64", runtime::bd_object_get_i64 as *const u8);
    builder.symbol("bd_object_set_i64", runtime::bd_object_set_i64 as *const u8);
    builder.symbol("bd_object_get_bool", runtime::bd_object_get_bool as *const u8);
    builder.symbol("bd_object_set_bool", runtime::bd_object_set_bool as *const u8);
    builder.symbol("bd_object_get_u8", runtime::bd_object_get_u8 as *const u8);
    builder.symbol("bd_object_set_u8", runtime::bd_object_set_u8 as *const u8);
    builder.symbol("bd_object_get_ref", runtime::bd_object_get_ref as *const u8);
    builder.symbol("bd_object_set_ref", runtime::bd_object_set_ref as *const u8);
    builder.symbol("bd_enum_variant", runtime::bd_enum_variant as *const u8);
    builder.symbol("bd_enum_payload_i64", runtime::bd_enum_payload_i64 as *const u8);
    builder.symbol("bd_enum_payload_bool", runtime::bd_enum_payload_bool as *const u8);
    builder.symbol("bd_enum_payload_u8", runtime::bd_enum_payload_u8 as *const u8);
    builder.symbol("bd_enum_payload_ref", runtime::bd_enum_payload_ref as *const u8);
    builder.symbol("bd_enum_set_payload_i64", runtime::bd_enum_set_payload_i64 as *const u8);
    builder.symbol("bd_enum_set_payload_bool", runtime::bd_enum_set_payload_bool as *const u8);
    builder.symbol("bd_enum_set_payload_u8", runtime::bd_enum_set_payload_u8 as *const u8);
    builder.symbol("bd_enum_set_payload_ref", runtime::bd_enum_set_payload_ref as *const u8);
    builder.symbol("bd_string_len", runtime::bd_string_len as *const u8);
    builder.symbol("bd_string_concat", runtime::bd_string_concat as *const u8);
    builder.symbol("bd_string_eq", runtime::bd_string_eq as *const u8);
    builder.symbol("bd_string_bytes", runtime::bd_string_bytes as *const u8);
    builder.symbol("bd_string_slice", runtime::bd_string_slice as *const u8);
    builder.symbol("bd_string_index_of", runtime::bd_string_index_of as *const u8);
    builder.symbol("bd_string_contains", runtime::bd_string_contains as *const u8);
    builder.symbol("bd_string_replace", runtime::bd_string_replace as *const u8);
    builder.symbol("bd_string_from_bytes", runtime::bd_string_from_bytes as *const u8);
    builder.symbol("bd_string_to_i64", runtime::bd_string_to_i64 as *const u8);
    builder.symbol("bd_string_from_i64", runtime::bd_string_from_i64 as *const u8);
    builder.symbol("bd_bytes_len", runtime::bd_bytes_len as *const u8);
    builder.symbol("bd_bytes_eq", runtime::bd_bytes_eq as *const u8);
    builder.symbol("bd_bytes_slice", runtime::bd_bytes_slice as *const u8);
    builder.symbol("bd_bytes_index_of", runtime::bd_bytes_index_of as *const u8);
    builder.symbol("bd_bytes_contains", runtime::bd_bytes_contains as *const u8);
    builder.symbol("bd_io_print", runtime::bd_io_print as *const u8);
    builder.symbol("bd_io_read_line", runtime::bd_io_read_line as *const u8);
    builder.symbol("bd_time_now_ms", runtime::bd_time_now_ms as *const u8);
    builder.symbol("bd_time_sleep_ms", runtime::bd_time_sleep_ms as *const u8);
    builder.symbol("bd_rand_seed", runtime::bd_rand_seed as *const u8);
    builder.symbol("bd_rand_range", runtime::bd_rand_range as *const u8);
    builder.symbol("bd_fs_read_text", runtime::bd_fs_read_text as *const u8);
    builder.symbol("bd_fs_write_text", runtime::bd_fs_write_text as *const u8);
    builder.symbol("bd_fs_read_bytes", runtime::bd_fs_read_bytes as *const u8);
    builder.symbol("bd_fs_write_bytes", runtime::bd_fs_write_bytes as *const u8);
    builder.symbol("bd_path_join", runtime::bd_path_join as *const u8);
    builder.symbol("bd_path_normalize", runtime::bd_path_normalize as *const u8);
    builder.symbol("bd_path_basename", runtime::bd_path_basename as *const u8);
    builder.symbol("bd_path_dirname", runtime::bd_path_dirname as *const u8);
    builder.symbol("bd_env_args", runtime::bd_env_args as *const u8);
    builder.symbol("bd_env_get", runtime::bd_env_get as *const u8);
    builder.symbol("bd_env_set", runtime::bd_env_set as *const u8);
    builder.symbol("bd_env_cwd", runtime::bd_env_cwd as *const u8);
    builder.symbol("bd_env_set_cwd", runtime::bd_env_set_cwd as *const u8);
    builder.symbol("bd_json_encode_i64", runtime::bd_json_encode_i64 as *const u8);
    builder.symbol("bd_json_encode_bool", runtime::bd_json_encode_bool as *const u8);
    builder.symbol("bd_json_encode_string", runtime::bd_json_encode_string as *const u8);
    builder.symbol("bd_json_decode_i64", runtime::bd_json_decode_i64 as *const u8);
    builder.symbol("bd_json_decode_bool", runtime::bd_json_decode_bool as *const u8);
    builder.symbol("bd_json_decode_string", runtime::bd_json_decode_string as *const u8);
    let mut module = JITModule::new(builder);
    let runtime_funcs = RuntimeFuncs::declare(&mut module)?;
    let (books, layout) = build_book_layouts(program)?;
    let enums = build_enum_layouts(program)?;
    let trace_table = build_trace_table(program);
    let function_sigs = collect_function_sigs(program)?;
    let function_ids =
        declare_functions(&mut module, program, &function_sigs, |name| name.to_string())?;
    let mut string_data = HashMap::new();
    let mut string_counter = 0usize;
    let functions = collect_functions(program);

    for (function, full_name) in functions {
        let mut ctx = module.make_context();
        ctx.func.signature = make_signature(&mut module, function);
        let mut func_ctx = FunctionBuilderContext::new();
        let mut function_builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let entry = function_builder.create_block();
        let error_block = function_builder.create_block();
        function_builder.append_block_params_for_function_params(entry);
        function_builder.switch_to_block(entry);
        function_builder.seal_block(entry);

        let rt_ptr = function_builder.block_params(entry)[0];
        let locals = collect_local_types(function, &function_sigs, &books, &enums)?;
        let root_slots = build_root_slots(&locals);
        let mut compiler = NativeCompiler::new(
            &mut function_builder,
            &mut module,
            runtime_funcs,
            rt_ptr,
            error_block,
            function.return_type.clone(),
            locals,
            root_slots,
            &books,
            &enums,
            &function_sigs,
            &function_ids,
            &mut string_data,
            &mut string_counter,
        );
        let trace_id = trace_table
            .ids
            .get(&full_name)
            .copied()
            .ok_or_else(|| native_error(format!("missing trace frame for '{}'.", full_name)))?;
        compiler.emit_trace_push(trace_id);
        compiler.emit_root_push();
        compiler.bind_params(function, entry)?;

        let mut returned = false;
        for stmt in &function.body {
            if compiler.emit_stmt(stmt)? {
                returned = true;
                break;
            }
        }
        if !returned {
            compiler.emit_root_pop();
            compiler.emit_trace_pop();
            if matches!(function.return_type, Type::Void) {
                compiler.builder.ins().return_(&[]);
            } else {
                let zero = compiler.builder.ins().iconst(types::I64, 0);
                compiler.builder.ins().return_(&[zero]);
            }
        }
        compiler.emit_error_block();
        function_builder.finalize();

        let func_id = *function_ids.get(&full_name).ok_or_else(|| {
            native_error(format!("missing function id for '{}'.", full_name))
        })?;
        module
            .define_function(func_id, &mut ctx)
            .map_err(|err| native_error(format!("native define failed: {err}")))?;
        module.clear_context(&mut ctx);
    }
    module
        .finalize_definitions()
        .map_err(|err| native_error(format!("native finalize failed: {err}")))?;

    let main_id = *function_ids
        .get("main")
        .ok_or_else(|| native_error("missing main function id."))?;
    let code = module.get_finalized_function(main_id);
    let mut runtime = runtime::Runtime::new();
    runtime.set_layout(layout);
    runtime.set_trace(trace_table.frames.clone());
    runtime.set_input(input);
    runtime.set_args(args);
    let func = unsafe { std::mem::transmute::<_, fn(*mut runtime::Runtime) -> i64>(code) };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| func(&mut runtime)));
    match result {
        Ok(value) => {
            if let Some(trap) = runtime.take_error() {
                return Err(native_error_with_code_and_trace(
                    trap.code,
                    trap.message,
                    trap.trace,
                ));
            }
            Ok((value, runtime.take_output()))
        }
        Err(payload) => {
            if let Some(trap) = payload.downcast_ref::<runtime::NativeTrap>() {
                Err(native_error_with_code_and_trace(
                    trap.code,
                    trap.message.clone(),
                    trap.trace.clone(),
                ))
            } else if let Some(message) = payload.downcast_ref::<&str>() {
                Err(native_error(format!("native runtime panic: {message}")))
            } else if let Some(message) = payload.downcast_ref::<String>() {
                Err(native_error(format!("native runtime panic: {message}")))
            } else {
                Err(native_error("native runtime panic."))
            }
        }
    }
}

pub fn emit_object(program: &Program) -> Result<Vec<u8>, NativeError> {
    if program.functions.is_empty() {
        return Err(native_error("missing main function."));
    }
    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| native_error("missing main function."))?;
    if !main.params.is_empty() {
        return Err(native_error("native main must take no parameters."));
    }
    if main.return_type != Type::I64 {
        return Err(native_error("native main must return i64."));
    }

    let mut flag_builder = settings::builder();
    flag_builder
        .set("is_pic", "true")
        .map_err(|err| native_error(format!("native isa flag error: {err}")))?;
    flag_builder
        .set("use_colocated_libcalls", "true")
        .map_err(|err| native_error(format!("native isa flag error: {err}")))?;
    let isa_builder = native_builder()
        .map_err(|err| native_error(format!("native isa builder failed: {err}")))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|err| native_error(format!("native isa finish failed: {err}")))?;
    let builder = ObjectBuilder::new(isa, "birddisk", cranelift_module::default_libcall_names())
        .map_err(|err| native_error(format!("native object builder failed: {err}")))?;
    let mut module = ObjectModule::new(builder);
    let runtime_funcs = RuntimeFuncs::declare(&mut module)?;
    let (books, _) = build_book_layouts(program)?;
    let enums = build_enum_layouts(program)?;
    let trace_table = build_trace_table(program);
    let function_sigs = collect_function_sigs(program)?;
    let function_ids =
        declare_functions(&mut module, program, &function_sigs, mangle_symbol)?;
    let mut string_data = HashMap::new();
    let mut string_counter = 0usize;
    let functions = collect_functions(program);

    for (function, full_name) in functions {
        let mut ctx = module.make_context();
        ctx.func.signature = make_signature(&mut module, function);
        let mut func_ctx = FunctionBuilderContext::new();
        let mut function_builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let entry = function_builder.create_block();
        let error_block = function_builder.create_block();
        function_builder.append_block_params_for_function_params(entry);
        function_builder.switch_to_block(entry);
        function_builder.seal_block(entry);

        let rt_ptr = function_builder.block_params(entry)[0];
        let locals = collect_local_types(function, &function_sigs, &books, &enums)?;
        let root_slots = build_root_slots(&locals);
        let mut compiler = NativeCompiler::new(
            &mut function_builder,
            &mut module,
            runtime_funcs,
            rt_ptr,
            error_block,
            function.return_type.clone(),
            locals,
            root_slots,
            &books,
            &enums,
            &function_sigs,
            &function_ids,
            &mut string_data,
            &mut string_counter,
        );
        let trace_id = trace_table
            .ids
            .get(&full_name)
            .copied()
            .ok_or_else(|| native_error(format!("missing trace frame for '{}'.", full_name)))?;
        compiler.emit_trace_push(trace_id);
        compiler.emit_root_push();
        compiler.bind_params(function, entry)?;

        let mut returned = false;
        for stmt in &function.body {
            if compiler.emit_stmt(stmt)? {
                returned = true;
                break;
            }
        }
        if !returned {
            compiler.emit_root_pop();
            compiler.emit_trace_pop();
            if matches!(function.return_type, Type::Void) {
                compiler.builder.ins().return_(&[]);
            } else {
                let zero = compiler.builder.ins().iconst(types::I64, 0);
                compiler.builder.ins().return_(&[zero]);
            }
        }
        compiler.emit_error_block();
        function_builder.finalize();

        let func_id = *function_ids.get(&full_name).ok_or_else(|| {
            native_error(format!("missing function id for '{}'.", full_name))
        })?;
        module
            .define_function(func_id, &mut ctx)
            .map_err(|err| native_error(format!("native define failed: {err}")))?;
        module.clear_context(&mut ctx);
    }
    let product = module.finish();
    product
        .emit()
        .map_err(|err| native_error(format!("native object emit failed: {err}")))
}

pub fn layout_for_program(program: &Program) -> Result<Vec<Vec<usize>>, NativeError> {
    let (_, layout) = build_book_layouts(program)?;
    Ok(layout)
}

pub fn trace_for_program(program: &Program) -> Result<Vec<TraceFrame>, NativeError> {
    Ok(build_trace_table(program).frames)
}
