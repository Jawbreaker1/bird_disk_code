use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, Type, UnaryOp};
use birddisk_core::runtime as abi;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, InstBuilder, Value};
use cranelift_codegen::settings;
use cranelift_codegen::settings::Configurable;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use cranelift_native::builder as native_builder;
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

pub use birddisk_native_runtime as runtime;

#[derive(Debug, Clone)]
pub struct NativeError {
    pub code: Option<&'static str>,
    pub message: String,
}

pub const NATIVE_MAIN_SYMBOL: &str = "bd_main";

fn native_error(message: impl Into<String>) -> NativeError {
    NativeError {
        code: None,
        message: message.into(),
    }
}

fn native_error_with_code(code: &'static str, message: impl Into<String>) -> NativeError {
    NativeError {
        code: Some(code),
        message: message.into(),
    }
}

#[derive(Clone, Copy)]
struct RuntimeFuncs {
    root_push: FuncId,
    root_pop: FuncId,
    root_set: FuncId,
    has_error: FuncId,
    alloc_string: FuncId,
    alloc_array: FuncId,
    array_get_i64: FuncId,
    array_set_i64: FuncId,
    array_get_bool: FuncId,
    array_set_bool: FuncId,
    array_get_u8: FuncId,
    array_set_u8: FuncId,
    array_get_ref: FuncId,
    array_set_ref: FuncId,
    alloc_object: FuncId,
    object_get_i64: FuncId,
    object_set_i64: FuncId,
    object_get_bool: FuncId,
    object_set_bool: FuncId,
    object_get_u8: FuncId,
    object_set_u8: FuncId,
    object_get_ref: FuncId,
    object_set_ref: FuncId,
    string_len: FuncId,
    string_concat: FuncId,
    string_eq: FuncId,
    string_bytes: FuncId,
    string_from_bytes: FuncId,
    string_to_i64: FuncId,
    string_from_i64: FuncId,
    bytes_len: FuncId,
    bytes_eq: FuncId,
    io_print: FuncId,
    io_read_line: FuncId,
}

impl RuntimeFuncs {
    fn declare<M: Module>(module: &mut M) -> Result<Self, NativeError> {
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
        let has_error =
            declare_runtime_func(module, "bd_has_error", &[types::I64], &[types::I64])?;
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
            &[types::I64],
        )?;
        let io_read_line =
            declare_runtime_func(module, "bd_io_read_line", &[types::I64], &[types::I64])?;
        Ok(Self {
            root_push,
            root_pop,
            root_set,
            has_error,
            alloc_string,
            alloc_array,
            array_get_i64,
            array_set_i64,
            array_get_bool,
            array_set_bool,
            array_get_u8,
            array_set_u8,
            array_get_ref,
            array_set_ref,
            alloc_object,
            object_get_i64,
            object_set_i64,
            object_get_bool,
            object_set_bool,
            object_get_u8,
            object_set_u8,
            object_get_ref,
            object_set_ref,
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

#[derive(Debug, Clone)]
struct VarInfo {
    var: Variable,
    ty: Type,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone)]
struct BookLayout {
    id: u32,
    fields: Vec<Type>,
    field_index: HashMap<String, usize>,
}

pub fn run(program: &Program) -> Result<i64, NativeError> {
    let (result, _) = run_with_io(program, "")?;
    Ok(result)
}

pub fn run_with_io(program: &Program, input: &str) -> Result<(i64, String), NativeError> {
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
    builder.symbol("bd_has_error", runtime::bd_has_error as *const u8);
    builder.symbol("bd_alloc_string", runtime::bd_alloc_string as *const u8);
    builder.symbol("bd_alloc_array", runtime::bd_alloc_array as *const u8);
    builder.symbol("bd_array_get_i64", runtime::bd_array_get_i64 as *const u8);
    builder.symbol("bd_array_set_i64", runtime::bd_array_set_i64 as *const u8);
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
    builder.symbol("bd_string_len", runtime::bd_string_len as *const u8);
    builder.symbol("bd_string_concat", runtime::bd_string_concat as *const u8);
    builder.symbol("bd_string_eq", runtime::bd_string_eq as *const u8);
    builder.symbol("bd_string_bytes", runtime::bd_string_bytes as *const u8);
    builder.symbol("bd_string_from_bytes", runtime::bd_string_from_bytes as *const u8);
    builder.symbol("bd_string_to_i64", runtime::bd_string_to_i64 as *const u8);
    builder.symbol("bd_string_from_i64", runtime::bd_string_from_i64 as *const u8);
    builder.symbol("bd_bytes_len", runtime::bd_bytes_len as *const u8);
    builder.symbol("bd_bytes_eq", runtime::bd_bytes_eq as *const u8);
    builder.symbol("bd_io_print", runtime::bd_io_print as *const u8);
    builder.symbol("bd_io_read_line", runtime::bd_io_read_line as *const u8);
    let mut module = JITModule::new(builder);
    let runtime_funcs = RuntimeFuncs::declare(&mut module)?;
    let (books, layout) = build_book_layouts(program)?;
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
        let locals = collect_local_types(function, &function_sigs, &books)?;
        let root_slots = build_root_slots(&locals);
        let mut compiler = NativeCompiler::new(
            &mut function_builder,
            &mut module,
            runtime_funcs,
            rt_ptr,
            error_block,
            locals,
            root_slots,
            &books,
            &function_sigs,
            &function_ids,
            &mut string_data,
            &mut string_counter,
        );
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
            let zero = compiler.builder.ins().iconst(types::I64, 0);
            compiler.emit_root_pop();
            compiler.builder.ins().return_(&[zero]);
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
    runtime.set_input(input);
    let func = unsafe { std::mem::transmute::<_, fn(*mut runtime::Runtime) -> i64>(code) };
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| func(&mut runtime)));
    match result {
        Ok(value) => {
            if let Some(trap) = runtime.take_error() {
                return Err(native_error_with_code(trap.code, trap.message));
            }
            Ok((value, runtime.take_output()))
        }
        Err(payload) => {
            if let Some(trap) = payload.downcast_ref::<runtime::NativeTrap>() {
                Err(native_error_with_code(trap.code, trap.message))
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
    let builder = ObjectBuilder::new(isa, "birddisk", cranelift_module::default_libcall_names())
        .map_err(|err| native_error(format!("native object builder failed: {err}")))?;
    let mut module = ObjectModule::new(builder);
    let runtime_funcs = RuntimeFuncs::declare(&mut module)?;
    let (books, _) = build_book_layouts(program)?;
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
        let locals = collect_local_types(function, &function_sigs, &books)?;
        let root_slots = build_root_slots(&locals);
        let mut compiler = NativeCompiler::new(
            &mut function_builder,
            &mut module,
            runtime_funcs,
            rt_ptr,
            error_block,
            locals,
            root_slots,
            &books,
            &function_sigs,
            &function_ids,
            &mut string_data,
            &mut string_counter,
        );
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
            let zero = compiler.builder.ins().iconst(types::I64, 0);
            compiler.emit_root_pop();
            compiler.builder.ins().return_(&[zero]);
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

struct NativeCompiler<'a, 'b, M: Module> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut M,
    runtime: RuntimeFuncs,
    rt_ptr: Value,
    error_block: cranelift_codegen::ir::Block,
    root_base: Option<Value>,
    root_slots: HashMap<String, u32>,
    locals: HashMap<String, Type>,
    books: &'a HashMap<String, BookLayout>,
    functions: &'a HashMap<String, FunctionSig>,
    func_ids: &'a HashMap<String, FuncId>,
    vars: HashMap<String, VarInfo>,
    string_data: &'a mut HashMap<String, DataId>,
    string_counter: &'a mut usize,
    next_var: u32,
}

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    fn new(
        builder: &'a mut FunctionBuilder<'b>,
        module: &'a mut M,
        runtime: RuntimeFuncs,
        rt_ptr: Value,
        error_block: cranelift_codegen::ir::Block,
        locals: HashMap<String, Type>,
        root_slots: HashMap<String, u32>,
        books: &'a HashMap<String, BookLayout>,
        functions: &'a HashMap<String, FunctionSig>,
        func_ids: &'a HashMap<String, FuncId>,
        string_data: &'a mut HashMap<String, DataId>,
        string_counter: &'a mut usize,
    ) -> Self {
        Self {
            builder,
            module,
            runtime,
            rt_ptr,
            error_block,
            root_base: None,
            root_slots,
            locals,
            books,
            functions,
            func_ids,
            vars: HashMap::new(),
            string_data,
            string_counter,
            next_var: 0,
        }
    }

    fn emit_root_push(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(types::I64, slot_count);
        let base = self.call_runtime_value(self.runtime.root_push, &[self.rt_ptr, slots]);
        self.root_base = Some(base);
    }

    fn emit_root_pop(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(types::I64, slot_count);
        self.call_runtime_void(self.runtime.root_pop, &[self.rt_ptr, slots]);
    }

    fn bind_params(&mut self, function: &birddisk_core::ast::Function, entry: cranelift_codegen::ir::Block) -> Result<(), NativeError> {
        let params: Vec<Value> = self.builder.block_params(entry).to_vec();
        for (index, param) in function.params.iter().enumerate() {
            let value = params[index + 1];
            let var = self.new_var();
            self.builder.def_var(var, value);
            self.vars.insert(
                param.name.clone(),
                VarInfo {
                    var,
                    ty: param.ty.clone(),
                },
            );
            self.update_root(&param.name, value);
        }
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<bool, NativeError> {
        match stmt {
            Stmt::Set { name, expr, .. } => {
                if self.vars.contains_key(name) {
                    return Err(native_error(format!(
                        "native backend does not support shadowing '{name}'."
                    )));
                }
                let var_ty = self
                    .locals
                    .get(name)
                    .ok_or_else(|| native_error(format!("missing type for '{name}'.")))?
                    .clone();
                let var = self.new_var();
                let value = self.emit_expr(expr, Some(&var_ty))?;
                self.builder.def_var(var, value);
                self.vars.insert(
                    name.clone(),
                    VarInfo {
                        var,
                        ty: var_ty,
                    },
                );
                self.update_root(name, value);
                Ok(false)
            }
            Stmt::Put { name, expr, .. } => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                let value = self.emit_expr(expr, Some(&var.ty))?;
                self.builder.def_var(var.var, value);
                self.update_root(name, value);
                Ok(false)
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                let Type::Array(elem_ty) = &var.ty else {
                    return Err(native_error("index assignment requires array type."));
                };
                let handle = self.builder.use_var(var.var);
                let index_val = self.emit_expr(index, Some(&Type::I64))?;
                let value = self.emit_expr(expr, Some(elem_ty.as_ref()))?;
                self.emit_array_set(elem_ty.as_ref(), handle, index_val, value)?;
                Ok(false)
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let base_info = self
                    .vars
                    .get(base)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
                let Type::Book(book_name) = &base_info.ty else {
                    return Err(native_error("field assignment requires book type."));
                };
                let layout = self
                    .books
                    .get(book_name)
                    .ok_or_else(|| native_error(format!("unknown book '{book_name}'.")))?;
                let index = layout
                    .field_index
                    .get(field)
                    .copied()
                    .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
                let field_ty = layout
                    .fields
                    .get(index)
                    .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
                let handle = self.builder.use_var(base_info.var);
                let index_val = self.builder.ins().iconst(types::I64, index as i64);
                let value = self.emit_expr(expr, Some(field_ty))?;
                self.emit_object_set(field_ty, handle, index_val, value)?;
                Ok(false)
            }
            Stmt::Yield { expr, .. } => {
                let value = self.emit_expr(expr, Some(&Type::I64))?;
                self.emit_root_pop();
                self.builder.ins().return_(&[value]);
                Ok(true)
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                let cond = self.i64_to_bool(cond_val);
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(cond, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                let then_returned = self.emit_block(then_body)?;
                if !then_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }
                self.builder.seal_block(then_block);

                self.builder.switch_to_block(else_block);
                let else_returned = self.emit_block(else_body)?;
                if !else_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }
                self.builder.seal_block(else_block);

                if then_returned && else_returned {
                    return Ok(true);
                }

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(false)
            }
            Stmt::Repeat { cond, body, .. } => {
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();
                self.builder.ins().jump(header_block, &[]);

                self.builder.switch_to_block(header_block);
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                let cond = self.i64_to_bool(cond_val);
                self.builder
                    .ins()
                    .brif(cond, body_block, &[], exit_block, &[]);

                self.builder.switch_to_block(body_block);
                let body_returned = self.emit_block(body)?;
                if !body_returned {
                    self.builder.ins().jump(header_block, &[]);
                }
                self.builder.seal_block(body_block);
                self.builder.seal_block(header_block);

                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(exit_block);
                Ok(false)
            }
        }
    }

    fn emit_block(&mut self, stmts: &[Stmt]) -> Result<bool, NativeError> {
        for stmt in stmts {
            if self.emit_stmt(stmt)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn emit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<Value, NativeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(self.builder.ins().iconst(types::I64, *value)),
            ExprKind::Bool(value) => {
                let bit = if *value { 1 } else { 0 };
                Ok(self.builder.ins().iconst(types::I64, bit))
            }
            ExprKind::String(value) => self.emit_string_literal(value),
            ExprKind::Ident(name) => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                Ok(self.builder.use_var(var.var))
            }
            ExprKind::Call { name, args } => self.emit_call(name, args, expected),
            ExprKind::New { book, args } => self.emit_new(book, args),
            ExprKind::MemberAccess { base, field } => self.emit_member_access(base, field),
            ExprKind::Unary { op, expr } => {
                let value = self.emit_expr(expr, None)?;
                match op {
                    UnaryOp::Neg => Ok(self.builder.ins().ineg(value)),
                    UnaryOp::Not => {
                        let cond = self.builder.ins().icmp_imm(IntCC::Equal, value, 0);
                        Ok(self.bool_to_i64(cond))
                    }
                }
            }
            ExprKind::Binary { left, op, right } => {
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let left = self.emit_expr(left, None)?;
                        let right = self.emit_expr(right, None)?;
                        let value = match op {
                            BinaryOp::Add => self.builder.ins().iadd(left, right),
                            BinaryOp::Sub => self.builder.ins().isub(left, right),
                            BinaryOp::Mul => self.builder.ins().imul(left, right),
                            BinaryOp::Div => self.builder.ins().sdiv(left, right),
                            BinaryOp::Mod => self.builder.ins().srem(left, right),
                            _ => unreachable!(),
                        };
                        Ok(value)
                    }
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq => {
                        let left = self.emit_expr(left, None)?;
                        let right = self.emit_expr(right, None)?;
                        let cond = match op {
                            BinaryOp::EqEq => self.builder.ins().icmp(IntCC::Equal, left, right),
                            BinaryOp::NotEq => {
                                self.builder.ins().icmp(IntCC::NotEqual, left, right)
                            }
                            BinaryOp::Lt => self.builder.ins().icmp(IntCC::SignedLessThan, left, right),
                            BinaryOp::LtEq => {
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedLessThanOrEqual, left, right)
                            }
                            BinaryOp::Gt => self.builder.ins().icmp(IntCC::SignedGreaterThan, left, right),
                            BinaryOp::GtEq => {
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, left, right)
                            }
                            _ => unreachable!(),
                        };
                        Ok(self.bool_to_i64(cond))
                    }
                    BinaryOp::AndAnd => self.emit_and(left, right),
                    BinaryOp::OrOr => self.emit_or(left, right),
                }
            }
            ExprKind::ArrayLit(elements) => self.emit_array_literal(elements, expected),
            ExprKind::ArrayNew { len } => self.emit_array_new(len, expected),
            ExprKind::Index { base, index } => self.emit_index_expr(base, index),
        }
    }

    fn bool_to_i64(&mut self, cond: Value) -> Value {
        let one = self.builder.ins().iconst(types::I64, 1);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().select(cond, one, zero)
    }

    fn i64_to_bool(&mut self, value: Value) -> Value {
        self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0)
    }

    fn emit_and(&mut self, left: &Expr, right: &Expr) -> Result<Value, NativeError> {
        let left_val = self.emit_expr(left, None)?;
        let cond = self.i64_to_bool(left_val);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        let right_val = self.emit_expr(right, None)?;
        self.builder.ins().jump(merge_block, &[right_val]);
        self.builder.seal_block(then_block);

        self.builder.switch_to_block(else_block);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(merge_block, &[zero]);
        self.builder.seal_block(else_block);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    fn emit_or(&mut self, left: &Expr, right: &Expr) -> Result<Value, NativeError> {
        let left_val = self.emit_expr(left, None)?;
        let cond = self.i64_to_bool(left_val);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        let one = self.builder.ins().iconst(types::I64, 1);
        self.builder.ins().jump(merge_block, &[one]);
        self.builder.seal_block(then_block);

        self.builder.switch_to_block(else_block);
        let right_val = self.emit_expr(right, None)?;
        self.builder.ins().jump(merge_block, &[right_val]);
        self.builder.seal_block(else_block);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    fn emit_string_literal(&mut self, value: &str) -> Result<Value, NativeError> {
        let data_id = self.string_data.get(value).cloned().map(Ok).unwrap_or_else(|| {
            let mut data_ctx = DataDescription::new();
            data_ctx.define(value.as_bytes().to_vec().into_boxed_slice());
            let name = format!("bd_str_{}", *self.string_counter);
            let data_id = self
                .module
                .declare_data(&name, Linkage::Local, false, false)
                .map_err(|err| native_error(format!("native declare data failed: {err}")))?;
            self.module
                .define_data(data_id, &data_ctx)
                .map_err(|err| native_error(format!("native define data failed: {err}")))?;
            self.string_data.insert(value.to_string(), data_id);
            *self.string_counter += 1;
            Ok(data_id)
        })?;
        let global = self.module.declare_data_in_func(data_id, self.builder.func);
        let ptr = self.builder.ins().global_value(types::I64, global);
        let len = self
            .builder
            .ins()
            .iconst(types::I64, value.as_bytes().len() as i64);
        Ok(self.call_runtime_value(
            self.runtime.alloc_string,
            &[self.rt_ptr, ptr, len],
        ))
    }

    fn emit_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        if name.starts_with("std::") {
            return self.emit_std_call(name, args, expected);
        }
        if let Some((base, method)) = name.split_once("::") {
            if let Some(Type::Book(book_name)) = self.lookup_local_type(base) {
                return self.emit_method_call(base, &book_name, method, args, expected);
            }
        }
        let sig = self
            .functions
            .get(name)
            .ok_or_else(|| native_error(format!("unknown function '{name}'.")))?;
        if sig.params.len() != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{name}': expected {}, got {}.",
                sig.params.len(),
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let func_id = self
            .func_ids
            .get(name)
            .copied()
            .ok_or_else(|| native_error(format!("missing function id for '{name}'.")))?;
        let func_ref = self
            .module
            .declare_func_in_func(func_id, self.builder.func);
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(self.rt_ptr);
        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
            let value = self.emit_expr(arg, Some(param_ty))?;
            call_args.push(value);
        }
        let call = self.builder.ins().call(func_ref, &call_args);
        Ok(self.builder.inst_results(call)[0])
    }

    fn emit_method_call(
        &mut self,
        base: &str,
        book: &str,
        method: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        let full_name = format!("{book}::{method}");
        let sig = self
            .functions
            .get(&full_name)
            .ok_or_else(|| native_error(format!("unknown method '{full_name}'.")))?;
        if sig.params.is_empty() {
            return Err(native_error(format!(
                "method '{full_name}' must take self."
            )));
        }
        let expected_args = sig.params.len().saturating_sub(1);
        if expected_args != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{full_name}': expected {}, got {}.",
                expected_args,
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let func_id = self
            .func_ids
            .get(&full_name)
            .copied()
            .ok_or_else(|| native_error(format!("missing function id for '{full_name}'.")))?;
        let func_ref = self
            .module
            .declare_func_in_func(func_id, self.builder.func);
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let mut call_args = Vec::with_capacity(args.len() + 2);
        call_args.push(self.rt_ptr);
        call_args.push(self.builder.use_var(base_info.var));
        for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
            call_args.push(self.emit_expr(arg, Some(param_ty))?);
        }
        let call = self.builder.ins().call(func_ref, &call_args);
        Ok(self.builder.inst_results(call)[0])
    }

    fn emit_std_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        let sig = stdlib_signature(name)
            .ok_or_else(|| native_error(format!("unknown function '{name}'.")))?;
        if sig.params.len() != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{name}': expected {}, got {}.",
                sig.params.len(),
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
            let mut arg_vals = Vec::with_capacity(args.len());
            for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                arg_vals.push(self.emit_expr(arg, Some(param_ty))?);
            }
            let value = match name {
            "std::string::len" => self.call_runtime_value(
                self.runtime.string_len,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::string::concat" => self.call_runtime_value(
                self.runtime.string_concat,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            ),
            "std::string::eq" => self.call_runtime_value(
                self.runtime.string_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            ),
            "std::string::bytes" => self.call_runtime_value(
                self.runtime.string_bytes,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::string::from_bytes" => self.call_runtime_value(
                self.runtime.string_from_bytes,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::string::to_i64" => self.call_runtime_value(
                self.runtime.string_to_i64,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::string::from_i64" => self.call_runtime_value(
                self.runtime.string_from_i64,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::bytes::len" => {
                self.call_runtime_value(self.runtime.bytes_len, &[self.rt_ptr, arg_vals[0]])
            }
            "std::bytes::eq" => self.call_runtime_value(
                self.runtime.bytes_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            ),
            "std::io::print" => self.call_runtime_value(
                self.runtime.io_print,
                &[self.rt_ptr, arg_vals[0]],
            ),
            "std::io::read_line" => {
                self.call_runtime_value(self.runtime.io_read_line, &[self.rt_ptr])
            }
            _ => return Err(native_error(format!("unknown function '{name}'."))),
        };
        Ok(value)
    }

    fn emit_array_new(&mut self, len: &Expr, expected: Option<&Type>) -> Result<Value, NativeError> {
        let Some(Type::Array(elem_ty)) = expected else {
            return Err(native_error(
                "array constructor requires explicit array type.",
            ));
        };
        let len_val = self.emit_expr(len, Some(&Type::I64))?;
        let elem_kind = elem_kind_for_type(elem_ty.as_ref())?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        );
        if elem_kind == abi::ARRAY_KIND_REF {
            let idx_var = self.new_var();
            let zero = self.builder.ins().iconst(types::I64, 0);
            self.builder.def_var(idx_var, zero);
            let loop_block = self.builder.create_block();
            let body_block = self.builder.create_block();
            let exit_block = self.builder.create_block();
            self.builder.ins().jump(loop_block, &[]);

            self.builder.switch_to_block(loop_block);
            let idx_val = self.builder.use_var(idx_var);
            let cond = self
                .builder
                .ins()
                .icmp(IntCC::SignedLessThan, idx_val, len_val);
            self.builder
                .ins()
                .brif(cond, body_block, &[], exit_block, &[]);

            self.builder.switch_to_block(body_block);
            let default_val = self.emit_default_value(elem_ty.as_ref())?;
            self.emit_array_set(elem_ty.as_ref(), handle, idx_val, default_val)?;
            let one = self.builder.ins().iconst(types::I64, 1);
            let next = self.builder.ins().iadd(idx_val, one);
            self.builder.def_var(idx_var, next);
            self.builder.ins().jump(loop_block, &[]);
            self.builder.seal_block(body_block);
            self.builder.seal_block(loop_block);

            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
        }
        Ok(handle)
    }

    fn emit_array_literal(
        &mut self,
        elements: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        let elem_ty = if let Some(Type::Array(inner)) = expected {
            inner.as_ref().clone()
        } else if elements.is_empty() {
            return Err(native_error(
                "array literal requires explicit array type.",
            ));
        } else {
            let first = self
                .infer_expr_type(&elements[0])
                .ok_or_else(|| native_error("array literal element has unknown type."))?;
            for elem in &elements[1..] {
                let ty = self
                    .infer_expr_type(elem)
                    .ok_or_else(|| native_error("array literal element has unknown type."))?;
                if ty != first {
                    return Err(native_error(
                        "array literal elements must have the same type.",
                    ));
                }
            }
            first
        };
        let len_val = self
            .builder
            .ins()
            .iconst(types::I64, elements.len() as i64);
        let elem_kind = elem_kind_for_type(&elem_ty)?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        );
        for (index, expr) in elements.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_expr(expr, Some(&elem_ty))?;
            self.emit_array_set(&elem_ty, handle, index_val, value)?;
        }
        Ok(handle)
    }

    fn emit_default_value(&mut self, ty: &Type) -> Result<Value, NativeError> {
        match ty {
            Type::I64 => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::Bool => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::U8 => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::String => self.emit_string_literal(""),
            Type::Array(elem_ty) => self.emit_empty_array(elem_ty.as_ref()),
            Type::Book(book) => self.emit_default_book(book),
        }
    }

    fn emit_default_book(&mut self, book: &str) -> Result<Value, NativeError> {
        let layout = self
            .books
            .get(book)
            .ok_or_else(|| native_error(format!("unknown book '{book}'.")))?;
        let book_id = self
            .builder
            .ins()
            .iconst(types::I64, layout.id as i64);
        let field_count = self
            .builder
            .ins()
            .iconst(types::I64, layout.fields.len() as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_object,
            &[self.rt_ptr, book_id, field_count],
        );
        for (index, field_ty) in layout.fields.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_default_value(field_ty)?;
            self.emit_object_set(field_ty, handle, index_val, value)?;
        }
        Ok(handle)
    }

    fn emit_empty_array(&mut self, elem_ty: &Type) -> Result<Value, NativeError> {
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let len_val = self.builder.ins().iconst(types::I64, 0);
        Ok(self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        ))
    }

    fn emit_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<Value, NativeError> {
        let base_ty = self
            .infer_expr_type(base)
            .ok_or_else(|| native_error("indexing requires array type."))?;
        let Type::Array(elem_ty) = base_ty else {
            return Err(native_error("indexing on non-array."));
        };
        let handle = self.emit_expr(base, None)?;
        let index_val = self.emit_expr(index, Some(&Type::I64))?;
        self.emit_array_get(&elem_ty, handle, index_val)
    }

    fn emit_array_get(
        &mut self,
        elem_ty: &Type,
        handle: Value,
        index: Value,
    ) -> Result<Value, NativeError> {
        match elem_ty {
            Type::I64 => Ok(self.call_runtime_value(
                self.runtime.array_get_i64,
                &[self.rt_ptr, handle, index],
            )),
            Type::Bool => Ok(self.call_runtime_value(
                self.runtime.array_get_bool,
                &[self.rt_ptr, handle, index],
            )),
            Type::U8 => Ok(self.call_runtime_value(
                self.runtime.array_get_u8,
                &[self.rt_ptr, handle, index],
            )),
            Type::String | Type::Array(_) | Type::Book(_) => Ok(self.call_runtime_value(
                self.runtime.array_get_ref,
                &[self.rt_ptr, handle, index],
            )),
        }
    }

    fn emit_array_set(
        &mut self,
        elem_ty: &Type,
        handle: Value,
        index: Value,
        value: Value,
    ) -> Result<(), NativeError> {
        match elem_ty {
            Type::I64 => self.call_runtime_void(
                self.runtime.array_set_i64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Bool => self.call_runtime_void(
                self.runtime.array_set_bool,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::U8 => self.call_runtime_void(
                self.runtime.array_set_u8,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_void(
                self.runtime.array_set_ref,
                &[self.rt_ptr, handle, index, value],
            ),
        }
        Ok(())
    }

    fn emit_new(&mut self, book: &str, args: &[Expr]) -> Result<Value, NativeError> {
        let layout = self
            .books
            .get(book)
            .ok_or_else(|| native_error(format!("unknown book '{book}'.")))?;
        let book_id = self
            .builder
            .ins()
            .iconst(types::I64, layout.id as i64);
        let field_count = self
            .builder
            .ins()
            .iconst(types::I64, layout.fields.len() as i64);
        let mut handle = self.call_runtime_value(
            self.runtime.alloc_object,
            &[self.rt_ptr, book_id, field_count],
        );
        for (index, field_ty) in layout.fields.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_default_value(field_ty)?;
            self.emit_object_set(field_ty, handle, index_val, value)?;
        }
        let init_name = format!("{book}::init");
        if let Some(sig) = self.functions.get(&init_name) {
            if sig.params.is_empty() {
                return Err(native_error(format!(
                    "method '{init_name}' must take self."
                )));
            }
            let expected_args = sig.params.len().saturating_sub(1);
            if expected_args != args.len() {
                return Err(native_error(format!(
                    "wrong number of arguments for '{init_name}': expected {}, got {}.",
                    expected_args,
                    args.len()
                )));
            }
            let func_id = self
                .func_ids
                .get(&init_name)
                .copied()
                .ok_or_else(|| native_error(format!("missing function id for '{init_name}'.")))?;
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
            let mut call_args = Vec::with_capacity(args.len() + 2);
            call_args.push(self.rt_ptr);
            call_args.push(handle);
            for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
                call_args.push(self.emit_expr(arg, Some(param_ty))?);
            }
            let call = self.builder.ins().call(func_ref, &call_args);
            handle = self.builder.inst_results(call)[0];
        } else if !args.is_empty() {
            return Err(native_error(format!(
                "missing constructor '{init_name}'."
            )));
        }
        Ok(handle)
    }

    fn emit_member_access(&mut self, base: &str, field: &str) -> Result<Value, NativeError> {
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let Type::Book(book_name) = &base_info.ty else {
            return Err(native_error("field access requires book type."));
        };
        let layout = self
            .books
            .get(book_name)
            .ok_or_else(|| native_error(format!("unknown book '{book_name}'.")))?;
        let index = layout
            .field_index
            .get(field)
            .copied()
            .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
        let field_ty = layout
            .fields
            .get(index)
            .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
        let handle = self.builder.use_var(base_info.var);
        let index_val = self.builder.ins().iconst(types::I64, index as i64);
        self.emit_object_get(field_ty, handle, index_val)
    }

    fn emit_object_get(
        &mut self,
        field_ty: &Type,
        handle: Value,
        index: Value,
    ) -> Result<Value, NativeError> {
        match field_ty {
            Type::I64 => Ok(self.call_runtime_value(
                self.runtime.object_get_i64,
                &[self.rt_ptr, handle, index],
            )),
            Type::Bool => Ok(self.call_runtime_value(
                self.runtime.object_get_bool,
                &[self.rt_ptr, handle, index],
            )),
            Type::U8 => Ok(self.call_runtime_value(
                self.runtime.object_get_u8,
                &[self.rt_ptr, handle, index],
            )),
            Type::String | Type::Array(_) | Type::Book(_) => Ok(self.call_runtime_value(
                self.runtime.object_get_ref,
                &[self.rt_ptr, handle, index],
            )),
        }
    }

    fn emit_object_set(
        &mut self,
        field_ty: &Type,
        handle: Value,
        index: Value,
        value: Value,
    ) -> Result<(), NativeError> {
        match field_ty {
            Type::I64 => self.call_runtime_void(
                self.runtime.object_set_i64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Bool => self.call_runtime_void(
                self.runtime.object_set_bool,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::U8 => self.call_runtime_void(
                self.runtime.object_set_u8,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_void(
                self.runtime.object_set_ref,
                &[self.rt_ptr, handle, index, value],
            ),
        }
        Ok(())
    }

    fn update_root(&mut self, name: &str, value: Value) {
        let Some(slot) = self.root_slots.get(name) else {
            return;
        };
        let base = match self.root_base {
            Some(base) => base,
            None => return,
        };
        let slot_val = self.builder.ins().iconst(types::I64, *slot as i64);
        let absolute = self.builder.ins().iadd(base, slot_val);
        self.call_runtime_void(
            self.runtime.root_set,
            &[self.rt_ptr, absolute, value],
        );
    }

    fn emit_error_block(&mut self) {
        self.builder.switch_to_block(self.error_block);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().return_(&[zero]);
        self.builder.seal_block(self.error_block);
    }

    fn emit_error_check(&mut self) {
        let func_ref = self
            .module
            .declare_func_in_func(self.runtime.has_error, self.builder.func);
        let call = self.builder.ins().call(func_ref, &[self.rt_ptr]);
        let flag = self.builder.inst_results(call)[0];
        let cond = self.builder.ins().icmp_imm(IntCC::NotEqual, flag, 0);
        let ok_block = self.builder.create_block();
        self.builder
            .ins()
            .brif(cond, self.error_block, &[], ok_block, &[]);
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
    }

    fn call_runtime_value(&mut self, func_id: FuncId, args: &[Value]) -> Value {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        let result = self.builder.inst_results(call)[0];
        self.emit_error_check();
        result
    }

    fn call_runtime_void(&mut self, func_id: FuncId, args: &[Value]) {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
        self.emit_error_check();
    }

    fn lookup_local_type(&self, name: &str) -> Option<Type> {
        self.vars
            .get(name)
            .map(|info| info.ty.clone())
            .or_else(|| self.locals.get(name).cloned())
    }

    fn infer_expr_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Int(_) => Some(Type::I64),
            ExprKind::Bool(_) => Some(Type::Bool),
            ExprKind::String(_) => Some(Type::String),
            ExprKind::Ident(name) => self
                .vars
                .get(name)
                .map(|info| info.ty.clone())
                .or_else(|| self.locals.get(name).cloned()),
            ExprKind::Call { name, .. } => {
                if let Some((base, method)) = name.split_once("::") {
                    if base != "std" {
                        if let Some(Type::Book(book)) = self.lookup_local_type(base) {
                            let full_name = format!("{book}::{method}");
                            return self.functions.get(&full_name).map(|sig| sig.return_type.clone());
                        }
                    }
                }
                stdlib_signature(name)
                    .map(|sig| sig.return_type)
                    .or_else(|| self.functions.get(name).map(|sig| sig.return_type.clone()))
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => self.infer_expr_type(expr),
                UnaryOp::Not => Some(Type::Bool),
            },
            ExprKind::Binary { op, .. } => match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod => Some(Type::I64),
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::AndAnd
                | BinaryOp::OrOr => Some(Type::Bool),
            },
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    None
                } else {
                    let first = self.infer_expr_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let ty = self.infer_expr_type(elem)?;
                        if ty != first {
                            return None;
                        }
                    }
                    Some(Type::Array(Box::new(first)))
                }
            }
            ExprKind::Index { base, .. } => {
                let base_ty = self.infer_expr_type(base)?;
                match base_ty {
                    Type::Array(inner) => Some(*inner),
                    _ => None,
                }
            }
            ExprKind::New { book, .. } => Some(Type::Book(book.clone())),
            ExprKind::MemberAccess { base, field } => {
                let Type::Book(book) = self.lookup_local_type(base)? else {
                    return None;
                };
                let layout = self.books.get(&book)?;
                let index = layout.field_index.get(field)?;
                layout.fields.get(*index).cloned()
            }
            _ => None,
        }
    }

    fn new_var(&mut self) -> Variable {
        let var = Variable::from_u32(self.next_var);
        self.next_var += 1;
        self.builder.declare_var(var, types::I64);
        var
    }
}

fn collect_local_types(
    function: &birddisk_core::ast::Function,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
) -> Result<HashMap<String, Type>, NativeError> {
    let mut locals = HashMap::new();
    for param in &function.params {
        if locals.contains_key(&param.name) {
            return Err(native_error(format!(
                "native backend does not support shadowing '{}'.",
                param.name
            )));
        }
        locals.insert(param.name.clone(), param.ty.clone());
    }
    collect_local_types_in_block(&function.body, &mut locals, functions, books)?;
    Ok(locals)
}

fn collect_local_types_in_block(
    stmts: &[Stmt],
    locals: &mut HashMap<String, Type>,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
) -> Result<(), NativeError> {
    for stmt in stmts {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                if locals.contains_key(name) {
                    return Err(native_error(format!(
                        "native backend does not support shadowing '{name}'."
                    )));
                }
                let var_ty = if let Some(ty) = ty {
                    ty.clone()
                } else {
                    infer_expr_type(expr, locals, functions, books).ok_or_else(|| {
                        native_error(format!(
                            "native backend requires explicit type for '{name}'."
                        ))
                    })?
                };
                locals.insert(name.clone(), var_ty);
            }
            Stmt::When {
                then_body,
                else_body,
                ..
            } => {
                collect_local_types_in_block(then_body, locals, functions, books)?;
                collect_local_types_in_block(else_body, locals, functions, books)?;
            }
            Stmt::Repeat { body, .. } => {
                collect_local_types_in_block(body, locals, functions, books)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn infer_expr_type(
    expr: &Expr,
    locals: &HashMap<String, Type>,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
) -> Option<Type> {
    match &expr.kind {
        ExprKind::Int(_) => Some(Type::I64),
        ExprKind::Bool(_) => Some(Type::Bool),
        ExprKind::String(_) => Some(Type::String),
        ExprKind::Ident(name) => locals.get(name).cloned(),
        ExprKind::Call { name, .. } => {
            if let Some((base, method)) = name.split_once("::") {
                if base != "std" {
                    if let Some(Type::Book(book)) = locals.get(base) {
                        let full_name = format!("{book}::{method}");
                        return functions.get(&full_name).map(|sig| sig.return_type.clone());
                    }
                }
            }
            stdlib_signature(name)
                .map(|sig| sig.return_type)
                .or_else(|| functions.get(name).map(|sig| sig.return_type.clone()))
        }
        ExprKind::Unary { op, expr } => match op {
            UnaryOp::Neg => infer_expr_type(expr, locals, functions, books),
            UnaryOp::Not => Some(Type::Bool),
        },
        ExprKind::Binary { op, .. } => match op {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::Mod => Some(Type::I64),
            BinaryOp::EqEq
            | BinaryOp::NotEq
            | BinaryOp::Lt
            | BinaryOp::LtEq
            | BinaryOp::Gt
            | BinaryOp::GtEq
            | BinaryOp::AndAnd
            | BinaryOp::OrOr => Some(Type::Bool),
        },
        ExprKind::ArrayLit(elements) => {
            if elements.is_empty() {
                None
            } else {
                let first = infer_expr_type(&elements[0], locals, functions, books)?;
                for elem in &elements[1..] {
                    let ty = infer_expr_type(elem, locals, functions, books)?;
                    if ty != first {
                        return None;
                    }
                }
                Some(Type::Array(Box::new(first)))
            }
        }
        ExprKind::Index { base, .. } => {
            let base_ty = infer_expr_type(base, locals, functions, books)?;
            match base_ty {
                Type::Array(inner) => Some(*inner),
                _ => None,
            }
        }
        ExprKind::New { book, .. } => Some(Type::Book(book.clone())),
        ExprKind::MemberAccess { base, field } => {
            let Type::Book(book) = locals.get(base)?.clone() else {
                return None;
            };
            let layout = books.get(&book)?;
            let index = layout.field_index.get(field)?;
            layout.fields.get(*index).cloned()
        }
        _ => None,
    }
}

fn build_root_slots(locals: &HashMap<String, Type>) -> HashMap<String, u32> {
    let mut slots = HashMap::new();
    let mut names: Vec<String> = locals
        .iter()
        .filter_map(|(name, ty)| if is_ref_type(ty) { Some(name.clone()) } else { None })
        .collect();
    names.sort();
    for (index, name) in names.into_iter().enumerate() {
        slots.insert(name, index as u32);
    }
    slots
}

fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}

fn elem_kind_for_type(ty: &Type) -> Result<u32, NativeError> {
    match ty {
        Type::I64 => Ok(abi::ARRAY_KIND_I64),
        Type::Bool => Ok(abi::ARRAY_KIND_BOOL),
        Type::U8 => Ok(abi::ARRAY_KIND_U8),
        Type::String | Type::Array(_) | Type::Book(_) => Ok(abi::ARRAY_KIND_REF),
    }
}

fn elem_size_for_kind(kind: u32) -> Result<u32, NativeError> {
    match kind {
        value if value == abi::ARRAY_KIND_I64 => Ok(8),
        value if value == abi::ARRAY_KIND_BOOL => Ok(1),
        value if value == abi::ARRAY_KIND_U8 => Ok(1),
        value if value == abi::ARRAY_KIND_REF => Ok(8),
        _ => Err(native_error("unknown array element kind.")),
    }
}

fn collect_function_sigs(program: &Program) -> Result<HashMap<String, FunctionSig>, NativeError> {
    let mut functions = HashMap::new();
    for func in &program.functions {
        insert_function_sig(&mut functions, &func.name, func)?;
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            insert_function_sig(&mut functions, &name, method)?;
        }
    }
    Ok(functions)
}

fn insert_function_sig(
    functions: &mut HashMap<String, FunctionSig>,
    name: &str,
    func: &birddisk_core::ast::Function,
) -> Result<(), NativeError> {
    if functions.contains_key(name) {
        return Err(native_error(format!(
            "native backend does not support duplicate function '{}'.",
            name
        )));
    }
    functions.insert(
        name.to_string(),
        FunctionSig {
            params: func.params.iter().map(|p| p.ty.clone()).collect(),
            return_type: func.return_type.clone(),
        },
    );
    Ok(())
}

fn declare_functions<M: Module>(
    module: &mut M,
    program: &Program,
    functions: &HashMap<String, FunctionSig>,
    name_mangler: impl Fn(&str) -> String,
) -> Result<HashMap<String, FuncId>, NativeError> {
    let mut ids = HashMap::new();
    for (func, name) in collect_functions(program) {
        let sig = make_signature(module, func);
        let symbol = name_mangler(&name);
        let linkage = if name == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        };
        let id = module
            .declare_function(&symbol, linkage, &sig)
            .map_err(|err| native_error(format!("native declare failed: {err}")))?;
        ids.insert(name, id);
    }
    for name in functions.keys() {
        if !ids.contains_key(name) {
            return Err(native_error(format!("missing declared function '{name}'.")));
        }
    }
    Ok(ids)
}

fn make_signature(
    module: &mut dyn Module,
    function: &birddisk_core::ast::Function,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    for _ in &function.params {
        sig.params.push(AbiParam::new(types::I64));
    }
    sig.returns.push(AbiParam::new(types::I64));
    sig
}

fn type_name(ty: &Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::Bool => "bool",
        Type::String => "string",
        Type::U8 => "u8",
        Type::Array(_) => "array",
        Type::Book(_) => "book",
    }
}

fn mangle_symbol(name: &str) -> String {
    if name == "main" {
        return NATIVE_MAIN_SYMBOL.to_string();
    }
    let mut out = String::from("bd_");
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            use std::fmt::Write;
            let _ = write!(&mut out, "_u{:04x}", ch as u32);
        }
    }
    out
}

fn stdlib_signature(name: &str) -> Option<FunctionSig> {
    match name {
        "std::string::len" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::string::concat" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::String,
        }),
        "std::string::eq" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::Bool,
        }),
        "std::string::bytes" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::Array(Box::new(Type::U8)),
        }),
        "std::string::from_bytes" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8))],
            return_type: Type::String,
        }),
        "std::string::to_i64" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::string::from_i64" => Some(FunctionSig {
            params: vec![Type::I64],
            return_type: Type::String,
        }),
        "std::bytes::len" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8))],
            return_type: Type::I64,
        }),
        "std::bytes::eq" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8)), Type::Array(Box::new(Type::U8))],
            return_type: Type::Bool,
        }),
        "std::io::print" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::io::read_line" => Some(FunctionSig {
            params: Vec::new(),
            return_type: Type::String,
        }),
        _ => None,
    }
}

fn collect_functions(program: &Program) -> Vec<(&birddisk_core::ast::Function, String)> {
    let mut out = Vec::new();
    for func in &program.functions {
        out.push((func, func.name.clone()));
    }
    for book in &program.books {
        for method in &book.methods {
            out.push((method, format!("{}::{}", book.name, method.name)));
        }
    }
    out
}

fn build_book_layouts(
    program: &Program,
) -> Result<(HashMap<String, BookLayout>, Vec<Vec<usize>>), NativeError> {
    let mut books = HashMap::new();
    let mut ref_fields = Vec::new();
    for (book_id, book) in program.books.iter().enumerate() {
        if books.contains_key(&book.name) {
            return Err(native_error(format!(
                "native backend does not support duplicate book '{}'.",
                book.name
            )));
        }
        let mut field_index = HashMap::new();
        let mut fields = Vec::new();
        let mut refs = Vec::new();
        for (index, field) in book.fields.iter().enumerate() {
            field_index.insert(field.name.clone(), index);
            fields.push(field.ty.clone());
            if is_ref_type(&field.ty) {
                refs.push(index);
            }
        }
        ref_fields.push(refs.clone());
        books.insert(
            book.name.clone(),
            BookLayout {
                id: book_id as u32,
                fields,
                field_index,
            },
        );
    }
    Ok((books, ref_fields))
}

#[cfg(test)]
mod tests {
    use super::{emit_object, run, run_with_io};
    use birddisk_core::{lexer, parser};

    fn run_source(source: &str) -> i64 {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        run(&program).unwrap()
    }

    fn run_source_with_io(source: &str, input: &str) -> (i64, String) {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        run_with_io(&program, input).unwrap()
    }

    fn run_source_error(source: &str) -> super::NativeError {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        run_with_io(&program, "").unwrap_err()
    }

    fn emit_source(source: &str) -> Vec<u8> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        emit_object(&program).unwrap()
    }

    #[test]
    fn native_runs_simple_arithmetic() {
        let result = run_source("rule main() -> i64:\n  yield 1 + 2 * 3.\nend\n");
        assert_eq!(result, 7);
    }

    #[test]
    fn native_runs_locals() {
        let result = run_source(
            "rule main() -> i64:\n  set a: i64 = 4.\n  set b: i64 = 2.\n  put a = a * b.\n  yield a + 1.\nend\n",
        );
        assert_eq!(result, 9);
    }

    #[test]
    fn native_runs_when_else() {
        let result =
            run_source("rule main() -> i64:\n  when 1 < 2:\n    yield 10.\n  otherwise:\n    yield 20.\n  end\nend\n");
        assert_eq!(result, 10);
    }

    #[test]
    fn native_runs_repeat_loop() {
        let result = run_source(
            "rule main() -> i64:\n  set i: i64 = 0.\n  set acc: i64 = 0.\n  repeat while i < 5:\n    put acc = acc + i.\n    put i = i + 1.\n  end\n  yield acc.\nend\n",
        );
        assert_eq!(result, 10);
    }

    #[test]
    fn native_runs_boolean_logic() {
        let result = run_source(
            "rule main() -> i64:\n  set ok: bool = true && false || true.\n  when ok:\n    yield 1.\n  otherwise:\n    yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 1);
    }

    #[test]
    fn native_runs_array_literal() {
        let result = run_source(
            "rule main() -> i64:\n  set xs: i64[] = [1, 2, 3].\n  put xs[1] = 5.\n  yield xs[0] + xs[1] + xs[2].\nend\n",
        );
        assert_eq!(result, 9);
    }

    #[test]
    fn native_runs_array_new() {
        let result = run_source(
            "rule main() -> i64:\n  set xs: i64[] = array(3).\n  put xs[0] = 7.\n  yield xs[0].\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn native_runs_string_literal() {
        let result =
            run_source("rule main() -> i64:\n  set s: string = \"hi\".\n  yield 1.\nend\n");
        assert_eq!(result, 1);
    }

    #[test]
    fn native_runs_ref_array_index() {
        let result = run_source(
            "rule main() -> i64:\n  set xs: string[] = [\"a\", \"b\"].\n  set s: string = xs[1].\n  yield 1.\nend\n",
        );
        assert_eq!(result, 1);
    }

    #[test]
    fn native_runs_function_call() {
        let result = run_source(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(3, 4).\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn native_runs_std_string_len() {
        let result = run_source(
            "import std::string.\nrule main() -> i64:\n  set s: string = \"Bird\".\n  yield std::string::len(s).\nend\n",
        );
        assert_eq!(result, 4);
    }

    #[test]
    fn native_runs_std_string_concat_eq() {
        let result = run_source(
            "import std::string.\nrule main() -> i64:\n  set a: string = \"hi\".\n  set b: string = \"bird\".\n  set c: string = std::string::concat(a, \"!\").\n  set ok: bool = std::string::eq(c, \"hi!\").\n  when ok:\n    yield std::string::len(b).\n  otherwise:\n    yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 4);
    }

    #[test]
    fn native_runs_std_string_bytes_roundtrip() {
        let result = run_source(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set data: u8[] = std::string::bytes(\"hi\").\n  set text: string = std::string::from_bytes(data).\n  set ok: bool = std::string::eq(text, \"hi\").\n  when ok:\n    yield std::bytes::len(data).\n  otherwise:\n    yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn native_runs_std_io_roundtrip() {
        let source = "import std::io.\nimport std::string.\nrule main() -> i64:\n  set name: string = std::io::read_line().\n  set greet: string = std::string::concat(\"Hello \", name).\n  set ignored: i64 = std::io::print(greet).\n  yield std::string::len(greet).\nend\n";
        let (result, output) = run_source_with_io(source, "Ada");
        assert_eq!(result, 9);
        assert_eq!(output, "Hello Ada");
    }

    #[test]
    fn native_runs_books_and_methods() {
        let source = "book Counter:\n  field value: i64.\n\n  rule init(self: Counter, start: i64) -> Counter:\n    put self::value = start.\n    yield self.\n  end\n\n  rule inc(self: Counter, delta: i64) -> i64:\n    put self::value = self::value + delta.\n    yield self::value.\n  end\nend\n\nrule main() -> i64:\n  set counter: Counter = new Counter(3).\n  yield counter::inc(4).\nend\n";
        let result = run_source(source);
        assert_eq!(result, 7);
    }

    #[test]
    fn native_reports_array_oob() {
        let err = run_source_error(
            "rule main() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n",
        );
        assert_eq!(err.code, Some("E0403"));
        assert_eq!(err.message, "Array index out of bounds.");
    }

    #[test]
    fn native_reports_invalid_utf8_from_bytes() {
        let err = run_source_error(
            "import std::string.\nrule main() -> i64:\n  set data: u8[] = [255].\n  set text: string = std::string::from_bytes(data).\n  yield 0.\nend\n",
        );
        assert_eq!(err.code, Some("E0400"));
        assert_eq!(err.message, "Invalid UTF-8 in std::string::from_bytes.");
    }

    #[test]
    fn native_emits_object_bytes() {
        let obj = emit_source("rule main() -> i64:\n  yield 1.\nend\n");
        assert!(!obj.is_empty());
    }
}
