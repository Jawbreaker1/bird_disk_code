use crate::analysis::{
    program_uses_arrays, program_uses_env, program_uses_fs, program_uses_io, program_uses_objects,
    program_uses_path, program_uses_profiler, program_uses_string_from_bytes, program_uses_strings,
    program_uses_time,
};
use crate::emit::{
    emit_wat, wasm_error, WasmError, HEAP_KIND_SHIFT, HEAP_KIND_STRING, HEAP_LEN_OFFSET,
    STRING_HEADER_SIZE, TRACE_STACK_DATA_OFFSET, TRACE_STACK_PTR_OFFSET, TRACE_STACK_SLOTS,
    TRAP_ARRAY_LEN_NEG, TRAP_ARRAY_OOB, TRAP_ARRAY_OOM, TRAP_CHANNEL_BLOCK, TRAP_ENV, TRAP_FS_IO,
    TRAP_HEAP_HEADER, TRAP_JSON_PARSE, TRAP_KIND_ARRAY, TRAP_KIND_BYTES, TRAP_KIND_ENUM,
    TRAP_KIND_OBJECT, TRAP_KIND_STRING, TRAP_NULL_DEREF, TRAP_PATH, TRAP_RAND_RANGE,
    TRAP_STRING_OOB, TRAP_STRING_PARSE, TRAP_STRING_UTF8, TRAP_TIME_NEG, TRAP_TRACE_OOM,
    TRAP_UTF8_INVALID,
};
use crate::trace::build_trace_table;
use birddisk_core::ast::{Program, Type};
use birddisk_core::TraceFrame;
use std::collections::VecDeque;
use std::path::{Component, Path, PathBuf};
use std::time::Instant;

struct IoState {
    args: Vec<String>,
    input: VecDeque<String>,
    output: String,
    pending_line: Option<Vec<u8>>,
    pending_file: Option<Vec<u8>>,
    pending_path: Option<Vec<u8>>,
    pending_env: Option<Vec<u8>>,
    start_time: Instant,
}

impl IoState {
    fn new(input: &str, args: &[String]) -> Self {
        let input = if input.is_empty() {
            VecDeque::new()
        } else {
            input
                .split('\n')
                .map(|line| line.strip_suffix('\r').unwrap_or(line).to_string())
                .collect()
        };
        Self {
            args: args.to_vec(),
            input,
            output: String::new(),
            pending_line: None,
            pending_file: None,
            pending_path: None,
            pending_env: None,
            start_time: Instant::now(),
        }
    }

    fn prepare_line(&mut self) -> i32 {
        if self.pending_line.is_none() {
            if let Some(line) = self.input.pop_front() {
                self.pending_line = Some(line.into_bytes());
            }
        }
        match self.pending_line {
            Some(ref line) => i32::try_from(line.len()).unwrap_or(-1),
            None => -1,
        }
    }

    fn consume_line(&mut self) -> Vec<u8> {
        self.pending_line.take().unwrap_or_default()
    }
}

pub fn run(program: &Program) -> Result<i64, WasmError> {
    let (result, _) = run_with_io(program, "", &[])?;
    Ok(result)
}

pub fn run_with_io(
    program: &Program,
    input: &str,
    args: &[String],
) -> Result<(i64, String), WasmError> {
    use wasmtime::{Engine, Linker, Module, Store};

    let uses_arrays = program_uses_arrays(program);
    let uses_strings = program_uses_strings(program);
    let uses_from_bytes = program_uses_string_from_bytes(program);
    let uses_io = program_uses_io(program);
    let uses_objects = program_uses_objects(program);
    let uses_time = program_uses_time(program) || program_uses_profiler(program);
    let uses_fs = program_uses_fs(program);
    let uses_path = program_uses_path(program);
    let uses_env = program_uses_env(program);
    let needs_validate_utf8 = uses_strings || uses_from_bytes || uses_fs || uses_path || uses_env;
    let uses_trace = true;
    let uses_heap = uses_arrays || uses_strings || uses_io || uses_objects || uses_trace;
    let trace_table = build_trace_table(program);
    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| wasm_error("E0400", "missing main entry point in entry file"))?;
    if &main.return_type != &Type::I64 {
        return Err(wasm_error("E0400", "main must return i64"));
    }

    let wat = emit_wat(program)?;
    let engine = Engine::default();
    let module = Module::new(&engine, wat)
        .map_err(|err| wasm_error("E0400", format!("WASM compile error: {err}")))?;
    let mut store = Store::new(&engine, IoState::new(input, args));
    let mut linker = Linker::new(&engine);
    link_imports(
        &mut linker,
        uses_heap,
        needs_validate_utf8,
        uses_io,
        uses_time,
        uses_fs,
        uses_path,
        uses_env,
    )?;
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|err| map_trap(err, "WASM instantiation error", Vec::new()))?;
    let func = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .map_err(|err| map_trap(err, "WASM missing main export", Vec::new()))?;
    let result = match func.call(&mut store, ()) {
        Ok(result) => result,
        Err(err) => {
            let trace = read_trace(&mut store, &instance, &trace_table.frames, None);
            return Err(map_trap(err, "WASM runtime error", trace));
        }
    };
    let output = store.data().output.clone();
    if let Some((message, trace_depth)) =
        read_error_state(&mut store, &instance, &trace_table.frames)
    {
        let trace = read_trace(&mut store, &instance, &trace_table.frames, trace_depth);
        let mut err = wasm_error("E0404", message);
        err.trace = trace;
        return Err(err);
    }
    Ok((result, output))
}

pub fn run_wasm_bytes(bytes: &[u8]) -> Result<i64, WasmError> {
    let (result, _) = run_wasm_bytes_with_io(bytes, "", &[])?;
    Ok(result)
}

pub fn run_wasm_bytes_with_io(
    bytes: &[u8],
    input: &str,
    args: &[String],
) -> Result<(i64, String), WasmError> {
    use wasmtime::{Engine, Linker, Module, Store};

    let engine = Engine::default();
    let module = Module::new(&engine, bytes)
        .map_err(|err| wasm_error("E0400", format!("WASM compile error: {err}")))?;
    let mut store = Store::new(&engine, IoState::new(input, args));
    let mut linker = Linker::new(&engine);
    link_imports(&mut linker, true, true, true, true, true, true, true)?;
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|err| map_trap(err, "WASM instantiation error", Vec::new()))?;
    let func = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .map_err(|err| map_trap(err, "WASM missing main export", Vec::new()))?;
    let result = match func.call(&mut store, ()) {
        Ok(result) => result,
        Err(err) => return Err(map_trap(err, "WASM runtime error", Vec::new())),
    };
    let output = store.data().output.clone();
    if let Some((message, _)) = read_error_state(&mut store, &instance, &[]) {
        return Err(wasm_error("E0404", message));
    }
    Ok((result, output))
}

fn link_imports(
    linker: &mut wasmtime::Linker<IoState>,
    uses_heap: bool,
    needs_validate_utf8: bool,
    uses_io: bool,
    uses_time: bool,
    uses_fs: bool,
    uses_path: bool,
    uses_env: bool,
) -> Result<(), WasmError> {
    if uses_heap {
        linker
            .func_wrap("env", "bd_trap", |code: i32| -> anyhow::Result<()> {
                Err(anyhow::anyhow!(format!("bd_trap:{code}")))
            })
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if needs_validate_utf8 {
        use wasmtime::{Caller, Extern};
        linker
            .func_wrap(
                "env",
                "bd_validate_utf8",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    if len < 0 || ptr < 0 {
                        return 0;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return 0,
                    };
                    validate_utf8(&memory, &mut caller, ptr as usize, len as usize)
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_io {
        use wasmtime::{Caller, Extern};
        linker
            .func_wrap(
                "env",
                "bd_print",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| {
                    if len <= 0 {
                        return;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return,
                    };
                    let text = {
                        let data = memory.data(&caller);
                        let start = ptr.max(0) as usize;
                        let end = start.saturating_add(len as usize);
                        if end > data.len() {
                            return;
                        }
                        std::str::from_utf8(&data[start..end])
                            .ok()
                            .map(|text| text.to_string())
                    };
                    if let Some(text) = text {
                        caller.data_mut().output.push_str(&text);
                    }
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_read_line_len",
                |mut caller: Caller<'_, IoState>| caller.data_mut().prepare_line(),
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_read_line_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| {
                    if len <= 0 {
                        caller.data_mut().consume_line();
                        return;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return,
                    };
                    let bytes = caller.data_mut().consume_line();
                    if bytes.is_empty() {
                        return;
                    }
                    let start = ptr.max(0) as usize;
                    let end = start.saturating_add(len as usize);
                    if end > memory.data_size(&caller) {
                        return;
                    }
                    let _ =
                        memory.write(&mut caller, start, &bytes[..bytes.len().min(len as usize)]);
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_time {
        linker
            .func_wrap(
                "env",
                "bd_time_now_ms",
                |caller: wasmtime::Caller<'_, IoState>| {
                    let elapsed = caller.data().start_time.elapsed().as_millis();
                    i64::try_from(elapsed).unwrap_or(i64::MAX)
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_time_sleep_ms",
                |_caller: wasmtime::Caller<'_, IoState>, millis: i64| -> anyhow::Result<i64> {
                    if millis < 0 {
                        return Err(anyhow::anyhow!(format!("bd_trap:{TRAP_TIME_NEG}")));
                    }
                    std::thread::sleep(std::time::Duration::from_millis(millis as u64));
                    Ok(millis)
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_fs {
        use wasmtime::{Caller, Extern};
        linker
            .func_wrap(
                "env",
                "bd_fs_read_len",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let path = match std::str::from_utf8(&bytes) {
                        Ok(path) => path,
                        Err(_) => return -1,
                    };
                    let data = match std::fs::read(path) {
                        Ok(data) => data,
                        Err(_) => return -1,
                    };
                    let len = data.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_file = Some(data);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_fs_read_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let Some(bytes) = caller.data_mut().pending_file.take() else {
                        return -1;
                    };
                    if len < 0 || bytes.len() != len as usize {
                        return -1;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return -1,
                    };
                    let start = ptr.max(0) as usize;
                    if memory.write(&mut caller, start, &bytes).is_err() {
                        return -1;
                    }
                    len
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_fs_write",
                |mut caller: Caller<'_, IoState>,
                 path_ptr: i32,
                 path_len: i32,
                 data_ptr: i32,
                 data_len: i32|
                 -> i64 {
                    let path_bytes = match memory_bytes(&mut caller, path_ptr, path_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let path = match std::str::from_utf8(&path_bytes) {
                        Ok(path) => path,
                        Err(_) => return -1,
                    };
                    let data = match memory_bytes(&mut caller, data_ptr, data_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    if std::fs::write(path, &data).is_err() {
                        return -1;
                    }
                    data_len as i64
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_path {
        use wasmtime::{Caller, Extern};
        linker
            .func_wrap(
                "env",
                "bd_path_join_len",
                |mut caller: Caller<'_, IoState>,
                 left_ptr: i32,
                 left_len: i32,
                 right_ptr: i32,
                 right_len: i32|
                 -> i32 {
                    let left = match memory_bytes(&mut caller, left_ptr, left_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let right = match memory_bytes(&mut caller, right_ptr, right_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let left = match std::str::from_utf8(&left) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let right = match std::str::from_utf8(&right) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let joined = std::path::Path::new(left).join(right);
                    let output = match joined.to_str() {
                        Some(value) => value.as_bytes().to_vec(),
                        None => return -1,
                    };
                    let len = output.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_path = Some(output);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_path_normalize_len",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let path = match std::str::from_utf8(&bytes) {
                        Ok(path) => path,
                        Err(_) => return -1,
                    };
                    let output = match normalize_path(path) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let len = output.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_path = Some(output);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_path_basename_len",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let path = match std::str::from_utf8(&bytes) {
                        Ok(path) => path,
                        Err(_) => return -1,
                    };
                    let output = std::path::Path::new(path)
                        .file_name()
                        .and_then(|name| name.to_str())
                        .unwrap_or("")
                        .as_bytes()
                        .to_vec();
                    let len = output.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_path = Some(output);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_path_dirname_len",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let path = match std::str::from_utf8(&bytes) {
                        Ok(path) => path,
                        Err(_) => return -1,
                    };
                    let path = std::path::Path::new(path);
                    let output = if let Some(parent) = path.parent() {
                        if parent.as_os_str().is_empty() {
                            ".".to_string()
                        } else {
                            match parent.to_str() {
                                Some(value) => value.to_string(),
                                None => return -1,
                            }
                        }
                    } else if path.has_root() {
                        match path.to_str() {
                            Some(value) => value.to_string(),
                            None => return -1,
                        }
                    } else {
                        ".".to_string()
                    };
                    let bytes = output.into_bytes();
                    let len = bytes.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_path = Some(bytes);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_path_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let Some(bytes) = caller.data_mut().pending_path.take() else {
                        return -1;
                    };
                    if len < 0 || bytes.len() != len as usize {
                        return -1;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return -1,
                    };
                    let start = ptr.max(0) as usize;
                    if memory.write(&mut caller, start, &bytes).is_err() {
                        return -1;
                    }
                    len
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_env {
        use wasmtime::{Caller, Extern};
        linker
            .func_wrap(
                "env",
                "bd_env_args_count",
                |caller: Caller<'_, IoState>| -> i32 {
                    let count = caller.data().args.len();
                    if count > i32::MAX as usize {
                        return -1;
                    }
                    count as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_args_len",
                |mut caller: Caller<'_, IoState>, index: i32| -> i32 {
                    if index < 0 {
                        return -1;
                    }
                    let idx = index as usize;
                    let arg = match caller.data().args.get(idx) {
                        Some(value) => value,
                        None => return -1,
                    };
                    let bytes = arg.as_bytes().to_vec();
                    let len = bytes.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_env = Some(bytes);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_args_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let Some(bytes) = caller.data_mut().pending_env.take() else {
                        return -1;
                    };
                    if len < 0 || bytes.len() != len as usize {
                        return -1;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return -1,
                    };
                    let start = ptr.max(0) as usize;
                    if memory.write(&mut caller, start, &bytes).is_err() {
                        return -1;
                    }
                    len
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_get_len",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let name = match std::str::from_utf8(&bytes) {
                        Ok(name) => name,
                        Err(_) => return -1,
                    };
                    let value = match std::env::var_os(name) {
                        Some(value) => value,
                        None => {
                            caller.data_mut().pending_env = Some(Vec::new());
                            return 0;
                        }
                    };
                    let value = match value.into_string() {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let bytes = value.into_bytes();
                    let len = bytes.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_env = Some(bytes);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_get_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let Some(bytes) = caller.data_mut().pending_env.take() else {
                        return -1;
                    };
                    if len < 0 || bytes.len() != len as usize {
                        return -1;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return -1,
                    };
                    let start = ptr.max(0) as usize;
                    if memory.write(&mut caller, start, &bytes).is_err() {
                        return -1;
                    }
                    len
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_cwd_len",
                |mut caller: Caller<'_, IoState>| -> i32 {
                    let cwd = match std::env::current_dir() {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let text = match cwd.to_str() {
                        Some(value) => value.as_bytes().to_vec(),
                        None => return -1,
                    };
                    let len = text.len();
                    if len > i32::MAX as usize {
                        return -1;
                    }
                    caller.data_mut().pending_env = Some(text);
                    len as i32
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_cwd_fill",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i32 {
                    let Some(bytes) = caller.data_mut().pending_env.take() else {
                        return -1;
                    };
                    if len < 0 || bytes.len() != len as usize {
                        return -1;
                    }
                    let memory = match caller.get_export("memory") {
                        Some(Extern::Memory(mem)) => mem,
                        _ => return -1,
                    };
                    let start = ptr.max(0) as usize;
                    if memory.write(&mut caller, start, &bytes).is_err() {
                        return -1;
                    }
                    len
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_set",
                |mut caller: Caller<'_, IoState>,
                 name_ptr: i32,
                 name_len: i32,
                 value_ptr: i32,
                 value_len: i32|
                 -> i64 {
                    let name = match memory_bytes(&mut caller, name_ptr, name_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    let value = match memory_bytes(&mut caller, value_ptr, value_len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    if name.contains(&0) || value.contains(&0) {
                        return -1;
                    }
                    let name = match std::str::from_utf8(&name) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    let value = match std::str::from_utf8(&value) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    std::env::set_var(name, value);
                    1
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
        linker
            .func_wrap(
                "env",
                "bd_env_set_cwd",
                |mut caller: Caller<'_, IoState>, ptr: i32, len: i32| -> i64 {
                    let bytes = match memory_bytes(&mut caller, ptr, len) {
                        Some(bytes) => bytes,
                        None => return -1,
                    };
                    if bytes.contains(&0) {
                        return -1;
                    }
                    let path = match std::str::from_utf8(&bytes) {
                        Ok(value) => value,
                        Err(_) => return -1,
                    };
                    if std::env::set_current_dir(path).is_err() {
                        return -1;
                    }
                    1
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    Ok(())
}

fn validate_utf8<T>(
    memory: &wasmtime::Memory,
    store: &mut wasmtime::Caller<'_, T>,
    ptr: usize,
    len: usize,
) -> i32 {
    let data = memory.data(store);
    if ptr.saturating_add(len) > data.len() {
        return 0;
    }
    if std::str::from_utf8(&data[ptr..ptr + len]).is_ok() {
        1
    } else {
        0
    }
}

fn memory_bytes<T>(caller: &mut wasmtime::Caller<'_, T>, ptr: i32, len: i32) -> Option<Vec<u8>> {
    if ptr < 0 || len < 0 {
        return None;
    }
    let memory = match caller.get_export("memory") {
        Some(wasmtime::Extern::Memory(mem)) => mem,
        _ => return None,
    };
    let data = memory.data(caller);
    let start = ptr as usize;
    let end = start.saturating_add(len as usize);
    if end > data.len() {
        return None;
    }
    Some(data[start..end].to_vec())
}

fn normalize_path(path: &str) -> Result<Vec<u8>, ()> {
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
        return Ok(b".".to_vec());
    }
    out.to_str()
        .map(|value| value.as_bytes().to_vec())
        .ok_or(())
}

fn map_trap(err: anyhow::Error, default_message: &str, trace: Vec<TraceFrame>) -> WasmError {
    let mut mapped = if let Some(code) = trap_code_from_error(&err) {
        match code {
            TRAP_ARRAY_OOB => wasm_error("E0403", "Array index out of bounds."),
            TRAP_ARRAY_LEN_NEG => wasm_error("E0400", "Array length must be >= 0."),
            TRAP_ARRAY_OOM => wasm_error("E0400", "Out of memory during allocation."),
            TRAP_UTF8_INVALID => wasm_error("E0400", "Invalid UTF-8 in std::string::from_bytes."),
            TRAP_TRACE_OOM => wasm_error("E0400", "Trace stack overflow."),
            TRAP_STRING_PARSE => wasm_error("E0400", "Invalid integer in std::string::to_i64."),
            TRAP_NULL_DEREF => wasm_error("E0400", "Null dereference."),
            TRAP_KIND_STRING => wasm_error("E0400", "Expected string handle."),
            TRAP_KIND_ARRAY => wasm_error("E0400", "Expected array handle."),
            TRAP_KIND_OBJECT => wasm_error("E0400", "Expected book handle."),
            TRAP_KIND_BYTES => wasm_error("E0400", "std::bytes expects u8 array."),
            TRAP_KIND_ENUM => wasm_error("E0400", "Expected enum handle."),
            TRAP_STRING_OOB => wasm_error("E0400", "std::string::slice out of bounds."),
            TRAP_STRING_UTF8 => wasm_error("E0400", "Invalid UTF-8 in string operation."),
            TRAP_HEAP_HEADER => wasm_error("E0400", "Invalid heap header."),
            TRAP_TIME_NEG => wasm_error("E0400", "Sleep duration must be >= 0."),
            TRAP_RAND_RANGE => wasm_error("E0400", "std::rand::range expects min < max."),
            TRAP_FS_IO => wasm_error("E0400", "std::fs operation failed."),
            TRAP_PATH => wasm_error("E0400", "std::path operation failed."),
            TRAP_ENV => wasm_error("E0400", "std::env operation failed."),
            TRAP_JSON_PARSE => wasm_error("E0400", "Invalid JSON input."),
            TRAP_CHANNEL_BLOCK => wasm_error("E0400", "Channel recv would block."),
            _ => wasm_error("E0400", format!("{default_message}: {err}")),
        }
    } else if let Some(trap) = err.downcast_ref::<wasmtime::Trap>() {
        if *trap == wasmtime::Trap::IntegerDivisionByZero {
            wasm_error("E0402", "Division or modulo by zero.")
        } else {
            wasm_error("E0400", format!("{default_message}: {trap}"))
        }
    } else {
        wasm_error("E0400", format!("{default_message}: {err}"))
    };
    if mapped.trace.is_empty() {
        mapped.trace = trace;
    }
    mapped
}

fn read_error_state(
    store: &mut wasmtime::Store<IoState>,
    instance: &wasmtime::Instance,
    frames: &[TraceFrame],
) -> Option<(String, Option<i32>)> {
    let has_error = instance
        .get_typed_func::<(), i32>(&mut *store, "__bd_has_error")
        .ok()?;
    let flag = has_error.call(&mut *store, ()).ok()?;
    if flag == 0 {
        return None;
    }
    let msg_func = instance
        .get_typed_func::<(), i32>(&mut *store, "__bd_error_message")
        .ok()?;
    let trace_func = instance
        .get_typed_func::<(), i32>(&mut *store, "__bd_error_trace")
        .ok();
    let handle = msg_func.call(&mut *store, ()).ok()?;
    let message =
        read_string(store, instance, handle).unwrap_or_else(|| "Uncaught throw.".to_string());
    let depth = trace_func.and_then(|func| func.call(&mut *store, ()).ok());
    let depth = match depth {
        Some(value) if value > 0 => Some(value.min(frames.len() as i32)),
        _ => None,
    };
    Some((message, depth))
}

fn read_string(
    store: &mut wasmtime::Store<IoState>,
    instance: &wasmtime::Instance,
    handle: i32,
) -> Option<String> {
    if handle <= 0 {
        return None;
    }
    let memory = instance.get_memory(&mut *store, "memory")?;
    let data = memory.data(store);
    let base = handle as usize;
    if base >= data.len() {
        return None;
    }
    let tag = read_i32(data, base);
    if tag == 0 {
        return None;
    }
    let kind = (tag >> HEAP_KIND_SHIFT) as i32;
    if kind != HEAP_KIND_STRING {
        return None;
    }
    let len = read_i32(data, base + HEAP_LEN_OFFSET as usize) as usize;
    let start = base + STRING_HEADER_SIZE as usize;
    let end = start.saturating_add(len);
    if end > data.len() {
        return None;
    }
    std::str::from_utf8(&data[start..end])
        .ok()
        .map(|value| value.to_string())
}

fn read_trace(
    store: &mut wasmtime::Store<IoState>,
    instance: &wasmtime::Instance,
    frames: &[TraceFrame],
    depth_override: Option<i32>,
) -> Vec<TraceFrame> {
    let Some(memory) = instance.get_memory(&mut *store, "memory") else {
        return Vec::new();
    };
    let data = memory.data(store);
    let sp = depth_override.unwrap_or_else(|| read_i32(data, TRACE_STACK_PTR_OFFSET as usize));
    if sp <= 0 {
        return Vec::new();
    }
    let max = TRACE_STACK_SLOTS.max(0);
    let limit = sp.min(max) as usize;
    let mut trace = Vec::new();
    for i in 0..limit {
        let offset = TRACE_STACK_DATA_OFFSET as usize + i * 4;
        let id = read_i32(data, offset);
        if id >= 0 && (id as usize) < frames.len() {
            trace.push(frames[id as usize].clone());
        }
    }
    trace.reverse();
    trace
}

fn read_i32(data: &[u8], offset: usize) -> i32 {
    if offset + 4 > data.len() {
        return 0;
    }
    i32::from_le_bytes([
        data[offset],
        data[offset + 1],
        data[offset + 2],
        data[offset + 3],
    ])
}

fn trap_code_from_error(err: &anyhow::Error) -> Option<i32> {
    for cause in err.chain() {
        if let Some(code) = trap_code_from_message(&cause.to_string()) {
            return Some(code);
        }
    }
    None
}

fn trap_code_from_message(message: &str) -> Option<i32> {
    let marker = "bd_trap:";
    let idx = message.find(marker)?;
    let code = message[idx + marker.len()..]
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect::<String>();
    if code.is_empty() {
        None
    } else {
        code.parse().ok()
    }
}

#[cfg(test)]
mod tests {
    use super::{map_trap, run_wasm_bytes_with_io, IoState};
    use crate::emit::{
        emit_wasm, TRAP_HEAP_HEADER, TRAP_KIND_ARRAY, TRAP_KIND_BYTES, TRAP_KIND_OBJECT,
        TRAP_KIND_STRING,
    };
    use birddisk_core::{lexer, parser};

    #[test]
    fn maps_kind_traps() {
        let err = map_trap(
            anyhow::anyhow!("bd_trap:{}", TRAP_KIND_STRING),
            "default",
            Vec::new(),
        );
        assert_eq!(err.message, "Expected string handle.");
        let err = map_trap(
            anyhow::anyhow!("bd_trap:{}", TRAP_KIND_ARRAY),
            "default",
            Vec::new(),
        );
        assert_eq!(err.message, "Expected array handle.");
        let err = map_trap(
            anyhow::anyhow!("bd_trap:{}", TRAP_KIND_OBJECT),
            "default",
            Vec::new(),
        );
        assert_eq!(err.message, "Expected book handle.");
        let err = map_trap(
            anyhow::anyhow!("bd_trap:{}", TRAP_KIND_BYTES),
            "default",
            Vec::new(),
        );
        assert_eq!(err.message, "std::bytes expects u8 array.");
        let err = map_trap(
            anyhow::anyhow!("bd_trap:{}", TRAP_HEAP_HEADER),
            "default",
            Vec::new(),
        );
        assert_eq!(err.message, "Invalid heap header.");
    }

    #[test]
    fn io_state_strips_cr() {
        let mut state = IoState::new("123\r\n456\r\n", &[]);
        assert_eq!(state.prepare_line(), 3);
        assert_eq!(state.consume_line(), b"123".to_vec());
        assert_eq!(state.prepare_line(), 3);
        assert_eq!(state.consume_line(), b"456".to_vec());
    }

    #[test]
    fn wasm_bytes_runner_executes_module() {
        let source = "rule main() -> i64:\n  yield 7.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let bytes = emit_wasm(&program).unwrap();
        let (result, stdout) = run_wasm_bytes_with_io(&bytes, "", &[]).unwrap();
        assert_eq!(result, 7);
        assert!(stdout.is_empty());
    }
}
