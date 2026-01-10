use crate::analysis::{
    program_uses_arrays, program_uses_io, program_uses_objects, program_uses_string_from_bytes,
    program_uses_strings,
};
use crate::emit::{
    emit_wat, wasm_error, WasmError, TRACE_STACK_DATA_OFFSET, TRACE_STACK_PTR_OFFSET,
    TRACE_STACK_SLOTS, TRAP_ARRAY_LEN_NEG, TRAP_ARRAY_OOB, TRAP_ARRAY_OOM, TRAP_KIND_ARRAY,
    TRAP_KIND_BYTES, TRAP_KIND_OBJECT, TRAP_KIND_STRING, TRAP_NULL_DEREF, TRAP_STRING_PARSE,
    TRAP_TRACE_OOM, TRAP_UTF8_INVALID, TRAP_HEAP_HEADER,
};
use crate::trace::build_trace_table;
use birddisk_core::ast::{Program, Type};
use birddisk_core::TraceFrame;
use std::collections::VecDeque;

struct IoState {
    input: VecDeque<String>,
    output: String,
    pending_line: Option<Vec<u8>>,
}

impl IoState {
    fn new(input: &str) -> Self {
        let input = if input.is_empty() {
            VecDeque::new()
        } else {
            input.split('\n').map(|line| line.to_string()).collect()
        };
        Self {
            input,
            output: String::new(),
            pending_line: None,
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
    let (result, _) = run_with_io(program, "")?;
    Ok(result)
}

pub fn run_with_io(program: &Program, input: &str) -> Result<(i64, String), WasmError> {
    use wasmtime::{Engine, Linker, Module, Store};

    let uses_arrays = program_uses_arrays(program);
    let uses_strings = program_uses_strings(program);
    let uses_from_bytes = program_uses_string_from_bytes(program);
    let uses_io = program_uses_io(program);
    let uses_objects = program_uses_objects(program);
    let uses_trace = true;
    let uses_heap = uses_arrays || uses_strings || uses_io || uses_objects || uses_trace;
    let trace_table = build_trace_table(program);
    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| wasm_error("E0400", "missing main function"))?;
    if &main.return_type != &Type::I64 {
        return Err(wasm_error("E0400", "main must return i64"));
    }

    let wat = emit_wat(program)?;
    let engine = Engine::default();
    let module = Module::new(&engine, wat)
        .map_err(|err| wasm_error("E0400", format!("WASM compile error: {err}")))?;
    let mut store = Store::new(&engine, IoState::new(input));
    let mut linker = Linker::new(&engine);
    if uses_heap {
        linker
            .func_wrap("env", "bd_trap", |code: i32| -> anyhow::Result<()> {
                Err(anyhow::anyhow!(format!("bd_trap:{code}")))
            })
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    if uses_from_bytes {
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
            .func_wrap("env", "bd_read_line_len", |mut caller: Caller<'_, IoState>| {
                caller.data_mut().prepare_line()
            })
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
                    let _ = memory.write(&mut caller, start, &bytes[..bytes.len().min(len as usize)]);
                },
            )
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|err| map_trap(err, "WASM instantiation error", Vec::new()))?;
    let func = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .map_err(|err| map_trap(err, "WASM missing main export", Vec::new()))?;
    let result = match func.call(&mut store, ()) {
        Ok(result) => result,
        Err(err) => {
            let trace = read_trace(&mut store, &instance, &trace_table.frames);
            return Err(map_trap(err, "WASM runtime error", trace));
        }
    };
    let output = store.data().output.clone();
    Ok((result, output))
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
            TRAP_HEAP_HEADER => wasm_error("E0400", "Invalid heap header."),
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

fn read_trace(
    store: &mut wasmtime::Store<IoState>,
    instance: &wasmtime::Instance,
    frames: &[TraceFrame],
) -> Vec<TraceFrame> {
    let Some(memory) = instance.get_memory(&mut *store, "memory") else {
        return Vec::new();
    };
    let data = memory.data(store);
    let sp = read_i32(data, TRACE_STACK_PTR_OFFSET as usize);
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
    use super::map_trap;
    use crate::emit::{
        TRAP_HEAP_HEADER, TRAP_KIND_ARRAY, TRAP_KIND_BYTES, TRAP_KIND_OBJECT, TRAP_KIND_STRING,
    };

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
}
