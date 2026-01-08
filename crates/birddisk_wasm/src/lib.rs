use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Function, Program, Stmt, Type, UnaryOp};
use birddisk_core::TraceFrame;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
pub struct WasmError {
    pub code: &'static str,
    pub message: String,
    pub trace: Vec<birddisk_core::TraceFrame>,
}

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

fn wasm_error(code: &'static str, message: impl Into<String>) -> WasmError {
    WasmError {
        code,
        message: message.into(),
        trace: Vec::new(),
    }
}

const ARRAY_HEADER_SIZE: i32 = 8;
const STRING_HEADER_SIZE: i32 = 4;
const OBJECT_FIELD_SIZE: i32 = 8;
const TRAP_ARRAY_OOB: i32 = 403;
const TRAP_ARRAY_LEN_NEG: i32 = 400;
const TRAP_ARRAY_OOM: i32 = 405;
const TRAP_UTF8_INVALID: i32 = 406;
const TRAP_TRACE_OOM: i32 = 407;
const TRAP_STRING_PARSE: i32 = 408;

const TRACE_STACK_PTR_OFFSET: i32 = 0;
const TRACE_STACK_DATA_OFFSET: i32 = 4;
const TRACE_STACK_SLOTS: i32 = 256;
const TRACE_STACK_BYTES: i32 = TRACE_STACK_DATA_OFFSET + TRACE_STACK_SLOTS * 4;
const HEAP_START: i32 = (TRACE_STACK_BYTES + 7) & !7;

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Clone)]
struct BookLayout {
    fields: Vec<Type>,
    field_index: HashMap<String, usize>,
}

struct TraceTable {
    frames: Vec<TraceFrame>,
    ids: HashMap<String, i32>,
}

fn build_trace_table(program: &Program) -> TraceTable {
    let mut frames = Vec::new();
    let mut ids = HashMap::new();
    let mut insert = |name: String, span| {
        let id = frames.len() as i32;
        frames.push(TraceFrame { function: name.clone(), span });
        ids.insert(name, id);
    };
    for func in &program.functions {
        insert(func.name.clone(), func.span);
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            insert(name, method.span);
        }
    }
    TraceTable { frames, ids }
}

pub fn emit_wat(program: &Program) -> Result<String, WasmError> {
    let uses_arrays = program_uses_arrays(program);
    let uses_strings = program_uses_strings(program);
    let uses_bytes = program_uses_bytes(program);
    let uses_from_bytes = program_uses_string_from_bytes(program);
    let uses_io = program_uses_io(program);
    let uses_objects = program_uses_objects(program);
    let uses_trace = true;
    let uses_heap = uses_arrays || uses_strings || uses_io || uses_objects || uses_trace;
    let export_memory = uses_from_bytes || uses_io || uses_trace;
    let trace_table = build_trace_table(program);
    let mut functions = HashMap::new();
    for func in &program.functions {
        functions.insert(
            func.name.clone(),
            FunctionSig {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                return_type: func.return_type.clone(),
            },
        );
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            functions.insert(
                name,
                FunctionSig {
                    params: method.params.iter().map(|p| p.ty.clone()).collect(),
                    return_type: method.return_type.clone(),
                },
            );
        }
    }
    let mut books = HashMap::new();
    for book in &program.books {
        let mut field_index = HashMap::new();
        let mut fields = Vec::new();
        for (idx, field) in book.fields.iter().enumerate() {
            field_index.insert(field.name.clone(), idx);
            fields.push(field.ty.clone());
        }
        books.insert(
            book.name.clone(),
            BookLayout {
                fields,
                field_index,
            },
        );
    }

    let mut emitter = WatEmitter::new();
    emitter.push_line("(module");
    emitter.indent();

    if uses_heap {
        emit_heap_runtime(&mut emitter, export_memory, uses_from_bytes, uses_io);
    }
    if uses_strings {
        emit_string_runtime(&mut emitter, uses_from_bytes);
    }
    if uses_bytes {
        emit_bytes_runtime(&mut emitter);
    }
    if uses_io {
        emit_io_runtime(&mut emitter);
    }

    let frame_id_for = |name: &str| {
        trace_table.ids.get(name).copied().ok_or_else(|| {
            wasm_error("E0400", format!("Missing trace frame for '{name}'"))
        })
    };

    for func in &program.functions {
        emit_function(
            &mut emitter,
            func,
            &functions,
            &books,
            frame_id_for(&func.name)?,
            None,
        )?;
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            emit_function(
                &mut emitter,
                method,
                &functions,
                &books,
                frame_id_for(&name)?,
                Some(&name),
            )?;
        }
    }

    if functions.contains_key("main") {
        emitter.push_line("(export \"main\" (func $main))");
    }

    emitter.dedent();
    emitter.push_line(")");

    Ok(emitter.finish())
}

pub fn emit_wasm(program: &Program) -> Result<Vec<u8>, WasmError> {
    let wat = emit_wat(program)?;
    wat::parse_str(&wat).map_err(|err| wasm_error("E0400", format!("WASM encode error: {err}")))
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

fn all_functions<'a>(program: &'a Program) -> Vec<&'a Function> {
    let mut functions = Vec::new();
    functions.extend(program.functions.iter());
    for book in &program.books {
        functions.extend(book.methods.iter());
    }
    functions
}

fn program_uses_objects(program: &Program) -> bool {
    for func in all_functions(program) {
        if matches!(func.return_type, Type::Book(_)) {
            return true;
        }
        for param in &func.params {
            if matches!(param.ty, Type::Book(_)) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_object(stmt) {
                return true;
            }
        }
    }
    false
}

fn program_uses_arrays(program: &Program) -> bool {
    for func in all_functions(program) {
        if type_has_array(&func.return_type) {
            return true;
        }
        for param in &func.params {
            if type_has_array(&param.ty) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_array(stmt) {
                return true;
            }
        }
    }
    false
}

fn program_uses_strings(program: &Program) -> bool {
    for func in all_functions(program) {
        if type_has_string(&func.return_type) {
            return true;
        }
        for param in &func.params {
            if type_has_string(&param.ty) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_string(stmt) {
                return true;
            }
        }
    }
    false
}

fn program_uses_string_from_bytes(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_string_from_bytes(stmt) {
                return true;
            }
        }
    }
    false
}

fn program_uses_bytes(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_bytes(stmt) {
                return true;
            }
        }
    }
    false
}

fn program_uses_io(program: &Program) -> bool {
    for func in all_functions(program) {
        for stmt in &func.body {
            if stmt_has_io(stmt) {
                return true;
            }
        }
    }
    false
}

fn type_has_array(ty: &Type) -> bool {
    match ty {
        Type::Array(_) => true,
        _ => false,
    }
}

fn type_has_string(ty: &Type) -> bool {
    match ty {
        Type::String => true,
        Type::Array(inner) => type_has_string(inner),
        Type::Book(_) => false,
        _ => false,
    }
}

fn stmt_has_array(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref().map_or(false, type_has_array) || expr_has_array(expr)
        }
        Stmt::Put { expr, .. } => expr_has_array(expr),
        Stmt::PutIndex { .. } => true,
        Stmt::PutField { expr, .. } => expr_has_array(expr),
        Stmt::Yield { expr, .. } => expr_has_array(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_array(cond)
                || then_body.iter().any(stmt_has_array)
                || else_body.iter().any(stmt_has_array)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_array(cond) || body.iter().any(stmt_has_array)
        }
    }
}

fn expr_has_array(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::ArrayLit(_) => true,
        ExprKind::ArrayNew { .. } => true,
        ExprKind::Index { base, index } => expr_has_array(base) || expr_has_array(index),
        ExprKind::Call { args, .. } => args.iter().any(expr_has_array),
        ExprKind::New { args, .. } => args.iter().any(expr_has_array),
        ExprKind::Unary { expr, .. } => expr_has_array(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_array(left) || expr_has_array(right)
        }
        _ => false,
    }
}

fn stmt_has_string(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref().map_or(false, type_has_string) || expr_has_string(expr)
        }
        Stmt::Put { expr, .. } => expr_has_string(expr),
        Stmt::PutIndex { index, expr, .. } => {
            expr_has_string(index) || expr_has_string(expr)
        }
        Stmt::PutField { expr, .. } => expr_has_string(expr),
        Stmt::Yield { expr, .. } => expr_has_string(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_string(cond)
                || then_body.iter().any(stmt_has_string)
                || else_body.iter().any(stmt_has_string)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_string(cond) || body.iter().any(stmt_has_string)
        }
    }
}

fn expr_has_string(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::String(_) => true,
        ExprKind::Call { name, args } => {
            name.starts_with("std::string::") || args.iter().any(expr_has_string)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_string),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_string),
        ExprKind::ArrayNew { len } => expr_has_string(len),
        ExprKind::Index { base, index } => expr_has_string(base) || expr_has_string(index),
        ExprKind::Unary { expr, .. } => expr_has_string(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_string(left) || expr_has_string(right)
        }
        _ => false,
    }
}

fn stmt_has_string_from_bytes(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Put { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::PutIndex { index, expr, .. } => {
            expr_has_string_from_bytes(index) || expr_has_string_from_bytes(expr)
        }
        Stmt::PutField { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::Yield { expr, .. } => expr_has_string_from_bytes(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_string_from_bytes(cond)
                || then_body.iter().any(stmt_has_string_from_bytes)
                || else_body.iter().any(stmt_has_string_from_bytes)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_string_from_bytes(cond) || body.iter().any(stmt_has_string_from_bytes)
        }
    }
}

fn expr_has_string_from_bytes(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name == "std::string::from_bytes" || args.iter().any(expr_has_string_from_bytes)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_string_from_bytes),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_string_from_bytes),
        ExprKind::ArrayNew { len } => expr_has_string_from_bytes(len),
        ExprKind::Index { base, index } => {
            expr_has_string_from_bytes(base) || expr_has_string_from_bytes(index)
        }
        ExprKind::Unary { expr, .. } => expr_has_string_from_bytes(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_string_from_bytes(left) || expr_has_string_from_bytes(right)
        }
        _ => false,
    }
}

fn stmt_has_bytes(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_bytes(expr),
        Stmt::Put { expr, .. } => expr_has_bytes(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_bytes(index) || expr_has_bytes(expr),
        Stmt::PutField { expr, .. } => expr_has_bytes(expr),
        Stmt::Yield { expr, .. } => expr_has_bytes(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_bytes(cond)
                || then_body.iter().any(stmt_has_bytes)
                || else_body.iter().any(stmt_has_bytes)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_bytes(cond) || body.iter().any(stmt_has_bytes)
        }
    }
}

fn expr_has_bytes(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::bytes::") || args.iter().any(expr_has_bytes)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_bytes),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_bytes),
        ExprKind::ArrayNew { len } => expr_has_bytes(len),
        ExprKind::Index { base, index } => expr_has_bytes(base) || expr_has_bytes(index),
        ExprKind::Unary { expr, .. } => expr_has_bytes(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_bytes(left) || expr_has_bytes(right)
        }
        _ => false,
    }
}

fn stmt_has_io(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { expr, .. } => expr_has_io(expr),
        Stmt::Put { expr, .. } => expr_has_io(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_io(index) || expr_has_io(expr),
        Stmt::PutField { expr, .. } => expr_has_io(expr),
        Stmt::Yield { expr, .. } => expr_has_io(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_io(cond) || then_body.iter().any(stmt_has_io) || else_body.iter().any(stmt_has_io)
        }
        Stmt::Repeat { cond, body, .. } => expr_has_io(cond) || body.iter().any(stmt_has_io),
    }
}

fn expr_has_io(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            name.starts_with("std::io::") || args.iter().any(expr_has_io)
        }
        ExprKind::New { args, .. } => args.iter().any(expr_has_io),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_io),
        ExprKind::ArrayNew { len } => expr_has_io(len),
        ExprKind::Index { base, index } => expr_has_io(base) || expr_has_io(index),
        ExprKind::Unary { expr, .. } => expr_has_io(expr),
        ExprKind::Binary { left, right, .. } => expr_has_io(left) || expr_has_io(right),
        _ => false,
    }
}

fn stmt_has_object(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref()
                .map(|ty| matches!(ty, Type::Book(_)))
                .unwrap_or(false)
                || expr_has_object(expr)
        }
        Stmt::Put { expr, .. } => expr_has_object(expr),
        Stmt::PutIndex { index, expr, .. } => expr_has_object(index) || expr_has_object(expr),
        Stmt::PutField { .. } => true,
        Stmt::Yield { expr, .. } => expr_has_object(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_object(cond)
                || then_body.iter().any(stmt_has_object)
                || else_body.iter().any(stmt_has_object)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_object(cond) || body.iter().any(stmt_has_object)
        }
    }
}

fn expr_has_object(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::New { .. } => true,
        ExprKind::MemberAccess { .. } => true,
        ExprKind::Call { args, .. } => args.iter().any(expr_has_object),
        ExprKind::ArrayLit(elements) => elements.iter().any(expr_has_object),
        ExprKind::ArrayNew { len } => expr_has_object(len),
        ExprKind::Index { base, index } => expr_has_object(base) || expr_has_object(index),
        ExprKind::Unary { expr, .. } => expr_has_object(expr),
        ExprKind::Binary { left, right, .. } => expr_has_object(left) || expr_has_object(right),
        _ => false,
    }
}

fn emit_heap_runtime(
    emitter: &mut WatEmitter,
    export_memory: bool,
    needs_validate_utf8: bool,
    needs_io: bool,
) {
    emitter.push_line("(import \"env\" \"bd_trap\" (func $bd_trap (param i32)))");
    if needs_validate_utf8 {
        emitter.push_line(
            "(import \"env\" \"bd_validate_utf8\" (func $bd_validate_utf8 (param i32 i32) (result i32)))",
        );
    }
    if needs_io {
        emitter.push_line("(import \"env\" \"bd_print\" (func $bd_print (param i32 i32)))");
        emitter.push_line("(import \"env\" \"bd_read_line_len\" (func $bd_read_line_len (result i32)))");
        emitter.push_line("(import \"env\" \"bd_read_line_fill\" (func $bd_read_line_fill (param i32 i32)))");
    }
    emitter.push_line("(memory $mem 1)");
    if export_memory {
        emitter.push_line("(export \"memory\" (memory $mem))");
    }
    emitter.push_line(format!(
        "(global $heap (mut i32) (i32.const {HEAP_START}))"
    ));
    emitter.push_line("(func $bd_alloc (param $size i32) (result i32) (local $ptr i32) (local $new_heap i32) (local $pages_needed i32) (local $cur_pages i32) (local $grow_by i32)");
    emitter.indent();
    emitter.push_line("global.get $heap");
    emitter.push_line("local.set $ptr");
    emitter.push_line("global.get $heap");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 7");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const -8");
    emitter.push_line("i32.and");
    emitter.push_line("local.set $new_heap");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("i32.const 65535");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 65536");
    emitter.push_line("i32.div_u");
    emitter.push_line("local.set $pages_needed");
    emitter.push_line("memory.size");
    emitter.push_line("local.set $cur_pages");
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $grow_by");
    emitter.push_line("local.get $grow_by");
    emitter.push_line("memory.grow");
    emitter.push_line("i32.const -1");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("global.set $heap");
    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_trace_push (param $id i32) (local $sp i32)");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $sp");
    emitter.push_line("local.get $sp");
    emitter.push_line(format!("i32.const {TRACE_STACK_SLOTS}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_TRACE_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {TRACE_STACK_DATA_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $id");
    emitter.push_line("i32.store");
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_trace_pop (local $sp i32)");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $sp");
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $sp");
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_string_runtime(emitter: &mut WatEmitter, allow_from_bytes: bool) {
    let max_len = i32::MAX as i64 - STRING_HEADER_SIZE as i64;
    let max_bytes_len = i32::MAX - ARRAY_HEADER_SIZE;

    emitter.push_line("(func $bd_string_len (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_concat (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $total i64)");
    emitter.push_line("(local $size i64)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $a_data i32)");
    emitter.push_line("(local $b_data i32)");
    emitter.push_line("(local $dst_data i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $total");

    emitter.push_line("local.get $total");
    emitter.push_line(format!("i64.const {max_len}"));
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $total");
    emitter.push_line(format!("i64.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i64.add");
    emitter.push_line("local.set $size");

    emitter.push_line("local.get $size");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $total");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $dst_data");

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $a_data");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $b_data");

    emitter.push_line("local.get $dst_data");
    emitter.push_line("local.get $a_data");
    emitter.push_line("local.get $len_a");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $dst_data");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $b_data");
    emitter.push_line("local.get $len_b");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_bytes (param $s i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");

    emitter.push_line("local.get $s");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {max_bytes_len}"));
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $s");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    if allow_from_bytes {
        emitter.push_line("(func $bd_string_from_bytes (param $arr i32) (result i32)");
        emitter.indent();
        emitter.push_line("(local $len i32)");
        emitter.push_line("(local $ptr i32)");
        emitter.push_line("(local $src i32)");

        emitter.push_line("local.get $arr");
        emitter.push_line("i32.load");
        emitter.push_line("local.set $len");

        emitter.push_line("local.get $arr");
        emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("local.set $src");

        emitter.push_line("local.get $src");
        emitter.push_line("local.get $len");
        emitter.push_line("call $bd_validate_utf8");
        emitter.push_line("i32.eqz");
        emitter.push_line("if");
        emitter.indent();
        emitter.push_line(format!("i32.const {TRAP_UTF8_INVALID}"));
        emitter.push_line("call $bd_trap");
        emitter.dedent();
        emitter.push_line("end");

        emitter.push_line("local.get $len");
        emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("call $bd_alloc");
        emitter.push_line("local.set $ptr");

        emitter.push_line("local.get $ptr");
        emitter.push_line("local.get $len");
        emitter.push_line("i32.store");

        emitter.push_line("local.get $ptr");
        emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("local.get $src");
        emitter.push_line("local.get $len");
        emitter.push_line("memory.copy");

        emitter.push_line("local.get $ptr");
        emitter.dedent();
        emitter.push_line(")");
    }

    emitter.push_line("(func $bd_string_to_i64 (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $sign i64)");
    emitter.push_line("(local $value i64)");
    emitter.push_line("(local $digit i64)");
    emitter.push_line("(local $ch i32)");
    emitter.push_line("(local $max_div10 i64)");
    emitter.push_line("(local $max_mod i64)");
    emitter.push_line("(local $data i32)");

    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $data");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");
    emitter.push_line("i64.const 1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 922337203685477580");
    emitter.push_line("local.set $max_div10");
    emitter.push_line("i64.const 7");
    emitter.push_line("local.set $max_mod");

    emitter.push_line("local.get $data");
    emitter.push_line("i32.load8_u");
    emitter.push_line("local.set $ch");
    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 45");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const -1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 8");
    emitter.push_line("local.set $max_mod");
    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $idx");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i64.const 0");
    emitter.push_line("local.set $value");

    let exit_label = "str_to_i64_exit";
    let loop_label = "str_to_i64_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");
    emitter.push_line("local.set $ch");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.lt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 57");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.sub");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.set $digit");

    emitter.push_line("local.get $value");
    emitter.push_line("local.get $max_div10");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("local.get $max_div10");
    emitter.push_line("i64.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $digit");
    emitter.push_line("local.get $max_mod");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.mul");
    emitter.push_line("local.get $digit");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $value");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $sign");
    emitter.push_line("i64.const -1");
    emitter.push_line("i64.eq");
    emitter.push_line("if (result i64)");
    emitter.indent();
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $value");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_from_i64 (param $value i64) (result i32)");
    emitter.indent();
    emitter.push_line("(local $tmp i64)");
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $sign i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $digit i64)");
    emitter.push_line("(local $data i32)");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {}", STRING_HEADER_SIZE + 1));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $ptr");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $sign");
    emitter.push_line("local.get $value");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $len");
    let len_exit = "str_i64_len_exit";
    let len_loop = "str_i64_len_loop";
    emitter.push_line(format!("block ${len_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${len_loop}"));
    emitter.indent();
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.eqz");
    emitter.push_line(format!("br_if ${len_exit}"));
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.div_u");
    emitter.push_line("local.set $tmp");
    emitter.push_line(format!("br ${len_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("local.get $sign");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $len");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $data");

    emitter.push_line("local.get $sign");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $data");
    emitter.push_line("i32.const 45");
    emitter.push_line("i32.store8");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $value");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $idx");

    let digit_exit = "str_i64_digit_exit";
    let digit_loop = "str_i64_digit_loop";
    emitter.push_line(format!("block ${digit_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${digit_loop}"));
    emitter.indent();
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.eqz");
    emitter.push_line(format!("br_if ${digit_exit}"));
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.rem_u");
    emitter.push_line("local.set $digit");
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.div_u");
    emitter.push_line("local.set $tmp");
    emitter.push_line("local.get $data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $digit");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.add");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${digit_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_eq (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let exit_label = "str_eq_exit";
    let loop_label = "str_eq_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_bytes_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_bytes_len (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_bytes_eq (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let exit_label = "bytes_eq_exit";
    let loop_label = "bytes_eq_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_io_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_io_print (param $str i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("local.get $str");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $str");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_print");
    emitter.push_line("local.get $len");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_io_read_line (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("call $bd_read_line_len");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_read_line_fill");
    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_function(
    emitter: &mut WatEmitter,
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
    frame_id: i32,
    name_override: Option<&str>,
) -> Result<(), WasmError> {
    let func_name = name_override.unwrap_or(&func.name);
    let mut signature = String::new();
    for param in &func.params {
        signature.push_str(&format!(" (param {})", wat_type(&param.ty)));
    }
    signature.push_str(&format!(" (result {})", wat_type(&func.return_type)));

    emitter.push_line(format!("(func ${}{}", func_name, signature));
    emitter.indent();

    let mut compiler = FuncCompiler::new(func, functions, books, frame_id);
    compiler.emit_body()?;

    for local in &compiler.locals {
        emitter.push_line(format!("(local {})", wat_type(local)));
    }

    for line in &compiler.code {
        emitter.push_line(line);
    }

    emitter.dedent();
    emitter.push_line(")");
    Ok(())
}

fn wat_type(ty: &Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::Bool => "i32",
        Type::String => "i32",
        Type::U8 => "i32",
        Type::Array(_) => "i32",
        Type::Book(_) => "i32",
    }
}

fn array_elem_size(ty: &Type) -> Result<i32, WasmError> {
    match ty {
        Type::I64 => Ok(8),
        Type::Bool => Ok(4),
        Type::String => Ok(4),
        Type::U8 => Ok(1),
        Type::Array(_) => Ok(4),
        Type::Book(_) => Ok(4),
    }
}

struct WatEmitter {
    lines: Vec<String>,
    indent: usize,
}

impl WatEmitter {
    fn new() -> Self {
        Self {
            lines: Vec::new(),
            indent: 0,
        }
    }

    fn push_line(&mut self, line: impl AsRef<str>) {
        let prefix = "  ".repeat(self.indent);
        self.lines.push(format!("{prefix}{}", line.as_ref()));
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    fn finish(self) -> String {
        self.lines.join("\n")
    }
}

#[derive(Clone)]
struct LocalInfo {
    idx: u32,
    ty: Type,
}

struct FuncCompiler<'a> {
    func: &'a Function,
    functions: &'a HashMap<String, FunctionSig>,
    books: &'a HashMap<String, BookLayout>,
    frame_id: i32,
    locals: Vec<Type>,
    scopes: Vec<HashMap<String, LocalInfo>>,
    code: Vec<String>,
    indent: usize,
    label_counter: usize,
}

impl<'a> FuncCompiler<'a> {
    fn new(
        func: &'a Function,
        functions: &'a HashMap<String, FunctionSig>,
        books: &'a HashMap<String, BookLayout>,
        frame_id: i32,
    ) -> Self {
        let mut compiler = Self {
            func,
            functions,
            books,
            frame_id,
            locals: Vec::new(),
            scopes: Vec::new(),
            code: Vec::new(),
            indent: 0,
            label_counter: 0,
        };
        compiler.push_scope();
        for (idx, param) in func.params.iter().enumerate() {
            compiler.current_scope_mut().insert(
                param.name.clone(),
                LocalInfo {
                    idx: idx as u32,
                    ty: param.ty.clone(),
                },
            );
        }
        compiler
    }

    fn emit_body(&mut self) -> Result<(), WasmError> {
        self.emit_trace_push();
        for stmt in &self.func.body {
            self.emit_stmt(stmt)?;
        }
        self.emit_default_return();
        Ok(())
    }

    fn emit_trace_push(&mut self) {
        self.push_line(format!("i32.const {}", self.frame_id));
        self.push_line("call $bd_trace_push");
    }

    fn emit_trace_pop(&mut self) {
        self.push_line("call $bd_trace_pop");
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), WasmError> {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                let inferred = match ty {
                    Some(ty) => ty.clone(),
                    None => self.infer_expr_type(expr)?,
                };
                let idx = self.reserve_local(inferred.clone());
                self.emit_expr(expr, Some(&inferred))?;
                self.push_line(format!("local.set {idx}"));
                self.bind_local(name, idx, inferred);
            }
            Stmt::Put { name, expr, .. } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.emit_expr(expr, Some(&info.ty))?;
                self.push_line(format!("local.set {}", info.idx));
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                let elem_ty = match &info.ty {
                    Type::Array(elem) => elem.as_ref().clone(),
                    _ => {
                        return Err(wasm_error(
                            "E0400",
                            "Index assignment requires array type.",
                        ))
                    }
                };
                let idx_local = self.temp_local(Type::I64);
                self.emit_expr(index, None)?;
                self.push_line(format!("local.set {idx_local}"));
                let val_local = self.temp_local(elem_ty.clone());
                self.emit_expr(expr, Some(&elem_ty))?;
                self.push_line(format!("local.set {val_local}"));
                self.emit_bounds_check(info.idx, idx_local)?;
                self.emit_array_address_index(info.idx, idx_local, array_elem_size(&elem_ty)?);
                self.push_line(format!("local.get {val_local}"));
                self.emit_store(&elem_ty);
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let info = self
                    .lookup(base)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = &info.ty else {
                    return Err(wasm_error("E0400", "Field assignment requires book type."));
                };
                let layout = self.books.get(book_name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book_name}'"))
                })?;
                let Some(index) = layout.field_index.get(field) else {
                    return Err(wasm_error(
                        "E0400",
                        format!("Unknown field '{field}' on '{book_name}'"),
                    ));
                };
                let field_ty = layout.fields.get(*index).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown field '{field}' on '{book_name}'"))
                })?;
                let val_local = self.temp_local(field_ty.clone());
                self.emit_expr(expr, Some(field_ty))?;
                self.push_line(format!("local.set {val_local}"));
                self.emit_field_address(info.idx, *index);
                self.push_line(format!("local.get {val_local}"));
                self.emit_field_store(field_ty);
            }
            Stmt::Yield { expr, .. } => {
                let ret_local = self.temp_local(self.func.return_type.clone());
                self.emit_expr(expr, Some(&self.func.return_type))?;
                self.push_line(format!("local.set {ret_local}"));
                self.emit_trace_pop();
                self.push_line(format!("local.get {ret_local}"));
                self.push_line("return");
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                self.emit_expr(cond, Some(&Type::Bool))?;
                self.push_line("if");
                self.indent += 1;
                self.push_scope();
                for stmt in then_body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.indent -= 1;
                self.push_line("else");
                self.indent += 1;
                self.push_scope();
                for stmt in else_body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.indent -= 1;
                self.push_line("end");
            }
            Stmt::Repeat { cond, body, .. } => {
                let exit_label = self.fresh_label("exit");
                let loop_label = self.fresh_label("loop");
                self.push_line(format!("block ${exit_label}"));
                self.indent += 1;
                self.push_line(format!("loop ${loop_label}"));
                self.indent += 1;
                self.emit_expr(cond, Some(&Type::Bool))?;
                self.push_line("i32.eqz");
                self.push_line(format!("br_if ${exit_label}"));
                self.push_scope();
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.push_line(format!("br ${loop_label}"));
                self.indent -= 1;
                self.push_line("end");
                self.indent -= 1;
                self.push_line("end");
            }
        }
        Ok(())
    }

    fn emit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<(), WasmError> {
        match &expr.kind {
            ExprKind::Int(value) => {
                if matches!(expected, Some(Type::U8)) {
                    if !(0..=u8::MAX as i64).contains(value) {
                        return Err(wasm_error("E0400", "u8 literal out of range."));
                    }
                    self.push_line(format!("i32.const {value}"));
                } else {
                    self.push_line(format!("i64.const {value}"));
                }
            }
            ExprKind::Bool(value) => {
                let bit = if *value { 1 } else { 0 };
                self.push_line(format!("i32.const {bit}"));
            }
            ExprKind::String(value) => {
                self.emit_string_literal(value)?;
            }
            ExprKind::Ident(name) => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.push_line(format!("local.get {}", info.idx));
            }
            ExprKind::Call { name, args } => {
                if let Some((base, method)) = name.split_once("::") {
                    if let Some(info) = self.lookup(base) {
                        if let Type::Book(book_name) = &info.ty {
                            let full_name = format!("{book_name}::{method}");
                            let sig = self.functions.get(&full_name).ok_or_else(|| {
                                wasm_error("E0400", format!("Unknown function '{full_name}'"))
                            })?;
                            if sig.params.len() < 1 {
                                return Err(wasm_error(
                                    "E0400",
                                    format!("Method '{full_name}' must take self."),
                                ));
                            }
                            if sig.params.len() - 1 != args.len() {
                                return Err(wasm_error(
                                    "E0400",
                                    format!(
                                        "Wrong number of arguments for '{full_name}': expected {}, got {}.",
                                        sig.params.len() - 1,
                                        args.len()
                                    ),
                                ));
                            }
                            self.push_line(format!("local.get {}", info.idx));
                            for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
                                self.emit_expr(arg, Some(param_ty))?;
                            }
                            self.push_line(format!("call ${full_name}"));
                            return Ok(());
                        }
                    }
                }
                if self.emit_string_call(name, args)? {
                    return Ok(());
                }
                if self.emit_bytes_call(name, args)? {
                    return Ok(());
                }
                if self.emit_io_call(name, args)? {
                    return Ok(());
                }
                let sig = self.functions.get(name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown function '{name}'"))
                })?;
                if sig.params.len() != args.len() {
                    return Err(wasm_error(
                        "E0400",
                        format!(
                            "Wrong number of arguments for '{name}': expected {}, got {}.",
                            sig.params.len(),
                            args.len()
                        ),
                    ));
                }
                for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                    self.emit_expr(arg, Some(param_ty))?;
                }
                self.push_line(format!("call ${name}"));
            }
            ExprKind::New { book, args } => {
                let layout = self.books.get(book).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book}'"))
                })?;
                let ptr_local = self.temp_local(Type::Book(book.clone()));
                let size = (layout.fields.len() as i32).saturating_mul(OBJECT_FIELD_SIZE);
                self.push_line(format!("i32.const {size}"));
                self.push_line("call $bd_alloc");
                self.push_line(format!("local.set {ptr_local}"));
                for (index, field_ty) in layout.fields.iter().enumerate() {
                    self.emit_field_address(ptr_local, index);
                    self.emit_default_value(field_ty)?;
                    self.emit_field_store(field_ty);
                }
                if let Some(sig) = self.functions.get(&format!("{book}::init")) {
                    if sig.params.len() < 1 {
                        return Err(wasm_error(
                            "E0400",
                            format!("Method '{book}::init' must take self."),
                        ));
                    }
                    if sig.params.len() - 1 != args.len() {
                        return Err(wasm_error(
                            "E0400",
                            format!(
                                "Wrong number of arguments for '{book}::init': expected {}, got {}.",
                                sig.params.len() - 1,
                                args.len()
                            ),
                        ));
                    }
                    self.push_line(format!("local.get {ptr_local}"));
                    for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
                        self.emit_expr(arg, Some(param_ty))?;
                    }
                    self.push_line(format!("call ${book}::init"));
                    self.push_line(format!("local.set {ptr_local}"));
                } else if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        format!("Missing constructor '{book}::init'."),
                    ));
                }
                self.push_line(format!("local.get {ptr_local}"));
            }
            ExprKind::MemberAccess { base, field } => {
                let info = self
                    .lookup(base)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = &info.ty else {
                    return Err(wasm_error("E0400", "Field access requires book type."));
                };
                let layout = self.books.get(book_name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book_name}'"))
                })?;
                let Some(index) = layout.field_index.get(field) else {
                    return Err(wasm_error(
                        "E0400",
                        format!("Unknown field '{field}' on '{book_name}'"),
                    ));
                };
                let field_ty = layout.fields.get(*index).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown field '{field}' on '{book_name}'"))
                })?;
                self.emit_field_address(info.idx, *index);
                self.emit_field_load(field_ty);
            }
            ExprKind::ArrayLit(elements) => self.emit_array_literal(elements, expected)?,
            ExprKind::ArrayNew { len } => self.emit_array_new(len, expected)?,
            ExprKind::Index { base, index } => self.emit_index_expr(base, index)?,
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    self.push_line("i64.const 0");
                    self.emit_expr(expr, None)?;
                    self.push_line("i64.sub");
                }
                UnaryOp::Not => {
                    self.emit_expr(expr, None)?;
                    self.push_line("i32.eqz");
                }
            },
            ExprKind::Binary { left, op, right } => {
                self.emit_expr(left, None)?;
                self.emit_expr(right, None)?;
                let instr = match op {
                    BinaryOp::Add => "i64.add",
                    BinaryOp::Sub => "i64.sub",
                    BinaryOp::Mul => "i64.mul",
                    BinaryOp::Div => "i64.div_s",
                    BinaryOp::Mod => "i64.rem_s",
                    BinaryOp::EqEq => "i64.eq",
                    BinaryOp::NotEq => "i64.ne",
                    BinaryOp::Lt => "i64.lt_s",
                    BinaryOp::LtEq => "i64.le_s",
                    BinaryOp::Gt => "i64.gt_s",
                    BinaryOp::GtEq => "i64.ge_s",
                    BinaryOp::AndAnd => "i32.and",
                    BinaryOp::OrOr => "i32.or",
                };
                self.push_line(instr);
            }
        }
        Ok(())
    }

    fn emit_array_literal(
        &mut self,
        elements: &[Expr],
        expected: Option<&Type>,
    ) -> Result<(), WasmError> {
        let expected_elem = match expected {
            Some(Type::Array(elem)) => Some(elem.as_ref().clone()),
            _ => None,
        };
        let elem_ty = if elements.is_empty() {
            match expected_elem {
                Some(elem) => elem,
                None => {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal requires explicit array type.",
                    ))
                }
            }
        } else if let Some(elem) = expected_elem.clone() {
            for element in elements {
                let actual = match (&element.kind, &elem) {
                    (ExprKind::Int(_), Type::U8) => Type::U8,
                    _ => self.infer_expr_type(element)?,
                };
                if actual != elem {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal elements must have the same type.",
                    ));
                }
            }
            elem
        } else {
            let first_ty = self.infer_expr_type(&elements[0])?;
            for elem in elements.iter().skip(1) {
                let ty = self.infer_expr_type(elem)?;
                if ty != first_ty {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal elements must have the same type.",
                    ));
                }
            }
            first_ty
        };

        let elem_size = array_elem_size(&elem_ty)?;
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        let byte_size = ARRAY_HEADER_SIZE + (elements.len() as i32 * elem_size);

        self.push_line(format!("i32.const {byte_size}"));
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", elements.len()));
        self.push_line("i32.store");

        for (idx, elem) in elements.iter().enumerate() {
            self.emit_array_address_const(ptr_local, idx as i64, elem_size);
            self.emit_expr(elem, Some(&elem_ty))?;
            self.emit_store(&elem_ty);
        }

        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_array_new(&mut self, len: &Expr, expected: Option<&Type>) -> Result<(), WasmError> {
        let elem_ty = match expected {
            Some(Type::Array(elem)) => elem.as_ref().clone(),
            _ => {
                return Err(wasm_error(
                    "E0400",
                    "Array constructor requires explicit array type.",
                ))
            }
        };
        let elem_size = array_elem_size(&elem_ty)?;
        let len_local = self.temp_local(Type::I64);
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));

        self.emit_expr(len, None)?;
        self.push_line(format!("local.set {len_local}"));
        self.emit_len_non_negative_check(len_local)?;
        self.emit_len_max_check(len_local, elem_size)?;

        self.push_line(format!("local.get {len_local}"));
        self.push_line(format!("i64.const {elem_size}"));
        self.push_line("i64.mul");
        self.push_line(format!("i64.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i64.add");
        self.push_line("i32.wrap_i64");
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));

        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i32.wrap_i64");
        self.push_line("i32.store");

        self.emit_array_init(ptr_local, len_local, &elem_ty, elem_size)?;
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<(), WasmError> {
        let base_ty = self.infer_expr_type(base)?;
        let elem_ty = match base_ty {
            Type::Array(elem) => elem.as_ref().clone(),
            _ => return Err(wasm_error("E0400", "Indexing requires array type.")),
        };
        let idx_local = self.temp_local(Type::I64);
        self.emit_expr(index, None)?;
        self.push_line(format!("local.set {idx_local}"));
        let base_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        self.emit_expr(base, None)?;
        self.push_line(format!("local.set {base_local}"));
        self.emit_bounds_check(base_local, idx_local)?;
        self.emit_array_address_index(base_local, idx_local, array_elem_size(&elem_ty)?);
        self.emit_load(&elem_ty);
        Ok(())
    }

    fn emit_string_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::string::len" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::len expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.push_line("call $bd_string_len");
                Ok(true)
            }
            "std::string::concat" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::concat expects 2 arguments",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.emit_expr(&args[1], Some(&Type::String))?;
                self.push_line("call $bd_string_concat");
                Ok(true)
            }
            "std::string::eq" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::eq expects 2 arguments",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.emit_expr(&args[1], Some(&Type::String))?;
                self.push_line("call $bd_string_eq");
                Ok(true)
            }
            "std::string::bytes" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::bytes expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.push_line("call $bd_string_bytes");
                Ok(true)
            }
            "std::string::from_bytes" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::from_bytes expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::Array(Box::new(Type::U8))))?;
                self.push_line("call $bd_string_from_bytes");
                Ok(true)
            }
            "std::string::to_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::to_i64 expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.push_line("call $bd_string_to_i64");
                Ok(true)
            }
            "std::string::from_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::from_i64 expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::I64))?;
                self.push_line("call $bd_string_from_i64");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_bytes_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::bytes::len" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::len expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::Array(Box::new(Type::U8))))?;
                self.push_line("call $bd_bytes_len");
                Ok(true)
            }
            "std::bytes::eq" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::eq expects 2 arguments",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::Array(Box::new(Type::U8))))?;
                self.emit_expr(&args[1], Some(&Type::Array(Box::new(Type::U8))))?;
                self.push_line("call $bd_bytes_eq");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_io_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::io::print" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::io::print expects 1 argument",
                    ));
                }
                self.emit_expr(&args[0], Some(&Type::String))?;
                self.push_line("call $bd_io_print");
                Ok(true)
            }
            "std::io::read_line" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "std::io::read_line expects 0 arguments",
                    ));
                }
                self.push_line("call $bd_io_read_line");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_string_literal(&mut self, value: &str) -> Result<(), WasmError> {
        let bytes = value.as_bytes();
        let len = i32::try_from(bytes.len()).map_err(|_| {
            wasm_error("E0400", "String literal is too large to encode.")
        })?;
        let byte_size = STRING_HEADER_SIZE + len;
        let ptr_local = self.temp_local(Type::String);

        self.push_line(format!("i32.const {byte_size}"));
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));

        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {len}"));
        self.push_line("i32.store");

        for (idx, byte) in bytes.iter().enumerate() {
            let offset = STRING_HEADER_SIZE + idx as i32;
            self.push_line(format!("local.get {ptr_local}"));
            self.push_line(format!("i32.const {offset}"));
            self.push_line("i32.add");
            self.push_line(format!("i32.const {byte}"));
            self.push_line("i32.store8");
        }

        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_default_return(&mut self) {
        let ret_local = self.temp_local(self.func.return_type.clone());
        match &self.func.return_type {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool | Type::String | Type::U8 | Type::Array(_) | Type::Book(_) => {
                self.push_line("i32.const 0")
            }
        }
        self.push_line(format!("local.set {ret_local}"));
        self.emit_trace_pop();
        self.push_line(format!("local.get {ret_local}"));
        self.push_line("return");
    }

    fn emit_len_non_negative_check(&mut self, len_local: u32) -> Result<(), WasmError> {
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.const 0");
        self.push_line("i64.lt_s");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_LEN_NEG);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    fn emit_len_max_check(&mut self, len_local: u32, elem_size: i32) -> Result<(), WasmError> {
        let max_len = (i32::MAX as i64 - ARRAY_HEADER_SIZE as i64) / elem_size as i64;
        self.push_line(format!("local.get {len_local}"));
        self.push_line(format!("i64.const {max_len}"));
        self.push_line("i64.gt_u");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOM);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    fn emit_bounds_check(&mut self, base_local: u32, idx_local: u32) -> Result<(), WasmError> {
        let len_local = self.temp_local(Type::I64);
        self.push_line(format!("local.get {base_local}"));
        self.push_line("i32.load");
        self.push_line("i64.extend_i32_u");
        self.push_line(format!("local.set {len_local}"));

        self.push_line(format!("local.get {idx_local}"));
        self.push_line("i64.const 0");
        self.push_line("i64.lt_s");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOB);
        self.indent -= 1;
        self.push_line("end");

        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.ge_u");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOB);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    fn emit_array_address_const(&mut self, base_local: u32, index: i64, elem_size: i32) {
        let offset = (index * elem_size as i64) as i32;
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("i32.const {offset}"));
        self.push_line("i32.add");
    }

    fn emit_array_address_index(&mut self, base_local: u32, idx_local: u32, elem_size: i32) {
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("i64.const {elem_size}"));
        self.push_line("i64.mul");
        self.push_line("i32.wrap_i64");
        self.push_line("i32.add");
    }

    fn emit_field_address(&mut self, base_local: u32, index: usize) {
        let offset = (index as i32).saturating_mul(OBJECT_FIELD_SIZE);
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {offset}"));
        self.push_line("i32.add");
    }

    fn emit_array_init(
        &mut self,
        base_local: u32,
        len_local: u32,
        elem_ty: &Type,
        elem_size: i32,
    ) -> Result<(), WasmError> {
        let idx_local = self.temp_local(Type::I64);
        self.push_line(format!("i64.const 0"));
        self.push_line(format!("local.set {idx_local}"));
        let exit_label = self.fresh_label("arr_init_exit");
        let loop_label = self.fresh_label("arr_init_loop");
        self.push_line(format!("block ${exit_label}"));
        self.indent += 1;
        self.push_line(format!("loop ${loop_label}"));
        self.indent += 1;
        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.ge_u");
        self.push_line(format!("br_if ${exit_label}"));
        self.emit_array_address_index(base_local, idx_local, elem_size);
        self.emit_default_value(elem_ty)?;
        self.emit_store(elem_ty);
        self.push_line(format!("local.get {idx_local}"));
        self.push_line("i64.const 1");
        self.push_line("i64.add");
        self.push_line(format!("local.set {idx_local}"));
        self.push_line(format!("br ${loop_label}"));
        self.indent -= 1;
        self.push_line("end");
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    fn emit_store(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.store"),
            Type::U8 => self.push_line("i32.store8"),
            Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => self.push_line("i32.store"),
        }
    }

    fn emit_field_store(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.store"),
            Type::U8 | Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => {
                self.push_line("i64.extend_i32_u");
                self.push_line("i64.store");
            }
        }
    }

    fn emit_load(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.load"),
            Type::U8 => self.push_line("i32.load8_u"),
            Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => self.push_line("i32.load"),
        }
    }

    fn emit_field_load(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.load"),
            Type::U8 | Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => {
                self.push_line("i64.load");
                self.push_line("i32.wrap_i64");
            }
        }
    }

    fn emit_trap(&mut self, code: i32) {
        self.push_line(format!("i32.const {code}"));
        self.push_line("call $bd_trap");
    }

    fn emit_default_value(&mut self, ty: &Type) -> Result<(), WasmError> {
        match ty {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool => self.push_line("i32.const 0"),
            Type::String => {
                self.emit_empty_string()?;
            }
            Type::U8 => {
                self.push_line("i32.const 0");
            }
            Type::Array(elem) => {
                self.emit_empty_array(elem.as_ref())?;
            }
            Type::Book(_) => {
                self.push_line("i32.const 0");
            }
        }
        Ok(())
    }

    fn emit_empty_array(&mut self, elem_ty: &Type) -> Result<(), WasmError> {
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_empty_string(&mut self) -> Result<(), WasmError> {
        let ptr_local = self.temp_local(Type::String);
        self.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn temp_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn infer_expr_type(&self, expr: &Expr) -> Result<Type, WasmError> {
        match &expr.kind {
            ExprKind::Int(_) => Ok(Type::I64),
            ExprKind::Bool(_) => Ok(Type::Bool),
            ExprKind::String(_) => Ok(Type::String),
            ExprKind::Ident(name) => self
                .lookup(name)
                .map(|info| info.ty)
                .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'"))),
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal requires explicit array type.",
                    ));
                }
                let first_ty = self.infer_expr_type(&elements[0])?;
                for element in elements.iter().skip(1) {
                    let ty = self.infer_expr_type(element)?;
                    if ty != first_ty {
                        return Err(wasm_error(
                            "E0400",
                            "Array literal elements must have the same type.",
                        ));
                    }
                }
                Ok(Type::Array(Box::new(first_ty)))
            }
            ExprKind::ArrayNew { .. } => Err(wasm_error(
                "E0400",
                "Array constructor requires explicit array type.",
            )),
            ExprKind::Index { base, .. } => {
                let base_ty = self.infer_expr_type(base)?;
                match base_ty {
                    Type::Array(elem) => Ok(elem.as_ref().clone()),
                    _ => Err(wasm_error("E0400", "Indexing requires array type.")),
                }
            }
            ExprKind::Call { name, .. } => {
                if let Some((base, method)) = name.split_once("::") {
                    if let Some(info) = self.lookup(base) {
                        if let Type::Book(book_name) = &info.ty {
                            let full_name = format!("{book_name}::{method}");
                            return self.infer_call_type(&full_name).ok_or_else(|| {
                                wasm_error("E0400", format!("Unknown function '{full_name}'"))
                            });
                        }
                    }
                }
                self.infer_call_type(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown function '{name}'")))
            }
            ExprKind::New { book, .. } => Ok(Type::Book(book.clone())),
            ExprKind::MemberAccess { base, field } => {
                let base_ty = self
                    .lookup(base)
                    .map(|info| info.ty)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = base_ty else {
                    return Err(wasm_error("E0400", "Field access requires book type."));
                };
                let layout = self.books.get(&book_name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book_name}'"))
                })?;
                let Some(index) = layout.field_index.get(field) else {
                    return Err(wasm_error(
                        "E0400",
                        format!("Unknown field '{field}' on '{book_name}'"),
                    ));
                };
                let field_ty = layout.fields.get(*index).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown field '{field}' on '{book_name}'"))
                })?;
                Ok(field_ty.clone())
            }
            ExprKind::Unary { op, .. } => match op {
                UnaryOp::Neg => Ok(Type::I64),
                UnaryOp::Not => Ok(Type::Bool),
            },
            ExprKind::Binary { op, .. } => match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod => Ok(Type::I64),
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::AndAnd
                | BinaryOp::OrOr => Ok(Type::Bool),
            },
        }
    }

    fn infer_call_type(&self, name: &str) -> Option<Type> {
        match name {
            "std::string::len" => Some(Type::I64),
            "std::string::concat" => Some(Type::String),
            "std::string::eq" => Some(Type::Bool),
            "std::string::bytes" => Some(Type::Array(Box::new(Type::U8))),
            "std::string::from_bytes" => Some(Type::String),
            "std::string::to_i64" => Some(Type::I64),
            "std::string::from_i64" => Some(Type::String),
            "std::bytes::len" => Some(Type::I64),
            "std::bytes::eq" => Some(Type::Bool),
            "std::io::print" => Some(Type::I64),
            "std::io::read_line" => Some(Type::String),
            _ => self.functions.get(name).map(|sig| sig.return_type.clone()),
        }
    }

    fn reserve_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn bind_local(&mut self, name: &str, idx: u32, ty: Type) {
        self.current_scope_mut().insert(
            name.to_string(),
            LocalInfo {
                idx,
                ty,
            },
        );
    }

    fn lookup(&self, name: &str) -> Option<LocalInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info.clone());
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, LocalInfo> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn push_line(&mut self, line: impl AsRef<str>) {
        let prefix = "  ".repeat(self.indent);
        self.code.push(format!("{prefix}{}", line.as_ref()));
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{prefix}{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser};

    fn compile_and_run(source: &str) -> Result<i64, WasmError> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        run(&program)
    }

    #[test]
    fn wasm_runs_minimal_main() {
        let result = compile_and_run("rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        assert_eq!(result, 0);
    }

    #[test]
    fn wasm_emits_binary() {
        let tokens = lexer::lex("rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let bytes = emit_wasm(&program).unwrap();
        assert!(!bytes.is_empty());
    }

    #[test]
    fn wasm_runs_loop_and_call() {
        let result = compile_and_run(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  set i: i64 = 0.\n  set sum: i64 = 0.\n  repeat while i < 4:\n    put sum = add(sum, i).\n    put i = i + 1.\n  end\n  yield sum.\nend\n",
        )
        .unwrap();
        assert_eq!(result, 6);
    }

    #[test]
    fn wasm_runs_when_otherwise() {
        let result = compile_and_run(
            "rule main() -> i64:\n  when true:\n    yield 2.\n  otherwise:\n    yield 3.\n  end\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_reports_div_by_zero() {
        let err = compile_and_run("rule main() -> i64:\n  yield 1 / 0.\nend\n").unwrap_err();
        assert_eq!(err.code, "E0402");
    }

    #[test]
    fn wasm_trace_includes_call_stack() {
        let err = compile_and_run(
            "rule boom() -> i64:\n  yield 1 / 0.\nend\n\nrule main() -> i64:\n  yield boom().\nend\n",
        )
        .unwrap_err();
        assert_eq!(err.code, "E0402");
        assert!(err.trace.len() >= 2);
        assert_eq!(err.trace[0].function, "boom");
        assert_eq!(err.trace[1].function, "main");
    }

    #[test]
    fn wasm_runs_string_ops() {
        let result = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  set left: string = \"hi\".\n  set right: string = \"!\".\n  set out: string = std::string::concat(left, right).\n  yield std::string::len(out).\nend\n",
        )
        .unwrap();
        assert_eq!(result, 3);
    }

    #[test]
    fn wasm_runs_bytes_ops() {
        let result = compile_and_run(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set data: u8[] = std::string::bytes(\"hi\").\n  yield std::bytes::len(data).\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_runs_u8_array_literal() {
        let result = compile_and_run(
            "import std::bytes.\nrule main() -> i64:\n  set data: u8[] = [1, 2, 3].\n  yield std::bytes::len(data).\nend\n",
        )
        .unwrap();
        assert_eq!(result, 3);
    }

    #[test]
    fn wasm_runs_string_from_bytes() {
        let result = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  set data: u8[] = [104, 105].\n  set text: string = std::string::from_bytes(data).\n  yield std::string::len(text).\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_runs_string_to_i64() {
        let result = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"456\").\nend\n",
        )
        .unwrap();
        assert_eq!(result, 456);
    }

    #[test]
    fn wasm_runs_string_from_i64() {
        let result = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  set text: string = std::string::from_i64(99).\n  yield std::string::len(text).\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_rejects_string_to_i64_invalid() {
        let err = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"4x\").\nend\n",
        )
        .unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn wasm_rejects_invalid_utf8_from_bytes() {
        let err = compile_and_run(
            "import std::string.\nrule main() -> i64:\n  set data: u8[] = [195, 40].\n  set text: string = std::string::from_bytes(data).\n  yield std::string::len(text).\nend\n",
        )
        .unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn wasm_runs_io_print_and_read_line() {
        let tokens = lexer::lex(
            "import std::io.\nimport std::string.\nrule main() -> i64:\n  set line: string = std::io::read_line().\n  set out: string = std::string::concat(line, \"!\").\n  yield std::io::print(out).\nend\n",
        )
        .unwrap();
        let program = parser::parse(&tokens).unwrap();
        let (result, output) = run_with_io(&program, "BirdDisk").unwrap();
        assert_eq!(result, 9);
        assert_eq!(output, "BirdDisk!");
    }
}
