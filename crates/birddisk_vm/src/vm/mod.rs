mod arrays;
mod casts;
mod enums;
mod eval;
mod io;
mod objects;

use crate::heap::{ElemKind, Heap, HeapHandle, HeapKind, HeapLayout, RootStack, RootValue};
use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::{coerce_value, value_type, Value};
use birddisk_core::ast::{Program, Type};
use birddisk_core::TraceFrame;
use std::collections::{HashMap, VecDeque};
use std::net::{TcpListener, TcpStream};
use std::thread::JoinHandle;
use std::time::Instant;

const RAND_SEED_DEFAULT: u64 = 0x9E37_79B9_7F4A_7C15;
const RAND_MULT: u64 = 0x2545_F491_4F6C_DD1D;

#[derive(Clone, Copy, Default)]
pub struct VmOptions {
    pub deterministic: bool,
}

pub fn eval(program: &Program) -> Result<i64, RuntimeError> {
    let (result, _) = eval_with_io(program, "", &[])?;
    Ok(result)
}

pub fn eval_with_io(
    program: &Program,
    input: &str,
    args: &[String],
) -> Result<(i64, String), RuntimeError> {
    eval_with_io_options(program, input, args, VmOptions::default())
}

pub fn eval_with_io_options(
    program: &Program,
    input: &str,
    args: &[String],
    options: VmOptions,
) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input, args, options);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

pub fn eval_with_io_streaming(
    program: &Program,
    input: &str,
    args: &[String],
    stdin_fallback: bool,
) -> Result<(i64, String), RuntimeError> {
    eval_with_io_streaming_options(program, input, args, stdin_fallback, VmOptions::default())
}

pub fn eval_with_io_streaming_options(
    program: &Program,
    input: &str,
    args: &[String],
    stdin_fallback: bool,
    options: VmOptions,
) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input, args, options);
    vm.set_stdout_live(true);
    vm.set_stdin_fallback(stdin_fallback);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

pub(crate) struct Vm<'a> {
    program: &'a Program,
    functions: HashMap<String, &'a birddisk_core::ast::Function>,
    books: HashMap<String, BookInfo>,
    enums: HashMap<String, EnumInfo>,
    scopes: Vec<Scope>,
    args: Vec<String>,
    input: VecDeque<String>,
    output: String,
    trace: Vec<TraceFrame>,
    start_time: Instant,
    heap: Heap,
    roots: RootStack,
    channels: HashMap<u32, ChannelState>,
    threads: HashMap<u32, ThreadState>,
    pending_threads: VecDeque<PendingThreadJob>,
    net_streams: HashMap<u32, TcpStream>,
    net_listeners: HashMap<u32, TcpListener>,
    net_pools: HashMap<u32, TcpPoolState>,
    gc_layout: GcLayout,
    gc_threshold: usize,
    stdin_fallback: bool,
    stdout_live: bool,
    rng_state: u64,
    deterministic: bool,
    virtual_time_ms: i64,
}

pub(crate) struct BookInfo {
    id: u32,
    field_types: Vec<Type>,
    field_index: HashMap<String, usize>,
}

#[derive(Clone)]
pub(crate) struct EnumInfo {
    id: u32,
    variants: HashMap<String, EnumVariantInfo>,
}

#[derive(Clone)]
pub(crate) struct EnumVariantInfo {
    id: u32,
    payload: Option<Type>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ChannelKind {
    I64,
    Bool,
    F64,
    U8,
    String,
    Bytes,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ChannelValue {
    I64(i64),
    Bool(bool),
    F64(f64),
    U8(u8),
    Ref(HeapHandle),
}

pub(crate) struct ChannelState {
    pub(crate) kind: ChannelKind,
    pub(crate) queue: VecDeque<ChannelValue>,
    pub(crate) closed: bool,
}

impl ChannelState {
    fn new(kind: ChannelKind) -> Self {
        Self {
            kind,
            queue: VecDeque::new(),
            closed: false,
        }
    }
}

pub(crate) enum ThreadStatus {
    Running,
    RunningHost(JoinHandle<Result<i64, RuntimeError>>),
    Completed(i64),
    Joined,
}

pub(crate) struct ThreadState {
    pub(crate) status: ThreadStatus,
}

impl ThreadState {
    fn running() -> Self {
        Self {
            status: ThreadStatus::Running,
        }
    }

    fn completed(result: i64) -> Self {
        Self {
            status: ThreadStatus::Completed(result),
        }
    }

    fn running_host(handle: JoinHandle<Result<i64, RuntimeError>>) -> Self {
        Self {
            status: ThreadStatus::RunningHost(handle),
        }
    }
}

pub(crate) struct PendingThreadJob {
    pub(crate) handle: HeapHandle,
    pub(crate) entry_name: String,
    pub(crate) args: Vec<Value>,
}

pub(crate) struct TcpPoolState {
    pub(crate) addr: String,
    pub(crate) max_idle: usize,
    pub(crate) idle: Vec<TcpStream>,
}

impl ChannelKind {
    pub(crate) fn from_book(book: &str) -> Option<Self> {
        match book {
            "ChannelI64" => Some(ChannelKind::I64),
            "ChannelBool" => Some(ChannelKind::Bool),
            "ChannelF64" => Some(ChannelKind::F64),
            "ChannelU8" => Some(ChannelKind::U8),
            "ChannelString" => Some(ChannelKind::String),
            "ChannelBytes" => Some(ChannelKind::Bytes),
            _ => None,
        }
    }

    pub(crate) fn from_ctor(name: &str) -> Option<Self> {
        match name {
            "std::channel::i64" => Some(ChannelKind::I64),
            "std::channel::bool" => Some(ChannelKind::Bool),
            "std::channel::f64" => Some(ChannelKind::F64),
            "std::channel::u8" => Some(ChannelKind::U8),
            "std::channel::string" => Some(ChannelKind::String),
            "std::channel::bytes" => Some(ChannelKind::Bytes),
            _ => None,
        }
    }

    pub(crate) fn book_name(self) -> &'static str {
        match self {
            ChannelKind::I64 => "ChannelI64",
            ChannelKind::Bool => "ChannelBool",
            ChannelKind::F64 => "ChannelF64",
            ChannelKind::U8 => "ChannelU8",
            ChannelKind::String => "ChannelString",
            ChannelKind::Bytes => "ChannelBytes",
        }
    }

    pub(crate) fn recv_name(self) -> &'static str {
        match self {
            ChannelKind::I64 => "RecvI64",
            ChannelKind::Bool => "RecvBool",
            ChannelKind::F64 => "RecvF64",
            ChannelKind::U8 => "RecvU8",
            ChannelKind::String => "RecvString",
            ChannelKind::Bytes => "RecvBytes",
        }
    }

    pub(crate) fn payload_type(self) -> Type {
        match self {
            ChannelKind::I64 => Type::I64,
            ChannelKind::Bool => Type::Bool,
            ChannelKind::F64 => Type::F64,
            ChannelKind::U8 => Type::U8,
            ChannelKind::String => Type::String,
            ChannelKind::Bytes => Type::Array(Box::new(Type::U8)),
        }
    }
}

#[derive(Debug)]
struct GcLayout {
    ref_fields: Vec<Vec<usize>>,
}

impl HeapLayout for GcLayout {
    fn object_ref_fields(&self, type_id: u32) -> &[usize] {
        self.ref_fields
            .get(type_id as usize)
            .map(|fields| fields.as_slice())
            .unwrap_or(&[])
    }
}

#[derive(Debug)]
struct Scope {
    values: HashMap<String, Value>,
    roots: HashMap<String, usize>,
}

impl Scope {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            roots: HashMap::new(),
        }
    }
}

impl<'a> Vm<'a> {
    pub(crate) fn new(
        program: &'a Program,
        input: &str,
        args: &[String],
        options: VmOptions,
    ) -> Self {
        let mut functions = HashMap::new();
        for func in &program.functions {
            functions.insert(func.name.clone(), func);
        }
        for book in &program.books {
            for method in &book.methods {
                let name = format!("{}::{}", book.name, method.name);
                functions.insert(name, method);
            }
        }
        let has_std_channel = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "channel"
        });
        let has_std_thread = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "thread"
        });
        let has_std_net = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "net"
        });
        let mut books = HashMap::new();
        let mut ref_fields = Vec::new();
        for (book_id, book) in program.books.iter().enumerate() {
            let mut field_types = Vec::new();
            let mut field_index = HashMap::new();
            let mut book_ref_fields = Vec::new();
            for (idx, field) in book.fields.iter().enumerate() {
                field_types.push(field.ty.clone());
                field_index.insert(field.name.clone(), idx);
                if is_ref_type(&field.ty) {
                    book_ref_fields.push(idx);
                }
            }
            books.insert(
                book.name.clone(),
                BookInfo {
                    id: book_id as u32,
                    field_types,
                    field_index,
                },
            );
            ref_fields.push(book_ref_fields);
        }
        if has_std_channel {
            for kind in [
                ChannelKind::I64,
                ChannelKind::Bool,
                ChannelKind::F64,
                ChannelKind::U8,
                ChannelKind::String,
                ChannelKind::Bytes,
            ] {
                let name = kind.book_name();
                if books.contains_key(name) {
                    continue;
                }
                let book_id = ref_fields.len() as u32;
                books.insert(
                    name.to_string(),
                    BookInfo {
                        id: book_id,
                        field_types: Vec::new(),
                        field_index: HashMap::new(),
                    },
                );
                ref_fields.push(Vec::new());
            }
        }
        if has_std_thread && !books.contains_key("Thread") {
            let book_id = ref_fields.len() as u32;
            books.insert(
                "Thread".to_string(),
                BookInfo {
                    id: book_id,
                    field_types: Vec::new(),
                    field_index: HashMap::new(),
                },
            );
            ref_fields.push(Vec::new());
        }
        if has_std_net {
            for name in ["TcpStream", "TcpListener", "TcpPool"] {
                if books.contains_key(name) {
                    continue;
                }
                let book_id = ref_fields.len() as u32;
                books.insert(
                    name.to_string(),
                    BookInfo {
                        id: book_id,
                        field_types: Vec::new(),
                        field_index: HashMap::new(),
                    },
                );
                ref_fields.push(Vec::new());
            }
        }
        let mut enums = HashMap::new();
        for (enum_id, enum_decl) in program.enums.iter().enumerate() {
            let mut variants = HashMap::new();
            for (variant_id, variant) in enum_decl.variants.iter().enumerate() {
                let info = EnumVariantInfo {
                    id: variant_id as u32,
                    payload: variant.payload.as_ref().map(|payload| payload.ty.clone()),
                };
                variants.insert(variant.name.clone(), info);
            }
            enums.insert(
                enum_decl.name.clone(),
                EnumInfo {
                    id: enum_id as u32,
                    variants,
                },
            );
        }
        if has_std_channel {
            for kind in [
                ChannelKind::I64,
                ChannelKind::Bool,
                ChannelKind::F64,
                ChannelKind::U8,
                ChannelKind::String,
                ChannelKind::Bytes,
            ] {
                let name = kind.recv_name();
                if enums.contains_key(name) {
                    continue;
                }
                let enum_id = enums.len() as u32;
                let mut variants = HashMap::new();
                variants.insert(
                    "Ok".to_string(),
                    EnumVariantInfo {
                        id: 0,
                        payload: Some(kind.payload_type()),
                    },
                );
                variants.insert(
                    "Closed".to_string(),
                    EnumVariantInfo {
                        id: 1,
                        payload: None,
                    },
                );
                enums.insert(
                    name.to_string(),
                    EnumInfo {
                        id: enum_id,
                        variants,
                    },
                );
            }
        }
        Self {
            program,
            functions,
            books,
            enums,
            scopes: Vec::new(),
            args: args.to_vec(),
            input: split_lines(input),
            output: String::new(),
            trace: Vec::new(),
            start_time: Instant::now(),
            heap: Heap::new(),
            roots: RootStack::new(),
            channels: HashMap::new(),
            threads: HashMap::new(),
            pending_threads: VecDeque::new(),
            net_streams: HashMap::new(),
            net_listeners: HashMap::new(),
            net_pools: HashMap::new(),
            gc_layout: GcLayout { ref_fields },
            gc_threshold: GC_MIN_THRESHOLD,
            stdin_fallback: false,
            stdout_live: false,
            rng_state: RAND_SEED_DEFAULT,
            deterministic: options.deterministic,
            virtual_time_ms: 0,
        }
    }

    fn bind_local(&mut self, name: String, value: Value) {
        let existing_slot = self
            .scopes
            .last()
            .and_then(|scope| scope.roots.get(&name).cloned());
        let slot = if let Some(slot) = existing_slot {
            slot
        } else {
            let slot = self.roots.extend_frame(1);
            if let Some(scope) = self.scopes.last_mut() {
                scope.roots.insert(name.clone(), slot);
            }
            slot
        };
        if let Some(scope) = self.scopes.last_mut() {
            scope.values.insert(name, value.clone());
        }
        self.update_root_slot(slot, &value);
    }

    fn assign_var(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        let mut updated: Option<(usize, Value)> = None;
        let mut found = false;
        for scope in self.scopes.iter_mut().rev() {
            if let Some(existing) = scope.values.get_mut(name) {
                let expected = value_type(existing)?;
                let coerced = coerce_value(value.clone(), &expected)?;
                *existing = coerced.clone();
                found = true;
                if let Some(slot) = scope.roots.get(name).cloned() {
                    updated = Some((slot, coerced));
                }
                break;
            }
        }
        if let Some((slot, value)) = updated {
            self.update_root_slot(slot, &value);
        }
        if found {
            return Ok(());
        }
        Err(runtime_error(
            "E0400",
            format!("Unknown name '{name}' at runtime."),
        ))
    }

    pub(crate) fn channel_state_mut(
        &mut self,
        handle: HeapHandle,
    ) -> Result<&mut ChannelState, RuntimeError> {
        self.channels
            .get_mut(&handle.as_u32())
            .ok_or_else(|| runtime_error("E0406", "Channel state missing at runtime."))
    }

    pub(crate) fn tcp_stream_mut(
        &mut self,
        handle: HeapHandle,
    ) -> Result<&mut TcpStream, RuntimeError> {
        self.net_streams
            .get_mut(&handle.as_u32())
            .ok_or_else(|| runtime_error("E0408", "TcpStream handle is invalid."))
    }

    pub(crate) fn tcp_listener_mut(
        &mut self,
        handle: HeapHandle,
    ) -> Result<&mut TcpListener, RuntimeError> {
        self.net_listeners
            .get_mut(&handle.as_u32())
            .ok_or_else(|| runtime_error("E0408", "TcpListener handle is invalid."))
    }

    pub(crate) fn register_tcp_stream(&mut self, handle: HeapHandle, stream: TcpStream) {
        self.net_streams.insert(handle.as_u32(), stream);
    }

    pub(crate) fn register_tcp_listener(&mut self, handle: HeapHandle, listener: TcpListener) {
        self.net_listeners.insert(handle.as_u32(), listener);
    }

    pub(crate) fn close_tcp_stream(&mut self, handle: HeapHandle) -> Option<TcpStream> {
        self.net_streams.remove(&handle.as_u32())
    }

    pub(crate) fn close_tcp_listener(&mut self, handle: HeapHandle) -> bool {
        self.net_listeners.remove(&handle.as_u32()).is_some()
    }

    pub(crate) fn tcp_pool_mut(
        &mut self,
        handle: HeapHandle,
    ) -> Result<&mut TcpPoolState, RuntimeError> {
        self.net_pools
            .get_mut(&handle.as_u32())
            .ok_or_else(|| runtime_error("E0408", "TcpPool handle is invalid."))
    }

    pub(crate) fn register_tcp_pool(&mut self, handle: HeapHandle, state: TcpPoolState) {
        self.net_pools.insert(handle.as_u32(), state);
    }

    pub(crate) fn close_tcp_pool(&mut self, handle: HeapHandle) -> Option<TcpPoolState> {
        self.net_pools.remove(&handle.as_u32())
    }

    pub(crate) fn function_by_name(&self, name: &str) -> Option<&'a birddisk_core::ast::Function> {
        self.functions.get(name).copied()
    }

    pub(crate) fn program_clone(&self) -> Program {
        self.program.clone()
    }

    pub(crate) fn register_thread(&mut self, handle: HeapHandle, result: i64) {
        self.threads
            .insert(handle.as_u32(), ThreadState::completed(result));
    }

    pub(crate) fn register_thread_running(&mut self, handle: HeapHandle) {
        self.threads.insert(handle.as_u32(), ThreadState::running());
    }

    pub(crate) fn register_thread_running_host(
        &mut self,
        handle: HeapHandle,
        join: JoinHandle<Result<i64, RuntimeError>>,
    ) {
        self.threads
            .insert(handle.as_u32(), ThreadState::running_host(join));
    }

    pub(crate) fn enqueue_thread_job(
        &mut self,
        handle: HeapHandle,
        entry_name: String,
        args: Vec<Value>,
    ) {
        self.pending_threads.push_back(PendingThreadJob {
            handle,
            entry_name,
            args,
        });
    }

    pub(crate) fn has_pending_thread_jobs(&self) -> bool {
        !self.pending_threads.is_empty()
    }

    pub(crate) fn is_deterministic(&self) -> bool {
        self.deterministic
    }

    pub(crate) fn run_next_pending_thread(&mut self) -> Result<bool, RuntimeError> {
        let Some(job) = self.pending_threads.pop_front() else {
            return Ok(false);
        };
        let function = self.function_by_name(&job.entry_name).ok_or_else(|| {
            runtime_error(
                "E0400",
                format!("Unknown thread entry rule '{}'.", job.entry_name),
            )
        })?;
        let result = self.eval_function(function, &job.args)?;
        let result_i64 = match result {
            Value::I64(value) => value,
            _ => return Err(runtime_error("E0400", "Thread entry rule must return i64.")),
        };
        self.register_thread(job.handle, result_i64);
        Ok(true)
    }

    fn schedule_until_thread_resolved(&mut self, handle: HeapHandle) -> Result<(), RuntimeError> {
        loop {
            let running = self
                .threads
                .get(&handle.as_u32())
                .map(|state| matches!(state.status, ThreadStatus::Running))
                .unwrap_or(false);
            if !running {
                break;
            }
            if !self.run_next_pending_thread()? {
                break;
            }
        }
        Ok(())
    }

    pub(crate) fn join_thread(&mut self, handle: HeapHandle) -> Result<i64, RuntimeError> {
        if self.deterministic {
            self.schedule_until_thread_resolved(handle)?;
        }
        let id = handle.as_u32();
        let state = self
            .threads
            .remove(&id)
            .ok_or_else(|| runtime_error("E0405", "Thread handle is invalid."))?;
        match state.status {
            ThreadStatus::Running => {
                self.threads.insert(id, state);
                Err(runtime_error("E0405", "Thread is still running."))
            }
            ThreadStatus::RunningHost(join) => {
                let result = match join.join() {
                    Ok(result) => result,
                    Err(_) => Err(runtime_error("E0405", "Thread panicked.")),
                };
                self.threads.insert(
                    id,
                    ThreadState {
                        status: ThreadStatus::Joined,
                    },
                );
                result
            }
            ThreadStatus::Completed(result) => {
                self.threads.insert(
                    id,
                    ThreadState {
                        status: ThreadStatus::Joined,
                    },
                );
                Ok(result)
            }
            ThreadStatus::Joined => {
                self.threads.insert(
                    id,
                    ThreadState {
                        status: ThreadStatus::Joined,
                    },
                );
                Err(runtime_error("E0405", "Thread has already been joined."))
            }
        }
    }

    fn update_root_slot(&mut self, slot: usize, value: &Value) {
        let root_value = match value.heap_handle() {
            Some(handle) => RootValue::Ptr(handle),
            None => RootValue::Null,
        };
        self.roots.set_slot(slot, root_value);
    }

    fn maybe_collect(&mut self) {
        let stats = self.heap.stats();
        if stats.bytes_in_use < self.gc_threshold {
            return;
        }
        let mut extra_roots = self.channel_ref_handles();
        extra_roots.extend(self.pending_thread_ref_handles());
        let extra_count = extra_roots.len();
        let base = if extra_count == 0 {
            None
        } else {
            Some(self.roots.push_frame(extra_count))
        };
        if let Some(base) = base {
            for (offset, handle) in extra_roots.iter().enumerate() {
                self.roots.set_slot(base + offset, RootValue::Ptr(*handle));
            }
        }
        let report = self.heap.gc_with_layout(&self.roots, &self.gc_layout);
        if let Some(base) = base {
            let _ = base;
            self.roots.pop_frame(extra_count);
        }
        let next = report.live_bytes.saturating_mul(2).max(GC_MIN_THRESHOLD);
        self.gc_threshold = next;
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.roots.push_frame(0);
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.roots.pop_frame_auto();
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.values.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn channel_ref_handles(&self) -> Vec<HeapHandle> {
        let mut handles = Vec::new();
        for state in self.channels.values() {
            for value in &state.queue {
                if let ChannelValue::Ref(handle) = value {
                    handles.push(*handle);
                }
            }
        }
        handles
    }

    fn pending_thread_ref_handles(&self) -> Vec<HeapHandle> {
        let mut handles = Vec::new();
        for job in &self.pending_threads {
            for value in &job.args {
                if let Some(handle) = value.heap_handle() {
                    handles.push(handle);
                }
            }
        }
        handles
    }
}

fn split_lines(input: &str) -> VecDeque<String> {
    if input.is_empty() {
        return VecDeque::new();
    }
    input
        .split('\n')
        .map(|line| line.strip_suffix('\r').unwrap_or(line).to_string())
        .collect()
}

fn trim_line_end(mut line: String) -> String {
    if line.ends_with('\n') {
        line.pop();
    }
    if line.ends_with('\r') {
        line.pop();
    }
    line
}

const GC_MIN_THRESHOLD: usize = 1024 * 64;

fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}

fn elem_kind_for_type(ty: &Type) -> Result<ElemKind, RuntimeError> {
    match ty {
        Type::I64 => Ok(ElemKind::I64),
        Type::F64 => Ok(ElemKind::F64),
        Type::Bool => Ok(ElemKind::Bool),
        Type::U8 => Ok(ElemKind::U8),
        Type::String | Type::Array(_) | Type::Book(_) => Ok(ElemKind::Ref),
        Type::Void => Err(runtime_error(
            "E0400",
            "Void is not a valid array element type.",
        )),
    }
}

fn elem_size(kind: ElemKind) -> usize {
    match kind {
        ElemKind::I64 => 8,
        ElemKind::F64 => 8,
        ElemKind::Bool => 1,
        ElemKind::U8 => 1,
        ElemKind::Ref => 8,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser};

    fn run_with_gc(source: &str, threshold: usize) -> (i64, usize) {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let mut vm = Vm::new(&program, "", &[], VmOptions::default());
        vm.gc_threshold = threshold;
        let result = vm.eval_main().unwrap();
        let gc_runs = vm.heap.stats().gc_runs;
        (result, gc_runs)
    }

    fn run_with_gc_stats(source: &str, threshold: usize) -> (i64, crate::heap::HeapStats) {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let mut vm = Vm::new(&program, "", &[], VmOptions::default());
        vm.gc_threshold = threshold;
        let result = vm.eval_main().unwrap();
        let stats = vm.heap.stats();
        (result, stats)
    }

    #[test]
    fn split_lines_strips_cr() {
        let lines = split_lines("123\r\n456\r\n");
        let collected: Vec<String> = lines.into_iter().collect();
        assert_eq!(
            collected,
            vec!["123".to_string(), "456".to_string(), "".to_string()]
        );
    }

    #[test]
    fn eval_time_deterministic_virtual_clock() {
        let source = "import std::time.\n\nrule main() -> i64:\n  set start: i64 = std::time::now_ms().\n  set ignored: i64 = std::time::sleep_ms(5).\n  set finish: i64 = std::time::now_ms().\n  yield finish - start.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let (result, _) = eval_with_io_options(
            &program,
            "",
            &[],
            VmOptions {
                deterministic: true,
            },
        )
        .unwrap();
        assert_eq!(result, 5);
    }

    #[test]
    fn gc_preserves_object_cycles() {
        let source = "book Node:\n  field link: Node[].\n  field value: i64.\n\n  rule init(self: Node, value: i64) -> Node:\n    put self::value = value.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set a: Node = new Node(1).\n  set once: i64 = 0.\n  repeat while once < 1:\n    set b: Node = new Node(2).\n    set al: Node[] = [b].\n    set bl: Node[] = [a].\n    put a::link = al.\n    put b::link = bl.\n    put once = once + 1.\n  end\n\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n\n  set nexts: Node[] = a::link.\n  set first: Node = nexts[0].\n  yield first::value.\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 2);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_preserves_nested_arrays_in_objects() {
        let source = "book Holder:\n  field grid: i64[][].\n\n  rule init(self: Holder) -> Holder:\n    set row1: i64[] = [1, 2].\n    set row2: i64[] = [3, 4].\n    set grid: i64[][] = [row1, row2].\n    put self::grid = grid.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set holder: Holder = new Holder().\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  set grid: i64[][] = holder::grid.\n  yield grid[1][0].\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 3);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_collects_unreachable_cycles_in_vm() {
        let source = "book Node:\n  field link: Node[].\n  field value: i64.\n\n  rule init(self: Node, value: i64) -> Node:\n    put self::value = value.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set once: i64 = 0.\n  repeat while once < 1:\n    set a: Node = new Node(1).\n    set b: Node = new Node(2).\n    set al: Node[] = [b].\n    set bl: Node[] = [a].\n    put a::link = al.\n    put b::link = bl.\n    put once = once + 1.\n  end\n\n  set junk: i64[] = array(2048).\n  set tiny: i64[] = array(1).\n  yield 0.\nend\n";
        let (result, stats) = run_with_gc_stats(source, 1024);
        assert_eq!(result, 0);
        assert!(stats.gc_runs > 0);
        assert!(stats.last_freed >= 4);
    }

    #[test]
    fn gc_roots_call_args_under_pressure() {
        let source = "book Node:\n  field value: i64.\n\n  rule init(self: Node, value: i64) -> Node:\n    put self::value = value.\n    yield self.\n  end\nend\n\nrule make_junk() -> i64:\n  set xs: i64[] = array(2048).\n  put xs[0] = 1.\n  yield xs[0].\nend\n\nrule consume(n: Node, junk: i64) -> i64:\n  set ys: i64[] = array(2048).\n  put ys[0] = junk.\n  yield n::value.\nend\n\nrule main() -> i64:\n  yield consume(new Node(7), make_junk()).\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 7);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_marks_ref_arrays_in_vm() {
        let source = "import std::string.\nrule main() -> i64:\n  set a: string = \"alpha\".\n  set b: string = \"beta\".\n  set items: string[] = [a, b].\n  set i: i64 = 0.\n  repeat while i < 4:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  set first: string = items[0].\n  yield std::string::len(first).\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 5);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_marks_nested_ref_arrays_in_vm() {
        let source = "rule main() -> i64:\n  set inner: i64[] = [11].\n  set outer: i64[][] = [inner].\n  set i: i64 = 0.\n  repeat while i < 4:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  yield outer[0][0].\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 11);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_stats_report_freed_and_peak_in_vm() {
        let source = "rule main() -> i64:\n  set i: i64 = 0.\n  repeat while i < 4:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  yield 0.\nend\n";
        let (result, stats) = run_with_gc_stats(source, 1024);
        assert_eq!(result, 0);
        assert!(stats.gc_runs > 0);
        assert!(stats.last_freed > 0);
        assert!(stats.peak_bytes_in_use >= stats.bytes_in_use);
        assert!(stats.peak_bytes_in_use > 0);
    }
}
