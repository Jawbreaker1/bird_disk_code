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
use std::time::Instant;

const RAND_SEED_DEFAULT: u64 = 0x9E37_79B9_7F4A_7C15;
const RAND_MULT: u64 = 0x2545_F491_4F6C_DD1D;

pub fn eval(program: &Program) -> Result<i64, RuntimeError> {
    let (result, _) = eval_with_io(program, "", &[])?;
    Ok(result)
}

pub fn eval_with_io(
    program: &Program,
    input: &str,
    args: &[String],
) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input, args);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

pub fn eval_with_io_streaming(
    program: &Program,
    input: &str,
    args: &[String],
    stdin_fallback: bool,
) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input, args);
    vm.set_stdout_live(true);
    vm.set_stdin_fallback(stdin_fallback);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

pub(crate) struct Vm<'a> {
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
    gc_layout: GcLayout,
    gc_threshold: usize,
    stdin_fallback: bool,
    stdout_live: bool,
    rng_state: u64,
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
    fn new(program: &'a Program, input: &str, args: &[String]) -> Self {
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
        Self {
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
            gc_layout: GcLayout { ref_fields },
            gc_threshold: GC_MIN_THRESHOLD,
            stdin_fallback: false,
            stdout_live: false,
            rng_state: RAND_SEED_DEFAULT,
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
        let report = self.heap.gc_with_layout(&self.roots, &self.gc_layout);
        let next = report
            .live_bytes
            .saturating_mul(2)
            .max(GC_MIN_THRESHOLD);
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
        Type::Void => Err(runtime_error("E0400", "Void is not a valid array element type.")),
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
        let mut vm = Vm::new(&program, "", &[]);
        vm.gc_threshold = threshold;
        let result = vm.eval_main().unwrap();
        let gc_runs = vm.heap.stats().gc_runs;
        (result, gc_runs)
    }

    fn run_with_gc_stats(source: &str, threshold: usize) -> (i64, crate::heap::HeapStats) {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let mut vm = Vm::new(&program, "", &[]);
        vm.gc_threshold = threshold;
        let result = vm.eval_main().unwrap();
        let stats = vm.heap.stats();
        (result, stats)
    }

    #[test]
    fn split_lines_strips_cr() {
        let lines = split_lines("123\r\n456\r\n");
        let collected: Vec<String> = lines.into_iter().collect();
        assert_eq!(collected, vec!["123".to_string(), "456".to_string(), "".to_string()]);
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
