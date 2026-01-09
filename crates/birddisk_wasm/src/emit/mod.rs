mod compiler;
mod runtime;
mod types;

use crate::analysis::{
    program_uses_arrays, program_uses_bytes, program_uses_io, program_uses_objects,
    program_uses_string_from_bytes, program_uses_strings,
};
use crate::trace::build_trace_table;
use birddisk_core::ast::{Program, Type};
use std::collections::HashMap;

pub(crate) use compiler::WatEmitter;

#[derive(Debug, Clone)]
pub struct WasmError {
    pub code: &'static str,
    pub message: String,
    pub trace: Vec<birddisk_core::TraceFrame>,
}

pub(crate) fn wasm_error(code: &'static str, message: impl Into<String>) -> WasmError {
    WasmError {
        code,
        message: message.into(),
        trace: Vec::new(),
    }
}

pub(crate) const ARRAY_HEADER_SIZE: i32 = 8;
pub(crate) const STRING_HEADER_SIZE: i32 = 4;
pub(crate) const OBJECT_HEADER_SIZE: i32 = 8;
pub(crate) const OBJECT_FIELD_SIZE: i32 = 8;
pub(crate) const ARRAY_KIND_I64: i32 = 1;
pub(crate) const ARRAY_KIND_BOOL: i32 = 2;
pub(crate) const ARRAY_KIND_U8: i32 = 3;
pub(crate) const ARRAY_KIND_REF: i32 = 4;
pub(crate) const TRAP_ARRAY_OOB: i32 = 403;
pub(crate) const TRAP_ARRAY_LEN_NEG: i32 = 400;
pub(crate) const TRAP_ARRAY_OOM: i32 = 405;
pub(crate) const TRAP_UTF8_INVALID: i32 = 406;
pub(crate) const TRAP_TRACE_OOM: i32 = 407;
pub(crate) const TRAP_STRING_PARSE: i32 = 408;

pub(crate) const TRACE_STACK_PTR_OFFSET: i32 = 0;
pub(crate) const TRACE_STACK_DATA_OFFSET: i32 = 4;
pub(crate) const TRACE_STACK_SLOTS: i32 = 256;
pub(crate) const TRACE_STACK_BYTES: i32 = TRACE_STACK_DATA_OFFSET + TRACE_STACK_SLOTS * 4;
pub(crate) const HEAP_START: i32 = (TRACE_STACK_BYTES + 7) & !7;

#[derive(Clone)]
pub(crate) struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

#[derive(Clone)]
pub(crate) struct BookLayout {
    id: u32,
    fields: Vec<Type>,
    field_index: HashMap<String, usize>,
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
    let mut ref_fields = Vec::new();
    for (book_id, book) in program.books.iter().enumerate() {
        let mut field_index = HashMap::new();
        let mut fields = Vec::new();
        let mut book_refs = Vec::new();
        for (idx, field) in book.fields.iter().enumerate() {
            field_index.insert(field.name.clone(), idx);
            fields.push(field.ty.clone());
            if is_ref_type(&field.ty) {
                book_refs.push(idx as u32);
            }
        }
        books.insert(
            book.name.clone(),
            BookLayout {
                id: book_id as u32,
                fields,
                field_index,
            },
        );
        ref_fields.push(book_refs);
    }

    let layout = build_layout_data(&ref_fields);
    let layout_base = HEAP_START;
    let heap_start = align_up(layout_base + layout.len() as i32, 8);

    let mut emitter = WatEmitter::new();
    emitter.push_line("(module");
    emitter.indent();

    if uses_heap {
        runtime::emit_heap_runtime(
            &mut emitter,
            export_memory,
            uses_from_bytes,
            uses_io,
            heap_start,
        );
    }
    if uses_strings {
        runtime::emit_string_runtime(&mut emitter, uses_from_bytes);
    }
    if uses_bytes {
        runtime::emit_bytes_runtime(&mut emitter);
    }
    if uses_io {
        runtime::emit_io_runtime(&mut emitter);
    }
    if uses_heap {
        runtime::emit_gc_layout_runtime(
            &mut emitter,
            ref_fields.len() as i32,
            layout_offsets_base(layout_base, ref_fields.len()),
            layout_counts_base(layout_base, ref_fields.len()),
            layout_fields_base(layout_base, ref_fields.len()),
        );
    }

    let frame_id_for = |name: &str| {
        trace_table.ids.get(name).copied().ok_or_else(|| {
            wasm_error("E0400", format!("Missing trace frame for '{name}'"))
        })
    };

    for func in &program.functions {
        compiler::emit_function(
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
            compiler::emit_function(
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

    if !layout.is_empty() {
        let data = encode_bytes(&layout);
        emitter.push_line(format!("(data (i32.const {layout_base}) \"{data}\")"));
    }

    emitter.dedent();
    emitter.push_line(")");

    Ok(emitter.finish())
}

fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}

fn build_layout_data(ref_fields: &[Vec<u32>]) -> Vec<u8> {
    let mut offsets = Vec::with_capacity(ref_fields.len());
    let mut counts = Vec::with_capacity(ref_fields.len());
    let mut flat = Vec::new();
    let mut cursor = 0u32;
    for fields in ref_fields {
        offsets.push(cursor);
        counts.push(fields.len() as u32);
        flat.extend_from_slice(fields);
        cursor = flat.len() as u32;
    }
    let mut bytes = Vec::with_capacity((offsets.len() + counts.len() + flat.len()) * 4);
    for value in offsets {
        bytes.extend_from_slice(&value.to_le_bytes());
    }
    for value in counts {
        bytes.extend_from_slice(&value.to_le_bytes());
    }
    for value in flat {
        bytes.extend_from_slice(&value.to_le_bytes());
    }
    bytes
}

fn encode_bytes(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 4);
    for byte in bytes {
        out.push('\\');
        out.push(hex_digit(byte >> 4));
        out.push(hex_digit(byte & 0x0f));
    }
    out
}

fn hex_digit(value: u8) -> char {
    match value {
        0..=9 => (b'0' + value) as char,
        _ => (b'a' + (value - 10)) as char,
    }
}

fn align_up(value: i32, align: i32) -> i32 {
    if align == 0 {
        return value;
    }
    let mask = align - 1;
    (value + mask) & !mask
}

fn layout_offsets_base(layout_base: i32, _book_count: usize) -> i32 {
    layout_base
}

fn layout_counts_base(layout_base: i32, book_count: usize) -> i32 {
    layout_base + (book_count as i32 * 4)
}

fn layout_fields_base(layout_base: i32, book_count: usize) -> i32 {
    layout_base + (book_count as i32 * 8)
}

pub fn emit_wasm(program: &Program) -> Result<Vec<u8>, WasmError> {
    let wat = emit_wat(program)?;
    wat::parse_str(&wat).map_err(|err| wasm_error("E0400", format!("WASM encode error: {err}")))
}

#[cfg(test)]
mod tests {
    use crate::{emit_wasm, run, run_with_io, WasmError};
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

    #[test]
    fn wasm_runs_object_cycles_under_pressure() {
        let result = compile_and_run(
            "book Node:\n  field link: Node[].\n  field value: i64.\n\n  rule init(self: Node, value: i64) -> Node:\n    put self::value = value.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set a: Node = new Node(1).\n  set once: i64 = 0.\n  repeat while once < 1:\n    set b: Node = new Node(2).\n    set al: Node[] = [b].\n    set bl: Node[] = [a].\n    put a::link = al.\n    put b::link = bl.\n    put once = once + 1.\n  end\n\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n\n  set nexts: Node[] = a::link.\n  set first: Node = nexts[0].\n  yield first::value.\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_runs_nested_arrays_in_objects_under_pressure() {
        let result = compile_and_run(
            "book Holder:\n  field grid: i64[][].\n\n  rule init(self: Holder) -> Holder:\n    set row1: i64[] = [1, 2].\n    set row2: i64[] = [3, 4].\n    set grid: i64[][] = [row1, row2].\n    put self::grid = grid.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set holder: Holder = new Holder().\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  set grid: i64[][] = holder::grid.\n  yield grid[1][0].\nend\n",
        )
        .unwrap();
        assert_eq!(result, 3);
    }
}
