use crate::{emit_object, run, run_with_io};
use birddisk_core::{attach_sources, lexer, parser};

fn parse_program(source: &str) -> birddisk_core::ast::Program {
    let tokens = lexer::lex(source).unwrap();
    let mut program = parser::parse(&tokens).unwrap();
    attach_sources(&mut program, "<memory>", source);
    program
}

fn run_source(source: &str) -> i64 {
    let program = parse_program(source);
    run(&program).unwrap()
}

fn run_source_with_io(source: &str, input: &str) -> (i64, String) {
    let program = parse_program(source);
    run_with_io(&program, input, &[]).unwrap()
}

fn run_source_error(source: &str) -> crate::NativeError {
    let program = parse_program(source);
    run_with_io(&program, "", &[]).unwrap_err()
}

fn emit_source(source: &str) -> Vec<u8> {
    let program = parse_program(source);
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
    let source = "import std::io.\nimport std::string.\nrule main() -> i64:\n  set name: string = std::io::read_line().\n  set greet: string = std::string::concat(\"Hello \", name).\n  std::io::print(greet).\n  yield std::string::len(greet).\nend\n";
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
fn native_runs_enum_match() {
    let source = "enum Choice:\n  case One.\n  case Two(value: i64).\nend\n\nrule main() -> i64:\n  set value: Choice = Choice::Two(9).\n  match value:\n    case Choice::One:\n      yield 1.\n    case Choice::Two(v):\n      yield v + 1.\n    otherwise:\n      yield 0.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 10);
}

#[test]
fn native_runs_enum_match_payload_string() {
    let source = "import std::string.\nenum Status:\n  case Ok(msg: string).\n  case Fail.\nend\n\nrule main() -> i64:\n  set value: Status = Status::Ok(\"hi\").\n  match value:\n    case Status::Ok(text):\n      yield std::string::len(text).\n    case Status::Fail:\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 2);
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
fn native_runtime_error_includes_trace() {
    let err = run_source_error(
        "rule boom() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n\nrule main() -> i64:\n  yield boom().\nend\n",
    );
    assert!(err.trace.len() >= 2);
    assert_eq!(err.trace[0].function, "boom");
    assert_eq!(err.trace[1].function, "main");
    assert_eq!(err.trace[0].file, "<memory>");
    assert!(err.trace[0].source.contains("rule boom"));
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
