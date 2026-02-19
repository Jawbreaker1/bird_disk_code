//! BirdDisk VM interpreter.

mod builtins;
mod heap;
mod runtime_error;
mod value;
mod vm;

pub use runtime_error::RuntimeError;
pub use vm::{
    eval, eval_with_io, eval_with_io_options, eval_with_io_streaming,
    eval_with_io_streaming_options, VmOptions,
};

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{attach_sources, lexer, parse_and_typecheck, parser};
    use std::path::PathBuf;

    fn parse_program(source: &str) -> birddisk_core::ast::Program {
        let tokens = lexer::lex(source).unwrap();
        let mut program = parser::parse(&tokens).unwrap();
        attach_sources(&mut program, "<memory>", source);
        program
    }

    fn eval_source(source: &str) -> i64 {
        let program = parse_program(source);
        eval(&program).unwrap()
    }

    fn fixture_path(rel: &str) -> PathBuf {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.pop();
        root.pop();
        root.push(rel);
        root
    }

    #[test]
    fn eval_minimal_main() {
        let result = eval_source("rule main() -> i64:\n  yield 0.\nend\n");
        assert_eq!(result, 0);
    }

    #[test]
    fn eval_when() {
        let result = eval_source(
            "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n  end\nend\n",
        );
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_repeat() {
        let result = eval_source(
            "rule main() -> i64:\n  set i = 0.\n  set sum: i64 = 0.\n\n  repeat while i < 5:\n    put sum = sum + i.\n    put i = i + 1.\n  end\n\n  yield sum.\nend\n",
        );
        assert_eq!(result, 10);
    }

    #[test]
    fn eval_enum_match() {
        let result = eval_source(
            "enum Result:\n  case Ok(value: i64).\n  case Err(message: string).\nend\n\nrule main() -> i64:\n  set r: Result = Result::Ok(7).\n  match r:\n    case Result::Ok(value):\n      yield value.\n    case Result::Err(message):\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn eval_enum_match_payload_string() {
        let result = eval_source(
            "enum Msg:\n  case Text(text: string).\nend\n\nrule main() -> i64:\n  set m: Msg = Msg::Text(\"hi\").\n  match m:\n    case Msg::Text(text):\n      yield std::string::len(text).\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_call() {
        let result = eval_source(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(2, 3).\nend\n",
        );
        assert_eq!(result, 5);
    }

    #[test]
    fn eval_div_by_zero_errors() {
        let program = parse_program("rule main() -> i64:\n  yield 1 / 0.\nend\n");
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
    }

    #[test]
    fn eval_rejects_wrong_arity() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n";
        let program = parse_program(source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
        assert!(err.message.contains("expected 2"));
    }

    #[test]
    fn eval_trace_includes_call_stack() {
        let source = "rule boom() -> i64:\n  yield 1 / 0.\nend\n\nrule main() -> i64:\n  yield boom().\nend\n";
        let program = parse_program(source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
        assert!(err.trace.len() >= 2);
        assert_eq!(err.trace[0].function, "boom");
        assert_eq!(err.trace[1].function, "main");
        assert_eq!(err.trace[0].file, "<memory>");
        assert!(err.trace[0].source.contains("rule boom"));
    }

    #[test]
    fn eval_throw_is_catchable() {
        let result = eval_source(
            "rule main() -> i64:\n  try:\n    throw \"boom\".\n  catch err:\n    yield 7.\n  end\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn eval_throw_uncaught_errors() {
        let program = parse_program("rule main() -> i64:\n  throw \"boom\".\n  yield 0.\nend\n");
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0404");
    }

    #[test]
    fn eval_array_literal_index() {
        let result =
            eval_source("rule main() -> i64:\n  set xs: i64[] = [1, 2, 3].\n  yield xs[1].\nend\n");
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_array_new_and_put_index() {
        let result = eval_source(
            "rule main() -> i64:\n  set xs: i64[] = array(3).\n  put xs[1] = 7.\n  yield xs[1].\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn eval_array_index_out_of_bounds() {
        let program =
            parse_program("rule main() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n");
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0403");
    }

    #[test]
    fn eval_array_out_of_bounds_fixture() {
        let path = fixture_path("vm_error_tests/arrays/array_index_out_of_bounds.bd");
        let program = parse_and_typecheck(path.to_str().unwrap()).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0403");
    }

    #[test]
    fn eval_string_len_concat() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set base: string = \"hi\".\n  set joined: string = std::string::concat(base, \"!\").\n  yield std::string::len(joined).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_bytes_len_and_eq() {
        let result = eval_source(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set left: u8[] = std::string::bytes(\"hi\").\n  set right: u8[] = std::string::bytes(\"hi\").\n  when std::bytes::eq(left, right):\n    yield std::bytes::len(left).\n  otherwise:\n    yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_u8_array_literal() {
        let result = eval_source(
            "import std::bytes.\nrule main() -> i64:\n  set data: u8[] = [65, 66, 67].\n  yield std::bytes::len(data).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_string_from_bytes_roundtrip() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set bytes: u8[] = std::string::bytes(\"hi\").\n  set text: string = std::string::from_bytes(bytes).\n  yield std::string::len(text).\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_string_from_bytes_rejects_invalid_utf8() {
        let tokens = lexer::lex(
            "import std::string.\nrule main() -> i64:\n  set bytes: u8[] = [195, 40].\n  set text: string = std::string::from_bytes(bytes).\n  yield std::string::len(text).\nend\n",
        )
        .unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn eval_string_to_i64() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"123\").\nend\n",
        );
        assert_eq!(result, 123);
    }

    #[test]
    fn eval_string_from_i64() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set text: string = std::string::from_i64(-42).\n  yield std::string::len(text).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_string_to_i64_rejects_invalid() {
        let tokens = lexer::lex(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"12x\").\nend\n",
        )
        .unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn eval_io_print_and_read_line() {
        let source = "import std::io.\nimport std::string.\nrule main() -> i64:\n  set line: string = std::io::read_line().\n  set out: string = std::string::concat(line, \"!\").\n  std::io::print(out).\n  yield std::string::len(out).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let (result, output) = eval_with_io(&program, "BirdDisk", &[]).unwrap();
        assert_eq!(result, 9);
        assert_eq!(output, "BirdDisk!");
    }

    #[test]
    fn eval_thread_spawn_and_join() {
        let result = eval_source(
            "import std::thread.\nrule worker(value: i64) -> i64:\n  yield value + 2.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\", 5).\n  yield std::thread::join(t).\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn eval_thread_join_twice_errors() {
        let source = "import std::thread.\nrule worker() -> i64:\n  yield 3.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\").\n  set first: i64 = std::thread::join(t).\n  set second: i64 = std::thread::join(t).\n  yield first + second.\nend\n";
        let program = parse_program(source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
        assert!(err.message.contains("already been joined"));
    }
}
