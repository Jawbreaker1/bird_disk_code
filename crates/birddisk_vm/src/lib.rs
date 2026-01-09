//! BirdDisk VM interpreter.

mod builtins;
mod heap;
mod runtime_error;
mod value;
mod vm;

pub use runtime_error::RuntimeError;
pub use vm::{eval, eval_with_io};

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser, parse_and_typecheck};
    use std::path::PathBuf;

    fn eval_source(source: &str) -> i64 {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
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
    fn eval_call() {
        let result = eval_source(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(2, 3).\nend\n",
        );
        assert_eq!(result, 5);
    }

    #[test]
    fn eval_div_by_zero_errors() {
        let tokens = lexer::lex("rule main() -> i64:\n  yield 1 / 0.\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
    }

    #[test]
    fn eval_rejects_wrong_arity() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
        assert!(err.message.contains("expected 2"));
    }

    #[test]
    fn eval_trace_includes_call_stack() {
        let source = "rule boom() -> i64:\n  yield 1 / 0.\nend\n\nrule main() -> i64:\n  yield boom().\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
        assert!(err.trace.len() >= 2);
        assert_eq!(err.trace[0].function, "boom");
        assert_eq!(err.trace[1].function, "main");
    }

    #[test]
    fn eval_array_literal_index() {
        let result = eval_source(
            "rule main() -> i64:\n  set xs: i64[] = [1, 2, 3].\n  yield xs[1].\nend\n",
        );
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
        let tokens =
            lexer::lex("rule main() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n")
                .unwrap();
        let program = parser::parse(&tokens).unwrap();
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
        let source = "import std::io.\nimport std::string.\nrule main() -> i64:\n  set line: string = std::io::read_line().\n  set out: string = std::string::concat(line, \"!\").\n  yield std::io::print(out).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let (result, output) = eval_with_io(&program, "BirdDisk").unwrap();
        assert_eq!(result, 9);
        assert_eq!(output, "BirdDisk!");
    }
}
