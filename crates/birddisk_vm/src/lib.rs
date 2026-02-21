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
    use std::fs;
    use std::net::TcpListener;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU64, Ordering};

    static TEMP_COUNTER: AtomicU64 = AtomicU64::new(1);

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

    fn write_repo_temp(name: &str, source: &str) -> PathBuf {
        let mut path = fixture_path("");
        let id = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        path.push(format!("tmp_{name}_{}_{}.bd", std::process::id(), id));
        fs::write(&path, source).unwrap();
        path
    }

    fn parse_program_with_modules(source: &str, name: &str) -> birddisk_core::ast::Program {
        let path = write_repo_temp(name, source);
        let program = parse_and_typecheck(path.to_str().unwrap()).unwrap();
        fs::remove_file(path).ok();
        program
    }

    fn eval_module_source(source: &str, name: &str) -> i64 {
        let program = parse_program_with_modules(source, name);
        eval(&program).unwrap()
    }

    fn free_tcp_port() -> Option<u16> {
        let listener = TcpListener::bind("127.0.0.1:0").ok()?;
        Some(listener.local_addr().ok()?.port())
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
    fn eval_deterministic_thread_scheduler_progresses_on_recv() {
        let source = "import std::channel.\nimport std::thread.\nimport std::time.\n\nrule worker(ch: ChannelI64) -> i64:\n  set ignored: i64 = std::time::sleep_ms(5).\n  set sent: bool = ch::send(1).\n  when sent:\n    yield 0.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set ch: ChannelI64 = std::channel::i64().\n  set t: Thread = std::thread::spawn(\"worker\", ch).\n  set before: i64 = std::time::now_ms().\n  set msg: RecvI64 = ch::recv().\n  set after: i64 = std::time::now_ms().\n  set done: i64 = std::thread::join(t).\n  match msg:\n    case RecvI64::Ok(v):\n      yield done + v + before + after.\n    case RecvI64::Closed:\n      yield -1.\n    otherwise:\n      yield -2.\n  end\nend\n";
        let program = parse_program(source);
        let (result, _) = eval_with_io_options(
            &program,
            "",
            &[],
            VmOptions {
                deterministic: true,
            },
        )
        .unwrap();
        assert_eq!(result, 6);
    }

    #[test]
    fn eval_thread_join_twice_errors() {
        let source = "import std::thread.\nrule worker() -> i64:\n  yield 3.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\").\n  set first: i64 = std::thread::join(t).\n  set second: i64 = std::thread::join(t).\n  yield first + second.\nend\n";
        let program = parse_program(source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0405");
        assert!(err.message.contains("already been joined"));
    }

    #[test]
    fn eval_channel_recv_would_block_errors() {
        let source = "import std::channel.\nrule main() -> i64:\n  set ch: ChannelI64 = std::channel::i64().\n  set value: RecvI64 = ch::recv().\n  match value:\n    case RecvI64::Ok(v):\n      yield v.\n    case RecvI64::Closed:\n      yield -1.\n    otherwise:\n      yield -2.\n  end\nend\n";
        let program = parse_program(source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0407");
        assert!(err.message.contains("would block"));
    }

    #[test]
    fn eval_thread_channel_stress() {
        let source = "import std::channel.\nimport std::thread.\nrule worker0() -> i64:\n  yield 3.\nend\nrule worker1() -> i64:\n  yield 5.\nend\nrule worker2() -> i64:\n  yield 7.\nend\nrule worker3() -> i64:\n  yield 11.\nend\nrule worker4() -> i64:\n  yield 13.\nend\nrule worker5() -> i64:\n  yield 17.\nend\nrule worker6() -> i64:\n  yield 19.\nend\nrule worker7() -> i64:\n  yield 23.\nend\nrule main() -> i64:\n  set t0: Thread = std::thread::spawn(\"worker0\").\n  set t1: Thread = std::thread::spawn(\"worker1\").\n  set t2: Thread = std::thread::spawn(\"worker2\").\n  set t3: Thread = std::thread::spawn(\"worker3\").\n  set t4: Thread = std::thread::spawn(\"worker4\").\n  set t5: Thread = std::thread::spawn(\"worker5\").\n  set t6: Thread = std::thread::spawn(\"worker6\").\n  set t7: Thread = std::thread::spawn(\"worker7\").\n  set thread_sum: i64 = std::thread::join(t0) + std::thread::join(t1) + std::thread::join(t2) + std::thread::join(t3) + std::thread::join(t4) + std::thread::join(t5) + std::thread::join(t6) + std::thread::join(t7).\n  set ch: ChannelI64 = std::channel::i64().\n  set i: i64 = 0.\n  repeat while i < 32:\n    set sent: bool = ch::send(i).\n    when sent:\n      put i = i + 1.\n    otherwise:\n      yield -1.\n    end\n  end\n  ch::close().\n  set channel_sum: i64 = 0.\n  set read: i64 = 0.\n  repeat while read < 32:\n    set msg: RecvI64 = ch::recv().\n    match msg:\n      case RecvI64::Ok(v):\n        put channel_sum = channel_sum + v.\n      case RecvI64::Closed:\n        yield -2.\n      otherwise:\n        yield -3.\n    end\n    put read = read + 1.\n  end\n  set tail: RecvI64 = ch::recv().\n  match tail:\n    case RecvI64::Ok(unused):\n      yield -4.\n    case RecvI64::Closed:\n      yield thread_sum + channel_sum + 1.\n    otherwise:\n      yield -5.\n  end\nend\n";
        let result = eval_source(source);
        assert_eq!(result, 595);
    }

    #[test]
    fn eval_net_tcp_roundtrip() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set line: string = std::net::read_line(stream).\n  set echoed: i64 = std::net::write_text(stream, std::string::concat(line, \"\\n\")).\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  yield echoed.\nend\n\nrule client(port: i64) -> i64:\n  set ignored: i64 = std::time::sleep_ms(30).\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set stream: TcpStream = std::net::connect(addr).\n  set wrote: i64 = std::net::write_text(stream, \"ping\\n\").\n  set recv: string = std::net::read_line(stream).\n  std::net::close_stream(stream).\n  when std::string::eq(recv, \"ping\"):\n    yield wrote.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set server_thread: Thread = std::thread::spawn(\"server\", port).\n  set client_bytes: i64 = client(port).\n  set server_bytes: i64 = std::thread::join(server_thread).\n  yield client_bytes + server_bytes + 1.\nend\n"
        );
        let result = eval_source(&source);
        assert_eq!(result, 11);
    }

    #[test]
    fn eval_net_pool_reuses_connection() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set line1: string = std::net::read_line(stream).\n  set echoed1: i64 = std::net::write_text(stream, std::string::concat(line1, \"\\n\")).\n  set line2: string = std::net::read_line(stream).\n  set echoed2: i64 = std::net::write_text(stream, std::string::concat(line2, \"\\n\")).\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  yield echoed1 + echoed2.\nend\n\nrule client(port: i64) -> i64:\n  set ignored: i64 = std::time::sleep_ms(30).\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set pool: TcpPool = std::net::pool(addr, 1).\n  set s1: TcpStream = std::net::pool_get(pool).\n  set w1: i64 = std::net::write_text(s1, \"a\\n\").\n  set r1: string = std::net::read_line(s1).\n  set keep1: bool = std::net::pool_put(pool, s1).\n  set s2: TcpStream = std::net::pool_get(pool).\n  set w2: i64 = std::net::write_text(s2, \"b\\n\").\n  set r2: string = std::net::read_line(s2).\n  set keep2: bool = std::net::pool_put(pool, s2).\n  std::net::pool_close(pool).\n  when std::string::eq(r1, \"a\") && std::string::eq(r2, \"b\") && keep1 && keep2:\n    yield w1 + w2.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set client_bytes: i64 = client(port).\n  set server_bytes: i64 = std::thread::join(t).\n  yield client_bytes + server_bytes + 1.\nend\n"
        );
        let result = eval_source(&source);
        assert_eq!(result, 9);
    }

    #[test]
    fn eval_std_http_get_roundtrip() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::http.\nimport std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set req: string = std::net::read_line(stream).\n  set sink: i64 = 0.\n  set reading: bool = true.\n  repeat while reading:\n    set line: string = std::net::read_line(stream).\n    when std::string::len(line) == 0:\n      put reading = false.\n    otherwise:\n      put sink = sink + std::string::len(line).\n    end\n  end\n  set ignored: i64 = std::net::write_text(stream, \"HTTP/1.1 200 OK\\n\").\n  put ignored = std::net::write_text(stream, \"cOnTeNt-LeNgTh: 11\\n\").\n  put ignored = std::net::write_text(stream, \"X-Test: 1\\n\").\n  put ignored = std::net::write_text(stream, \"\\n\").\n  put ignored = std::net::write_text(stream, \"hello\\nworld\").\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  when std::string::eq(req, \"GET /ping HTTP/1.1\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set url0: string = std::string::concat(\"http://127.0.0.1:\", std::string::from_i64(port)).\n  set url: string = std::string::concat(url0, \"/ping\").\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  set done: i64 = std::thread::join(t).\n  when done == 1 && status == 200 && std::string::contains(headers, \"X-Test: 1\") && std::string::eq(body, \"hello\\nworld\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
        );
        let result = eval_module_source(&source, "http_get_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_get_chunked_roundtrip() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::http.\nimport std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set req: string = std::net::read_line(stream).\n  set sink: i64 = 0.\n  set reading: bool = true.\n  repeat while reading:\n    set line: string = std::net::read_line(stream).\n    when std::string::len(line) == 0:\n      put reading = false.\n    otherwise:\n      put sink = sink + std::string::len(line).\n    end\n  end\n  set ignored: i64 = std::net::write_text(stream, \"HTTP/1.1 200 OK\\n\").\n  put ignored = std::net::write_text(stream, \"tRaNsFeR-EnCoDiNg: ChUnKeD\\n\").\n  put ignored = std::net::write_text(stream, \"X-Test: chunked\\n\").\n  put ignored = std::net::write_text(stream, \"\\n\").\n  put ignored = std::net::write_text(stream, \"5\\n\").\n  put ignored = std::net::write_text(stream, \"hello\\n\").\n  put ignored = std::net::write_text(stream, \"6\\n\").\n  put ignored = std::net::write_text(stream, \" world\\n\").\n  put ignored = std::net::write_text(stream, \"0\\n\").\n  put ignored = std::net::write_text(stream, \"\\n\").\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  when std::string::eq(req, \"GET /chunk HTTP/1.1\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set url0: string = std::string::concat(\"http://127.0.0.1:\", std::string::from_i64(port)).\n  set url: string = std::string::concat(url0, \"/chunk\").\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  set done: i64 = std::thread::join(t).\n  when done == 1 && status == 200 && std::string::contains(headers, \"tRaNsFeR-EnCoDiNg: ChUnKeD\") && std::string::eq(body, \"hello world\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
        );
        let result = eval_module_source(&source, "http_get_chunked_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_get_fallback_read_to_end_preserves_body() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::http.\nimport std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set req: string = std::net::read_line(stream).\n  set sink: i64 = 0.\n  set reading: bool = true.\n  repeat while reading:\n    set line: string = std::net::read_line(stream).\n    when std::string::len(line) == 0:\n      put reading = false.\n    otherwise:\n      put sink = sink + std::string::len(line).\n    end\n  end\n  set ignored: i64 = std::net::write_text(stream, \"HTTP/1.1 200 OK\\n\").\n  put ignored = std::net::write_text(stream, \"X-Test: fallback\\n\").\n  put ignored = std::net::write_text(stream, \"\\n\").\n  put ignored = std::net::write_text(stream, \"line1\\n\\nline3\\n\").\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  when std::string::eq(req, \"GET /fallback HTTP/1.1\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set url0: string = std::string::concat(\"http://127.0.0.1:\", std::string::from_i64(port)).\n  set url: string = std::string::concat(url0, \"/fallback\").\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  set done: i64 = std::thread::join(t).\n  when done == 1 && status == 200 && std::string::contains(headers, \"X-Test: fallback\") && std::string::eq(body, \"line1\\n\\nline3\\n\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
        );
        let result = eval_module_source(&source, "http_get_fallback_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_post_roundtrip() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::http.\nimport std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set req: string = std::net::read_line(stream).\n  set sink: i64 = 0.\n  set reading: bool = true.\n  repeat while reading:\n    set line: string = std::net::read_line(stream).\n    when std::string::len(line) == 0:\n      put reading = false.\n    otherwise:\n      put sink = sink + std::string::len(line).\n    end\n  end\n  set payload: string = std::net::read_exact(stream, 4).\n  set ignored: i64 = std::net::write_text(stream, \"HTTP/1.1 201 Created\\n\").\n  put ignored = std::net::write_text(stream, \"Content-Length: 2\\n\").\n  put ignored = std::net::write_text(stream, \"\\n\").\n  put ignored = std::net::write_text(stream, \"ok\").\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  when std::string::eq(req, \"POST /submit HTTP/1.1\") && std::string::eq(payload, \"ping\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set url0: string = std::string::concat(\"http://127.0.0.1:\", std::string::from_i64(port)).\n  set url: string = std::string::concat(url0, \"/submit\").\n  set response: string = std::http::post(url, \"ping\").\n  set status: i64 = std::http::status(response).\n  set body: string = std::http::body(response).\n  set done: i64 = std::thread::join(t).\n  when done == 1 && status == 201 && std::string::eq(body, \"ok\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
        );
        let result = eval_module_source(&source, "http_post_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_status_invalid_response_errors() {
        let source = "import std::http.\n\nrule main() -> i64:\n  set bad: string = \"NOT_HTTP\\n\\nbody\".\n  yield std::http::status(bad).\nend\n";
        let program = parse_program_with_modules(source, "http_invalid_vm");
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0404");
    }

    #[test]
    fn eval_std_http_helpers_accept_crlf_response() {
        let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set p0: string = std::string::concat(\"HTTP/1.1 200 OK\", eol).\n  set p1: string = std::string::concat(\"X-Test: 1\", eol).\n  set p2: string = std::string::concat(eol, \"hello\").\n  set response: string = std::string::concat(std::string::concat(p0, p1), p2).\n  set st: i64 = std::http::status(response).\n  set hs: string = std::http::headers(response).\n  set bd: string = std::http::body(response).\n  when st == 200 && std::string::contains(hs, \"X-Test: 1\") && std::string::eq(bd, \"hello\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
        let result = eval_module_source(source, "http_crlf_helpers_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_helpers_accept_crlf_response_with_utf8_body() {
        let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set body_bytes: u8[] = [104, 195, 169, 108, 108, 111].\n  set body_text: string = std::string::from_bytes(body_bytes).\n  set p0: string = std::string::concat(\"HTTP/1.1 200 OK\", eol).\n  set p1: string = std::string::concat(\"X-Test: 1\", eol).\n  set p2: string = std::string::concat(eol, body_text).\n  set response: string = std::string::concat(std::string::concat(p0, p1), p2).\n  set st: i64 = std::http::status(response).\n  set hs: string = std::http::headers(response).\n  set bd: string = std::http::body(response).\n  when st == 200 && std::string::contains(hs, \"X-Test: 1\") && std::string::eq(bd, body_text):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
        let result = eval_module_source(source, "http_crlf_helpers_utf8_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_build_request_uses_crlf_and_exact_post_length() {
        let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set req: string = std::http::build_request(\"POST\", \"http://example.com/submit\", \"ping\").\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set line_ok: bool = std::string::contains(req, std::string::concat(\"POST /submit HTTP/1.1\", eol)).\n  set host_ok: bool = std::string::contains(req, std::string::concat(\"Host: example.com\", eol)).\n  set len_ok: bool = std::string::contains(req, std::string::concat(\"Content-Length: 4\", eol)).\n  set body_ok: bool = std::string::contains(req, std::string::concat(std::string::concat(eol, eol), \"ping\")).\n  set old_len: bool = std::string::contains(req, \"Content-Length: 5\").\n  set old_lf: bool = std::string::contains(req, \"\\n\\nping\").\n  when old_len || old_lf:\n    yield -1.\n  otherwise:\n    when line_ok && host_ok && len_ok && body_ok:\n      yield 1.\n    otherwise:\n      yield -1.\n    end\n  end\nend\n";
        let result = eval_module_source(source, "http_build_request_crlf_vm");
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_std_http_timeout_errors() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::http.\nimport std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set ignored: i64 = std::time::sleep_ms(120).\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  yield ignored.\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set t: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set url0: string = std::string::concat(\"http://127.0.0.1:\", std::string::from_i64(port)).\n  set url: string = std::string::concat(url0, \"/slow\").\n  set response: string = std::http::get_with_timeout(url, 20).\n  set done: i64 = std::thread::join(t).\n  yield done + std::string::len(response).\nend\n"
        );
        let program = parse_program_with_modules(&source, "http_timeout_vm");
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0408");
    }

    #[test]
    fn eval_net_connect_refused_errors() {
        let source = "import std::net.\n\nrule main() -> i64:\n  set stream: TcpStream = std::net::connect(\"127.0.0.1:1\").\n  std::net::close_stream(stream).\n  yield 0.\nend\n";
        let program = parse_program(&source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0408");
    }

    #[test]
    fn eval_net_read_timeout_errors() {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let source = format!(
            "import std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set ignored: i64 = std::time::sleep_ms(120).\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  yield 0.\nend\n\nrule main() -> i64:\n  set port: i64 = {port}.\n  set server_thread: Thread = std::thread::spawn(\"server\", port).\n  set ignored: i64 = std::time::sleep_ms(30).\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set stream: TcpStream = std::net::connect(addr).\n  set timeout: i64 = std::net::set_read_timeout_ms(stream, 20).\n  set line: string = std::net::read_line(stream).\n  std::net::close_stream(stream).\n  set done: i64 = std::thread::join(server_thread).\n  yield timeout + done + std::string::len(line).\nend\n"
        );
        let program = parse_program(&source);
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0408");
    }
}
