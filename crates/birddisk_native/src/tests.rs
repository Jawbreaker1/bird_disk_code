use crate::{emit_object, run, run_with_io};
use birddisk_core::{attach_sources, lexer, parse_and_typecheck, parser};
use std::fs;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::thread::JoinHandle;
use std::time::Duration;

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(1);

fn parse_program(source: &str) -> birddisk_core::ast::Program {
    let tokens = lexer::lex(source).unwrap();
    let mut program = parser::parse(&tokens).unwrap();
    attach_sources(&mut program, "<memory>", source);
    program
}

fn repo_root() -> PathBuf {
    let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    root.pop();
    root.pop();
    root
}

fn write_repo_temp(name: &str, source: &str) -> PathBuf {
    let mut path = repo_root();
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

fn run_source(source: &str) -> i64 {
    let program = parse_program(source);
    run(&program).unwrap()
}

fn run_module_source(source: &str, name: &str) -> i64 {
    let program = parse_program_with_modules(source, name);
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

fn run_module_source_error(source: &str, name: &str) -> crate::NativeError {
    let program = parse_program_with_modules(source, name);
    run_with_io(&program, "", &[]).unwrap_err()
}

fn emit_source(source: &str) -> Vec<u8> {
    let program = parse_program(source);
    emit_object(&program).unwrap()
}

fn spawn_tcp_server_once<F>(handler: F) -> Option<(u16, JoinHandle<()>)>
where
    F: FnOnce(TcpStream) + Send + 'static,
{
    let listener = TcpListener::bind("127.0.0.1:0").ok()?;
    let port = listener.local_addr().ok()?.port();
    let handle = std::thread::spawn(move || {
        let (stream, _) = listener.accept().expect("server accept failed");
        handler(stream);
    });
    Some((port, handle))
}

fn trimmed_line(reader: &mut BufReader<TcpStream>) -> String {
    let mut line = String::new();
    let _ = reader.read_line(&mut line);
    line.trim_end_matches(['\r', '\n']).to_string()
}

fn free_tcp_port() -> Option<u16> {
    let listener = TcpListener::bind("127.0.0.1:0").ok()?;
    Some(listener.local_addr().ok()?.port())
}

fn spawn_delayed_echo_client(port: u16, delay: Duration) -> JoinHandle<()> {
    std::thread::spawn(move || {
        std::thread::sleep(delay);
        let addr = format!("127.0.0.1:{port}");
        let mut connected = None;
        for _ in 0..200 {
            match TcpStream::connect(&addr) {
                Ok(stream) => {
                    connected = Some(stream);
                    break;
                }
                Err(_) => std::thread::sleep(Duration::from_millis(10)),
            }
        }
        let mut stream =
            connected.unwrap_or_else(|| panic!("client failed to connect to {addr}"));
        stream
            .set_read_timeout(Some(Duration::from_secs(3)))
            .expect("client read timeout");
        stream.write_all(b"ping\n").expect("client write failed");
        stream.flush().expect("client flush failed");
        let mut reader = BufReader::new(stream);
        let line = trimmed_line(&mut reader);
        assert_eq!(line, "ping");
    })
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
    let result = run_source("rule main() -> i64:\n  set s: string = \"hi\".\n  yield 1.\nend\n");
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
fn native_runs_thread_spawn_join() {
    let source = "import std::thread.\nrule worker(value: i64) -> i64:\n  yield value + 2.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\", 5).\n  yield std::thread::join(t).\nend\n";
    let result = run_source(source);
    assert_eq!(result, 7);
}

#[test]
fn native_runs_thread_net_server_roundtrip_i64_arg() {
    for _ in 0..3 {
        let Some(port) = free_tcp_port() else {
            return;
        };
        let client = spawn_delayed_echo_client(port, Duration::from_millis(180));
        let spawn_budget_ms = 120;
        let source = format!(
            "import std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule server(port: i64) -> i64:\n  set addr: string = std::string::concat(\"127.0.0.1:\", std::string::from_i64(port)).\n  set listener: TcpListener = std::net::listen(addr).\n  set stream: TcpStream = std::net::accept(listener).\n  set line: string = std::net::read_line(stream).\n  set wrote: i64 = std::net::write_text(stream, std::string::concat(line, \"\\n\")).\n  std::net::close_stream(stream).\n  std::net::close_listener(listener).\n  yield wrote.\nend\n\nrule main() -> i64:\n  set before: i64 = std::time::now_ms().\n  set t: Thread = std::thread::spawn(\"server\", {port}).\n  set after: i64 = std::time::now_ms().\n  set done: i64 = std::thread::join(t).\n  set delta: i64 = after - before.\n  when done > 0 && delta >= 0 && delta < {spawn_budget_ms}:\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
        );
        let result = run_source(&source);
        assert_eq!(result, 1);
        client.join().expect("client join failed");
    }
}

#[test]
fn native_runs_thread_net_worker_stream_i64_args() {
    let source = "import std::net.\nimport std::string.\nimport std::thread.\nimport std::time.\n\nrule worker(stream: TcpStream, delay_ms: i64) -> i64:\n  set slept: i64 = std::time::sleep_ms(delay_ms).\n  set wrote: i64 = std::net::write_text(stream, \"ok\\n\").\n  std::net::close_stream(stream).\n  when slept >= 0:\n    yield wrote.\n  otherwise:\n    yield -1.\n  end\nend\n\nrule main() -> i64:\n  set listener: TcpListener = std::net::listen(\"127.0.0.1:0\").\n  set addr: string = std::net::listener_addr(listener).\n  set client: TcpStream = std::net::connect(addr).\n  set server: TcpStream = std::net::accept(listener).\n  set before: i64 = std::time::now_ms().\n  set t: Thread = std::thread::spawn(\"worker\", server, 180).\n  set after: i64 = std::time::now_ms().\n  set recv: string = std::net::read_line(client).\n  std::net::close_stream(client).\n  std::net::close_listener(listener).\n  set done: i64 = std::thread::join(t).\n  set delta: i64 = after - before.\n  when done > 0 && delta >= 0 && delta < 120 && std::string::eq(recv, \"ok\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 1);
}

#[test]
fn native_runs_thread_channel_roundtrip() {
    let source = "import std::thread.\nimport std::channel.\nrule worker(ch: ChannelI64) -> i64:\n  set ok: bool = ch::send(9).\n  when ok:\n    yield 1.\n  otherwise:\n    yield 0.\n  end\nend\n\nrule main() -> i64:\n  set ch: ChannelI64 = std::channel::i64().\n  set t: Thread = std::thread::spawn(\"worker\", ch).\n  set done: i64 = std::thread::join(t).\n  set recv: RecvI64 = ch::recv().\n  match recv:\n    case RecvI64::Ok(v):\n      yield done + v.\n    case RecvI64::Closed:\n      yield -1.\n    otherwise:\n      yield -2.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 10);
}

#[test]
fn native_runs_thread_channel_stress() {
    let source = "import std::channel.\nimport std::thread.\nrule worker0() -> i64:\n  yield 3.\nend\nrule worker1() -> i64:\n  yield 5.\nend\nrule worker2() -> i64:\n  yield 7.\nend\nrule worker3() -> i64:\n  yield 11.\nend\nrule worker4() -> i64:\n  yield 13.\nend\nrule worker5() -> i64:\n  yield 17.\nend\nrule worker6() -> i64:\n  yield 19.\nend\nrule worker7() -> i64:\n  yield 23.\nend\nrule main() -> i64:\n  set t0: Thread = std::thread::spawn(\"worker0\").\n  set t1: Thread = std::thread::spawn(\"worker1\").\n  set t2: Thread = std::thread::spawn(\"worker2\").\n  set t3: Thread = std::thread::spawn(\"worker3\").\n  set t4: Thread = std::thread::spawn(\"worker4\").\n  set t5: Thread = std::thread::spawn(\"worker5\").\n  set t6: Thread = std::thread::spawn(\"worker6\").\n  set t7: Thread = std::thread::spawn(\"worker7\").\n  set thread_sum: i64 = std::thread::join(t0) + std::thread::join(t1) + std::thread::join(t2) + std::thread::join(t3) + std::thread::join(t4) + std::thread::join(t5) + std::thread::join(t6) + std::thread::join(t7).\n  set ch: ChannelI64 = std::channel::i64().\n  set i: i64 = 0.\n  repeat while i < 32:\n    set sent: bool = ch::send(i).\n    when sent:\n      put i = i + 1.\n    otherwise:\n      yield -1.\n    end\n  end\n  ch::close().\n  set channel_sum: i64 = 0.\n  set read: i64 = 0.\n  repeat while read < 32:\n    set msg: RecvI64 = ch::recv().\n    match msg:\n      case RecvI64::Ok(v):\n        put channel_sum = channel_sum + v.\n      case RecvI64::Closed:\n        yield -2.\n      otherwise:\n        yield -3.\n    end\n    put read = read + 1.\n  end\n  set tail: RecvI64 = ch::recv().\n  match tail:\n    case RecvI64::Ok(unused):\n      yield -4.\n    case RecvI64::Closed:\n      yield thread_sum + channel_sum + 1.\n    otherwise:\n      yield -5.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 595);
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
    let err =
        run_source_error("rule main() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n");
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
fn native_thread_join_twice_errors() {
    let source = "import std::thread.\nrule worker() -> i64:\n  yield 3.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\").\n  set first: i64 = std::thread::join(t).\n  set second: i64 = std::thread::join(t).\n  yield first + second.\nend\n";
    let err = run_source_error(source);
    assert_eq!(err.code, Some("E0405"));
    assert_eq!(err.message, "Thread has already been joined.");
}

#[test]
fn native_channel_recv_would_block_errors() {
    let source =
        "import std::channel.\nrule main() -> i64:\n  set ch: ChannelI64 = std::channel::i64().\n  set value: RecvI64 = ch::recv().\n  match value:\n    case RecvI64::Ok(v):\n      yield v.\n    case RecvI64::Closed:\n      yield -1.\n    otherwise:\n      yield -2.\n  end\nend\n";
    let err = run_source_error(source);
    assert_eq!(err.code, Some("E0407"));
    assert_eq!(err.message, "Channel recv would block.");
}

#[test]
fn native_runs_net_listener_addr_roundtrip() {
    let source = "import std::net.\nimport std::string.\n\nrule main() -> i64:\n  set listener: TcpListener = std::net::listen(\"127.0.0.1:0\").\n  set addr: string = std::net::listener_addr(listener).\n  set client: TcpStream = std::net::connect(addr).\n  set server: TcpStream = std::net::accept(listener).\n  std::net::close_stream(client).\n  std::net::close_stream(server).\n  std::net::close_listener(listener).\n  when std::string::contains(addr, \"127.0.0.1:\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
    let result = run_source(source);
    assert_eq!(result, 1);
}

#[test]
fn native_runs_net_tcp_roundtrip() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let line = trimmed_line(&mut reader);
        assert_eq!(line, "ping");
        let mut stream = stream;
        stream.write_all(b"ping\n").expect("server write failed");
        stream.flush().expect("server flush failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::net.\nimport std::string.\n\nrule main() -> i64:\n  set addr: string = \"127.0.0.1:{port}\".\n  set stream: TcpStream = std::net::connect(addr).\n  set wrote: i64 = std::net::write_text(stream, \"ping\\n\").\n  set recv: string = std::net::read_line(stream).\n  std::net::close_stream(stream).\n  when wrote > 0 && std::string::eq(recv, \"ping\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_source(&source);
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_runs_net_pool_reuses_connection() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let mut stream = stream;
        let line1 = trimmed_line(&mut reader);
        assert_eq!(line1, "a");
        stream.write_all(b"a\n").expect("server write1 failed");
        stream.flush().expect("server flush1 failed");
        let line2 = trimmed_line(&mut reader);
        assert_eq!(line2, "b");
        stream.write_all(b"b\n").expect("server write2 failed");
        stream.flush().expect("server flush2 failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::net.\nimport std::string.\n\nrule main() -> i64:\n  set addr: string = \"127.0.0.1:{port}\".\n  set pool: TcpPool = std::net::pool(addr, 1).\n  set s1: TcpStream = std::net::pool_get(pool).\n  set w1: i64 = std::net::write_text(s1, \"a\\n\").\n  set r1: string = std::net::read_line(s1).\n  set keep1: bool = std::net::pool_put(pool, s1).\n  set s2: TcpStream = std::net::pool_get(pool).\n  set w2: i64 = std::net::write_text(s2, \"b\\n\").\n  set r2: string = std::net::read_line(s2).\n  set keep2: bool = std::net::pool_put(pool, s2).\n  std::net::pool_close(pool).\n  when w1 > 0 && w2 > 0 && keep1 && keep2 && std::string::eq(r1, \"a\") && std::string::eq(r2, \"b\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_source(&source);
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_runs_std_http_get_roundtrip() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let req = trimmed_line(&mut reader);
        assert_eq!(req, "GET /ping HTTP/1.1");
        loop {
            if trimmed_line(&mut reader).is_empty() {
                break;
            }
        }
        let mut stream = stream;
        stream
            .write_all(b"HTTP/1.1 200 OK\r\ncOnTeNt-LeNgTh: 11\r\nX-Test: 1\r\n\r\nhello\nworld")
            .expect("server write failed");
        stream.flush().expect("server flush failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set url: string = \"http://127.0.0.1:{port}/ping\".\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  when status == 200 && std::string::contains(headers, \"X-Test: 1\") && std::string::eq(body, \"hello\\nworld\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_module_source(&source, "http_get_native");
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_runs_std_http_get_chunked_roundtrip() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let req = trimmed_line(&mut reader);
        assert_eq!(req, "GET /chunk HTTP/1.1");
        loop {
            if trimmed_line(&mut reader).is_empty() {
                break;
            }
        }
        let mut stream = stream;
        stream
            .write_all(
                b"HTTP/1.1 200 OK\r\ntRaNsFeR-EnCoDiNg: ChUnKeD\r\nX-Test: chunked\r\n\r\n5\r\nhello\r\n6\r\n world\r\n0\r\n\r\n",
            )
            .expect("server write failed");
        stream.flush().expect("server flush failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set url: string = \"http://127.0.0.1:{port}/chunk\".\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  when status == 200 && std::string::contains(headers, \"tRaNsFeR-EnCoDiNg: ChUnKeD\") && std::string::eq(body, \"hello world\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_module_source(&source, "http_get_chunked_native");
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_runs_std_http_get_fallback_read_to_end_preserves_body() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let req = trimmed_line(&mut reader);
        assert_eq!(req, "GET /fallback HTTP/1.1");
        loop {
            if trimmed_line(&mut reader).is_empty() {
                break;
            }
        }
        let mut stream = stream;
        stream
            .write_all(b"HTTP/1.1 200 OK\r\nX-Test: fallback\r\n\r\nline1\n\nline3\n")
            .expect("server write failed");
        stream.flush().expect("server flush failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set url: string = \"http://127.0.0.1:{port}/fallback\".\n  set response: string = std::http::get(url).\n  set status: i64 = std::http::status(response).\n  set headers: string = std::http::headers(response).\n  set body: string = std::http::body(response).\n  when status == 200 && std::string::contains(headers, \"X-Test: fallback\") && std::string::eq(body, \"line1\\n\\nline3\\n\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_module_source(&source, "http_get_fallback_native");
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_runs_std_http_post_roundtrip() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        let mut reader = BufReader::new(stream.try_clone().expect("server clone failed"));
        let req = trimmed_line(&mut reader);
        assert_eq!(req, "POST /submit HTTP/1.1");
        loop {
            if trimmed_line(&mut reader).is_empty() {
                break;
            }
        }
        let mut payload = [0u8; 4];
        reader.read_exact(&mut payload).expect("server read payload failed");
        assert_eq!(&payload, b"ping");
        let mut stream = stream;
        stream
            .write_all(b"HTTP/1.1 201 Created\r\nContent-Length: 2\r\n\r\nok")
            .expect("server write failed");
        stream.flush().expect("server flush failed");
    }) else {
        return;
    };
    let source = format!(
        "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set url: string = \"http://127.0.0.1:{port}/submit\".\n  set response: string = std::http::post(url, \"ping\").\n  set status: i64 = std::http::status(response).\n  set body: string = std::http::body(response).\n  when status == 201 && std::string::eq(body, \"ok\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n"
    );
    let result = run_module_source(&source, "http_post_native");
    assert_eq!(result, 1);
    server.join().expect("server join failed");
}

#[test]
fn native_std_http_status_invalid_response_errors() {
    let source = "import std::http.\n\nrule main() -> i64:\n  set bad: string = \"NOT_HTTP\\n\\nbody\".\n  yield std::http::status(bad).\nend\n";
    let err = run_module_source_error(source, "http_invalid_native");
    assert_eq!(err.code, Some("E0404"));
}

#[test]
fn native_std_http_helpers_accept_crlf_response() {
    let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set p0: string = std::string::concat(\"HTTP/1.1 200 OK\", eol).\n  set p1: string = std::string::concat(\"X-Test: 1\", eol).\n  set p2: string = std::string::concat(eol, \"hello\").\n  set response: string = std::string::concat(std::string::concat(p0, p1), p2).\n  set st: i64 = std::http::status(response).\n  set hs: string = std::http::headers(response).\n  set bd: string = std::http::body(response).\n  when st == 200 && std::string::contains(hs, \"X-Test: 1\") && std::string::eq(bd, \"hello\"):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
    let result = run_module_source(source, "http_crlf_helpers_native");
    assert_eq!(result, 1);
}

#[test]
fn native_std_http_helpers_accept_crlf_response_with_utf8_body() {
    let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set body_bytes: u8[] = [104, 195, 169, 108, 108, 111].\n  set body_text: string = std::string::from_bytes(body_bytes).\n  set p0: string = std::string::concat(\"HTTP/1.1 200 OK\", eol).\n  set p1: string = std::string::concat(\"X-Test: 1\", eol).\n  set p2: string = std::string::concat(eol, body_text).\n  set response: string = std::string::concat(std::string::concat(p0, p1), p2).\n  set st: i64 = std::http::status(response).\n  set hs: string = std::http::headers(response).\n  set bd: string = std::http::body(response).\n  when st == 200 && std::string::contains(hs, \"X-Test: 1\") && std::string::eq(bd, body_text):\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
    let result = run_module_source(source, "http_crlf_helpers_utf8_native");
    assert_eq!(result, 1);
}

#[test]
fn native_std_http_build_request_uses_crlf_and_exact_post_length() {
    let source = "import std::http.\nimport std::string.\n\nrule main() -> i64:\n  set req: string = std::http::build_request(\"POST\", \"http://example.com/submit\", \"ping\").\n  set cr_code: u8 = 13.\n  set cr_bytes: u8[] = array(1).\n  put cr_bytes[0] = cr_code.\n  set cr: string = std::string::from_bytes(cr_bytes).\n  set eol: string = std::string::concat(cr, \"\\n\").\n  set line_ok: bool = std::string::contains(req, std::string::concat(\"POST /submit HTTP/1.1\", eol)).\n  set host_ok: bool = std::string::contains(req, std::string::concat(\"Host: example.com\", eol)).\n  set len_ok: bool = std::string::contains(req, std::string::concat(\"Content-Length: 4\", eol)).\n  set body_ok: bool = std::string::contains(req, std::string::concat(std::string::concat(eol, eol), \"ping\")).\n  set old_len: bool = std::string::contains(req, \"Content-Length: 5\").\n  set old_lf: bool = std::string::contains(req, \"\\n\\nping\").\n  when old_len || old_lf:\n    yield -1.\n  otherwise:\n    when line_ok && host_ok && len_ok && body_ok:\n      yield 1.\n    otherwise:\n      yield -1.\n    end\n  end\nend\n";
    let result = run_module_source(source, "http_build_request_crlf_native");
    assert_eq!(result, 1);
}

#[test]
fn native_std_web_helpers() {
    let source = "import std::web.\nimport std::string.\n\nrule main() -> i64:\n  set req: string = \"GET /features HTTP/1.1\".\n  set method: string = std::web::request_method(req).\n  set path: string = std::web::route_path(req).\n  set code: i64 = std::web::route_code(path).\n  set file_direct: string = std::web::route_file(path).\n  set file_code: string = std::web::route_file_from_code(code).\n  set css_type: string = std::web::content_type_for_file(\"style.css\").\n  set threaded_ok: bool = std::web::is_threaded_candidate(method, path).\n  set shutdown_threaded: bool = std::web::is_threaded_candidate(\"GET\", \"/shutdown\").\n  set unknown_file: string = std::web::route_file(\"/missing\").\n  set response: string = std::web::build_response(200, \"OK\", \"text/plain; charset=utf-8\", \"hi\").\n  set has_status: bool = std::string::contains(response, \"HTTP/1.1 200 OK\").\n  set has_len: bool = std::string::contains(response, \"Content-Length: 2\").\n  set has_body: bool = std::string::contains(response, \"hi\").\n  when std::string::eq(method, \"GET\") && std::string::eq(path, \"/features\") && code == 2 && std::string::eq(file_direct, \"features.html\") && std::string::eq(file_code, \"features.html\") && std::string::eq(css_type, \"text/css; charset=utf-8\") && threaded_ok && !shutdown_threaded && std::string::len(unknown_file) == 0 && has_status && has_len && has_body:\n    yield 1.\n  otherwise:\n    yield -1.\n  end\nend\n";
    let result = run_module_source(source, "web_helpers_native");
    assert_eq!(result, 1);
}

#[test]
fn native_std_http_timeout_errors() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        std::thread::sleep(Duration::from_millis(120));
        drop(stream);
    }) else {
        return;
    };
    let source = format!(
        "import std::http.\n\nrule main() -> i64:\n  set url: string = \"http://127.0.0.1:{port}/slow\".\n  set response: string = std::http::get_with_timeout(url, 20).\n  yield 0.\nend\n"
    );
    let err = run_module_source_error(&source, "http_timeout_native");
    assert_eq!(err.code, Some("E0408"));
    server.join().expect("server join failed");
}

#[test]
fn native_net_connect_refused_errors() {
    let source = "import std::net.\n\nrule main() -> i64:\n  set stream: TcpStream = std::net::connect(\"127.0.0.1:1\").\n  std::net::close_stream(stream).\n  yield 0.\nend\n";
    let err = run_source_error(&source);
    assert_eq!(err.code, Some("E0408"));
}

#[test]
fn native_net_read_timeout_errors() {
    let Some((port, server)) = spawn_tcp_server_once(|stream| {
        std::thread::sleep(Duration::from_millis(120));
        drop(stream);
    }) else {
        return;
    };
    let source = format!(
        "import std::net.\n\nrule main() -> i64:\n  set addr: string = \"127.0.0.1:{port}\".\n  set stream: TcpStream = std::net::connect(addr).\n  set ignored: i64 = std::net::set_read_timeout_ms(stream, 20).\n  set line: string = std::net::read_line(stream).\n  std::net::close_stream(stream).\n  yield 0.\nend\n"
    );
    let err = run_source_error(&source);
    assert_eq!(err.code, Some("E0408"));
    server.join().expect("server join failed");
}

#[test]
fn native_emits_object_bytes() {
    let obj = emit_source("rule main() -> i64:\n  yield 1.\nend\n");
    assert!(!obj.is_empty());
}
