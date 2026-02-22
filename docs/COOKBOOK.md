# BirdDisk Cookbook (v0.1)

All examples must parse, typecheck, and run in the VM (golden).
Unless noted, they should also run in WASM + native.

---

## 1) Minimal main
```birddisk
rule main() -> i64:
  yield 0.
end
```

## 2) when / otherwise
```birddisk
rule main() -> i64:
  set x = 10.
  when x > 5:
    yield 1.
  otherwise:
    yield 2.
  end
end
```

## 3) repeat while
```birddisk
rule main() -> i64:
  set i = 0.
  set sum: i64 = 0.

  repeat while i < 10:
    put sum = sum + i.
    put i = i + 1.
  end

  yield sum.
end
```

## 4) Function call
```birddisk
rule add(a: i64, b: i64) -> i64:
  yield a + b.
end

rule main() -> i64:
  set x = add(2, 3).
  yield x.
end
```

## 5) Fibonacci (iterative)
```birddisk
rule main() -> i64:
  set n = 10.
  set a: i64 = 0.
  set b: i64 = 1.
  set i: i64 = 0.

  repeat while i < n:
    set next = a + b.
    put a = b.
    put b = next.
    put i = i + 1.
  end

  yield a.
end
```

## 6) Arrays
```birddisk
rule main() -> i64:
  set xs: i64[] = array(3).
  put xs[0] = 2.
  put xs[1] = 3.
  put xs[2] = 5.
  yield xs[0] + xs[1] + xs[2].
end
```

## 7) Objects (book)
Books can live in imported modules; module files may contain only books or helper rules.
```birddisk
book Counter:
  field value: i64.

  rule init(self: Counter, start: i64) -> Counter:
    put self::value = start.
    yield self.
  end

  rule add(self: Counter, delta: i64) -> i64:
    put self::value = self::value + delta.
    yield self::value.
  end
end

rule main() -> i64:
  set c: Counter = new Counter(10).
  yield c::add(5).
end
```

## 8) Math (std::math)
```birddisk
import std::math.

rule main() -> i64:
  set base: i64 = 2.
  set power: i64 = std::math::pow(base, 5).
  set reduced: i64 = std::math::div(power, 4).
  set rem: i64 = std::math::mod(reduced, 7).
  set g: i64 = std::math::gcd(48, 18).
  yield std::math::add(rem, g).
end
```

## 9) Strings (std::string)
```birddisk
import std::string.

rule main() -> i64:
  set name: string = "Bird".
  set suffix: string = "Disk".
  set full: string = std::string::concat(name, suffix).
  yield std::string::len(full).
end
```

## 10) String to i64
```birddisk
import std::string.

rule main() -> i64:
  set value: i64 = std::string::to_i64("42").
  set text: string = std::string::from_i64(value).
  yield std::string::len(text).
end
```

```birddisk
import std::string.

rule main() -> i64:
  set text: string = "banana".
  set part: string = std::string::slice(text, 1, 3).
  set idx: i64 = std::string::index_of(text, "na").
  set ok: bool = std::string::contains(text, "nan").
  set replaced: string = std::string::replace(text, "na", "NA").
  when std::string::eq(part, "ana") && idx == 2 && ok && std::string::eq(replaced, "baNANA"):
    yield 1.
  otherwise:
    yield 0.
  end
end
```

```birddisk
import std::bytes.

rule main() -> i64:
  set xs: u8[] = [1, 2, 3, 4, 2].
  set part: u8[] = std::bytes::slice(xs, 1, 3).
  set ok_part: bool = std::bytes::eq(part, [2, 3, 4]).
  set idx: i64 = std::bytes::index_of(xs, 2).
  set has: bool = std::bytes::contains(xs, 5).
  when ok_part && idx == 1 && has == false:
    yield 1.
  otherwise:
    yield 0.
  end
end
```

## 11) JSON (std::json)
```birddisk
import std::json.
import std::string.

rule main() -> i64:
  set encoded: string = std::json::encode_string("hi\n").
  set decoded: string = std::json::decode_string(encoded).
  when std::string::eq(decoded, "hi\n"):
    yield 1.
  otherwise:
    yield 0.
  end
end
```

## 12) Bytes (u8[])
```birddisk
import std::string.
import std::bytes.

rule main() -> i64:
  set data: u8[] = std::string::bytes("hi").
  yield std::bytes::len(data).
end
```

## 13) Bytes to string
```birddisk
import std::string.

rule main() -> i64:
  set data: u8[] = [66, 105, 114, 100].
  set text: string = std::string::from_bytes(data).
  yield std::string::len(text).
end
```

## 14) IO (read + print)
```birddisk
import std::io.
import std::string.

rule main() -> i64:
  set line: string = std::io::read_line().
  set out: string = std::string::concat(line, "!").
  std::io::print(out).
  yield std::string::len(out).
end
```

## 15) Time (std::time)
```birddisk
import std::time.

rule main() -> i64:
  set t1: i64 = std::time::now_ms().
  set slept: i64 = std::time::sleep_ms(1).
  set t2: i64 = std::time::now_ms().
  when slept == 1:
    when t2 >= t1:
      yield 1.
    otherwise:
      yield 0.
    end
  otherwise:
    yield 0.
  end
end
```

## 16) Random (std::rand)
```birddisk
import std::rand.

rule main() -> i64:
  std::rand::seed(123).
  set a: i64 = std::rand::range(0, 10).
  set b: i64 = std::rand::range(0, 10).
  when a == 4 && b == 7:
    yield 1.
  otherwise:
    yield 0.
  end
end
```

## 17) Testing helpers (std::test)
```birddisk
import std::test.
import std::string.

rule test_add() -> void:
  std::test::assert_eq_i64(2 + 3, 5, "2 + 3 should be 5").
end

rule test_strings() -> void:
  set left: string = "hi".
  set right: string = std::string::concat("h", "i").
  std::test::assert_eq_string(left, right, "strings should match").
end
```

## 18) Files (std::fs)
```birddisk
import std::fs.
import std::string.

rule main() -> i64:
  set text: string = std::fs::read_text("data/input.txt").
  yield std::string::len(text).
end
```

## 19) Paths (std::path)
```birddisk
import std::path.
import std::string.

rule main() -> i64:
  set joined: string = std::path::join("alpha", "beta").
  set name: string = std::path::basename(joined).
  when std::string::eq(name, "beta"):
    yield 1.
  otherwise:
    yield 0.
  end
end
```

## 20) Environment (std::env)
```birddisk
import std::env.
import std::string.

rule main() -> i64:
  set name: string = std::env::get("USER").
  when std::string::len(name) > 0:
    yield 1.
  otherwise:
    yield 0.
  end
end
```
Pass program args after `--`:
```sh
./target/debug/birddiskc run examples/main.bd --json -- alpha beta
```

## 21) Error handling (try/catch/throw)
```birddisk
import std::io.
import std::string.

rule safe_div(divisor: i64) -> i64:
  when divisor == 0:
    throw "division by zero".
  otherwise:
    yield 100 / divisor.
  end
end

rule main() -> i64:
  try:
    set value: i64 = safe_div(0).
    std::io::print(std::string::concat("value=", std::string::from_i64(value))).
    std::io::print("\n").
    yield 0.
  catch message:
    std::io::print(std::string::concat("error: ", message)).
    std::io::print("\n").
    yield 1.
  end
end
```

## 22) Native AOT (emit exe)
```birddisk
rule main() -> i64:
  yield 0.
end
```
Build a native executable on the host:
```sh
./target/debug/birddiskc run path/to/file.bd --engine native --emit exe --out ./bird_app
./bird_app
```
Optional reporting:
- `BIRDDISK_JSON=1 ./bird_app` prints a JSON run report (includes `result` + `stdout`).
- `BIRDDISK_RESULT=1 ./bird_app` prints the return value to stderr.

Example runtime error JSON with trace:
```birddisk
rule boom() -> i64:
  set xs: i64[] = [1].
  yield xs[2].
end

rule main() -> i64:
  yield boom().
end
```
```sh
./target/debug/birddiskc run path/to/file.bd --engine native --emit exe --out ./trace_app
BIRDDISK_JSON=1 ./trace_app
```
Test note (optional):
```sh
BIRDDISK_RUN_NATIVE_AOT_TEST=1 cargo test -p birddiskc --bin birddiskc native_aot_json_trace_smoke
```

## 23) Large example (Yahtzee)
See `examples/yahtzee/` for a multi-file terminal demo.
```sh
cargo run -p birddiskc -- run examples/yahtzee/main.bd --engine vm --json --stdin examples/yahtzee/demo.stdin
```

## 24) Enums and match
```birddisk
enum Result:
  case Ok(value: i64).
  case Err(message: string).
end

rule main() -> i64:
  set r: Result = Result::Ok(7).
  match r:
    case Result::Ok(value):
      yield value.
    case Result::Err(message):
      yield 0.
    otherwise:
      yield 0.
  end
end
```
See `examples/enum_result.bd` for a runnable file example.

## 25) Floats (f64)
```birddisk
rule main() -> i64:
  set a: f64 = 1.5.
  set b: f64 = 2.25.
  set c: f64 = a + b.
  when c == 3.75:
    yield 1.
  otherwise:
    yield 0.
  end
end
```
See `tests/floats/float_basic.bd` for a runnable file example.

## 26) Explicit casts (i64 <-> f64)
```birddisk
rule main() -> i64:
  set a: i64 = 5.
  set b: f64 = a as f64.
  set c: i64 = b as i64.
  when c == 5:
    yield 1.
  otherwise:
    yield 0.
  end
end
```

## 27) TCP loopback with std::net (VM/native)
Runnable file: `examples/net_tcp_echo.bd` (single-process loopback with ephemeral bind + `listener_addr`: listen/connect/accept/read/write/close).
```sh
cargo run -p birddiskc -- run examples/net_tcp_echo.bd --engine vm --json
cargo run -p birddiskc -- run examples/net_tcp_echo.bd --engine native --json
```
Note: requires localhost socket bind permissions.

## 28) HTTP helpers with std::http
Runnable file: `examples/http_local_get.bd` (demonstrates `build_request`, `status`, `headers`, and `body` with CRLF response text).
```sh
cargo run -p birddiskc -- run examples/http_local_get.bd --engine vm --json
cargo run -p birddiskc -- run examples/http_local_get.bd --engine native --json
```

## 29) Threaded TCP echo with std::thread + std::net
Runnable file: `examples/net_thread_echo.bd` (spawns a TCP server rule, then connects from main and validates roundtrip + non-blocking spawn timing).
```sh
cargo run -p birddiskc -- run examples/net_thread_echo.bd --engine vm --json
cargo run -p birddiskc -- run examples/net_thread_echo.bd --engine native --json
```

## 30) Minimal web server (native-friendly)
Runnable file: `examples/web_server_simple/main.bd` (serves linked HTML pages plus `/style.css` and `/app.js`, handles multiple requests, and stops on `/shutdown`).
Static files are loaded from `examples/web_server_simple/public/`.
```sh
# optional: configure settings (key value entries)
cat examples/web_server_simple/web_server_simple.conf
# host 127.0.0.1
# port 18080
# max_requests 200

# terminal 1
cargo run -p birddiskc -- run examples/web_server_simple/main.bd --engine native

# terminal 2
curl -i http://127.0.0.1:18080/hello
curl -i http://127.0.0.1:18080/
curl -i http://127.0.0.1:18080/features
curl -i http://127.0.0.1:18080/about
curl -i http://127.0.0.1:18080/style.css
curl -i http://127.0.0.1:18080/app.js
curl -i http://127.0.0.1:18080/api/status
curl -i http://127.0.0.1:18080/health
curl -i http://127.0.0.1:18080/shutdown

# automated native smoke checks
./scripts/web_server_simple_smoke.sh

# config validation/error-path checks
./scripts/web_server_simple_config_errors.sh
```

---

Notes:
- Division or modulo by zero is a runtime error.
- Array index out of bounds is a runtime error.
