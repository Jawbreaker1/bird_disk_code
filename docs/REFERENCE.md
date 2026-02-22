# BirdDisk reference manual (LLM-friendly)

This is a concise, machine-readable summary of the v0.1 language behavior.
If there is a conflict, the repo-root `SPEC.md` is the source of truth.

## 1) File + module model
- Source files are `.bd`.
- Top-level items: `import`, `enum`, `book`, `rule`.
- Entry programs must define `rule main() -> i64:` in the entry file.
- Imported modules may omit `main`.

### Imports
- Syntax: `import std::module.` or `import app::module.`
- `std::` resolves to `stdlib/` (e.g. `import std::math.`).
- Non-stdlib modules resolve by:
  1) entry file directory
  2) project root (manifest dir or nearest ancestor containing `stdlib/`)
  3) dependency roots in `birddisk.json`
- Imported rules are namespaced under the module path.

### Qualified name resolution (`::`)
When the parser sees `name::member`:
1) If the fully-qualified rule exists (stdlib or imported module), call it.
2) Otherwise, if `name` is a local binding of a book type, treat it as field access or method call.
3) Otherwise it is unresolved (compile error).

## 1.1 Testing conventions (v0.x)
- Tests live under `tests/` and use separate files.
- Test rules are named `test_*` and return `void`.
- Failures should `throw` a string (directly or via `std::test`).
- Expected test file path mirrors the source file path under `tests/` with a `_test` suffix.
  - Example: `src/foo/bar.bd` → `tests/src/foo/bar_test.bd`.
- Per-rule enforcement (when `--require-tests` is enabled):
  - Free rule `rule add(...)` → test `rule test_add() -> void:`
  - Method `book Counter::inc` → test `rule test_Counter_inc() -> void:`
  - `main` and `init` are exempt.
  - If a file only contains exempt rules, no test file is required.
- A manifest can include `test_exclude` paths to skip test requirements for specific files or folders.

## 2) Types
Built-in types:
- `i64`, `f64`, `bool`, `string`, `u8`, `void`
- Arrays: `T[]`

Rules:
- No implicit casts.
- Explicit casts use `expr as type` and are limited to `i64 <-> f64` in v0.1.
- Float literals are `digits "." digits` (no exponent form).

## 3) Enums
```
enum Result:
  case Ok(value: i64).
  case Err(message: string).
end
```
- Variants have 0 or 1 payload.
- Construct with `Enum::Variant(value)` or `Enum::Variant()` for empty payload.

## 4) Books (objects)
```
book Counter:
  field value: i64.

  rule init(self: Counter, start: i64) -> Counter:
    put self::value = start.
    yield self.
  end
end
```
- Fields use `field name: type.`.
- Methods are `rule` inside the book.
- First parameter must be `self: BookName`.
- Construct with `new BookName(args)`.
- If `BookName::init(self, ...) -> BookName` exists, it is called automatically.

## 5) Statements (all end with `.`)

### set (binding)
- `set name: Type = expr.`
- `set name = expr.` (type inference)

### put (assignment)
- `put name = expr.`
- `put name[index] = expr.`
- `put obj::field = expr.`

### call statement
- `function(args).` is allowed only for `void` rules.

### yield (return)
- `yield expr.` exits the current rule.
- `void` rules must not yield a value.

### when/otherwise
```
when cond:
  ...
otherwise:
  ...
end
```
- `otherwise` is required.

### repeat while
```
repeat while cond:
  ...
end
```

### try/catch/throw
```
try:
  ...
catch message:
  ...
end

throw "message".
```
- Only explicit `throw` is catchable in v0.x.

### match/case
```
match expr:
  case Enum::Variant(value):
    ...
  otherwise:
    ...
end
```
- `otherwise` is required.

## 6) Expressions
- Literals: integers, floats, strings, `true`/`false`.
- Calls: `name(args)` or `obj::method(args)`.
- Member access: `obj::field`
- Array indexing: `xs[i]`
- Unary: `-`, `!`
- Binary: `+ - * / % == != < <= > >= && ||`
- Cast: `expr as type`

## 7) Arrays
- Array types: `T[]`
- `array(len)` requires explicit type: `set xs: i64[] = array(3).`
- Empty literal also requires type: `set xs: i64[] = [].`

## 8) Error codes (runtime)
- E0402: division/modulo by zero
- E0403: array index out of bounds
- E0404: uncaught throw
- E0405: thread runtime error (invalid/joined handle misuse)
- E0406: channel runtime error (invalid handle/kind mismatch)
- E0407: channel recv would block
- E0408: network runtime error (connect/listen/read/write/timeout/handle misuse)
- E0400: runtime error (OOM, invalid UTF-8, etc.)

See `docs/DIAGNOSTICS.md` for full error list.

## 9) Standard library (current)

### std::string
- `len(s: string) -> i64`
- `concat(a: string, b: string) -> string`
- `eq(a: string, b: string) -> bool`
- `bytes(s: string) -> u8[]`
- `from_bytes(bytes: u8[]) -> string`
- `to_i64(s: string) -> i64`
- `from_i64(value: i64) -> string`
- `slice(s: string, start: i64, len: i64) -> string`
- `index_of(s: string, sub: string) -> i64`
- `contains(s: string, sub: string) -> bool`
- `replace(s: string, from: string, to: string) -> string`

### std::bytes
- `len(bytes: u8[]) -> i64`
- `eq(a: u8[], b: u8[]) -> bool`
- `slice(bytes: u8[], start: i64, len: i64) -> u8[]`
- `index_of(bytes: u8[], value: i64) -> i64`
- `contains(bytes: u8[], value: i64) -> bool`

### std::io
- `print(s: string) -> void`
- `read_line() -> string`

### std::time
- `now_ms() -> i64`
- `sleep_ms(ms: i64) -> i64`

### std::profiler
- `uptime_ms() -> i64`
- `alloc_count() -> i64`
- `bytes_allocated() -> i64`
- `bytes_in_use() -> i64`
- `peak_bytes_in_use() -> i64`
- `gc_runs() -> i64`
- `last_freed() -> i64`
- `last_live() -> i64`
- `last_freed_bytes() -> i64`
- `last_live_bytes() -> i64`

### std::rand
- `seed(value: i64) -> void`
- `range(min: i64, max: i64) -> i64`

### std::test
- `assert(cond: bool, msg: string) -> void`
- `assert_eq_i64(a: i64, b: i64, msg: string) -> void`
- `assert_eq_bool(a: bool, b: bool, msg: string) -> void`
- `assert_eq_string(a: string, b: string, msg: string) -> void`

### std::fs
- `read_text(path: string) -> string`
- `write_text(path: string, text: string) -> i64`
- `read_bytes(path: string) -> u8[]`
- `write_bytes(path: string, bytes: u8[]) -> i64`

### std::path
- `join(left: string, right: string) -> string`
- `normalize(path: string) -> string`
- `basename(path: string) -> string`
- `dirname(path: string) -> string`

### std::env
- `args() -> string[]`
- `get(name: string) -> string`
- `set_var(name: string, value: string) -> i64`
- `cwd() -> string`
- `set_cwd(path: string) -> i64`

### std::json
- `encode_i64(value: i64) -> string`
- `encode_bool(value: bool) -> string`
- `encode_string(text: string) -> string`
- `decode_i64(text: string) -> i64`
- `decode_bool(text: string) -> bool`
- `decode_string(text: string) -> string`

### std::channel
- `i64() -> ChannelI64`
- `bool() -> ChannelBool`
- `f64() -> ChannelF64`
- `u8() -> ChannelU8`
- `string() -> ChannelString`
- `bytes() -> ChannelBytes`
- `ChannelI64::send(value: i64) -> bool`
- `ChannelI64::recv() -> RecvI64`
- `ChannelI64::close() -> void`
- `ChannelBool::send(value: bool) -> bool`
- `ChannelBool::recv() -> RecvBool`
- `ChannelBool::close() -> void`
- `ChannelF64::send(value: f64) -> bool`
- `ChannelF64::recv() -> RecvF64`
- `ChannelF64::close() -> void`
- `ChannelU8::send(value: u8) -> bool`
- `ChannelU8::recv() -> RecvU8`
- `ChannelU8::close() -> void`
- `ChannelString::send(value: string) -> bool`
- `ChannelString::recv() -> RecvString`
- `ChannelString::close() -> void`
- `ChannelBytes::send(value: u8[]) -> bool`
- `ChannelBytes::recv() -> RecvBytes`
- `ChannelBytes::close() -> void`

### std::thread
- `spawn(entry: string, args...) -> Thread` (VM + native; WASM rejects with E0325)
  - VM deterministic mode (`--deterministic`) runs spawned work in FIFO scheduler order.
  - In non-deterministic VM/native mode, host-OS threaded execution is enabled for `i64` return signatures with:
    - `()` and `(i64)` in VM + native
    - `(TcpStream, i64)` in native (stream ownership is moved into the child thread runtime)
  - Other signatures currently execute eagerly in the caller thread for compatibility with existing channel/reference behavior.
- `join(handle: Thread) -> i64` (VM + native; WASM rejects with E0325)

### std::net
- `connect(addr: string) -> TcpStream` (VM + native; WASM rejects with E0326)
- `listen(addr: string) -> TcpListener` (VM + native; WASM rejects with E0326)
- `listener_addr(listener: TcpListener) -> string` (VM + native; WASM rejects with E0326)
- `accept(listener: TcpListener) -> TcpStream` (VM + native; WASM rejects with E0326)
- `write_text(stream: TcpStream, text: string) -> i64` (VM + native; WASM rejects with E0326)
- `read_line(stream: TcpStream) -> string` (VM + native; WASM rejects with E0326)
- `read_exact(stream: TcpStream, len: i64) -> string` (VM + native; WASM rejects with E0326)
- `read_to_end(stream: TcpStream) -> string` (VM + native; WASM rejects with E0326)
- `set_read_timeout_ms(stream: TcpStream, timeout_ms: i64) -> i64` (0 disables timeout)
- `close_stream(stream: TcpStream) -> void`
- `close_listener(listener: TcpListener) -> void`
- `pool(addr: string, max_idle: i64) -> TcpPool` (VM + native; WASM rejects with E0326)
- `pool_get(pool: TcpPool) -> TcpStream`
- `pool_put(pool: TcpPool, stream: TcpStream) -> bool`
- `pool_close(pool: TcpPool) -> void`

### std::http (BirdDisk module)
- `request(method: string, url: string, body_text: string) -> string`
- `request_with_timeout(method: string, url: string, body_text: string, timeout_ms: i64) -> string`
- `get(url: string) -> string`
- `post(url: string, body_text: string) -> string`
- `get_with_timeout(url: string, timeout_ms: i64) -> string`
- `post_with_timeout(url: string, body_text: string, timeout_ms: i64) -> string`
- `status(response: string) -> i64`
- `headers(response: string) -> string`
- `body(response: string) -> string`
- Current scope:
  - Supports `http://` URLs.
  - Parses status line + headers, then prefers `Content-Length` for exact body reads.
  - Emits requests with CRLF (`\r\n`) line endings and HTTP header/body separator.
  - POST `Content-Length` is computed from `body_text` bytes only (no implicit trailing newline).
  - Header-name matching for `Content-Length` and `Transfer-Encoding` is case-insensitive.
  - Supports `Transfer-Encoding: chunked` decoding for chunked bodies.
  - `status`/`headers`/`body` helpers normalize CRLF/CR response text to LF.
  - If neither `Content-Length` nor chunked transfer encoding is present, falls back to exact EOF body reads.

### std::math (BirdDisk module)
- `add(a: i64, b: i64) -> i64`
- `sub(a: i64, b: i64) -> i64`
- `mul(a: i64, b: i64) -> i64`
- `div(a: i64, b: i64) -> i64`
- `mod(a: i64, b: i64) -> i64`
- `abs(value: i64) -> i64`
- `sign(value: i64) -> i64`
- `is_even(value: i64) -> bool`
- `clamp(value: i64, low: i64, high: i64) -> i64`
- `min(a: i64, b: i64) -> i64`
- `max(a: i64, b: i64) -> i64`
- `pow(base: i64, exp: i64) -> i64`
- `gcd(a: i64, b: i64) -> i64`
- `lcm(a: i64, b: i64) -> i64`

### std::array (BirdDisk module)
- `sum_i64(xs: i64[], len: i64) -> i64`
- `min_i64(xs: i64[], len: i64) -> i64`
- `max_i64(xs: i64[], len: i64) -> i64`
- `contains_i64(xs: i64[], len: i64, value: i64) -> bool`
- `index_of_i64(xs: i64[], len: i64, value: i64) -> i64`
- `count_i64(xs: i64[], len: i64, value: i64) -> i64`
