# BirdDisk v0.1 — Specification (POC)

This spec is intentionally small and strict.
If something is not defined here, it is not part of v0.1.

## 1. Program structure
A program is a sequence of top-level items: imports, enum declarations, book
declarations, and function rules. Imports are top-level only.

Syntax:
- `import std::module.`

The entry point (when running a program) is:

- `rule main() -> i64: ... end`

Imported modules do not need a `main` rule and may contain only books or helper rules.

### 1.1 Imports
- `std::` imports resolve to the stdlib directory (`stdlib/`).
- Non-stdlib imports map `::` segments to path segments ending in `.bd`.
- Resolution order for non-stdlib modules:
  1) Relative to the entry file directory
  2) Relative to the project root (manifest directory if `birddisk.json` exists, otherwise nearest ancestor containing `stdlib/`)
  3) Dependency roots declared in `birddisk.json` (match the first segment of the import path)
- `birddisk.json` (project manifest) fields:
  - `name` (string)
  - `version` (string)
  - `entry` (path to the entry `.bd` file; defaults to `src/main.bd`)
  - `deps` (map of module name → entry). Each entry is either a path string or an object:
    `{ "path": "...", "version": "..." }`. Version is parsed but not used in v0.1.
    For `import util::math.`, resolution uses `<deps.util>/math.bd`. A root import
    `import util.` resolves to `<deps.util>/mod.bd` if present.
- Imported functions are namespaced under their module path
  (e.g. `import app::util.` defines `app::util::foo`).
- Book/type names are global in v0.1; avoid collisions across modules.
- Qualified names resolve as:
  - If the fully qualified rule name exists (stdlib or imported module), that rule is called.
  - Otherwise, if `name` is a local binding of a book type, `name::member` is a field access or method call (if followed by `()`).
  - Otherwise `name::member` is unresolved.
- Import aliases and wildcard imports are not supported in v0.1.

## 2. Types
Built-in types:
- `i64`
- `bool`
- `string`
- `u8` (unsigned 8-bit integer, 0..255)
- `void` (return type only)
- `T[]` array type (where `T` is a built-in type)

No implicit casts.
No floats in v0.1.

### 2.1 Enums
Enums declare a closed set of variants. Variants may optionally carry a single payload.

Syntax:
```
enum Result:
  case Ok(value: i64).
  case Err(message: string).
end
```

Rules:
- Enum names are global in v0.1 (avoid collisions with books).
- Variants may have zero or one payload (name + type).
- Construct variants using calls: `Result::Ok(1)` or `Result::Err("oops")`.
- Zero-payload variants use an empty call: `Result::None()`.

## 3. Names, scope, and shadowing
- Lexical, block-based scope.
- `set` introduces a new binding in the current scope.
- Shadowing is allowed in v0.1.

## 4. Functions
Syntax:
`rule <name>(<param>: <type>, ...) -> <type>: <stmts> end`

Rules:
- Parameter types are required.
- Return type is required.
- Functions are top-level only (no nesting) in v0.1.
- All control-flow paths must `yield` a value compatible with the declared return type, unless the
  return type is `void`.
- `void` functions must not `yield` a value.

## 5. Statements
All statements end with a `.` terminator, except block openers/closers.

### 5.1 Binding (`set`)
Syntax:
- `set name: Type = expr.`
- `set name = expr.` (type inference)

Rules:
- If `: Type` is present, `expr` must typecheck exactly as `Type`.
- If omitted, the compiler must infer the type of `expr`.
- If the type cannot be inferred unambiguously, it is a compile error (require explicit type annotation).

### 5.2 Assignment (`put`)
Syntax:
- `put name = expr.`
- `put name[index] = expr.`

Rules:
- `name` must resolve to an existing binding in scope.
- `expr` must typecheck to exactly the type of `name`.
- For indexed assignment, `name` must be an array, `index` must be `i64`,
  and `expr` must match the array element type.

### 5.3 Call statement
Syntax:
- `function(args).`

Rules:
- Call statements are only valid for functions returning `void`.
- Non-void calls must be used in expressions or assigned to bindings.

### 5.4 Return (`yield`)
Syntax:
- `yield expr.`

Rules:
- `expr` type must match the function return type exactly.
- `yield` exits the current function.

### 5.5 Conditional (`when/otherwise/end`)
Syntax:

when cond:
…
otherwise:
…
end

Rules:
- `cond` must be `bool`.
- `otherwise` is required in v0.1 (simplifies typing and reduces ambiguity).
- Both branches are separate lexical scopes.
- The typechecker must ensure function-level “all paths yield” (it is OK if one branch yields and the other yields; if one branch doesn’t yield, later statements can still yield).

### 5.6 Loop (`repeat while/end`)
Syntax:
repeat while cond:
…
end

Rules:
- `cond` must be `bool`.
- Body is a lexical scope.
- `yield` is allowed inside loops.

### 5.7 Error handling (`try/catch/throw`)
Syntax:
try:
…
catch name:
…
end

throw expr.

Rules:
- `throw` requires a `string` expression and raises a runtime error (E0404).
- `try` executes the try-body; if a `throw` occurs, control jumps to `catch`.
- `catch` binds the thrown message as `string` in a new lexical scope.
- Only explicit `throw` is catchable in v0.x; runtime traps still abort.

### 5.8 Match (`match/case/otherwise/end`)
Syntax:
```
match expr:
  case Result::Ok(value):
    yield value.
  case Result::Err(message):
    yield 0.
  otherwise:
    yield 0.
end
```

Rules:
- `match` evaluates `expr` once.
- `case` names must be `Enum::Variant` and may bind a payload name.
- `otherwise` is required in v0.1.
- Each case body is a new lexical scope; the bound name is available only inside its case.
- Case bindings are only allowed when the variant carries a payload.

## 6. Expressions
### 6.1 Literals
- Integer literal: `123` (type `i64`)
- Bool literal: `true`, `false` (type `bool`)
- String literal: `"hello"` (type `string`)
Notes:
- Integer literals may be used in `u8` contexts when within `0..255`.

### 6.2 Identifiers
An identifier expression resolves to the type of the bound name.

### 6.3 Unary operators
- `-x` requires `x: i64` and returns `i64`
- `!x` requires `x: bool` and returns `bool`

### 6.4 Binary operators
Arithmetic (i64, i64 -> i64):
- `+ - * / %`

Comparison (i64, i64 -> bool):
- `== != < <= > >=`

Boolean (bool, bool -> bool):
- `&& ||`

Notes:
- `==` and `!=` are only defined for `i64` in v0.1 to keep things simple.
- Division or modulo by zero is a runtime error.

### 6.5 Function calls
Syntax:
`name(arg1, arg2, ...)`

Rules:
- `name` must be a known function.
- Arity must match.
- Argument types must match exactly.
- Fully qualified names use `::` (e.g. `std::string::len(s)`).

### 6.6 Arrays
Syntax:
- Array literal: `[expr1, expr2, ...]` (or `[]`)
- Array constructor: `array(len)`
- Indexing: `expr[index]`

Rules:
- Full semantics are defined in section 8.

## 7. Type inference (v0.1)
Inference only applies to `set name = expr.` when `expr` has a well-defined type.

The type of `expr` is inferred by these rules:
- literals: `123 => i64`, `true/false => bool`, `"text" => string`
- unary ops: from operand rules
- binary ops: from operator rules
- function calls: from function signature return type
- identifier expressions: from binding type

If any step is ambiguous or unknown (e.g. unresolved name), inference fails and should surface as an error.

Notes:
- Empty array literals (`[]`) and `array(len)` require explicit array type context.

## 8. Arrays (v0.1)
BirdDisk supports fixed-length arrays in v0.1.

### 8.1 Types
- Array types use `T[]` where `T` is a built-in type or another array type.
- Nested arrays are allowed, e.g. `i64[][]`.
- Arrays have fixed length; elements are mutable via indexed assignment.

### 8.2 Array literals
Syntax:
- `[expr1, expr2, ...]`
- `[]`

Rules:
- All elements must have the same type.
- The array literal type is `T[]` where `T` is the element type.
- `[]` requires an explicit array type (e.g. `set xs: i64[] = [].`).

### 8.3 Array constructor
Syntax:
- `array(len)`

Rules:
- `len` must be `i64`.
- `array(len)` requires explicit array type context, e.g.:
  `set xs: i64[] = array(10).`
- The array is filled with default values (`0` for `i64`, `false` for `bool`).
- Negative lengths are runtime errors (E0400).

### 8.4 Indexing
Syntax:
- `xs[index]`
- `put xs[index] = expr.`

Rules:
- `xs` must be an array.
- `index` must be `i64`.
- The expression `xs[index]` has the element type of the array.
- `put xs[index] = expr.` requires `expr` to match the element type.
- Negative or out-of-bounds indices are runtime errors (E0403).

## 9. Strings (v0.1)
BirdDisk supports immutable UTF-8 strings in v0.1.

### 9.1 Literals
- String literals use double quotes: `"hello"`.
- Supported escapes: `\"`, `\\`, `\n`.
- String literals cannot span multiple lines.

### 9.2 std::string module
To use string operations, import the module:
- `import std::string.`

Functions:
- `std::string::len(s: string) -> i64` (byte length)
- `std::string::concat(a: string, b: string) -> string`
- `std::string::eq(a: string, b: string) -> bool`
- `std::string::bytes(s: string) -> u8[]`
- `std::string::from_bytes(bytes: u8[]) -> string` (validates UTF-8; invalid bytes are runtime errors)
- `std::string::to_i64(s: string) -> i64` (decimal, optional leading `-`; invalid or out-of-range input is a runtime error)
- `std::string::from_i64(value: i64) -> string`

### 9.3 std::json module
To use JSON helpers, import the module:
- `import std::json.`

Functions:
- `std::json::encode_i64(value: i64) -> string` (returns a JSON number)
- `std::json::encode_bool(value: bool) -> string` (returns `"true"` or `"false"`)
- `std::json::encode_string(text: string) -> string` (returns a JSON string literal)
- `std::json::decode_i64(text: string) -> i64` (trims ASCII whitespace)
- `std::json::decode_bool(text: string) -> bool` (trims ASCII whitespace)
- `std::json::decode_string(text: string) -> string` (trims ASCII whitespace)

Notes:
- `decode_string` supports escapes `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, and `\t`.
- `\u` escapes are not supported yet (runtime error).
- `encode_string` errors on control characters other than `\n`, `\r`, `\t`, `\b`, and `\f`.

## 10. Bytes (v0.1)
BirdDisk treats `u8[]` as a byte array.

### 10.1 std::bytes module
To use byte helpers, import the module:
- `import std::bytes.`

Functions:
- `std::bytes::len(bytes: u8[]) -> i64`
- `std::bytes::eq(a: u8[], b: u8[]) -> bool`

## 11. IO (v0.1)
BirdDisk exposes minimal IO through the stdlib.

### 11.1 std::io module
To use IO, import the module:
- `import std::io.`

Functions:
- `std::io::print(s: string) -> void`
  - Appends `s` to stdout without adding a newline.
  - Returns the byte length of `s`.
- `std::io::read_line() -> string`
  - Reads the next line from stdin without the trailing newline.
  - Returns an empty string on EOF.

## 12. Time (v0.1)
BirdDisk exposes minimal timing helpers through the stdlib.

### 12.1 std::time module
To use time helpers, import the module:
- `import std::time.`

Functions:
- `std::time::now_ms() -> i64`
  - Returns milliseconds elapsed since the program started (monotonic).
- `std::time::sleep_ms(ms: i64) -> i64`
  - Sleeps for `ms` milliseconds and returns the duration.
  - Negative values are runtime errors.

## 13. Files and Paths (v0.1)
BirdDisk exposes minimal filesystem and path helpers through the stdlib.

### 13.1 std::fs module
To use file helpers, import the module:
- `import std::fs.`

Functions:
- `std::fs::read_text(path: string) -> string`
  - Reads UTF-8 text from `path`.
  - Invalid UTF-8 or IO failures are runtime errors.
- `std::fs::write_text(path: string, text: string) -> i64`
  - Writes `text` to `path` and returns bytes written.
- `std::fs::read_bytes(path: string) -> u8[]`
  - Reads raw bytes from `path`.
- `std::fs::write_bytes(path: string, bytes: u8[]) -> i64`
  - Writes bytes to `path` and returns bytes written.

### 13.2 std::path module
To use path helpers, import the module:
- `import std::path.`

Functions:
- `std::path::join(left: string, right: string) -> string`
  - Joins `left` and `right` using the host platform separators.
- `std::path::normalize(path: string) -> string`
  - Removes `.` segments and resolves `..` lexically (no filesystem access).
  - Returns `.` when the result is empty.
- `std::path::basename(path: string) -> string`
  - Returns the final path segment or an empty string if none exists.
- `std::path::dirname(path: string) -> string`
  - Returns the parent path or `.` when no parent exists.

### 13.3 std::env module
To use environment helpers, import the module:
- `import std::env.`

Functions:
- `std::env::args() -> string[]`
  - Returns process arguments (excluding the executable name).
  - For `birddiskc run`, pass args after `--`.
- `std::env::get(name: string) -> string`
  - Returns the variable value or an empty string if missing.
  - Invalid UTF-8 is a runtime error.
- `std::env::set_var(name: string, value: string) -> i64`
  - Sets the variable and returns 1 on success.
  - Strings containing NUL are runtime errors.
- `std::env::cwd() -> string`
  - Returns the current working directory.
- `std::env::set_cwd(path: string) -> i64`
  - Sets the current working directory and returns 1 on success.

## 14. Stdlib layout
BirdDisk ships stdlib modules on disk for reusable logic.

- Stdlib modules live under `stdlib/` at the project root.
- Import paths map directly to files:
  - `import std::math.` resolves to `stdlib/std/math.bd`.
- Functions in stdlib modules are compiled with their module prefix
  (e.g. `std::math::add`).
- `std::string`, `std::bytes`, `std::io`, `std::time`, `std::fs`, `std::path`, and `std::env` remain implemented in Rust for now.
- In v0.1, BirdDisk stdlib modules should be self-contained and avoid
  importing other modules.

### 14.1 std::math module
To use math helpers, import the module:
- `import std::math.`

Functions:
- `std::math::add(a: i64, b: i64) -> i64`
- `std::math::sub(a: i64, b: i64) -> i64`
- `std::math::mul(a: i64, b: i64) -> i64`
- `std::math::div(a: i64, b: i64) -> i64`
- `std::math::mod(a: i64, b: i64) -> i64`
- `std::math::abs(value: i64) -> i64`
- `std::math::sign(value: i64) -> i64` (returns -1, 0, or 1)
- `std::math::is_even(value: i64) -> bool`
- `std::math::clamp(value: i64, low: i64, high: i64) -> i64`
- `std::math::min(a: i64, b: i64) -> i64`
- `std::math::max(a: i64, b: i64) -> i64`
- `std::math::pow(base: i64, exp: i64) -> i64` (returns 0 when `exp < 0`)
- `std::math::gcd(a: i64, b: i64) -> i64` (uses absolute values)
- `std::math::lcm(a: i64, b: i64) -> i64` (returns 0 if `a` or `b` is 0)

## 15. Objects (v0.1)
BirdDisk supports a minimal OO model via `book` declarations.

### 14.1 Book declarations
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
```

- Fields are declared with `field name: type.`.
- Methods are declared with `rule` inside the book.
- The first parameter of a method must be named `self` and have the book type.
- Book-typed fields are not supported in v0.1.

### 14.2 Construction
- `new BookName(args)` constructs a new instance.
- If `BookName::init(self, ...) -> BookName` exists, it is called with the new instance
  plus the provided arguments.
- If `init` is missing, `new BookName()` is allowed and returns a zero-initialized instance.
- Passing arguments without an `init` method is a compile-time error.

### 14.3 Member access
- Field access: `obj::field`
- Method call: `obj::method(args)`
