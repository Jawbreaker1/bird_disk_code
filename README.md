# BirdDisk — AI-first compiled language (POC)

BirdDisk is a proof-of-concept compiled programming language and toolchain designed for an agentic/LLM-driven workflow.

BirdDisk focuses on:
- **Unique, LLM-friendly syntax** (low ambiguity, few special cases)
- **Strong static typing** with **local type inference**
- **Deterministic formatter** (one canonical style)
- **Structured JSON diagnostics + fix-its** (machine actionable)
- **Golden reference execution** (VM/interpreter)
- **WASM as first compilation target** (portable baseline)
- **Differential testing**: VM output == WASM output

Longer-term: native backends (arm64 + x86_64) and scalable multi-CPU support via continuous automated validation.

## BirdDisk syntax (v0.1)
BirdDisk code is visually distinct:
- Imports: `import std::module.`
- Functions: `rule`
- Bindings: `set`
- Assignment: `put`
- Conditionals: `when / otherwise / end`
- Loops: `repeat while / end`
- Return: `yield`
- Blocks: `:` ... `end`
- Statement terminator: `.`

Example:
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

Quick start
1) Create a `.bd` file (BirdDisk source).
```birddisk
rule main() -> i64:
  yield 42.
end
```
2) Build the CLI.
```sh
cargo build -p birddiskc
```
3) Run in the VM.
```sh
./target/debug/birddisk run path/to/file.bd --engine vm --json
```
4) Run with a JSON report file (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine vm --report report.json
```
5) Run with stdin/stdout files (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine vm --json --stdin input.txt --stdout output.txt
```
6) Run in WASM (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine wasm --json
```
7) Inspect generated WAT (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine wasm --emit wat
```
8) Write a .wasm artifact (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine wasm --emit wasm
```
If the program uses arrays, the emitted WASM module imports `env.bd_trap`
for runtime error reporting; `birddisk run` provides it automatically.
If the program uses `std::string::from_bytes`, the emitted WASM module
also imports `env.bd_validate_utf8` and exports `memory`.
9) Run differential tests (optional).
```sh
./target/debug/birddisk test --json
```
10) Filter tests by directory, tag, or engine (optional).
```sh
./target/debug/birddisk test --json --dir examples --tag while
./target/debug/birddisk test --json --engine vm --dir vm_tests
```
Tags match directory names and file stem tokens (split on non-alphanumeric).
Default test dirs are `examples/` and `tests/` if present.

Examples
- `examples/minimal_main.bd` (smallest runnable program)
- `examples/book_account.bd` (book with methods and constructor)
- `examples/book_point.bd` (field access + method calls)
- `examples/terminal_calculator.bd` (terminal IO + operator dispatch)

Typing model (v0.1)
- Built-in types: i64, bool, string, u8
- Array types: T[]
- Function params and return types are always explicit
- set name = expr. may omit the type if expr is inferable
- No implicit casts
- stdlib string ops live in `std::string` (import required)
- byte helpers live in `std::bytes` (import required)
- `std::string::from_bytes(u8[])` validates UTF-8 and returns a string
- `std::string::to_i64(string)` parses a decimal integer; `std::string::from_i64(i64)` formats one
- stdlib modules on disk live under `stdlib/` (e.g. `import std::math.`)
- book types are declared with `book` and constructed via `new Book(...)`

See docs/SPEC.md.

Targets
- VM/interpreter (implemented)
- wasm32 (WASM backend, minimal)
- Later: native aarch64 (arm64) + x86_64 via LLVM or Cranelift


Repo layout
docs/
  SPEC.md
  GRAMMAR.md
  DIAGNOSTICS.md
  COOKBOOK.md
  STYLES.md
  DECISIONS.md
crates/
  birddiskc/        # CLI compiler driver
  birddisk_core/    # lexer/parser/AST/types/diagnostics/formatter
  birddisk_vm/      # interpreter (golden)
  birddisk_wasm/    # wasm codegen
examples/
tests/
vm_tests/          # VM-only fixtures (reserved for future features)
vm_error_tests/    # VM fixtures expected to fail (diagnostics/runtime errors)
eval/               # LLM syntax evaluation tasks + scoring notes

CLI (current)

- JSON output is supported for check/run/test; non-JSON paths are stubbed.
- birddisk fmt <file|dir> (canonical formatter)
- birddisk check <file|dir> [--json] (JSON implemented)
- birddisk run <file> [--engine vm|wasm] [--json] [--stdin <file>] [--stdout <file>] [--report <file>] (VM + WASM implemented)
- birddisk run <file> --engine wasm --emit wat (print generated WAT)
- birddisk run <file> --engine wasm --emit wasm [--out <file>] (write .wasm)
- birddisk test [--json] [--engine vm|wasm] [--dir <path>] [--tag <tag>] (VM vs WASM diff by default)

Development principles
	•	Keep the language core small and orthogonal.
	•	Deterministic formatting and stable AST printing.
	•	Diagnostics must be structured and actionable (fix-its where possible).
	•	Every new feature must update:
	•	docs/SPEC.md
	•	docs/GRAMMAR.md
	•	docs/COOKBOOK.md
	•	tests (VM + WASM parity once WASM exists)

Roadmap

See TASKS.md.
For agent workflow rules, see AGENT.md.

Upcoming (planned):
- VSCode extension (syntax highlighting)
- GC runtime (tracing mark/sweep)
- std::time (clock/timers)
- Native backend spike

Status
- Implemented: lexer, parser, AST, typechecker, VM interpreter
- Implemented: JSON diagnostics (check/run) + fix-its + suggestions
- Implemented: eval harness with task runner
- Implemented: WASM backend (minimal, via wasmtime)
- Implemented: WASM emission via `birddisk run --engine wasm --emit wasm`
- Implemented: WAT emission via `birddisk run --engine wasm --emit wat`
- Implemented: differential test harness (`birddisk test --json`)
- Implemented: formatter (`birddisk fmt`)
- Implemented: arrays + indexing (VM + WASM)
- Implemented: strings + std::string (VM + WASM)
- Implemented: u8 + std::bytes + std::string::bytes (VM + WASM)
- Implemented: std::io (VM + WASM)
- Implemented: stdlib module loading + `std::math` (BirdDisk)
- Implemented: OO core (book + fields + methods, VM + WASM)
- Stubbed: non-JSON CLI paths

License

TBD (MIT recommended for early POC).
