# BirdDisk — AI-first compiled language (POC)

BirdDisk is a proof-of-concept compiled programming language and toolchain designed for an agentic/LLM-driven workflow.

**Why this exists:** modern teams increasingly rely on LLMs to generate and modify code, but general-purpose languages and tooling are noisy for machines, leading to ambiguous parses, fragile edits, and slow fix loops. BirdDisk explores a language + compiler stack that is intentionally easier for LLMs to read, write, and correct while staying clear to humans, delivering faster iteration, fewer broken builds, and portable execution (VM/WASM/native) with deterministic formatting and machine-readable diagnostics.

BirdDisk focuses on:
- **Unique, LLM-friendly syntax** (low ambiguity, few special cases)
- **Strong static typing** with **local type inference**
- **Deterministic formatter** (one canonical style)
- **Structured JSON diagnostics + fix-its** (machine actionable)
- **Golden reference execution** (VM/interpreter)
- **WASM as first compilation target** (portable baseline)
- **Differential testing**: VM output == WASM output

Longer-term: broaden native coverage (arm64 + x86_64) and scalable multi-CPU support via continuous automated validation.

## BirdDisk syntax (v0.1)
BirdDisk code is visually distinct:
- Imports: `import std::module.` or `import app::module.`
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
1) Build the CLI.
```sh
cargo build -p birddiskc
```
2) Create `hello.bd`.
```birddisk
import std::io.

rule main() -> i64:
  yield std::io::print("Hello, BirdDisk!\n").
end
```
3) Run in the VM (reference interpreter).
```sh
./target/debug/birddiskc run hello.bd --engine vm --json
```
4) Run in WASM (portable backend).
```sh
./target/debug/birddiskc run hello.bd --engine wasm --json
```
5) Build a native executable (host).
```sh
./target/debug/birddiskc run hello.bd --engine native --emit exe --out ./bird_hello
./bird_hello
```
6) Optional: install the VSCode extension (syntax + LSP).
See docs/VSCODE.md.

The VM is the golden reference for correctness; WASM and native backends
are compared against it during development.

WASM notes (advanced):
- If a program uses arrays, the emitted WASM module imports `env.bd_trap`
  for runtime error reporting; `birddiskc run` provides it automatically.
- If a program uses `std::string::from_bytes`, the emitted WASM module
  also imports `env.bd_validate_utf8` and exports `memory`.

More commands and flags are listed in the CLI section below.

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
- non-stdlib modules resolve to `<path>.bd` (entry dir first, then project root)
- book types are declared with `book` and constructed via `new Book(...)`

See docs/SPEC.md.

Targets
- VM/interpreter (implemented)
- wasm32 (WASM backend, minimal)
- Native (host JIT + AOT object/exe) via Cranelift


Repo layout
docs/
  SPEC.md
  GRAMMAR.md
  DIAGNOSTICS.md
  COOKBOOK.md
  STYLES.md
  DECISIONS.md
  RUNTIME.md
crates/
  birddiskc/        # CLI compiler driver
  birddisk_core/    # lexer/parser/AST/types/diagnostics/formatter
  birddisk_native_runtime/ # native runtime support (AOT/JIT)
  birddisk_vm/      # interpreter (golden)
  birddisk_wasm/    # wasm codegen
examples/
tests/
vm_tests/          # VM-only fixtures (reserved for future features)
vm_error_tests/    # VM fixtures expected to fail (diagnostics/runtime errors)
eval/               # LLM syntax evaluation tasks + scoring notes

CLI (current)

- JSON output is supported for check/run/test; non-JSON paths are stubbed.
- birddiskc fmt <file|dir> (canonical formatter)
- birddiskc check <file|dir> [--json] (JSON implemented)
- birddiskc run <file> [--engine vm|wasm|native] [--json] [--stdin <file>] [--stdout <file>] [--report <file>] (VM + WASM + native implemented)
- birddiskc run <file> --engine wasm --emit wat (print generated WAT)
- birddiskc run <file> --engine wasm --emit wasm [--out <file>] (write .wasm)
- birddiskc run <file> --engine native --emit obj [--out <file>] (write native .o)
- birddiskc run <file> --engine native --emit exe [--out <file>] (write native executable)
- birddiskc test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>] (VM vs WASM diff by default)

Native AOT notes
- `--engine native --emit obj` writes a host object file (`.o`).
- `--engine native --emit exe` links a standalone host executable using `rustc`.
- The executable reads stdin, runs `bd_main`, writes stdout, and exits 1 on runtime error.
- Requires a Rust toolchain and a built workspace so the runtime `.rlib` is available.

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
For local VSCode install instructions, see docs/VSCODE.md.

VSCode extension (local install)
- In VSCode: Extensions → “Install Extension from Location…”
- Select `editors/vscode` from this repo
- Open a `.bd` file to activate syntax highlighting, snippets, completions, diagnostics, and formatting
- Optional: build and enable the LSP server for hover/go-to/rename, semantic tokens, and inlay hints (`docs/VSCODE.md`).

Upcoming (planned):
- VSCode extension maintenance (syntax/LSP updates)
- GC runtime (tracing mark/sweep)
- std::time (clock/timers)
- Native backend spike

Status
- Implemented: lexer, parser, AST, typechecker, VM interpreter
- Implemented: JSON diagnostics (check/run) + fix-its + suggestions
- Implemented: eval harness with task runner
- Implemented: WASM backend (minimal, via wasmtime)
- Implemented: WASM emission via `birddiskc run --engine wasm --emit wasm`
- Implemented: WAT emission via `birddiskc run --engine wasm --emit wat`
- Implemented: differential test harness (`birddiskc test --json`)
- Implemented: formatter (`birddiskc fmt`)
- Implemented: arrays + indexing (VM + WASM)
- Implemented: strings + std::string (VM + WASM)
- Implemented: u8 + std::bytes + std::string::bytes (VM + WASM)
- Implemented: std::io (VM + WASM)
- Implemented: std::time (VM + WASM)
- Implemented: std::fs (VM + WASM)
- Implemented: std::path (VM + WASM)
- Implemented: std::env (VM + WASM)
- Implemented: stdlib module loading + `std::math` (BirdDisk)
- Implemented: OO core (book + fields + methods, VM + WASM)
- Stubbed: non-JSON CLI paths


Easter egg marker: quartz-mongoose-47-lantern-squid-velvet-axiom-candle.

License

TBD (MIT recommended for early POC).
