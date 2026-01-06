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
4) Run in WASM (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine wasm --json
```
5) Inspect generated WAT (optional).
```sh
./target/debug/birddisk run path/to/file.bd --engine wasm --emit wat
```

Typing model (v0.1)
- Built-in types: i64, bool
- Function params and return types are always explicit
- set name = expr. may omit the type if expr is inferable
- No implicit casts

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
crates/
  birddiskc/        # CLI compiler driver
  birddisk_core/    # lexer/parser/AST/types/diagnostics/formatter
  birddisk_vm/      # interpreter (golden)
  birddisk_wasm/    # wasm codegen
examples/
tests/
eval/               # LLM syntax evaluation tasks + scoring notes

CLI (current)

- JSON output is supported for check/run/test; non-JSON paths are stubbed.
- birddisk fmt <file|dir> (stub)
- birddisk check <file|dir> [--json] (JSON implemented)
- birddisk run <file> [--engine vm|wasm] [--json] (VM + WASM implemented)
- birddisk run <file> --engine wasm --emit wat (print generated WAT)
- birddisk test [--json] (stub)

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

Status
- Implemented: lexer, parser, AST, typechecker, VM interpreter
- Implemented: JSON diagnostics (check/run) + fix-its + suggestions
- Implemented: eval harness with task runner
- Implemented: WASM backend (minimal, via wasmtime)
- Implemented: WAT emission via `birddisk run --engine wasm --emit wat`
- Stubbed: formatter, non-JSON CLI paths

License

TBD (MIT recommended for early POC).
