# BirdDisk POC tasks & milestones

Keep scope tight. If it expands language surface area, it's likely out-of-scope for v0.1.

---

## Sprint 0 — VSCode extension (minimal) (0.5–1 day)
- [ ] Create a VSCode extension for BirdDisk with syntax highlighting
- [ ] Add basic language configuration (comments reserved, brackets, auto-closing)
- [ ] Publish guidance for installing the extension locally

Acceptance:
- `.bd` files highlight correctly in VSCode
- Extension can be installed locally from the repo

---

## Sprint 0 — Repo bootstrap (0.5–1 day)
- [x] Initialize Rust workspace with crates:
  - [x] crates/birddisk_core
  - [x] crates/birddiskc
  - [x] crates/birddisk_vm
  - [x] crates/birddisk_wasm (stub)
- [x] Baseline: `cargo test` green
- [x] CLI stub: `birddisk --help`

Acceptance:
- `cargo test` passes
- CLI prints help

---

## Sprint 0.5 — Eval harness (minimal) (0.5 day)
- [x] Add minimal eval harness in `eval/` (task schema + runner + seed tasks)
- [x] Document expected JSON output for `check` and `run`

Acceptance:
- `python3 eval/run.py --birddisk <path>` runs tasks and reports pass/fail

---

## Sprint 1 — Lexer + Parser + AST (2–7 days)
Implement:
- [x] Lexer (tokens + spans)
- [x] Parser for:
  - [x] `rule` functions
  - [x] `set` / `put` / `yield`
  - [x] `when/otherwise/end`
  - [x] `repeat while/end`
  - [x] expressions with precedence (unary + binary)
- [x] AST nodes

Add tests:
- [x] parse valid programs
- [x] parse invalid programs (expect stable error codes + spans)

Acceptance:
- Parse `docs/COOKBOOK.md` examples
- Parse errors have spans and stable codes

---

## Sprint 2 — Typechecking + local inference (3–10 days)
Implement:
- [x] Types: i64, bool
- [x] Scopes + name resolution
- [x] Typechecking per `docs/SPEC.md`
- [x] Inference for `set name = expr.` where expr is inferable
- [x] Enforce explicit function param + return types
- [x] Diagnostics JSON (`birddisk check --json`) + basic fix-its

Add tests:
- [x] typecheck happy paths
- [x] failures: unknown name, wrong arity, type mismatch, non-bool condition, non-inferable `set`

Acceptance:
- `birddisk check --json` returns structured diagnostics

---

## Sprint 3 — VM interpreter (golden) (3–10 days)
Implement:
- [x] Evaluate typed AST (or minimal IR)
- [x] Deterministic execution
- [x] Entry: `rule main() -> i64`

Add tests:
- [x] run fixtures (return values)
- [x] control flow correctness (when/repeat)

Acceptance:
- `birddisk run --engine vm examples/*.bd` works

---

## Sprint 4 — Formatter (2–7 days)
Implement:
- [x] `birddisk fmt` prints canonical code from AST
- [x] Idempotent formatting: fmt(fmt(code)) == fmt(code)

Add tests:
- [x] golden formatting tests

Acceptance:
- formatting is stable and idempotent

---

## Sprint 5 — WASM backend + differential harness (2–4 weeks)
Implement:
- [x] WASM codegen (wasm32) for i64/bool ops, control flow, calls
- [x] Run WASM via `wasmtime` in tests
- [x] Differential testing: VM result == WASM result

Add tests:
- [x] compile/run WASM for examples
- [ ] basic differential fuzz/mutations (optional stretch)

Acceptance:
- `birddisk run --engine wasm` works
- `birddisk test` compares VM vs WASM outputs

---

## Sprint 6 — AI-first diagnostics polish (ongoing)
- [x] Expand JSON diagnostics:
  - [x] `spec_refs` to doc anchors
  - [x] more fix-its
  - [x] “did you mean” suggestions (similar identifiers)
- [ ] Create `eval/` tasks to measure LLM-friendliness

Acceptance:
- common mistakes can be fixed mechanically from fix-its

---

## Sprint 7 — Decision sprint (priority, 1–3 days)
Record decisions in `docs/DECISIONS.md` for each item before implementation.

- [ ] Member access syntax (confirmed: `::`) and method call shape
- [ ] Object model scope: class/struct keywords, methods, constructors
- [ ] Memory model/GC strategy
- [ ] Runtime error model + stack trace format
- [ ] String type + literal syntax + encoding
- [ ] Primitive types beyond `i64`/`bool`
- [ ] Stdlib scope and module/import system
- [ ] Native backend approach (Cranelift vs LLVM) + target order

Acceptance:
- Each item above has a decided entry in `docs/DECISIONS.md`

---

## Sprint 8 — Strings + minimal stdlib (1–2 weeks)
Implement:
- [ ] `string` type (literal parsing, typechecking, runtime)
- [ ] core string ops (len, concat, equality; optional slice)
- [ ] stdlib module layout + `import` syntax (minimal)
 - [ ] `std::string` module (core string ops)

Add tests:
- [ ] parse/typecheck string literals
- [ ] runtime tests for string ops (VM + WASM parity)

Acceptance:
- `string` works end-to-end in VM + WASM

---

## Sprint 8.5 — Basic IO stdlib (0.5–1 week)
Implement:
- [ ] `std::io::print(string)`
- [ ] `std::io::read_line() -> string`
- [ ] WASM host imports for IO in `birddisk_wasm`
- [ ] VM IO bindings

Add tests:
- [ ] harnessed IO tests with fixed input/output

Acceptance:
- IO works in VM + WASM with deterministic harnessed inputs

---

## Sprint 9 — OO core (2–4 weeks)
Implement:
- [ ] `class` or `struct` declarations
- [ ] member access using `::`
- [ ] methods with `self`/`this`
- [ ] constructor story (`new` or `init`)

Add tests:
- [ ] member access + method calls
- [ ] simple object construction and method invocation

Acceptance:
- OO "hello world" compiles and runs in VM + WASM

---

## Sprint 10 — Memory runtime (2–4 weeks)
Implement:
- [ ] Chosen GC/RC strategy
- [ ] runtime metadata for objects/arrays/strings
- [ ] OOM + null deref diagnostics

Add tests:
- [ ] allocation stress tests
- [ ] runtime error diagnostics with stack traces

Acceptance:
- Memory strategy works across VM + WASM

---

## Sprint 11 — Native backend spike (2–6 weeks)
Implement:
- [ ] Choose backend (Cranelift or LLVM)
- [ ] minimal native codegen for arithmetic + control flow
- [ ] runtime ABI alignment with VM/WASM

Acceptance:
- `rule main() -> i64:` executes on one native target

---

## Future — Full eval suite
- [ ] Expand `eval/` with mutations, report generation, and cross-language comparison
