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
- [x] Add runtime stack traces in JSON diagnostics (function + line/col)
- [x] Add minimal cross-language eval tasks (Python/JS/Java)
- [ ] Expand `eval/` tasks to measure LLM-friendliness
- [ ] Expand cross-language eval suite as the language matures (arrays/IO/OO/error cases)
- [ ] Add `std::time` (clock/timers) once runtime APIs are defined

Acceptance:
- common mistakes can be fixed mechanically from fix-its

---

## Sprint 6.5 — Refactor pass (recurring)
Goal: keep core modules readable as the language grows.

Implement:
- [ ] Establish a refactor checkpoint rule (split modules once they exceed ~800–1000 LOC)
- [ ] Split `crates/birddisk_vm/src/lib.rs` into focused modules (vm, builtins, values, errors)
- [ ] Split `crates/birddisk_wasm/src/lib.rs` into emitter + runtime helpers
- [ ] Split `crates/birddisk_core/src/typecheck.rs` into submodules (stdlib, oo, core)

Add tests:
- [ ] Ensure existing tests still pass after module moves

Acceptance:
- Core modules are under the checkpoint threshold or clearly partitioned

---

## Sprint 7 — Decision sprint (priority, 1–3 days)
Record decisions in `docs/DECISIONS.md` for each item before implementation.

- [x] Member access syntax (confirmed: `::`) and method call shape
- [x] Object model scope: class/struct keywords, methods, constructors
- [x] Memory model/GC strategy
- [x] Runtime error model + stack trace format
- [x] String type + literal syntax + encoding
- [x] Primitive types beyond `i64`/`bool`
- [x] Stdlib scope and module/import system
- [ ] Native backend approach (Cranelift vs LLVM) + target order

Acceptance:
- Each item above has a decided entry in `docs/DECISIONS.md`

---

## Sprint 8 — Strings + minimal stdlib (1–2 weeks)
Implement:
- [x] `string` type (literal parsing, typechecking, runtime)
- [x] core string ops (len, concat, equality; optional slice)
- [x] stdlib module layout + `import` syntax (minimal)
 - [x] `std::string` module (core string ops)

Add tests:
- [x] parse/typecheck string literals
- [x] runtime tests for string ops (VM + WASM parity)

Acceptance:
- `string` works end-to-end in VM + WASM

---

## Sprint 8.5 — Basic IO stdlib (0.5–1 week)
Implement:
- [x] `std::io::print(string)`
- [x] `std::io::read_line() -> string`
- [x] WASM host imports for IO in `birddisk_wasm`
- [x] VM IO bindings

Add tests:
- [x] harnessed IO tests with fixed input/output

Acceptance:
- IO works in VM + WASM with deterministic harnessed inputs

---

## Sprint 9 — OO core (2–4 weeks)
Implement:
- [x] `class` or `struct` declarations
- [x] member access using `::`
- [x] methods with `self`/`this`
- [x] constructor story (`new` or `init`)

Add tests:
- [x] member access + method calls
- [x] simple object construction and method invocation

Acceptance:
- OO "hello world" compiles and runs in VM + WASM

---

## Sprint 9.5 — Stdlib in BirdDisk (scaffold) (1–2 weeks)
Implement:
- [x] Define stdlib packaging layout (`stdlib/`), module naming rules, and import resolution
- [x] Add compiler support for cross-module compilation + linking (BirdDisk sources)
- [x] Establish versioning + compatibility story for stdlib modules
- [x] Implement first pure-BirdDisk module (e.g. `std::math` or `std::util`)
- [x] Decide boundary: which stdlib modules remain in Rust (strings/bytes/IO) for now
- [x] Add small stdlib utilities (array helpers; parsing helpers pending until u8 ops)

Add tests:
- [x] Cross-module compile tests (stdlib + user module)
- [x] Simple stdlib unit tests authored in BirdDisk

Acceptance:
- At least one stdlib module implemented in BirdDisk and imported by user code

---

## Sprint 10 — Memory runtime (2–4 weeks)
Implement:
- [x] Chosen GC/RC strategy
- [x] runtime metadata for objects/arrays/strings
- [x] OOM + null deref diagnostics
- [x] root stack tracking in VM + WASM
- [x] WASM mark/sweep + free list reuse (GC)
- [x] WASM free-list coalescing
- [x] GC runtime sanity checks (header validation in tests)
- [x] GC metrics exports for WASM tests

Add tests:
- [x] allocation stress tests (GC trigger + survival of live objects)
- [x] unreachable cycle collection (object <-> object, array cycles)
- [x] array/string scanning correctness (nested arrays/strings)
- [x] runtime error diagnostics with stack traces
- [x] GC parity tests for VM + WASM (rooting, ref arrays, nested arrays)

Acceptance:
- Memory strategy works across VM + WASM

---

## Sprint 11 — Native backend spike (2–6 weeks)
Implement:
- [x] Choose backend (Cranelift or LLVM)
- [x] minimal native codegen for arithmetic + control flow
- [x] runtime ABI alignment with VM/WASM
- [x] native runtime scaffolding (GC, root stack)
- [x] native JIT prototype (i64 arithmetic + locals)
- [x] native object emission (Cranelift ObjectModule)
- [x] native CLI emit obj/exe (AOT workflow)

Acceptance:
- `rule main() -> i64:` executes on one native target
- native artifacts build as `.o` and standalone executables

---

## Future — Full eval suite
- [ ] Expand `eval/` with mutations, report generation, and cross-language comparison

## Future — Native-era features
- [ ] Parallel execution + threading model (VM, WASM, native backends)
- [ ] Networking standard library (client/server primitives)
- [ ] Graphics/windowing library (cross-platform surface)

## Future — Maintainability
- [ ] Add concise module/class-level comments for core components (VM/WASM/native/runtime/stdlib)
