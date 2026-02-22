# BirdDisk POC tasks & milestones

Keep scope tight. If it expands language surface area, it's likely out-of-scope for v0.1.

---

## Sprint 0 — VSCode extension (phased)
### Sprint 0A — Syntax + config (0.5–1 day)
- [x] Create a VSCode extension for BirdDisk with syntax highlighting (TextMate)
- [x] Add basic language configuration (comments reserved, brackets, auto-closing, indentation)
- [x] Add snippets for core constructs (`rule`, `when`, `repeat`, `book`)
- [x] Publish guidance for installing the extension locally

### Sprint 0B — Basic completions + diagnostics (1–3 days)
- [x] Add keyword + stdlib module completions (no type info)
- [x] Add file-based module completions for `import`
- [x] Wire `birddiskc check --json` as diagnostics on save
- [x] Optional: hook `birddiskc fmt` as a document formatter

### Sprint 0C — LSP/semantic features (future)
- [x] Define LSP scope (hover, go-to, rename, type-aware completions)
- [x] Implement a minimal language server wrapper around the compiler
- [x] Add semantic tokens + inlay hints (stretch)

Acceptance:
- `.bd` files highlight correctly in VSCode
- Extension can be installed locally from the repo

---

## Sprint 0 — Repo bootstrap (0.5–1 day)
- [x] Initialize Rust workspace with crates:
- [x] crates/birddisk_core
- [x] crates/birddiskc
- [x] crates/birddisk_vm
- [x] crates/birddisk_wasm
- [x] Baseline: `cargo test` green
- [x] CLI stub: `birddiskc --help`

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
- [x] Diagnostics JSON (`birddiskc check --json`) + basic fix-its

Add tests:
- [x] typecheck happy paths
- [x] failures: unknown name, wrong arity, type mismatch, non-bool condition, non-inferable `set`

Acceptance:
- `birddiskc check --json` returns structured diagnostics

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
- `birddiskc run --engine vm examples/*.bd` works

---

## Sprint 4 — Formatter (2–7 days)
Implement:
- [x] `birddiskc fmt` prints canonical code from AST
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
- `birddiskc run --engine wasm` works
- `birddiskc test` compares VM vs WASM vs native outputs

---

## Sprint 6 — AI-first diagnostics polish (closed)
- [x] Expand JSON diagnostics:
  - [x] `spec_refs` to doc anchors
  - [x] more fix-its
  - [x] “did you mean” suggestions (similar identifiers)
- [x] Add runtime stack traces in JSON diagnostics (function + line/col)
- [x] Add minimal cross-language eval tasks (Python/JS/Java)
- [x] Add `std::time` (clock/timers) once runtime APIs are defined

Acceptance:
- common mistakes can be fixed mechanically from fix-its

---

## Sprint 6.5 — Refactor pass (recurring)
Goal: keep core modules readable as the language grows.

Implement:
- [x] Establish a refactor checkpoint rule (split modules once they exceed ~800–1000 LOC)
- [x] Split `crates/birddisk_vm/src/lib.rs` into focused modules (vm, builtins, values, errors)
- [x] Split `crates/birddisk_wasm/src/lib.rs` into emitter + runtime helpers
- [x] Split `crates/birddisk_core/src/typecheck.rs` into submodules (stdlib, oo, core)
- [x] Split `crates/birddisk_native/src/lib.rs` (currently ~2.7k LOC)
- [x] Split `crates/birddisk_native_runtime/src/lib.rs` (currently ~2.6k LOC)
- [x] Split VM eval + builtins; split WASM runtime modules (string/json/gc/env/path/bytes)

Add tests:
- [x] Ensure existing tests still pass after module moves

Acceptance:
- Core modules are under the checkpoint threshold or clearly partitioned

Refactor checkpoint rule (active):
- Split modules once they exceed ~800–1000 LOC, unless there is a strong, documented reason not to.

---

## Sprint 7 — Decision sprint (priority, 1–3 days)
Record decisions in `docs/DECISIONS.md` for each item before implementation.

- [x] Member access syntax (confirmed: `::`) and method call shape
- [x] Object model scope: class/struct keywords, methods, constructors
- [x] Constructor naming/format for book instantiation (e.g. `new`, `init`, factories)
- [x] Memory model/GC strategy
- [x] Runtime error model + stack trace format
- [x] String type + literal syntax + encoding
- [x] Primitive types beyond `i64`/`bool`
- [x] Stdlib scope and module/import system
- [x] Namespacing rules (module vs book resolution, aliasing, and conflicts)
- [x] Native backend approach (Cranelift vs LLVM) + target order

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

## Sprint 13 — Error handling + stack traces (2–4 weeks)
Implement:
- [x] Decide error model (exceptions vs result types)
- [x] Runtime error propagation (`try`/`catch` or `Result` helpers)
- [x] Stack trace frames enriched with source spans + code snippets
- [x] Standard error output format in JSON (match `docs/DIAGNOSTICS.md`)

Add tests:
- [x] VM + WASM parity tests for error propagation
- [x] Diagnostics tests for stack trace format stability

Acceptance:
- Runtime errors provide actionable traces and can be handled in user code

---

## Sprint 12 — Stdlib essentials (2–4 weeks)
Implement (v0.x essentials, keep scope tight):
- [x] `std::fs` (read/write file contents)
- [x] `std::path` (join, normalize, basename/dirname)
- [x] `std::time` (clock/timers)
- [x] `std::env` (args, env vars)
- [x] Local module imports (entry dir + project root resolution)
- [x] `std::json` (encode/decode basic values)

Add tests:
- [x] VM + WASM parity tests for each module
- [x] IO harness tests for file and env operations

Acceptance:
- Minimal CLI programs can read/write files and parse JSON on VM + WASM

---

## Sprint 14 — Package & module workflow (1–3 weeks)
Implement:
- [x] Module resolution rules beyond stdlib (project packages + manifest)
- [x] Simple manifest format (project name, version, deps)
- [x] CLI workflow: `birddiskc build`, `birddiskc run` with manifest
- [x] Align CLI naming: rename `birddiskc` crate or binary so build instructions match output name

Add tests:
- [x] Multi-module compilation tests
- [x] Versioned dependency resolution tests (minimal)

Acceptance:
- Projects can declare and build dependencies deterministically

---

## Sprint 15 — Language ergonomics v0.x (2–6 weeks)
Implement (small slices, one per PR):
- [x] Enums/variants (no generics yet; VM + WASM + native done)
- [x] Pattern matching for enums (VM + WASM + native done)
- [x] Floats (f64) (no implicit casts)
- [x] Explicit float conversions (i64 <-> f64)
- [x] Improved string/bytes APIs (slice, search, replace)

Add tests:
- [x] Typecheck + runtime tests for each feature
- [x] VM + WASM + native parity per feature

Acceptance:
- Core data modeling is practical for small apps

---

## Sprint 16 — Tooling & quality (ongoing)
Implement:
- [x] Linter with opinionated rules for LLM-friendly code (initial rules)
- [x] Expand linter rules + autofix hints (naming, unused vars, shadowing, complexity)
- [x] Doc generator (`birddiskc doc` from source)
- [x] Reference manual (LLM-friendly semantics lookup)
- [x] Human-readable runtime error output for CLI runs (stacktrace + snippets; non-JSON)
- [x] `std::rand` (basic RNG with seed + uniform range)
- [x] Decide on mandatory testing model (separate test files, exemptions like `main`/`init`, minimal syntax + runner)
- [x] Implement require-tests enforcement (flag + manifest setting; lint/test/build)
- [x] Add `std::test` helpers (assert/eq; throw on failure)
- [x] Add manifest test exclude list support
- [x] Plan stricter test requirements (per rule or threshold-based)
  - options: per-file minimum tests, ratio-based, manifest overrides
- [x] Require-tests per-rule mapping (test_<rule> / test_<book>_<rule>)
- [x] Profiler hooks (GC stats, runtime timers)
- [x] Enhanced test runner (parallel, filters, snapshots)

Add tests:
- [x] Golden tests for linter output
- [x] Doc generation snapshots

Acceptance:
- Tooling supports larger projects and debugging

---

## Sprint 17 — Performance & portability (ongoing)
Implement:
- [x] Native backend coverage for arm64/x86_64 (macOS/Linux/Windows)
  - [x] CI: macOS arm64 (macos-14)
  - [x] CI: macOS x86_64 (macos-13)
  - [x] CI: Linux x86_64 (ubuntu-latest)
  - [x] CI: Linux arm64 (QEMU)
  - [x] CI: Windows x86_64 (windows-latest)
- [x] Optimization passes (const fold, dead code, inlining)
  - [x] Const folding + basic branch pruning
  - [x] Basic DCE (drop statements after yield/throw)
  - [x] Inlining (small leaf rules)
- [x] GC tuning + performance metrics
- [x] Concurrency model decision + minimal primitives
- [x] LSP: Go-to-definition across imported modules (multi-file symbol index)

Add tests:
- [x] Cross-platform build checks (CI matrix)
- [x] Performance regression harness

Acceptance:
- Native builds work across major platforms with stable performance

---

## Sprint 18 — Concurrency foundation (planning)
Implement:
- [x] Define `std::thread` + `std::channel` API (spawn/join, send/recv)
- [x] Decide value ownership across threads (copy/clone rules, allowed types)
- [x] VM deterministic scheduler (opt-in, e.g. `--deterministic` or test-only)
  - [x] CLI flag + docs (virtual time + deterministic clock)
  - [x] Deterministic thread scheduling (once threads exist)
- [x] Native threading runtime scaffolding (thread registry + join bookkeeping)
- [x] WASM behavior: compile-time error for threading (clear diagnostic)
  - [x] WASM compile-time error for std::thread/std::channel + tests

Add tests:
- [x] VM: deterministic scheduling fixture tests
- [x] Compile-time error tests for WASM threading

Acceptance:
- Spec + stdlib surface finalized; VM deterministic mode documented

---

## Sprint 19 — Concurrency implementation (VM + native)
Implement:
- [x] VM: spawn/join + channels (message passing only)
- [x] Native: spawn/join + channels (host-thread spawn enabled for `i64` entry signatures; shared-reference/channel parallel execution model remains future work)
- [x] CLI/test harness support for concurrency fixtures
- [x] Error codes for thread/channel failures (join misuse, channel misuse/would-block)
  - `send` on closed channel still returns `false` by design (not a runtime error)

Add tests:
- [x] VM spawn/join fixtures (`vm_tests/`)
- [x] VM vs native parity tests (deterministic mode in VM)
- [x] Stress tests with multiple threads + channels

Acceptance:
- Basic concurrent programs run in VM + native with parity coverage

---

## Sprint 20 — std::net TCP (blocking)
Implement:
- [x] std::net TCP client (connect, read, write, close, timeouts)
- [x] std::net TCP server (listen, accept, read, write, close)
- [x] std::net minimal TCP pooling (pool/pool_get/pool_put/pool_close)
- [x] Error mapping (IO failures -> stable runtime codes)
- [x] WASM: compile-time error when `std::net` is imported/used

Add tests:
- [x] VM/native TCP roundtrip tests (localhost)
- [x] VM/native TcpPool reuse tests (localhost)
- [x] Error cases (refused connect, timeout)
- [x] WASM compile error fixtures for std::net usage

Acceptance:
- Blocking TCP works in VM + native; WASM rejects net usage clearly

---

## Sprint 21 — std::http (minimal client)
Implement:
- [x] HTTP client on top of std::net (GET/POST, status, headers, body)
- [x] Response parsing (status line + headers + body)
- [x] `Content-Length` exact body parsing (`std::net::read_exact`) with line-based fallback
- [x] `Transfer-Encoding: chunked` body decoding
- [x] Minimal request builder (method, url, headers, body)

Add tests:
- [x] Local HTTP server fixtures (VM/native) for GET/POST
- [x] Error handling tests (invalid response, timeout)
  - [x] Invalid response tests (VM/native)
  - [x] Timeout tests (VM/native)
- [x] Typechecker import propagation: stdlib module builtin imports should not require duplicate entry-file imports

Acceptance:
- Minimal HTTP client works in VM + native for local endpoints

---

## Future — Language features
- [ ] Add generics/type parameters (types + functions + enums)
- [ ] Use generics to replace typed channels with `Channel<T>` + `Recv<T>`
- [ ] Update concurrency/channel examples to use `Channel<T>` once generics ship (keep a non-generic compatibility example for migration docs)

## Future — Full eval suite
- [ ] Expand `eval/` with mutations, report generation, and cross-language comparison
- [ ] Expand `eval/` tasks to measure LLM-friendliness
- [ ] Expand cross-language eval suite as the language matures (arrays/IO/OO/error cases)
- [ ] Produce a comparison report template (LLM-friendliness + runtime performance)

## Future — Native-era features
- [ ] Parallel execution + threading model (VM, WASM, native backends)
- [ ] Networking standard library (blocking TCP first)
- [ ] HTTP client layer (after std::net)
- [ ] UDP sockets follow-up (after TCP stabilizes)
- [ ] Graphics/windowing library (cross-platform surface)
- [ ] Plan stdlib bootstrap: move most stdlib to BirdDisk while keeping a minimal Rust host layer (ABI/layout, build + link order, tests)

## Future — Maintainability
- [ ] Add concise module/class-level comments for core components (VM/WASM/native/runtime/stdlib)
- [ ] Keep the VSCode extension (syntax/LSP + lint/diagnostics) updated as the language surface grows
- [ ] Refactor large files into feature modules (see `docs/REFACTOR.md`, ongoing)
- [ ] Streamline CLI defaults/flags for common workflows (make compile/run simpler)
 - [ ] Monitor file sizes and refactor when any file exceeds ~1000 LOC (ongoing)

## Future — Quality gates (exploration)
- [ ] Evaluate a test-required policy (lint/tooling first, not compiler), with clear opt-outs and minimal friction

## Future — Native AOT follow-ups
- [x] AOT wrapper: embed or pass GC layout so native GC can scan references correctly
- [x] AOT wrapper: improve runtime lookup/linking error messaging (missing runtime rlib/staticlib)
- [x] AOT wrapper: add JSON/trace-friendly runtime error output and optional return value reporting

## Native runtime parity gaps (review)
- [x] Native runtime: capture call stacks so runtime errors include trace frames (match VM/WASM JSON diagnostics)
