# BirdDisk POC tasks & milestones

Keep scope tight. If it expands language surface area, it's likely out-of-scope for v0.1.

---

## Sprint 0 — Repo bootstrap (0.5–1 day)
- [ ] Initialize Rust workspace with crates:
  - [ ] crates/birddisk_core
  - [ ] crates/birddiskc
  - [ ] crates/birddisk_vm
  - [ ] crates/birddisk_wasm (stub)
- [ ] Baseline: `cargo test` green
- [ ] CLI stub: `birddisk --help`

Acceptance:
- `cargo test` passes
- CLI prints help

---

## Sprint 1 — Lexer + Parser + AST (2–7 days)
Implement:
- [ ] Lexer (tokens + spans)
- [ ] Parser for:
  - [ ] `rule` functions
  - [ ] `set` / `put` / `yield`
  - [ ] `when/otherwise/end`
  - [ ] `repeat while/end`
  - [ ] expressions with precedence (unary + binary)
- [ ] AST nodes

Add tests:
- [ ] parse valid programs
- [ ] parse invalid programs (expect stable error codes + spans)

Acceptance:
- Parse `docs/COOKBOOK.md` examples
- Parse errors have spans and stable codes

---

## Sprint 2 — Typechecking + local inference (3–10 days)
Implement:
- [ ] Types: i64, bool
- [ ] Scopes + name resolution
- [ ] Typechecking per `docs/SPEC.md`
- [ ] Inference for `set name = expr.` where expr is inferable
- [ ] Enforce explicit function param + return types
- [ ] Diagnostics JSON (`birddisk check --json`) + basic fix-its

Add tests:
- [ ] typecheck happy paths
- [ ] failures: unknown name, wrong arity, type mismatch, non-bool condition, non-inferable `set`

Acceptance:
- `birddisk check --json` returns structured diagnostics

---

## Sprint 3 — VM interpreter (golden) (3–10 days)
Implement:
- [ ] Evaluate typed AST (or minimal IR)
- [ ] Deterministic execution
- [ ] Entry: `rule main() -> i64`

Add tests:
- [ ] run fixtures (return values)
- [ ] control flow correctness (when/repeat)

Acceptance:
- `birddisk run --engine vm examples/*.bd` works

---

## Sprint 4 — Formatter (2–7 days)
Implement:
- [ ] `birddisk fmt` prints canonical code from AST
- [ ] Idempotent formatting: fmt(fmt(code)) == fmt(code)

Add tests:
- [ ] golden formatting tests

Acceptance:
- formatting is stable and idempotent

---

## Sprint 5 — WASM backend + differential harness (2–4 weeks)
Implement:
- [ ] WASM codegen (wasm32) for i64/bool ops, control flow, calls
- [ ] Run WASM via `wasmtime` in tests
- [ ] Differential testing: VM result == WASM result

Add tests:
- [ ] compile/run WASM for cookbook/examples
- [ ] basic differential fuzz/mutations (optional stretch)

Acceptance:
- `birddisk run --engine wasm` works
- `birddisk test` compares VM vs WASM outputs

---

## Sprint 6 — AI-first diagnostics polish (ongoing)
- [ ] Expand JSON diagnostics:
  - [ ] `spec_refs` to doc anchors
  - [ ] more fix-its
  - [ ] “did you mean” suggestions (similar identifiers)
- [ ] Create `eval/` tasks to measure LLM-friendliness

Acceptance:
- common mistakes can be fixed mechanically from fix-its
