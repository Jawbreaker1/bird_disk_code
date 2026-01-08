# BirdDisk decision log (draft)

This document lists the open product/design/tech decisions we need to make.
Take them one at a time and record the outcome here so we don't lose context.

How to record a decision:
- status: open | proposed | decided
- date: YYYY-MM-DD
- decision: the chosen approach
- rationale: why we picked it
- impact: which parts of the compiler/runtime/docs/tests change

---

## 1) Core language surface (v0.x scope)
status: open
decision: _
rationale: _
impact: SPEC/GRAMMAR/COOKBOOK, parser/typechecker, formatter, tests

Questions:
- What is the minimal feature set for v0.2 beyond arrays?
- Are comments introduced next or still reserved?
- Are floats in scope, or remain out-of-scope?

---

## 2) Object model (OO core)
status: decided
date: 2026-01-08
decision: Use `book` as the OO container keyword. Member access uses `::` (e.g. `obj::field`, `obj::method()`), keep `.` as the statement terminator. Methods use receiver name `self`. Constructors use `new BookName(...)`. Inheritance is deferred (composition-only).
rationale: Unique language flavor with minimal ambiguity; `self` is LLM-friendly and conventional. Keeps the terminator unchanged.
impact: grammar, parser, formatter, spec updates, OO typechecker + codegen, runtime layout

Questions:
- Syntax for member access and calls (dot is a terminator): `obj::field`, `obj->field`,
  `obj.field` with alternate terminator, or keyword-based access?
- Class/struct keywords: `class`, `struct`, or unify?
- Methods: `self` vs `this`, explicit receiver?
- Constructors: `new Type(...)` vs `Type(...)` vs `init` method?
- Field mutability: `mut` keyword or `set/put` semantics?

---

## 3) Inheritance vs composition
status: open
decision: _
rationale: _
impact: type system, method dispatch, runtime layout, codegen

Questions:
- Allow inheritance in v0.2 or composition-only?
- Interfaces/traits? If yes, static vs dynamic dispatch?

---

## 4) Type system extensions
status: open
decision: _
rationale: _
impact: parser/typechecker, diagnostics, formatter

Questions:
- Nominal vs structural typing for objects?
- Nullability: implicit nulls or explicit `Option<T>`?
- Generics: out of scope now or minimal generic types?
- Overloading: allow or forbid (LLM ambiguity)?

---

## 5) Memory model & GC
status: decided
date: 2026-01-07
decision: Use tracing GC (mark/sweep) as the primary memory strategy.
rationale: Handles cycles cleanly, keeps user model simple, and is most LLM-friendly.
impact: VM/WASM/native runtime, ABI, stack maps/roots, tests, docs

Questions:
- GC strategy: tracing (mark/sweep) vs ARC vs manual?
- Object/array header layout and alignment rules?
- Is memory deterministic and portable across backends?

Options:
1) Tracing GC (mark/sweep or mark/compact)
   - Pros: simplest semantics for users, handles cycles, matches OO patterns.
   - Cons: runtime complexity, needs stack maps or conservative scanning.
2) ARC/RC (automatic reference counting)
   - Pros: deterministic frees, simpler runtime in WASM/native.
   - Cons: cycles leak unless you add cycle collection; more complexity in codegen.
3) Manual memory
   - Pros: smallest runtime, explicit control.
   - Cons: highest user burden, least LLM-friendly, errors are common.

---

## 6) Runtime errors and exception model
status: decided
date: 2026-01-07
decision: v0.x uses runtime traps with stable error codes plus a minimal JSON stack trace (function + line/col). Exceptions are deferred to a later sprint.
rationale: Keeps runtime simple and deterministic for VM/WASM/native while still giving LLMs actionable traces.
impact: diagnostics schema, VM/WASM/native runtime, CLI JSON output, tests

Questions:
- Do we add exceptions or keep explicit error returns?
- Standardize runtime error codes for OOM/null deref/etc?

Options:
1) No exceptions (errors are runtime traps with stable error codes)
   - Pros: simple semantics, no new control flow.
   - Cons: users must pre-check; no recovery within the language.
2) Checked result types (explicit `Result<T, E>`-style)
   - Pros: explicit, LLM-friendly, no hidden control flow.
   - Cons: more syntax; type system complexity.
3) Exceptions (try/catch)
   - Pros: ergonomic recovery, familiar to OO users.
   - Cons: complex codegen and stack unwinding; higher ambiguity risk.

---

## 7) Module system & standard library
status: decided
date: 2026-01-08
decision: Use `import std::module.` with `::` path separators. Imports are top-level only. Calls use fully qualified names (e.g. `std::string::len(s)`). Arrays are core; `string` is a core type with ops provided in `std::string`.
rationale: Unambiguous, consistent with `::` syntax, avoids aliasing complexity.
impact: parser/typechecker, module resolver, stdlib layout, docs, tests

Scope note:
- Implement `std::string` first; add `std::io` immediately after strings land.

Questions:
- `import` syntax and module resolution rules?
- Stdlib scope for v0.x (strings, io, collections, math, time)?
- Packaging format (toml/json) and versioning?

---

## 8) Extensions and FFI
status: open
decision: _
rationale: _
impact: compiler, runtime ABI, tooling

Questions:
- `extern` declarations in source or manifest-based?
- WASM host bindings: how are imports named and versioned?

---

## 9) Compiler architecture (IR + backends)
status: open
decision: _
rationale: _
impact: birddisk_core, birddisk_vm, birddisk_wasm, future native backends

Questions:
- Shared IR between VM/WASM/native?
- Lowering pipeline and validation points?

---

## 10) Native backends (arm64/x86_64)
status: open
decision: _
rationale: _
impact: runtime ABI, build system, CI

Questions:
- Cranelift vs LLVM?
- ABI and runtime support (GC, stack maps)?
- Target order: arm64 first or x86_64?

---

## 11) Tooling & UX
status: open
decision: _
rationale: _
impact: CLI, editor support, tests

Questions:
- VSCode extension scope (syntax only vs LSP)?
- Formatter configuration roadmap?
- Debugging support (source maps, stack traces)?

Options:
1) Minimal stack trace: function names + line/col (VM + WASM)
2) Full trace + locals (requires debug metadata and runtime support)
3) JSON trace only vs human-readable + JSON

---

## 12) Strings
status: decided
date: 2026-01-07
decision: Add `string` type with UTF-8 encoding, immutable. Literals use double quotes with `\"`, `\\`, `\n` escapes. Core ops: `len`, `concat`, `eq` (slice later).
rationale: Standard, simple, LLM-friendly; minimal surface area for v0.x.
impact: lexer/parser, typechecker, runtime layout/GC, stdlib APIs, formatter, tests

Questions:
- String literal syntax: double quotes only? escapes? multiline?
- String type name: `string` or `str`?
- Encoding: UTF-8 bytes vs ASCII-only (v0.x)?
- Immutability: immutable strings only or mutable buffers?
- Core ops: concat, length, slice, equality?

---

## 13) Primitive types beyond i64/bool
status: decided
date: 2026-01-07
decision: Add `i32` and `f64` as the next primitive types. No implicit casts; explicit casts and literal suffixes are deferred.
rationale: `i32` covers memory/interop needs; `f64` covers core float use cases while keeping surface area small.
impact: lexer/parser, typechecker, VM/WASM/native codegen, stdlib, tests

Questions:
- Integer widths: add `i32`, `i16`, `i8` (and/or unsigned `u64`, `u32`)?
- Floats: `f32`/`f64` in v0.x or defer?
- Char type: `char` vs `string` only?
- Implicit casts: still none, or allow explicit casts only?
- Literal suffixes: `1i32`, `1u64`, `1f64`?
