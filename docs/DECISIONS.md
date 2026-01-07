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
status: open
decision: _
rationale: _
impact: syntax, parser, typechecker, runtime layout, codegen

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
status: open
decision: _
rationale: _
impact: VM/WASM/native runtime, ABI, tests

Questions:
- GC strategy: tracing (mark/sweep) vs ARC vs manual?
- Object/array header layout and alignment rules?
- Is memory deterministic and portable across backends?

---

## 6) Runtime errors and exception model
status: open
decision: _
rationale: _
impact: diagnostics, runtime, JSON reports

Questions:
- Do we add exceptions or keep explicit error returns?
- Standardize runtime error codes for OOM/null deref/etc?

---

## 7) Module system & standard library
status: open
decision: _
rationale: _
impact: CLI, parser, package tooling, docs

Questions:
- `import` syntax and module resolution rules?
- Stdlib scope for v0.x (strings, io, collections)?
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
