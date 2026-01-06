# Agent instructions (CodexCLI / LLM contributors)

This repo is designed to be built with heavy help from code agents.
Implement BirdDisk v0.1 incrementally with tight verification loops.

## Non-negotiables
1) Small, safe diffs. One concern per change.
2) Add tests as you go. No large refactors without tests.
3) Never guess language rules. `docs/SPEC.md` and `docs/GRAMMAR.ebnf` are the source of truth.
4) Diagnostics must be machine-readable (JSON) and stable (stable error codes).
5) Formatting must be deterministic and idempotent.
6) Prefer portability + correctness over early performance.

## BirdDisk v0.1 definition
### Keywords and syntax
- Functions: `rule name(params...) -> Type: ... end`
- Bindings: `set name[: Type] = expr.`
- Assignment: `put name = expr.`
- Return: `yield expr.`
- Conditional: `when cond: ... otherwise: ... end`
- Loop: `repeat while cond: ... end`

### Types
- Built-in: `i64`, `bool`
- Strong static typing
- Function params + return types are explicit
- Local type inference allowed for `set name = expr.` only when inferable
- No implicit casts

### Out of scope (v0.1)
No floats, generics, concurrency, exceptions, closures, arrays, structs, pattern matching.

## Always follow this workflow
- Make a change
- Run tests
- If syntax/semantics changed:
  - update `docs/SPEC.md`
  - update `docs/GRAMMAR.ebnf`
  - add/update an example in `docs/COOKBOOK.md`
  - add tests (VM now; VM+WASM parity once WASM exists)

## CLI contracts
- `birddisk check --json` returns structured diagnostics per `docs/DIAGNOSTICS.md`
- `birddisk fmt` is deterministic
- `birddisk run` supports:
  - `--engine vm` (golden)
  - `--engine wasm` (once wasm backend exists)

## Error handling
- Internal compiler errors: non-zero exit with a clear message (and ideally a bug code).
- User errors: structured JSON diagnostics with:
  - stable error `code`
  - file + span (line/col)
  - message + help
  - optional fix-its

## Implementation notes (recommended)
Rust workspace with crates:
- `birddisk_core`: lexer/parser/AST/types/diagnostics/formatter
- `birddisk_vm`: interpreter
- `birddisk_wasm`: wasm emitter (e.g., `wasm-encoder`)
- `birddiskc`: CLI driver

Do not micro-optimize v0.1. Correctness + determinism first.
