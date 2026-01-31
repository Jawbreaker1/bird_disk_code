# Teaching LLMs BirdDisk (quick onboarding)

This guide is a compact, LLM-friendly entry point. It pairs with the full spec
and examples so an LLM can generate valid BirdDisk code with minimal ambiguity.

Start here
1) Read `docs/QUICKREF.md` for syntax and typing rules.
2) Scan `docs/COOKBOOK.md` for runnable examples.
3) Use `SPEC.md` for edge cases and precise rules.
4) Use `docs/DIAGNOSTICS.md` for error codes + fix-its.

Core syntax rules (must-follow)
- Statements end with `.` (dot). Blocks end with `end`.
- Use `rule` to define functions; `book` for objects.
- `set` declares a new binding; `put` updates an existing binding/field/index.
- `when` requires an `otherwise` block (even if empty).
- `array(len)` requires an explicit array type, e.g. `set xs: i64[] = array(3).`
- `::` is used for member access and module-qualified calls.
- `try`/`catch` handles only explicit `throw "message"` (string).
- Explicit casts use `expr as type` and are limited to `i64 <-> f64` in v0.1.

LLM-safe workflow
1) Generate code + tests (use `docs/COOKBOOK.md` patterns).
2) Run `birddiskc check --json` first to get structured diagnostics.
3) Run `birddiskc run --engine vm --json` to validate execution.
4) For parity, run `birddiskc test --json` (defaults to VM + WASM + native).

Project template (preferred for multi-file)
`birddisk.json`:
```json
{
  "name": "demo",
  "version": "0.1.0",
  "entry": "src/main.bd",
  "deps": {
    "util": "deps/util"
  }
}
```
Then run:
```sh
./target/debug/birddiskc run --engine vm --json
```

JSON outputs (important for self-correction)
- `birddiskc check --json` returns machine-readable errors + fix-its.
- `birddiskc run --json` returns `{ ok, result, stdout, diagnostics }`.
- Native AOT supports `BIRDDISK_JSON=1` for the same report.

Common pitfalls
- Missing `otherwise` after `when` (parser error).
- Using a non-void call as a statement (must assign or use in expression).
- Omitting array type on `array(len)` or empty `[]`.
- Using `.` for member access (use `::`).

Recommended minimum test pattern
```birddisk
rule add(a: i64, b: i64) -> i64:
  yield a + b.
end

rule main() -> i64:
  yield add(2, 3).
end
```

Next steps
- Follow the examples in `docs/COOKBOOK.md`.
- Use `docs/QUICKREF.md` as the tight reference when generating code.
