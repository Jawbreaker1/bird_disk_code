# Refactor strategy (v0.x)

Goal: reduce file size and risk by splitting large modules into feature‑focused files with no behavior changes.

Status update (2026-01-31)
- Completed: WASM runtime split (and further split string/json/gc/env/path/bytes), VM split, VM builtins split, VM eval split.
- Remaining large modules are listed below.

Global rules
- No logic changes; only move code, rename locals if needed, and re-export.
- One subsystem at a time; keep diffs small.
- Preserve public APIs and symbol names.
- Compile + tests after each split (`cargo test -q`, `birddiskc test --json`).

Priority list (largest → smallest)
1) `crates/birddisk_lsp/src/main.rs` (~3.4k LOC)
2) `crates/birddiskc/src/main.rs` (~2.5k LOC)
3) `crates/birddisk_core/src/parser.rs` (~1.9k LOC)
4) `crates/birddisk_native/src/compiler.rs` (~1.8k LOC)
5) `crates/birddisk_native_runtime/src/api_std.rs` (~1.5k LOC)
6) `crates/birddisk_native_runtime/src/api_mem.rs` (~1.4k LOC)

Safe order (lowest risk → highest impact)
1) CLI split
2) LSP split
3) Native runtime split
4) Native compiler split
5) VM split
6) WASM runtime split

Per-module strategy

1) WASM runtime split
- Create `emit/runtime/` with: `gc.rs`, `heap.rs`, `string.rs`, `bytes.rs`, `json.rs`, `path.rs`, `env.rs`, `io.rs`, `time.rs`, `rand.rs`, `mod.rs`.
- Move functions by feature and re-export in `mod.rs`.
- Keep import order in `emit/mod.rs` the same.
Status: DONE (further split string/json/gc/env/path/bytes into submodules).

2) CLI split (`birddiskc`)
- Create `cli/` with `args.rs`, `run.rs`, `build.rs`, `report.rs`, `mod.rs`.
- Keep `main.rs` as thin dispatcher.

3) LSP split
- Create `lsp/` with `server.rs`, `completion.rs`, `signature.rs`, `hover.rs`, `definitions.rs`, `imports.rs`, `stdlib.rs`.
- JSON wiring stays in `server.rs`.

4) VM split
- Create `vm/` with `mod.rs`, `eval.rs`, `arrays.rs`, `objects.rs`, `enums.rs`, `casts.rs`, `io.rs`.
- Create `builtins/` with one file per module (`string`, `bytes`, `io`, `time`, `rand`, `fs`, `path`, `env`, `json`).
- Keep `Vm` struct in `vm/mod.rs`; move methods into `impl Vm` blocks in submodules.
Status: DONE (eval split into `vm/eval/{mod,stmt,expr,ops}.rs`).

5) Native compiler split
- Create `native/compiler/` with `expr.rs`, `stmt.rs`, `call.rs`, `types.rs`, `helpers.rs`.
- Keep `NativeCompiler` in `compiler/mod.rs`.

6) Native runtime split
- `api_std/`: `string.rs`, `bytes.rs`, `io.rs`, `time.rs`, `rand.rs`, `fs.rs`, `path.rs`, `env.rs`, `json.rs`.
- `api_mem/`: `alloc.rs`, `array.rs`, `object.rs`, `enum.rs`, `error.rs`.
- `lib.rs` re-exports all.
