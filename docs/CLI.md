# BirdDisk CLI (current)

This document lists the current CLI commands, flags, and runtime notes.

## Commands
- `birddiskc fmt <file|dir>` (canonical formatter)
- `birddiskc check <file|dir> [--json]` (JSON diagnostics)
- `birddiskc lint <file|dir> [--json] [--require-tests]` (LLM-friendly lint warnings)
- `birddiskc doc [<file|dir>] [--out <file>]` (generate Markdown docs)
- `birddiskc build [<file|dir>] [--engine vm|wasm|native] [--emit wat|wasm|obj|exe] [--out <file>] [--require-tests]`
- `birddiskc run [<file|dir>] [--engine vm|wasm|native] [--json] [--stdin <file>] [--stdout <file>] [--report <file>] [--deterministic]`
- `birddiskc run <file> --engine wasm --emit wat` (print generated WAT)
- `birddiskc run <file> --engine wasm --emit wasm [--out <file>]` (write .wasm)
- `birddiskc run <file> --engine native --emit obj [--out <file>]` (write native .o)
- `birddiskc run <file> --engine native --emit exe [--out <file>]` (write native executable)
- `birddiskc test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>] [--filter <text>] [--jobs <n>] [--snapshot] [--require-tests] [--deterministic]`
- `birddiskc perf [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>] [--filter <text>] [--baseline <file>] [--update-baseline] [--iterations <n>] [--warmup <n>] [--max-regression <pct>]`

Notes:
- JSON output is supported for `check`, `run`, and `test`.
- Non-JSON `run` is supported for VM interactive mode and native AOT executables.
- `test` compares VM vs WASM vs native outputs by default.
- `test --filter` matches substring(s) in the test path (repeatable).
- `test --jobs` enables parallel workers (default 1; use 1 for IO-heavy tests).
- `test --snapshot` writes/updates `.stdout` files from the selected engine (VM by default).
- `--deterministic` enables the VM deterministic scheduler (VM only). In v0.1 it also uses a virtual clock: `std::time::sleep_ms` advances time without sleeping, and `std::time::now_ms` reads that virtual time.
- If a `birddisk.json` manifest is present, `run`/`build` can omit the file path and will use the manifest entry.
- `perf` defaults to `tests/perf` if it exists; otherwise it scans `tests/`.
- `perf --update-baseline` writes `tests/perf/perf_baseline.json` (or `--baseline <file>` if supplied).
- `perf` compares to a baseline only when one exists (or when explicitly provided).

Manifest (`birddisk.json`)
```json
{
  "name": "demo",
  "version": "0.1.0",
  "entry": "src/main.bd",
  "require_tests": false,
  "test_exclude": ["src/generated", "src/legacy.bd"],
  "deps": {
    "util": "deps/util"
  }
}
```
Notes:
- `deps` entries can also be objects: `"util": { "path": "deps/util", "version": "0.1.0" }` (version is parsed but not used in v0.1).
- `require_tests` enables per-rule test enforcement for `lint`, `test`, and `build` (opt-in).
- `test_exclude` skips test requirements for listed files or directories (paths are relative to the manifest root).

## WASM runtime notes
- If a program uses arrays, the emitted WASM module imports `env.bd_trap`
  for runtime error reporting; `birddiskc run` provides it automatically.
- If a program uses `std::string::from_bytes`, the emitted WASM module
  also imports `env.bd_validate_utf8` and exports `memory`.

## Native AOT notes
- `--engine native --emit obj` writes a host object file (`.o`).
- `--engine native --emit exe` links a standalone host executable using `rustc`.
- The executable reads stdin, runs `bd_main`, writes stdout, and exits 1 on runtime error.
- Requires a Rust toolchain and a built workspace so the runtime `.rlib` is available.
- Supported targets + limitations are documented in `docs/NATIVE.md`.

## Yahtzee scripted run (non-interactive validation)
```sh
cargo run -p birddiskc -- run examples/yahtzee/main.bd --engine vm --json --stdin examples/yahtzee/demo.stdin
cargo run -p birddiskc -- run examples/yahtzee/main.bd --engine wasm --json --stdin examples/yahtzee/demo.stdin
cargo run -p birddiskc -- run examples/yahtzee/main.bd --engine native --json --stdin examples/yahtzee/demo.stdin
```
