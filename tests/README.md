# BirdDisk tests

This folder contains runnable BirdDisk programs used by the differential
test harness (`birddiskc test --json`).

## Conventions
- Keep each file small and focused (one feature per file).
- Prefer predictable outputs (no randomness; IO requires fixtures).
- Avoid redundant combinations; use tags to group related cases.

## Tags
Tags are derived from:
- Directory names (e.g. `tests/loops/` → tag `loops`)
- File stem tokens split on non-alphanumeric (e.g. `nested_repeat.bd` → tags
  `nested`, `repeat`)

Examples:
```sh
./target/debug/birddiskc test --json --tag loops
./target/debug/birddiskc test --json --tag repeat
./target/debug/birddiskc test --json --filter stdlib
./target/debug/birddiskc test --json --dir tests --tag unary
```

## VM-only fixtures
VM-only programs live under `vm_tests/`. Run them with the VM engine:
```sh
./target/debug/birddiskc test --json --engine vm --dir vm_tests
```
Currently, array fixtures are in `tests/arrays/` and run in VM + WASM + native.

## VM error fixtures
Programs expected to fail live under `vm_error_tests/`. They are not part
of the test harness; run them directly with `check` or `run` to inspect
diagnostics.

## WASM error fixtures
WASM-specific compile-time failures live under `wasm_error_tests/`. Run them with:
```sh
./target/debug/birddiskc test --json --engine wasm --dir wasm_error_tests
```

## Native error fixtures
Native-specific compile-time failures live under `native_error_tests/`. Run them with:
```sh
./target/debug/birddiskc test --json --engine native --dir native_error_tests
```

## Adding a test
1) Pick a folder or create a new tag folder.
2) Add a `.bd` file with a `rule main() -> i64:` entry point.
3) Keep the expected result obvious and stable.

## IO fixtures
For IO tests, place optional companion files alongside the `.bd` source:
- `<name>.stdin` supplies stdin content.
- `<name>.stdout` is the expected stdout string.
- `<name>.args` supplies command-line args (one per line) for `std::env::args()`.

Snapshot helpers:
- `birddiskc test --json --snapshot` writes `.stdout` files from actual output.
- Use `--engine` to choose the snapshot source (VM is default).

## Expected error fixtures
To assert a compile-time or runtime error, add a companion `.error` file:
- `<name>.error` contains one or more diagnostic codes (whitespace-separated).
- The test harness passes if any reported diagnostic matches one of the codes.
- If the program typechecks, VM, WASM, and native must report a matching error code.

## Perf harness
Performance fixtures live under `tests/perf/` and are executed with:
```sh
./target/debug/birddiskc perf --engine native
```

Notes:
- Keep perf fixtures deterministic and avoid stdout (use the normal test harness for correctness).
- `perf --update-baseline` writes `tests/perf/perf_baseline.json`.
- `perf` compares against the baseline only when one exists (or when `--baseline` is provided).
