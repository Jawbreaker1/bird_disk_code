# BirdDisk tests

This folder contains runnable BirdDisk programs used by the differential
test harness (`birddisk test --json`).

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
./target/debug/birddisk test --json --tag loops
./target/debug/birddisk test --json --tag repeat
./target/debug/birddisk test --json --dir tests --tag unary
```

## VM-only fixtures
VM-only programs live under `vm_tests/`. Run them with the VM engine:
```sh
./target/debug/birddisk test --json --engine vm --dir vm_tests
```
Currently, array fixtures are in `tests/arrays/` and run in VM + WASM.

## VM error fixtures
Programs expected to fail live under `vm_error_tests/`. They are not part
of the test harness; run them directly with `check` or `run` to inspect
diagnostics.

## Adding a test
1) Pick a folder or create a new tag folder.
2) Add a `.bd` file with a `rule main() -> i64:` entry point.
3) Keep the expected result obvious and stable.

## IO fixtures
For IO tests, place optional companion files alongside the `.bd` source:
- `<name>.stdin` supplies stdin content.
- `<name>.stdout` is the expected stdout string.

## Expected error fixtures
To assert a compile-time or runtime error, add a companion `.error` file:
- `<name>.error` contains one or more diagnostic codes (whitespace-separated).
- The test harness passes if any reported diagnostic matches one of the codes.
- If the program typechecks, both VM and WASM must report a matching error code.
