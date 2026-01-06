# Eval Harness (minimal)

This folder contains a small, CLI-driven evaluation harness for BirdDisk.
It is intentionally small and will grow into a full suite later.

Goals:
- Run small tasks through `birddisk check` and `birddisk run`
- Provide a stable task schema for cross-language comparisons
- Keep requirements minimal (Python 3 + birddisk CLI)

Files:
- `eval/tasks.json`: task definitions
- `eval/run.py`: task runner

Task schema (minimal)
```json
{
  "id": "run_minimal_main_001",
  "kind": "run",
  "input": "rule main() -> i64:\n  yield 0.\nend\n",
  "engine": "vm",
  "expect": {
    "ok": true,
    "result": 0,
    "codes": ["E0000"]
  }
}
```
Notes:
- `expect.result` and `expect.codes` are optional.
- `engine` is optional for `run` and defaults to `vm` in the runner.

Expected JSON output
- `birddisk check --json` should follow `docs/DIAGNOSTICS.md`.
- `birddisk run --json` should return:
```json
{
  "tool": "birddisk",
  "version": "0.1",
  "ok": true,
  "result": 0
}
```
For errors, `ok` should be `false` with `diagnostics` per the diagnostics schema.

Usage:
```sh
cargo build -p birddiskc
python3 eval/run.py --birddisk ./target/debug/birddisk
```
