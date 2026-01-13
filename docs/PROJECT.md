# BirdDisk project structure

## Repo layout
docs/
  QUICKREF.md
  SPEC.md
  GRAMMAR.md (repo root)
  DIAGNOSTICS.md
  COOKBOOK.md
  STYLES.md
  DECISIONS.md
  RUNTIME.md
  VSCODE.md
  CLI.md
  STATUS.md
crates/
  birddiskc/        # CLI compiler driver
  birddisk_core/    # lexer/parser/AST/types/diagnostics/formatter
  birddisk_native_runtime/ # native runtime support (AOT/JIT)
  birddisk_vm/      # interpreter (golden)
  birddisk_wasm/    # wasm codegen
examples/
tests/
vm_tests/          # VM-only fixtures (reserved for future features)
vm_error_tests/    # VM fixtures expected to fail (diagnostics/runtime errors)
eval/              # LLM syntax evaluation tasks + scoring notes

## Development principles
- Keep the language core small and orthogonal.
- Deterministic formatting and stable AST printing.
- Diagnostics must be structured and actionable (fix-its where possible).
- Every new feature must update:
  - `docs/SPEC.md`
  - `GRAMMAR.md`
  - `docs/COOKBOOK.md`
  - tests (VM + WASM parity once WASM exists)
