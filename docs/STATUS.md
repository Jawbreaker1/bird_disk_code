# BirdDisk status

## Targets
- VM/interpreter (implemented, golden reference)
- wasm32 (WASM backend, minimal)
- Native (host JIT + AOT object/exe) via Cranelift

## Implemented
- Lexer, parser, AST, typechecker
- JSON diagnostics (check/run) + fix-its + suggestions
- Eval harness with task runner
- WASM backend (via wasmtime)
- WASM emission (`birddiskc run --engine wasm --emit wasm`)
- WAT emission (`birddiskc run --engine wasm --emit wat`)
- Differential test harness (`birddiskc test --json`)
- Formatter (`birddiskc fmt`)
- Native backend (JIT + AOT via Cranelift)
- Arrays + indexing (VM + WASM + native)
- Strings + std::string (VM + WASM + native)
- try/catch/throw error handling (VM + WASM + native)
- u8 + std::bytes + std::string::bytes (VM + WASM + native)
- std::io (VM + WASM + native)
- std::time (VM + WASM + native)
- std::fs (VM + WASM + native)
- std::path (VM + WASM + native)
- std::env (VM + WASM + native)
- std::json (VM + WASM + native)
- stdlib module loading + std::math (BirdDisk)
- OO core (book + fields + methods, VM + WASM + native)
- VSCode extension: LSP hover, go-to def/type, references (imports + open docs), signature help (rules + constructors + stdlib), document/workspace symbols, type-aware member + module completions

## Upcoming (planned)
- VSCode extension maintenance (syntax/LSP updates)
- GC improvements (free-list coalescing + reuse)
- Multi-file project manifests + build workflow
- Native backend expansion (more targets + AOT polish)

## Longer-term
- Broaden native coverage (arm64 + x86_64) and scalable multi-CPU support.
- VM is the golden reference; WASM and native backends are compared against it.

## Roadmap
See `TASKS.md`.
