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
- Differential test harness (`birddiskc test --json`, VM + WASM + native)
- Formatter (`birddiskc fmt`)
- Native backend (JIT + AOT via Cranelift)
- Arrays + indexing (VM + WASM + native)
- Strings + std::string (VM + WASM + native)
- try/catch/throw error handling (VM + WASM + native)
- u8 + std::bytes + std::string::bytes (VM + WASM + native)
- f64 floats (VM + WASM + native)
- explicit i64 <-> f64 casts (`as`)
- std::io (VM + WASM + native)
- std::time (VM + WASM + native)
- std::profiler (VM + WASM + native)
- std::rand (VM + WASM + native)
- std::test (VM + WASM + native)
- std::fs (VM + WASM + native)
- std::path (VM + WASM + native)
- std::env (VM + WASM + native)
- std::json (VM + WASM + native)
- std::channel typed channels (i64/bool/f64/u8/string/bytes) (VM + WASM + native)
- std::thread spawn/join (VM + native; host-OS threaded path for `()`/`(i64)` entry signatures in non-deterministic mode, plus native `(TcpStream, i64)` worker handoff; WASM emits compile-time diagnostic E0325)
- std::net TCP (connect/listen/accept/read_line/read_exact/read_to_end/write_text/timeout/close + TcpPool get/put/close) (VM + native; WASM emits compile-time diagnostic E0326)
- std::http minimal client module (GET/POST + status/headers parsing with Content-Length and chunked body decoding; EOF fallback when both are missing, VM + native)
- VM deterministic thread scheduler mode (`--deterministic`) for stable concurrency fixtures
- stdlib module loading + std::math (BirdDisk)
- Project manifest + build workflow (`birddisk.json`, `birddiskc build`)
- OO core (book + fields + methods, VM + WASM + native)
- Enums + match (VM + WASM + native implemented)
- Optimization pass (const folding + branch pruning + basic DCE + inlining)
- Performance regression harness (`birddiskc perf`)
- GC tuning (thresholded collection + live bytes tracking)
- CI native build matrix (macOS/Linux/Windows + Linux arm64 emulated)
- Native backend targets + limitations documented (docs/NATIVE.md)
- VSCode extension: LSP hover, go-to def/type, references (imports + open docs), signature help (rules + constructors + stdlib), document/workspace symbols, type-aware member + module completions

## Upcoming (planned)
- VSCode extension maintenance (syntax/LSP updates)
- GC improvements (free-list coalescing + reuse)
- Native backend expansion (more targets + AOT polish)

## Longer-term
- Broaden native coverage (arm64 + x86_64) and scalable multi-CPU support.
- VM is the golden reference; WASM and native backends are compared against it.

## Roadmap
See `TASKS.md`.
