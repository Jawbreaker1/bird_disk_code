# Native backend (status + targets)

This document captures the current native backend coverage and known limitations.

## Supported targets (CI)
Native builds/tests are exercised in CI on:
- macOS 14 (arm64 / Apple Silicon)
- macOS 13 (x86_64 / Intel)
- Linux x86_64 (ubuntu-latest)
- Windows x86_64 (windows-latest)
- Linux arm64 (QEMU-emulated)

## What “native” means in BirdDisk
- JIT: uses Cranelift to compile and run on the host CPU.
- AOT: emits `.o` or executable via `rustc` (host toolchain required).

## Requirements
- A Rust toolchain is required for AOT builds (`--emit exe`).
- On Windows, the MSVC toolchain must be available in the environment.

## Known limitations
- Cross-compiling to non-host targets is not supported yet.
- Linux arm64 in CI runs under QEMU emulation (slower, not hardware-validated).
- Performance tuning is early; expect changes as GC/optimizer improve.

## Quick sanity check (host)
```sh
./target/debug/birddiskc run hello.bd --engine native --emit exe --out ./bird_hello
./bird_hello
```
