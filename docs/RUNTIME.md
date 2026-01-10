# BirdDisk runtime memory (draft)

This document describes the planned GC-backed runtime layout for v0.x.
It is intentionally concrete so VM/WASM/native implementations stay aligned.

## Goals
- Reliability first (deterministic behavior, debuggable layout).
- Good performance once the runtime matures (room for future optimizations).

## Heap object layout
All heap objects use a uniform header (aligned to 8 bytes).

Header (16 bytes, little-endian):
- `u32 tag`   = kind (upper 8 bits) + type id (lower 24 bits)
- `u32 flags` = mark bits + reserved
- `u32 len_or_size` = string byte length / array element count / object field count
- `u32 aux`   = array element kind (for arrays), reserved for other kinds

Kinds:
- `1 = string`
- `2 = array`
- `3 = object`
- `255 = free` (used by the allocator)

Type id rules:
- `object` uses book id (compiler-assigned).
- `string` and `array` use `0` for now.

Array element kinds (aux):
- `1 = i64`
- `2 = bool`
- `3 = u8`
- `4 = ref` (string/array/object pointer)

Payload layout:
- Strings: UTF-8 bytes immediately after header.
- Arrays: packed elements; size computed from element kind + count.
- Objects: fixed-width fields (8 bytes per field) for v0.x consistency.

Notes:
- VM and WASM now use the uniform header for all heap objects.
- Canonical numeric constants live in `crates/birddisk_core/src/runtime.rs`.

## Root tracking (explicit root stack)
Root stack is maintained by codegen/runtime (no conservative scan).

Conceptual API:
- `root_push_frame(slot_count) -> base_slot`
- `root_set(base_slot + idx, ptr)`
- `root_pop_frame(slot_count)`

Compiler rules:
- Each function precomputes the number of reference-typed locals/temps.
- On entry, push a frame for those slots.
- On assignment to a reference local, update its slot.
- On return, pop the frame.

VM implementation:
- Use `Vec<Vec<Value>>` or a flat `Vec<Value>` with frame offsets.

WASM implementation:
- Shadow stack stored in linear memory with fixed-capacity growth.
- Similar to trace stack (pointer + contiguous slot array).

Native implementation:
- Thread-local root stack.

## GC algorithm (stop-the-world mark/sweep)
Trigger:
- Allocation checks heap usage; if above threshold, run GC.
- Threshold grows with live size (e.g., 2x live bytes).

Mark:
- Start from root stack and globals.
- For each heap object:
  - Mark the header.
  - If `array` with elem kind `ref`, mark each element pointer.
  - If `object`, walk reference fields via the book layout table.
  - Strings have no references.

Sweep:
- Walk heap blocks and free unmarked ones.
- Add freed blocks to a free list; reuse for future allocations.
- Clear mark bits on live blocks.

Allocator:
- Try free list first, then bump pointer.
- If allocation fails, GC then retry; if still fails, trap OOM.
- Free blocks use `kind=255` and store `size` + `next` in header fields.
- Free list is kept address-sorted and coalesced to reduce fragmentation.

Debug checks (tests only):
- Optional header sanity checks validate kind/aux and trap with "Invalid heap header."

## GC metrics (v0.x)
Expose simple counters for trust + performance tracking:
- `alloc_count`, `bytes_allocated` (lifetime counters).
- `bytes_in_use`, `peak_bytes_in_use` (live memory tracking).
- `gc_runs`, `last_freed`, `last_live` (object counts).
- `last_freed_bytes`, `last_live_bytes` (per-GC memory stats).
- WASM tests expose `__bd_gc_last_freed` and `__bd_heap_high_water`.

## Book layout table (object scanning)
Compiler emits a table of reference-field offsets per book id.
GC uses this table to traverse only reference fields in objects.

## Test plan (GC sprint)
- Allocation pressure triggers GC and preserves live values.
- Cycles are collected (object <-> object, array cycles).
- Arrays/strings scanned correctly (nested arrays, mixed element kinds).
- OOM and null deref produce stable error codes + stack traces.

## Current GC sprint status
- VM: root stack, mark/sweep, and GC metrics in place with parity tests. VM does not yet reuse or coalesce freed blocks (known perf gap).
- WASM: root stack, mark/sweep, free list reuse + coalescing, and test-only stats exports.
- Header sanity checks available in tests (invalid kind/aux traps).
