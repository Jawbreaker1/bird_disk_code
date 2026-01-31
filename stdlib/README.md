BirdDisk stdlib (v0.1)

Layout
- stdlib/std/<module>.bd
- import paths map directly to file paths
  - import std::math. -> stdlib/std/math.bd

Modules (BirdDisk)
- std::math
  - add(a, b) -> i64
  - sub(a, b) -> i64
  - mul(a, b) -> i64
  - div(a, b) -> i64
  - mod(a, b) -> i64
  - abs(value) -> i64
  - sign(value) -> i64
  - is_even(value) -> bool
  - clamp(value, low, high) -> i64
  - min(a, b) -> i64
  - max(a, b) -> i64
  - pow(base, exp) -> i64 (exp < 0 yields 0)
  - gcd(a, b) -> i64
  - lcm(a, b) -> i64
- std::array
  - sum_i64(xs, len) -> i64
  - min_i64(xs, len) -> i64
  - max_i64(xs, len) -> i64
  - contains_i64(xs, len, value) -> bool
  - index_of_i64(xs, len, value) -> i64
  - count_i64(xs, len, value) -> i64

Runtime modules (implemented in Rust)
- std::string
  - len(text: string) -> i64
  - concat(left: string, right: string) -> string
  - eq(left: string, right: string) -> bool
  - bytes(text: string) -> u8[]
  - slice(text: string, start: i64, len: i64) -> string
  - index_of(text: string, needle: string) -> i64
  - contains(text: string, needle: string) -> bool
  - replace(text: string, needle: string, replacement: string) -> string
  - from_bytes(bytes: u8[]) -> string
  - to_i64(text: string) -> i64
  - from_i64(value: i64) -> string
- std::bytes
  - len(bytes: u8[]) -> i64
  - eq(left: u8[], right: u8[]) -> bool
  - slice(bytes: u8[], start: i64, len: i64) -> u8[]
  - index_of(bytes: u8[], needle: u8) -> i64
  - contains(bytes: u8[], needle: u8) -> bool
- std::io
- std::time
  - now_ms() -> i64
  - sleep_ms(ms: i64) -> i64
- std::rand
  - seed(seed: i64) -> void
  - range(min: i64, max: i64) -> i64
- std::fs
  - read_text(path: string) -> string
  - write_text(path: string, text: string) -> i64
  - read_bytes(path: string) -> u8[]
  - write_bytes(path: string, bytes: u8[]) -> i64
- std::path
  - join(left: string, right: string) -> string
  - normalize(path: string) -> string
  - basename(path: string) -> string
  - dirname(path: string) -> string
- std::env
  - args() -> string[]
  - get(name: string) -> string
  - set_var(name: string, value: string) -> i64
  - cwd() -> string
  - set_cwd(path: string) -> i64
- std::json
  - encode_i64(value: i64) -> string
  - encode_bool(value: bool) -> string
  - encode_string(text: string) -> string
  - decode_i64(text: string) -> i64
  - decode_bool(text: string) -> bool
  - decode_string(text: string) -> string

Notes
- std::string, std::bytes, std::io, std::time, std::fs, std::path, std::env, and std::json are still implemented in Rust.
- BirdDisk stdlib modules are currently self-contained and should not
  import other modules yet.
- Stdlib versioning is tied to the compiler version during v0.x.
- For now, array utilities take the array length explicitly.
