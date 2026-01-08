BirdDisk stdlib (v0.1)

Layout
- stdlib/std/<module>.bd
- import paths map directly to file paths
  - import std::math. -> stdlib/std/math.bd

Modules
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

Notes
- std::string, std::bytes, and std::io are still implemented in Rust.
- BirdDisk stdlib modules are currently self-contained and should not
  import other modules yet.
- Stdlib versioning is tied to the compiler version during v0.x.
