# BirdDisk quick reference (v0.1)

## Syntax snapshot
- Imports: `import std::module.` or `import app::module.`
- Functions: `rule`
- Bindings: `set`
- Assignment: `put`
- Conditionals: `when / otherwise / end`
- Loops: `repeat while / end`
- Error handling: `try / catch / throw`
- Return: `yield`
- Blocks: `:` ... `end`
- Statement terminator: `.`

Example:
```birddisk
rule main() -> i64:
  set x = 10.
  when x > 5:
    yield 1.
  otherwise:
    yield 2.
  end
end
```

## Typing model
- Built-in types: i64, bool, string, u8, void
- Array types: T[]
- Function params and return types are always explicit
- `set name = expr.` may omit the type if expr is inferable
- No implicit casts
- stdlib string ops live in `std::string` (import required)
- byte helpers live in `std::bytes` (import required)
- `std::string::from_bytes(u8[])` validates UTF-8 and returns a string
- `std::string::to_i64(string)` parses a decimal integer; `std::string::from_i64(i64)` formats one
- JSON helpers live in `std::json` (encode/decode of i64/bool/string)
- stdlib modules on disk live under `stdlib/` (e.g. `import std::math.`)
- non-stdlib modules resolve to `<path>.bd` (entry dir first, then project root)
- book types are declared with `book` and constructed via `new Book(...)`

See `docs/SPEC.md` and `GRAMMAR.md` for the full language reference.
