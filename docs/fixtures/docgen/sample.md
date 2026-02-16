# BirdDisk Docs
Entry: docs/fixtures/docgen/sample.bd

## Enums
### Result
- Ok(value: i64)
- Err(message: string)

## Books
### Counter
Fields:
- value: i64
Methods:
- rule inc(self: Counter) -> Counter
- rule init(self: Counter, start: i64) -> Counter

## Functions
- rule add(left: i64, right: i64) -> i64 (docs/fixtures/docgen/sample.bd)
- rule main() -> i64 (docs/fixtures/docgen/sample.bd)
