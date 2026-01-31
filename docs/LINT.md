# BirdDisk linter (early)

The linter flags patterns that are legal but harm LLM reliability or make
automated fixes harder. It is intentionally strict and will grow over time.

Run:
```
birddiskc lint <file|dir> [--json]
```

Current rules (v0.1)

L1001 — Missing explicit type on `set`
- Prefer `set value: i64 = ...` over `set value = ...`
- Reason: explicit types reduce ambiguity for LLMs.

L1002 — Too-short names
- Single-character names are discouraged except `i`, `j`, `k`.
- Reason: descriptive names improve code generation and repair.

L1003 — Deep nesting
- Warns when nesting depth exceeds 3.
- Reason: deep nesting reduces LLM correctness; extract helper rules.

L1004 — Unused local binding
- Warns when a `set` binding is never used.
- Reason: unused locals add noise for LLMs.

L1005 — Unused parameter
- Warns when a rule parameter is never used.
- Reason: unused params add noise for LLMs.

L1006 — Shadowing
- Warns when a binding shadows an existing name in an outer scope.
- Reason: shadowing makes LLM edits more error-prone.

L1007 — Unused import
- Warns when an `import` is never referenced in the file.
- Reason: unused imports add noise for LLMs.

Notes
- Lints are warnings (non-fatal) but `birddiskc lint` exits non-zero if any
  warnings are emitted.
- Rules are expected to expand as the language grows.
- Names starting with `_` are treated as intentionally unused and will not
  trigger unused warnings.
