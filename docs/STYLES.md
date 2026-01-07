# BirdDisk Style Guide (v0.1)

This document describes the **canonical formatting rules** for BirdDisk.
The formatter (`birddisk fmt`) is the single source of truth.

If code does not match this style, it is considered *unformatted*.

---

## General rules
- Indentation: **2 spaces**
- One statement per line
- Statements end with `.` (dot)
- Blocks open with `:` and close with `end`
- No trailing whitespace
- Unix line endings (`\n`)

---

## Blocks
Blocks always follow this structure:

```birddisk
keyword condition:
  statement.
  statement.
end

Examples:
when x > 5:
  yield 1.
otherwise:
  yield 2.
end

repeat while i < 10:
  put i = i + 1.
end
```

Functions

Functions are always top-level.

rule add(a: i64, b: i64) -> i64:
  yield a + b.
end

Rules:
	•	rule starts at column 0
	•	Parameters are separated by ,  (comma + space)
	•	Return type uses ->
	•	No blank line between rule and first statement
	•	end always on its own line

Bindings and assignment
set x = 10.
set y: i64 = 20.
put x = x + y.

Rules:
	•	Exactly one space around =
	•	Type annotation uses :  (colon + space)
	•	set introduces a new binding
	•	put updates an existing binding

Expressions
	•	Binary operators have spaces around them: a + b
	•	Unary operators have no space: -x, !flag
	•	Parentheses are allowed for clarity

set x = (a + b) * c.

Arrays
	•	Array types are written as T[]: i64[], bool[]
	•	Array literals use [a, b, c] with spaces after commas
	•	Array constructor uses array(len)
	•	Indexing has no spaces: xs[0], xs[i]

set xs: i64[] = [1, 2, 3].
put xs[1] = 7.
yield xs[0] + xs[1].

Control flow

when / otherwise
	•	otherwise is always required in v0.1
	•	otherwise aligns with when

when flag:
  yield 1.
otherwise:
  yield 0.
end

repeat while
repeat while condition:
  put counter = counter + 1.
end

Blank lines
	•	One blank line allowed between logical sections
	•	No blank lines inside small blocks
	•	Formatter may remove or normalize blank lines

⸻

Comments (reserved for v0.2)
	•	Comments are not part of v0.1
	•	Any comment syntax is reserved and must error if encountered

⸻

Formatter contract

The formatter must be:
	•	Deterministic
	•	Idempotent: fmt(fmt(code)) == fmt(code)
	•	Lossless (no semantic changes)

If the formatter changes code, it must only change whitespace and layout.
