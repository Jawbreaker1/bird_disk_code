# BirdDisk v0.1 — Specification (POC)

This spec is intentionally small and strict.
If something is not defined here, it is not part of v0.1.

## 1. Program structure
A program is a set of function definitions.
The entry point is:

- `rule main() -> i64: ... end`

## 2. Types
Built-in types:
- `i64`
- `bool`
- `T[]` array type (where `T` is a built-in type)

No implicit casts.
No floats in v0.1.

## 3. Names, scope, and shadowing
- Lexical, block-based scope.
- `set` introduces a new binding in the current scope.
- Shadowing is allowed in v0.1.

## 4. Functions
Syntax:
`rule <name>(<param>: <type>, ...) -> <type>: <stmts> end`

Rules:
- Parameter types are required.
- Return type is required.
- Functions are top-level only (no nesting) in v0.1.
- All control-flow paths must `yield` a value compatible with the declared return type.

## 5. Statements
All statements end with a `.` terminator, except block openers/closers.

### 5.1 Binding (`set`)
Syntax:
- `set name: Type = expr.`
- `set name = expr.` (type inference)

Rules:
- If `: Type` is present, `expr` must typecheck exactly as `Type`.
- If omitted, the compiler must infer the type of `expr`.
- If the type cannot be inferred unambiguously, it is a compile error (require explicit type annotation).

### 5.2 Assignment (`put`)
Syntax:
- `put name = expr.`
- `put name[index] = expr.`

Rules:
- `name` must resolve to an existing binding in scope.
- `expr` must typecheck to exactly the type of `name`.
- For indexed assignment, `name` must be an array, `index` must be `i64`,
  and `expr` must match the array element type.

### 5.3 Return (`yield`)
Syntax:
- `yield expr.`

Rules:
- `expr` type must match the function return type exactly.
- `yield` exits the current function.

### 5.4 Conditional (`when/otherwise/end`)
Syntax:

when cond:
…
otherwise:
…
end

Rules:
- `cond` must be `bool`.
- `otherwise` is required in v0.1 (simplifies typing and reduces ambiguity).
- Both branches are separate lexical scopes.
- The typechecker must ensure function-level “all paths yield” (it is OK if one branch yields and the other yields; if one branch doesn’t yield, later statements can still yield).

### 5.5 Loop (`repeat while/end`)
Syntax:
repeat while cond:
…
end

Rules:
- `cond` must be `bool`.
- Body is a lexical scope.
- `yield` is allowed inside loops.

## 6. Expressions
### 6.1 Literals
- Integer literal: `123` (type `i64`)
- Bool literal: `true`, `false` (type `bool`)

### 6.2 Identifiers
An identifier expression resolves to the type of the bound name.

### 6.3 Unary operators
- `-x` requires `x: i64` and returns `i64`
- `!x` requires `x: bool` and returns `bool`

### 6.4 Binary operators
Arithmetic (i64, i64 -> i64):
- `+ - * / %`

Comparison (i64, i64 -> bool):
- `== != < <= > >=`

Boolean (bool, bool -> bool):
- `&& ||`

Notes:
- `==` and `!=` are only defined for `i64` in v0.1 to keep things simple.
- Division or modulo by zero is a runtime error.

### 6.5 Function calls
Syntax:
`name(arg1, arg2, ...)`

Rules:
- `name` must be a known function.
- Arity must match.
- Argument types must match exactly.

### 6.6 Arrays
Syntax:
- Array literal: `[expr1, expr2, ...]` (or `[]`)
- Array constructor: `array(len)`
- Indexing: `expr[index]`

Rules:
- Full semantics are defined in section 8.

## 7. Type inference (v0.1)
Inference only applies to `set name = expr.` when `expr` has a well-defined type.

The type of `expr` is inferred by these rules:
- literals: `123 => i64`, `true/false => bool`
- unary ops: from operand rules
- binary ops: from operator rules
- function calls: from function signature return type
- identifier expressions: from binding type

If any step is ambiguous or unknown (e.g. unresolved name), inference fails and should surface as an error.

Notes:
- Empty array literals (`[]`) and `array(len)` require explicit array type context.

## 8. Arrays (v0.1)
BirdDisk supports fixed-length arrays in v0.1.

### 8.1 Types
- Array types use `T[]` where `T` is a built-in type or another array type.
- Nested arrays are allowed, e.g. `i64[][]`.
- Arrays have fixed length; elements are mutable via indexed assignment.

### 8.2 Array literals
Syntax:
- `[expr1, expr2, ...]`
- `[]`

Rules:
- All elements must have the same type.
- The array literal type is `T[]` where `T` is the element type.
- `[]` requires an explicit array type (e.g. `set xs: i64[] = [].`).

### 8.3 Array constructor
Syntax:
- `array(len)`

Rules:
- `len` must be `i64`.
- `array(len)` requires explicit array type context, e.g.:
  `set xs: i64[] = array(10).`
- The array is filled with default values (`0` for `i64`, `false` for `bool`).
- Negative lengths are runtime errors (E0400).

### 8.4 Indexing
Syntax:
- `xs[index]`
- `put xs[index] = expr.`

Rules:
- `xs` must be an array.
- `index` must be `i64`.
- The expression `xs[index]` has the element type of the array.
- `put xs[index] = expr.` requires `expr` to match the element type.
- Negative or out-of-bounds indices are runtime errors (E0403).
