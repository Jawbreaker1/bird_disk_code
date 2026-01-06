# BirdDisk diagnostics JSON schema (v0.1)

The compiler must be able to emit diagnostics in JSON for agent workflows.

## Top-level shape
```json
{
  "tool": "birddisk",
  "version": "0.1",
  "ok": false,
  "diagnostics": []
}
Diagnostic object
{
  "code": "E0203",
  "severity": "error",
  "message": "Type mismatch: expected i64, got bool",
  "file": "examples/test.bd",
  "span": {
    "start": { "line": 3, "col": 12 },
    "end": { "line": 3, "col": 16 }
  },
  "notes": [
    "Arithmetic operators require i64 operands."
  ],
  "help": "Change the expression to produce an i64 value.",
  "spec_refs": [
    "SPEC.md#6-4-binary-operators"
  ],
  "fixits": [
    {
      "title": "Replace `true` with `1`",
      "edits": [
        {
          "file": "examples/test.bd",
          "span": {
            "start": { "line": 3, "col": 12 },
            "end": { "line": 3, "col": 16 }
          },
          "replacement": "1"
        }
      ]
    }
  ]
}

Stability requirements
	•	code must be stable for the same class of error.
	•	span must be best-effort accurate.
	•	spec_refs should point to doc anchors where possible.

## Error code table (v0.1)
These codes are the current stable set. New codes should only be added
when needed and documented here.

E0000: Not implemented (compiler stub)
E0001: Unable to read file
E0002: Expected at least one function rule

E0100: Unexpected character
E0101: Integer literal out of range
E0102: Comments are not supported in v0.1

E0200: Unexpected token in statement or syntax position
E0201: Expected 'rule' to start a function
E0202: Unexpected end of file while parsing block
E0203: Expected expression
E0205: Expected identifier
E0206: Expected type
E0207: Missing 'otherwise' block for when statement
E0208: Unexpected 'otherwise'

E0300: Type mismatch
E0301: Unknown name
E0302: Wrong number of arguments
E0303: Unknown function
E0304: Condition must be bool
E0305: Cannot infer type for binding
E0306: Not all control-flow paths yield a value
E0307: Duplicate function
E0308: Duplicate parameter
E0309: Missing 'main' entry point

E0400: Runtime error
E0401: WASM backend error (reserved)
E0402: Division or modulo by zero

Minimum required fields

Required:
	•	code
	•	severity
	•	message
	•	file
	•	span

Optional but strongly recommended:
	•	notes
	•	help
	•	spec_refs
	•	fixits
