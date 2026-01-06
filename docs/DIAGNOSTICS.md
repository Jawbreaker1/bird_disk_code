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

