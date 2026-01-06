# BirdDisk — AI-first compiled language (POC)

BirdDisk is a proof-of-concept compiled programming language and toolchain designed for an agentic/LLM-driven workflow.

BirdDisk focuses on:
- **Unique, LLM-friendly syntax** (low ambiguity, few special cases)
- **Strong static typing** with **local type inference**
- **Deterministic formatter** (one canonical style)
- **Structured JSON diagnostics + fix-its** (machine actionable)
- **Golden reference execution** (VM/interpreter)
- **WASM as first compilation target** (portable baseline)
- **Differential testing**: VM output == WASM output

Longer-term: native backends (arm64 + x86_64) and scalable multi-CPU support via continuous automated validation.

## BirdDisk syntax (v0.1)
BirdDisk code is visually distinct:
- Functions: `rule`
- Bindings: `set`
- Assignment: `put`
- Conditionals: `when / otherwise / end`
- Loops: `repeat while / end`
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

Typing model (v0.1)
	•	Built-in types: i64, bool
	•	Function params and return types are always explicit
	•	set name = expr. may omit the type if expr is inferable
	•	No implicit casts

See docs/SPEC.md.

Targets (planned)
	•	Golden: VM/interpreter
	•	Portable: wasm32 (WASM backend)
	•	Later: native aarch64 (arm64) + x86_64 via LLVM or Cranelift


Repo layout (planned)
docs/
  SPEC.md
  GRAMMAR.ebnf
  DIAGNOSTICS.md
  COOKBOOK.md
  STYLE.md
crates/
  birddiskc/        # CLI compiler driver
  birddisk_core/    # lexer/parser/AST/types/diagnostics/formatter
  birddisk_vm/      # interpreter (golden)
  birddisk_wasm/    # wasm codegen
examples/
tests/
eval/               # LLM syntax evaluation tasks + scoring notes

CLI (planned)

All commands should support JSON output for agent workflows.
	•	birddisk fmt <file|dir>
	•	birddisk check <file|dir> [--json]
	•	birddisk run <file> [--engine vm|wasm] [--json]
	•	birddisk test [--json]

Development principles
	•	Keep the language core small and orthogonal.
	•	Deterministic formatting and stable AST printing.
	•	Diagnostics must be structured and actionable (fix-its where possible).
	•	Every new feature must update:
	•	docs/SPEC.md
	•	docs/GRAMMAR.ebnf
	•	docs/COOKBOOK.md
	•	tests (VM + WASM parity once WASM exists)

Roadmap

See TASKS.md.
For agent workflow rules, see AGENT.md.

License

TBD (MIT recommended for early POC).
