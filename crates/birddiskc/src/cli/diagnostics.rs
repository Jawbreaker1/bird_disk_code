pub(crate) fn mismatch_diagnostic(
    path: &str,
    vm_result: i64,
    wasm_result: i64,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0500",
        severity: "error",
        message: format!("Backend mismatch: left={vm_result}, right={wasm_result}."),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Differential test failure.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn output_mismatch_diagnostic(
    path: &str,
    left_engine: &str,
    right_engine: &str,
    left: &str,
    right: &str,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0502",
        severity: "error",
        message: format!(
            "Backend output mismatch: {left_engine}='{left}', {right_engine}='{right}'."
        ),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["IO output differs between backends.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn output_expected_diagnostic(
    path: &str,
    engine: &str,
    expected: &str,
    actual: &str,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0502",
        severity: "error",
        message: format!("Output mismatch ({engine}): expected='{expected}', got='{actual}'."),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Output does not match .stdout fixture.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn expected_error_diagnostic(
    path: &str,
    message: impl Into<String>,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0503",
        severity: "error",
        message: message.into(),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Expected error did not match.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn harness_diagnostic(
    path: &str,
    message: String,
    code: &'static str,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code,
        severity: "error",
        message,
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Test harness error.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn test_harness_diagnostic(message: impl Into<String>) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0501",
        severity: "error",
        message: message.into(),
        file: "<tests>".to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Test harness error.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn perf_harness_diagnostic(
    path: &str,
    message: impl Into<String>,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0504",
        severity: "error",
        message: message.into(),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Performance harness error.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn wasm_threading_diagnostic(path: &str) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0325",
        severity: "error",
        message: "WASM backend does not support std::thread.".to_string(),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Threading is supported only in the VM backend for now.".to_string()],
        spec_refs: vec!["SPEC.md#14-concurrency-planned-not-implemented-in-v0-1".to_string()],
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn wasm_net_diagnostic(path: &str) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0326",
        severity: "error",
        message: "WASM backend does not support std::net.".to_string(),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["TCP networking is supported only in VM/native backends for now.".to_string()],
        spec_refs: vec!["SPEC.md#144-stdnet-module-planned".to_string()],
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn runtime_diagnostic(
    path: &str,
    message: String,
    code: &'static str,
    spec_refs: Vec<String>,
    trace: Vec<birddisk_core::TraceFrame>,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code,
        severity: "error",
        message,
        file: path.to_string(),
        span: birddisk_core::Span::new(
            birddisk_core::Position::new(1, 1),
            birddisk_core::Position::new(1, 1),
        ),
        trace,
        notes: vec!["Runtime error".to_string()],
        spec_refs,
        fixits: Vec::new(),
        help: None,
    }
}

pub(crate) fn runtime_spec_refs(code: &str) -> Vec<String> {
    match code {
        "E0402" => vec!["SPEC.md#6-4-binary-operators".to_string()],
        "E0403" => vec!["SPEC.md#8-4-indexing".to_string()],
        "E0404" => vec!["SPEC.md#5-7-error-handling-try-catch-throw".to_string()],
        "E0405" => vec!["SPEC.md#142-stdthread-module-planned".to_string()],
        "E0406" => vec!["SPEC.md#143-stdchannel-module-planned".to_string()],
        "E0407" => vec!["SPEC.md#143-stdchannel-module-planned".to_string()],
        "E0408" => vec!["SPEC.md#144-stdnet-module-planned".to_string()],
        _ => Vec::new(),
    }
}

pub(crate) fn require_tests_diagnostic(path: &str, expected: &str) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "L2000",
        severity: "error",
        message: format!("Missing test file for '{path}'."),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec![format!("Expected test file at '{expected}'.")],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: Some("Add the test file with matching `rule test_*() -> void` rules.".to_string()),
    }
}

pub(crate) fn require_tests_rule_diagnostic(
    path: &str,
    rule: &str,
    expected: &str,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "L2002",
        severity: "error",
        message: format!("Missing test rule '{expected}' for '{rule}'."),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Per-rule tests are required when --require-tests is enabled.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: Some("Add the missing test rule to the test file.".to_string()),
    }
}

pub(crate) fn require_tests_config_diagnostic(
    message: impl Into<String>,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "L2001",
        severity: "error",
        message: message.into(),
        file: "<tests>".to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Require-tests enforcement could not be applied.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: Some("Add a birddisk.json manifest or pass an explicit entry file.".to_string()),
    }
}

fn default_span() -> birddisk_core::Span {
    birddisk_core::Span::new(
        birddisk_core::Position::new(1, 1),
        birddisk_core::Position::new(1, 1),
    )
}

pub(crate) fn format_diagnostics_human(diagnostics: &[birddisk_core::Diagnostic]) -> String {
    if diagnostics.is_empty() {
        return "error: unknown failure".to_string();
    }
    let mut out = String::new();
    for (idx, diag) in diagnostics.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        out.push_str(&format!(
            "{}[{}]: {}\n",
            diag.severity, diag.code, diag.message
        ));
        if let Some(location) = primary_location(diag) {
            out.push_str(&format!(
                "  --> {}:{}:{}\n",
                location.file, location.line, location.col
            ));
            if let Some(function) = location.function {
                out.push_str(&format!("  in {function}\n"));
            }
            if let Some(source) = location.source_line {
                out.push_str(&format!("  | {source}\n"));
            }
        }
        if !diag.trace.is_empty() {
            out.push_str("  stack trace:\n");
            for (i, frame) in diag.trace.iter().enumerate() {
                out.push_str(&format!(
                    "    {i}: {} ({}:{}:{})\n",
                    frame.function, frame.file, frame.span.start.line, frame.span.start.col
                ));
            }
        }
        if !diag.notes.is_empty() {
            out.push_str("  notes:\n");
            for note in &diag.notes {
                out.push_str(&format!("    - {note}\n"));
            }
        }
        if !diag.spec_refs.is_empty() {
            out.push_str(&format!("  see: {}\n", diag.spec_refs.join(", ")));
        }
    }
    out.trim_end().to_string()
}

struct Location {
    file: String,
    line: u32,
    col: u32,
    function: Option<String>,
    source_line: Option<String>,
}

fn primary_location(diag: &birddisk_core::Diagnostic) -> Option<Location> {
    if let Some(frame) = diag.trace.first() {
        let source_line = first_source_line(&frame.source);
        return Some(Location {
            file: frame.file.clone(),
            line: frame.span.start.line,
            col: frame.span.start.col,
            function: Some(frame.function.clone()),
            source_line,
        });
    }
    if !diag.file.is_empty() {
        return Some(Location {
            file: diag.file.clone(),
            line: diag.span.start.line,
            col: diag.span.start.col,
            function: None,
            source_line: read_source_line(&diag.file, diag.span.start.line),
        });
    }
    None
}

fn first_source_line(source: &str) -> Option<String> {
    for line in source.lines() {
        let trimmed = line.trim();
        if !trimmed.is_empty() {
            return Some(trimmed.to_string());
        }
    }
    None
}

fn read_source_line(path: &str, line: u32) -> Option<String> {
    let content = std::fs::read_to_string(path).ok()?;
    let line_index = line.saturating_sub(1) as usize;
    content
        .lines()
        .nth(line_index)
        .map(|value| value.trim_end().to_string())
}

#[cfg(test)]
mod tests {
    use super::{format_diagnostics_human, runtime_diagnostic};
    use birddisk_core::{Position, Span, TraceFrame};

    #[test]
    fn format_runtime_diagnostic_with_trace() {
        let trace = vec![
            TraceFrame {
                function: "boom".to_string(),
                file: "main.bd".to_string(),
                span: Span::new(Position::new(12, 5), Position::new(12, 10)),
                source: "rule boom() -> i64:".to_string(),
            },
            TraceFrame {
                function: "main".to_string(),
                file: "main.bd".to_string(),
                span: Span::new(Position::new(1, 1), Position::new(1, 1)),
                source: "rule main() -> i64:".to_string(),
            },
        ];
        let diag = runtime_diagnostic(
            "main.bd",
            "Array index out of bounds.".to_string(),
            "E0403",
            vec!["SPEC.md#8-4-indexing".to_string()],
            trace,
        );
        let output = format_diagnostics_human(&[diag]);
        assert!(output.contains("error[E0403]: Array index out of bounds."));
        assert!(output.contains("boom (main.bd:12:5)"));
        assert!(output.contains("stack trace"));
        assert!(output.contains("rule boom() -> i64:"));
    }
}
