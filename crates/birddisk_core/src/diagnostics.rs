use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Position {
    pub line: u32,
    pub col: u32,
}

impl Position {
    pub fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Edit {
    pub file: String,
    pub span: Span,
    pub replacement: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FixIt {
    pub title: String,
    pub edits: Vec<Edit>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct TraceFrame {
    pub function: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Diagnostic {
    pub code: &'static str,
    pub severity: &'static str,
    pub message: String,
    pub file: String,
    pub span: Span,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub trace: Vec<TraceFrame>,
    pub notes: Vec<String>,
    pub spec_refs: Vec<String>,
    pub fixits: Vec<FixIt>,
    pub help: Option<String>,
}

pub(crate) fn diagnostic(
    code: &'static str,
    severity: &'static str,
    message: String,
    file: &str,
    span: Span,
    notes: Vec<String>,
    spec_refs: Vec<String>,
    fixits: Vec<FixIt>,
    help: Option<String>,
) -> Diagnostic {
    Diagnostic {
        code,
        severity,
        message,
        file: file.to_string(),
        span,
        trace: Vec::new(),
        notes,
        spec_refs,
        fixits,
        help,
    }
}
