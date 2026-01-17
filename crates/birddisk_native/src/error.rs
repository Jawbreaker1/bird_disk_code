use birddisk_core::TraceFrame;

#[derive(Debug, Clone)]
pub struct NativeError {
    pub code: Option<&'static str>,
    pub message: String,
    pub trace: Vec<TraceFrame>,
}

pub(crate) fn native_error(message: impl Into<String>) -> NativeError {
    NativeError {
        code: None,
        message: message.into(),
        trace: Vec::new(),
    }
}

pub(crate) fn native_error_with_code_and_trace(
    code: &'static str,
    message: impl Into<String>,
    trace: Vec<TraceFrame>,
) -> NativeError {
    NativeError {
        code: Some(code),
        message: message.into(),
        trace,
    }
}
