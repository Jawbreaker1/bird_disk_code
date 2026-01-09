use birddisk_core::TraceFrame;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub code: &'static str,
    pub message: String,
    pub trace: Vec<TraceFrame>,
}

pub(crate) fn runtime_error(code: &'static str, message: impl Into<String>) -> RuntimeError {
    RuntimeError {
        code,
        message: message.into(),
        trace: Vec::new(),
    }
}
