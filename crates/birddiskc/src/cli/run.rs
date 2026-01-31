use super::diagnostics::{runtime_diagnostic, runtime_spec_refs};

pub(crate) fn run_report(
    path: &str,
    config: &birddisk_core::ModuleConfig,
    engine: birddisk_core::Engine,
    input: &str,
    args: &[String],
) -> birddisk_core::RunReport {
    if engine == birddisk_core::Engine::Wasm {
        let bytes = match std::fs::read(path) {
            Ok(bytes) => bytes,
            Err(err) => {
                return birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    stdout: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        format!("Unable to read file: {err}"),
                        "E0400",
                        runtime_spec_refs("E0400"),
                        Vec::new(),
                    )],
                };
            }
        };
        if is_wasm_bytes(&bytes) {
            return match birddisk_wasm::run_wasm_bytes_with_io(&bytes, input, args) {
                Ok((result, stdout)) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    stdout: Some(stdout),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    stdout: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    )],
                },
            };
        }
    }
    match birddisk_core::parse_and_typecheck_with_config(path, config) {
        Ok(program) => match engine {
            birddisk_core::Engine::Vm => match birddisk_vm::eval_with_io(&program, input, args) {
                Ok((result, stdout)) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    stdout: Some(stdout),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    stdout: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    )],
                },
            },
            birddisk_core::Engine::Wasm => match birddisk_wasm::run_with_io(&program, input, args) {
                Ok((result, stdout)) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    stdout: Some(stdout),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    stdout: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    )],
                },
            },
            birddisk_core::Engine::Native => match birddisk_native::run_with_io(&program, input, args) {
                Ok((result, stdout)) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    stdout: Some(stdout),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    stdout: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code.unwrap_or("E0400"),
                        runtime_spec_refs(err.code.unwrap_or("E0400")),
                        err.trace,
                    )],
                },
            },
        },
        Err(diagnostics) => birddisk_core::RunReport {
            tool: birddisk_core::TOOL_NAME,
            version: birddisk_core::VERSION,
            ok: false,
            result: None,
            stdout: None,
            diagnostics,
        },
    }
}

fn is_wasm_bytes(bytes: &[u8]) -> bool {
    bytes.starts_with(b"\0asm")
}
