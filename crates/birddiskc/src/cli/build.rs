use super::args::EmitFormat;
use super::diagnostics::format_diagnostics_human;
use super::threading::wasm_threading_guard;
use std::env;
use std::path::{Path, PathBuf};
use std::process;

pub(crate) fn emit_compiled(
    path: &str,
    config: &birddisk_core::ModuleConfig,
    engine: birddisk_core::Engine,
    format: EmitFormat,
    out: Option<String>,
) -> Result<(), String> {
    if engine == birddisk_core::Engine::Wasm {
        if let Some(diag) = wasm_threading_guard(path, config) {
            return Err(format_diagnostics_human(&[diag]));
        }
    }
    let mut program = birddisk_core::parse_and_typecheck_with_config(path, config)
        .map_err(|_| "emit failed; run `birddiskc check --json` for diagnostics".to_string())?;
    birddisk_core::optimize_program(&mut program);
    match engine {
        birddisk_core::Engine::Wasm => {
            let out_path = out.or_else(|| match format {
                EmitFormat::Wat => None,
                EmitFormat::Wasm => Some(default_emit_path(path, "wasm")),
                _ => None,
            });
            match format {
                EmitFormat::Wat => {
                    let wat = birddisk_wasm::emit_wat(&program).map_err(|err| err.message)?;
                    match out_path {
                        Some(path) => {
                            std::fs::write(&path, wat).map_err(|err| err.to_string())?;
                            Ok(())
                        }
                        None => {
                            println!("{wat}");
                            Ok(())
                        }
                    }
                }
                EmitFormat::Wasm => {
                    let bytes = birddisk_wasm::emit_wasm(&program).map_err(|err| err.message)?;
                    let path = out_path.ok_or_else(|| "--out is required for wasm".to_string())?;
                    std::fs::write(&path, bytes).map_err(|err| err.to_string())?;
                    Ok(())
                }
                _ => Err("emit format not supported for --engine wasm".to_string()),
            }
        }
        birddisk_core::Engine::Native => match format {
            EmitFormat::Obj => {
                let out_path = out.unwrap_or_else(|| default_emit_path(path, "o"));
                let bytes = birddisk_native::emit_object(&program).map_err(|err| err.message)?;
                std::fs::write(&out_path, bytes).map_err(|err| err.to_string())?;
                Ok(())
            }
            EmitFormat::Exe => {
                let out_path = out.unwrap_or_else(|| default_exe_path(path));
                let bytes = birddisk_native::emit_object(&program).map_err(|err| err.message)?;
                let layout =
                    birddisk_native::layout_for_program(&program).map_err(|err| err.message)?;
                let trace =
                    birddisk_native::trace_for_program(&program).map_err(|err| err.message)?;
                build_native_executable(&bytes, path, &out_path, &layout, &trace)
            }
            _ => Err("emit format not supported for --engine native".to_string()),
        },
        _ => Err("emit is only supported for --engine wasm or native".to_string()),
    }
}

pub(crate) fn default_build_emit(engine: birddisk_core::Engine) -> Result<EmitFormat, String> {
    match engine {
        birddisk_core::Engine::Wasm => Ok(EmitFormat::Wasm),
        birddisk_core::Engine::Native => Ok(EmitFormat::Exe),
        _ => Err("build requires --engine wasm or native".to_string()),
    }
}

fn default_emit_path(path: &str, extension: &str) -> String {
    let mut output = std::path::PathBuf::from(path);
    output.set_extension(extension);
    output.to_string_lossy().to_string()
}

fn default_exe_path(path: &str) -> String {
    let path = Path::new(path);
    let stem = path
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("birddisk_out");
    let mut output = path.to_path_buf();
    output.set_file_name(stem);
    output.to_string_lossy().to_string()
}

pub(crate) fn build_native_executable(
    obj_bytes: &[u8],
    source_path: &str,
    out_path: &str,
    layout: &[Vec<usize>],
    trace: &[birddisk_core::TraceFrame],
) -> Result<(), String> {
    let work_dir = native_work_dir()?;
    std::fs::create_dir_all(&work_dir).map_err(|err| err.to_string())?;
    let stem = Path::new(source_path)
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("birddisk");
    let obj_path = work_dir.join(format!("{stem}.o"));
    std::fs::write(&obj_path, obj_bytes).map_err(|err| err.to_string())?;

    let wrapper_path = work_dir.join(format!("{stem}_wrapper.rs"));
    let wrapper = native_wrapper_source(layout, trace);
    std::fs::write(&wrapper_path, wrapper).map_err(|err| err.to_string())?;

    let target_dir = target_profile_dir()?;
    let deps_dir = target_dir.join("deps");
    let runtime_rlib = find_runtime_rlib(&deps_dir)?;
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());

    if let Some(parent) = Path::new(out_path).parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(|err| err.to_string())?;
        }
    }

    let output = process::Command::new(&rustc)
        .arg("--edition=2021")
        .arg(&wrapper_path)
        .arg("-o")
        .arg(out_path)
        .arg("--extern")
        .arg(format!(
            "birddisk_native_runtime={}",
            runtime_rlib.display()
        ))
        .arg("-L")
        .arg(&deps_dir)
        .arg("-C")
        .arg(format!("link-arg={}", obj_path.display()))
        .output()
        .map_err(|err| format!("failed to invoke rustc ({rustc}): {err}"))?;

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let hint = "run `cargo build -p birddisk_native_runtime` to build the runtime crate";
        Err(format!(
            "native link failed (rustc exit {}). {hint}\n{stderr}",
            output.status
        ))
    }
}

pub(crate) fn native_wrapper_source(
    layout: &[Vec<usize>],
    trace: &[birddisk_core::TraceFrame],
) -> String {
    let entry = birddisk_native::NATIVE_MAIN_SYMBOL;
    let layout_literal = format_layout_literal(layout);
    let trace_literal = format_trace_literal(trace);
    let tool = birddisk_core::TOOL_NAME;
    let version = birddisk_core::VERSION;
    let template = r#"use std::fmt::Write;
use std::io::{IsTerminal, Read};

const TOOL: &str = "__TOOL__";
const VERSION: &str = "__VERSION__";

extern "C" {
    fn __ENTRY__(rt: *mut birddisk_native_runtime::Runtime) -> i64;
}

fn json_enabled() -> bool {
    match std::env::var("BIRDDISK_JSON") {
        Ok(value) => value != "0",
        Err(_) => false,
    }
}

fn result_enabled() -> bool {
    match std::env::var("BIRDDISK_RESULT") {
        Ok(value) => value != "0",
        Err(_) => false,
    }
}

fn json_escape(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            ch if ch < '\u{20}' => {
                let _ = write!(out, "\\u{:04x}", ch as u32);
            }
            _ => out.push(ch),
        }
    }
    out
}

fn emit_json_success(result: i64, stdout: &str) {
    let mut out = String::new();
    out.push_str("{\"tool\":\"");
    out.push_str(TOOL);
    out.push_str("\",\"version\":\"");
    out.push_str(VERSION);
    out.push_str("\",\"ok\":true,\"result\":");
    out.push_str(&result.to_string());
    out.push_str(",\"stdout\":\"");
    out.push_str(&json_escape(stdout));
    out.push_str("\",\"diagnostics\":[]}");
    println!("{out}");
}

fn push_trace_json(out: &mut String, trace: &[birddisk_native_runtime::TraceFrame]) {
    out.push_str("\"trace\":[");
    for (idx, frame) in trace.iter().enumerate() {
        if idx > 0 {
            out.push_str(",");
        }
        out.push_str("{\"function\":\"");
        out.push_str(&json_escape(&frame.function));
        out.push_str("\",\"file\":\"");
        out.push_str(&json_escape(&frame.file));
        out.push_str("\",\"span\":{\"start\":{\"line\":");
        out.push_str(&frame.span.start.line.to_string());
        out.push_str(",\"col\":");
        out.push_str(&frame.span.start.col.to_string());
        out.push_str("},\"end\":{\"line\":");
        out.push_str(&frame.span.end.line.to_string());
        out.push_str(",\"col\":");
        out.push_str(&frame.span.end.col.to_string());
        out.push_str("}},\"source\":\"");
        out.push_str(&json_escape(&frame.source));
        out.push_str("\"}");
    }
    out.push_str("]");
}

fn emit_json_error(code: &str, message: &str, trace: &[birddisk_native_runtime::TraceFrame]) -> ! {
    let mut out = String::new();
    out.push_str("{\"tool\":\"");
    out.push_str(TOOL);
    out.push_str("\",\"version\":\"");
    out.push_str(VERSION);
    out.push_str("\",\"ok\":false,\"result\":null,\"stdout\":null,\"diagnostics\":[");
    out.push_str("{\"code\":\"");
    out.push_str(code);
    out.push_str("\",\"severity\":\"error\",\"message\":\"");
    out.push_str(&json_escape(message));
    out.push_str("\",\"file\":\"\",\"span\":{\"start\":{\"line\":1,\"col\":1},\"end\":{\"line\":1,\"col\":1}},");
    push_trace_json(&mut out, trace);
    out.push_str(",\"notes\":[],\"spec_refs\":[],\"fixits\":[],\"help\":null}");
    out.push_str("]}");
    println!("{out}");
    std::process::exit(1);
}

fn maybe_report_result(result: i64) {
    if result_enabled() {
        eprintln!("birddiskc result: {result}");
    }
}

fn main() {
    let mut input = String::new();
    let mut stdin = std::io::stdin();
    if !stdin.is_terminal() {
        let _ = stdin.read_to_string(&mut input);
    }
    let json = json_enabled();
    let mut runtime = birddisk_native_runtime::Runtime::new();
    let layout: Vec<Vec<usize>> = __LAYOUT__;
    runtime.set_layout(layout);
    let trace: Vec<birddisk_native_runtime::TraceFrame> = __TRACE__;
    runtime.set_trace(trace);
    runtime.set_input(&input);
    runtime.set_stdin_fallback(true);
    runtime.set_stdout_live(!json);
    let args: Vec<String> = std::env::args().skip(1).collect();
    runtime.set_args(&args);
    let result = unsafe { __ENTRY__(&mut runtime) };
    if let Some(err) = runtime.take_error() {
        if json {
            emit_json_error(err.code, &err.message, &err.trace);
        }
        eprintln!("runtime error {}: {}", err.code, err.message);
        std::process::exit(1);
    }
    if json {
        let output = runtime.take_output();
        emit_json_success(result, &output);
        return;
    }
    maybe_report_result(result);
}
"#;
    template
        .replace("__ENTRY__", entry)
        .replace("__LAYOUT__", &layout_literal)
        .replace("__TRACE__", &trace_literal)
        .replace("__TOOL__", tool)
        .replace("__VERSION__", version)
}

fn format_layout_literal(layout: &[Vec<usize>]) -> String {
    let mut output = String::from("vec![");
    for (outer_idx, fields) in layout.iter().enumerate() {
        if outer_idx > 0 {
            output.push_str(", ");
        }
        output.push_str("vec![");
        for (idx, field) in fields.iter().enumerate() {
            if idx > 0 {
                output.push_str(", ");
            }
            output.push_str(&field.to_string());
        }
        output.push(']');
    }
    output.push(']');
    output
}

fn format_trace_literal(frames: &[birddisk_core::TraceFrame]) -> String {
    let mut output = String::from("vec![");
    for (idx, frame) in frames.iter().enumerate() {
        if idx > 0 {
            output.push_str(", ");
        }
        let function = format!("{:?}", frame.function);
        let file = format!("{:?}", frame.file);
        let source = format!("{:?}", frame.source);
        output.push_str("birddisk_native_runtime::TraceFrame { function: ");
        output.push_str(&function);
        output.push_str(".to_string()");
        output.push_str(", file: ");
        output.push_str(&file);
        output.push_str(".to_string()");
        output.push_str(", span: birddisk_native_runtime::Span { start: birddisk_native_runtime::Position { line: ");
        output.push_str(&frame.span.start.line.to_string());
        output.push_str(", col: ");
        output.push_str(&frame.span.start.col.to_string());
        output.push_str(" }, end: birddisk_native_runtime::Position { line: ");
        output.push_str(&frame.span.end.line.to_string());
        output.push_str(", col: ");
        output.push_str(&frame.span.end.col.to_string());
        output.push_str(" } }");
        output.push_str(", source: ");
        output.push_str(&source);
        output.push_str(".to_string() }");
    }
    output.push(']');
    output
}

fn native_work_dir() -> Result<PathBuf, String> {
    let target_dir = target_profile_dir()?;
    Ok(target_dir.join("native"))
}

fn target_profile_dir() -> Result<PathBuf, String> {
    let root = workspace_root()?;
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    Ok(root.join("target").join(profile))
}

fn workspace_root() -> Result<PathBuf, String> {
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = manifest
        .parent()
        .and_then(|path| path.parent())
        .ok_or_else(|| "unable to resolve workspace root".to_string())?;
    Ok(root.to_path_buf())
}

fn find_runtime_rlib(deps_dir: &Path) -> Result<PathBuf, String> {
    let entries = std::fs::read_dir(deps_dir).map_err(|err| {
        format!(
            "unable to read runtime deps dir '{}': {err}. Run `cargo build -p birddisk_native_runtime` first.",
            deps_dir.display()
        )
    })?;
    let mut best: Option<(std::time::SystemTime, PathBuf)> = None;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if !name.starts_with("libbirddisk_native_runtime-") || !name.ends_with(".rlib") {
            continue;
        }
        let modified = path
            .metadata()
            .and_then(|meta| meta.modified())
            .unwrap_or(std::time::UNIX_EPOCH);
        match &best {
            Some((current, _)) if *current >= modified => {}
            _ => {
                best = Some((modified, path));
            }
        }
    }
    if let Some((_, path)) = best {
        return Ok(path);
    }
    Err(format!(
        "unable to locate birddisk_native_runtime rlib in '{}'. Run `cargo build -p birddisk_native_runtime` first.",
        deps_dir.display()
    ))
}

#[cfg(test)]
pub(crate) fn native_wrapper_contains_toggles() -> bool {
    let wrapper = native_wrapper_source(&[], &[]);
    wrapper.contains("BIRDDISK_JSON") && wrapper.contains("BIRDDISK_RESULT")
}
