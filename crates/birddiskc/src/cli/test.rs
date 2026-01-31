use super::diagnostics::{
    expected_error_diagnostic, harness_diagnostic, mismatch_diagnostic, output_expected_diagnostic,
    output_mismatch_diagnostic, runtime_diagnostic, runtime_spec_refs, test_harness_diagnostic,
};
use serde::Serialize;
use std::collections::HashSet;

#[derive(Serialize)]
pub(crate) struct TestCase {
    pub(crate) path: String,
    pub(crate) ok: bool,
    pub(crate) vm_result: Option<i64>,
    pub(crate) wasm_result: Option<i64>,
    pub(crate) native_result: Option<i64>,
    pub(crate) vm_stdout: Option<String>,
    pub(crate) wasm_stdout: Option<String>,
    pub(crate) native_stdout: Option<String>,
    pub(crate) diagnostics: Vec<birddisk_core::Diagnostic>,
}

#[derive(Serialize)]
pub(crate) struct TestReport {
    pub(crate) tool: &'static str,
    pub(crate) version: &'static str,
    pub(crate) ok: bool,
    pub(crate) cases: Vec<TestCase>,
    pub(crate) diagnostics: Vec<birddisk_core::Diagnostic>,
}

pub(crate) fn report_with_diagnostics(diagnostics: Vec<birddisk_core::Diagnostic>) -> String {
    let report = TestReport {
        tool: birddisk_core::TOOL_NAME,
        version: birddisk_core::VERSION,
        ok: false,
        cases: Vec::new(),
        diagnostics,
    };
    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

pub(crate) fn run_tests_json(
    engine: Option<birddisk_core::Engine>,
    dirs: &[String],
    tags: &[String],
) -> String {
    let mut report = TestReport {
        tool: birddisk_core::TOOL_NAME,
        version: birddisk_core::VERSION,
        ok: true,
        cases: Vec::new(),
        diagnostics: Vec::new(),
    };

    let paths = match collect_test_paths(dirs, tags) {
        Ok(paths) => paths,
        Err(diag) => {
            report.ok = false;
            report.diagnostics.push(diag);
            return serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string());
        }
    };

    for path in paths {
        let case = run_test_case(&path, engine);
        if !case.ok {
            report.ok = false;
        }
        report.cases.push(case);
    }

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

fn collect_test_paths(
    dirs: &[String],
    tags: &[String],
) -> Result<Vec<String>, birddisk_core::Diagnostic> {
    let roots = if dirs.is_empty() {
        default_test_dirs()
    } else {
        dirs.to_vec()
    };

    if roots.is_empty() {
        return Err(test_harness_diagnostic(
            "No default test directories found (expected examples/ or tests/)",
        ));
    }

    let mut paths = Vec::new();
    for dir in roots {
        let root = std::path::Path::new(&dir);
        if !root.exists() {
            return Err(test_harness_diagnostic(format!(
                "Test directory not found: {dir}"
            )));
        }
        collect_bd_files(root, &mut paths).map_err(test_harness_diagnostic)?;
    }

    let mut paths: Vec<String> = paths
        .into_iter()
        .filter(|path| matches_tags(path, tags))
        .collect();
    paths.sort();
    if paths.is_empty() {
        let message = if tags.is_empty() {
            "No .bd files found in test directories".to_string()
        } else {
            format!("No .bd files matched tags: {}", tags.join(", "))
        };
        return Err(test_harness_diagnostic(message));
    }
    Ok(paths)
}

fn default_test_dirs() -> Vec<String> {
    let mut roots = Vec::new();
    for candidate in ["examples", "tests"] {
        if std::path::Path::new(candidate).exists() {
            roots.push(candidate.to_string());
        }
    }
    roots
}

fn collect_bd_files(dir: &std::path::Path, paths: &mut Vec<String>) -> Result<(), String> {
    let entries = std::fs::read_dir(dir).map_err(|err| err.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            collect_bd_files(&path, paths)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("bd") {
            paths.push(path.to_string_lossy().to_string());
        }
    }
    Ok(())
}

fn companion_path(path: &str, extension: &str) -> String {
    let mut output = std::path::PathBuf::from(path);
    output.set_extension(extension);
    output.to_string_lossy().to_string()
}

fn read_optional_file(path: &str) -> Result<Option<String>, String> {
    if std::path::Path::new(path).exists() {
        std::fs::read_to_string(path)
            .map(Some)
            .map_err(|err| err.to_string())
    } else {
        Ok(None)
    }
}

fn read_test_input(path: &str) -> Result<String, String> {
    let stdin_path = companion_path(path, "stdin");
    Ok(read_optional_file(&stdin_path)?.unwrap_or_default())
}

fn read_test_args(path: &str) -> Result<Vec<String>, String> {
    let args_path = companion_path(path, "args");
    let Some(contents) = read_optional_file(&args_path)? else {
        return Ok(Vec::new());
    };
    Ok(contents
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .map(|line| line.to_string())
        .collect())
}

fn read_expected_output(path: &str) -> Result<Option<String>, String> {
    let stdout_path = companion_path(path, "stdout");
    read_optional_file(&stdout_path)
}

fn read_expected_error(path: &str) -> Result<Option<Vec<String>>, String> {
    let error_path = companion_path(path, "error");
    match read_optional_file(&error_path)? {
        Some(contents) => Ok(Some(parse_expected_error(&contents)?)),
        None => Ok(None),
    }
}

fn parse_expected_error(contents: &str) -> Result<Vec<String>, String> {
    let mut codes = Vec::new();
    for line in contents.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        for token in trimmed.split_whitespace() {
            if token.starts_with('#') {
                break;
            }
            codes.push(token.to_string());
        }
    }
    if codes.is_empty() {
        Err("expected error file did not contain any codes".to_string())
    } else {
        Ok(codes)
    }
}

fn matches_tags(path: &str, tags: &[String]) -> bool {
    if tags.is_empty() {
        return true;
    }
    let tokens = tag_tokens(std::path::Path::new(path));
    tags.iter().all(|tag| tokens.contains(&tag.to_lowercase()))
}

fn tag_tokens(path: &std::path::Path) -> HashSet<String> {
    let mut tokens = HashSet::new();
    for component in path.components() {
        if let std::path::Component::Normal(name) = component {
            if let Some(name) = name.to_str() {
                tokens.insert(name.to_lowercase());
            }
        }
    }
    if let Some(stem) = path.file_stem().and_then(|name| name.to_str()) {
        for token in stem.split(|ch: char| !ch.is_ascii_alphanumeric()) {
            if !token.is_empty() {
                tokens.insert(token.to_lowercase());
            }
        }
    }
    tokens
}

fn run_test_case(path: &str, engine: Option<birddisk_core::Engine>) -> TestCase {
    let mut case = TestCase {
        path: path.to_string(),
        ok: true,
        vm_result: None,
        wasm_result: None,
        native_result: None,
        vm_stdout: None,
        wasm_stdout: None,
        native_stdout: None,
        diagnostics: Vec::new(),
    };

    let input = match read_test_input(path) {
        Ok(input) => input,
        Err(err) => {
            case.ok = false;
            case.diagnostics
                .push(harness_diagnostic(path, err, "E0501"));
            return case;
        }
    };
    let expected_output = match read_expected_output(path) {
        Ok(output) => output,
        Err(err) => {
            case.ok = false;
            case.diagnostics
                .push(harness_diagnostic(path, err, "E0501"));
            return case;
        }
    };
    let args = match read_test_args(path) {
        Ok(args) => args,
        Err(err) => {
            case.ok = false;
            case.diagnostics
                .push(harness_diagnostic(path, err, "E0501"));
            return case;
        }
    };
    let expected_error = match read_expected_error(path) {
        Ok(error) => error,
        Err(err) => {
            case.ok = false;
            case.diagnostics
                .push(harness_diagnostic(path, err, "E0501"));
            return case;
        }
    };

    let program = match birddisk_core::parse_and_typecheck(path) {
        Ok(program) => program,
        Err(diagnostics) => {
            if let Some(expected) = expected_error.as_ref() {
                if diagnostics_match(expected, &diagnostics) {
                    return case;
                }
                case.ok = false;
                case.diagnostics = diagnostics;
                case.diagnostics.push(expected_error_diagnostic(
                    path,
                    format!(
                        "Expected error code(s) {}, but parser/typechecker reported different codes.",
                        expected.join(", ")
                    ),
                ));
                return case;
            }
            case.ok = false;
            case.diagnostics = diagnostics;
            return case;
        }
    };

    if let Some(expected) = expected_error.as_ref() {
        match engine {
            Some(birddisk_core::Engine::Vm) => {
                let vm = birddisk_vm::eval_with_io(&program, &input, &args);
                if !check_expected_vm_error(vm, expected, &mut case) {
                    return case;
                }
            }
            Some(birddisk_core::Engine::Wasm) => {
                let wasm = birddisk_wasm::run_with_io(&program, &input, &args);
                if !check_expected_wasm_error(wasm, expected, &mut case) {
                    return case;
                }
            }
            Some(birddisk_core::Engine::Native) => {
                let native = birddisk_native::run_with_io(&program, &input, &args);
                if !check_expected_native_error(native, expected, &mut case) {
                    return case;
                }
            }
            None => {
                let vm = birddisk_vm::eval_with_io(&program, &input, &args);
                let wasm = birddisk_wasm::run_with_io(&program, &input, &args);
                let native = birddisk_native::run_with_io(&program, &input, &args);
                let vm_ok = check_expected_vm_error(vm, expected, &mut case);
                let wasm_ok = check_expected_wasm_error(wasm, expected, &mut case);
                let native_ok = check_expected_native_error(native, expected, &mut case);
                if !vm_ok || !wasm_ok || !native_ok {
                    return case;
                }
            }
        }
        return case;
    }

    match engine {
        Some(birddisk_core::Engine::Vm) => {
            let vm = birddisk_vm::eval_with_io(&program, &input, &args);
            match vm {
                Ok((result, stdout)) => {
                    case.vm_result = Some(result);
                    case.vm_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    ));
                }
            }
        }
        Some(birddisk_core::Engine::Wasm) => {
            let wasm = birddisk_wasm::run_with_io(&program, &input, &args);
            match wasm {
                Ok((result, stdout)) => {
                    case.wasm_result = Some(result);
                    case.wasm_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    ));
                }
            }
        }
        Some(birddisk_core::Engine::Native) => {
            let native = birddisk_native::run_with_io(&program, &input, &args);
            match native {
                Ok((result, stdout)) => {
                    case.native_result = Some(result);
                    case.native_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code.unwrap_or("E0400"),
                        runtime_spec_refs(err.code.unwrap_or("E0400")),
                        err.trace,
                    ));
                }
            }
        }
        None => {
            let vm = birddisk_vm::eval_with_io(&program, &input, &args);
            let wasm = birddisk_wasm::run_with_io(&program, &input, &args);
            let native = birddisk_native::run_with_io(&program, &input, &args);

            match vm {
                Ok((result, stdout)) => {
                    case.vm_result = Some(result);
                    case.vm_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    ));
                }
            }

            match wasm {
                Ok((result, stdout)) => {
                    case.wasm_result = Some(result);
                    case.wasm_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                        err.trace,
                    ));
                }
            }

            match native {
                Ok((result, stdout)) => {
                    case.native_result = Some(result);
                    case.native_stdout = Some(stdout);
                }
                Err(err) => {
                    case.ok = false;
                    case.diagnostics.push(runtime_diagnostic(
                        path,
                        err.message,
                        err.code.unwrap_or("E0400"),
                        runtime_spec_refs(err.code.unwrap_or("E0400")),
                        err.trace,
                    ));
                }
            }
        }
    }

    if case.ok {
        if let (Some(vm_result), Some(wasm_result)) = (case.vm_result, case.wasm_result) {
            if vm_result != wasm_result {
                case.ok = false;
                case.diagnostics
                    .push(mismatch_diagnostic(path, vm_result, wasm_result));
            }
        }
        if let (Some(vm_stdout), Some(wasm_stdout)) =
            (case.vm_stdout.as_ref(), case.wasm_stdout.as_ref())
        {
            if vm_stdout != wasm_stdout {
                case.ok = false;
                case.diagnostics.push(output_mismatch_diagnostic(
                    path,
                    "vm",
                    "wasm",
                    vm_stdout,
                    wasm_stdout,
                ));
            }
        }
        if let (Some(vm_result), Some(native_result)) = (case.vm_result, case.native_result) {
            if vm_result != native_result {
                case.ok = false;
                case.diagnostics
                    .push(mismatch_diagnostic(path, vm_result, native_result));
            }
        }
        if let (Some(vm_stdout), Some(native_stdout)) =
            (case.vm_stdout.as_ref(), case.native_stdout.as_ref())
        {
            if vm_stdout != native_stdout {
                case.ok = false;
                case.diagnostics.push(output_mismatch_diagnostic(
                    path,
                    "vm",
                    "native",
                    vm_stdout,
                    native_stdout,
                ));
            }
        }
        if let (Some(wasm_result), Some(native_result)) =
            (case.wasm_result, case.native_result)
        {
            if wasm_result != native_result {
                case.ok = false;
                case.diagnostics
                    .push(mismatch_diagnostic(path, wasm_result, native_result));
            }
        }
        if let (Some(wasm_stdout), Some(native_stdout)) =
            (case.wasm_stdout.as_ref(), case.native_stdout.as_ref())
        {
            if wasm_stdout != native_stdout {
                case.ok = false;
                case.diagnostics.push(output_mismatch_diagnostic(
                    path,
                    "wasm",
                    "native",
                    wasm_stdout,
                    native_stdout,
                ));
            }
        }
        if let Some(expected) = expected_output.as_ref() {
            if let Some(vm_stdout) = case.vm_stdout.as_ref() {
                if vm_stdout != expected {
                    case.ok = false;
                    case.diagnostics.push(output_expected_diagnostic(
                        path,
                        "vm",
                        expected,
                        vm_stdout,
                    ));
                }
            }
            if let Some(wasm_stdout) = case.wasm_stdout.as_ref() {
                if wasm_stdout != expected {
                    case.ok = false;
                    case.diagnostics.push(output_expected_diagnostic(
                        path,
                        "wasm",
                        expected,
                        wasm_stdout,
                    ));
                }
            }
            if let Some(native_stdout) = case.native_stdout.as_ref() {
                if native_stdout != expected {
                    case.ok = false;
                    case.diagnostics.push(output_expected_diagnostic(
                        path,
                        "native",
                        expected,
                        native_stdout,
                    ));
                }
            }
        }
    }

    case
}

fn diagnostics_match(expected: &[String], diagnostics: &[birddisk_core::Diagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|diag| expected.iter().any(|code| code == diag.code))
}

fn check_expected_vm_error(
    result: Result<(i64, String), birddisk_vm::RuntimeError>,
    expected: &[String],
    case: &mut TestCase,
) -> bool {
    match result {
        Ok((result, stdout)) => {
            case.ok = false;
            case.vm_result = Some(result);
            case.vm_stdout = Some(stdout);
            case.diagnostics.push(expected_error_diagnostic(
                &case.path,
                format!(
                    "Expected error code(s) {}, but vm succeeded.",
                    expected.join(", ")
                ),
            ));
            false
        }
        Err(err) => {
            if expected.iter().any(|code| code == err.code) {
                true
            } else {
                case.ok = false;
                case.diagnostics.push(expected_error_diagnostic(
                    &case.path,
                    format!(
                        "Expected error code(s) {}, got {} from vm.",
                        expected.join(", "),
                        err.code
                    ),
                ));
                false
            }
        }
    }
}

fn check_expected_wasm_error(
    result: Result<(i64, String), birddisk_wasm::WasmError>,
    expected: &[String],
    case: &mut TestCase,
) -> bool {
    match result {
        Ok((result, stdout)) => {
            case.ok = false;
            case.wasm_result = Some(result);
            case.wasm_stdout = Some(stdout);
            case.diagnostics.push(expected_error_diagnostic(
                &case.path,
                format!(
                    "Expected error code(s) {}, but wasm succeeded.",
                    expected.join(", ")
                ),
            ));
            false
        }
        Err(err) => {
            if expected.iter().any(|code| code == err.code) {
                true
            } else {
                case.ok = false;
                case.diagnostics.push(expected_error_diagnostic(
                    &case.path,
                    format!(
                        "Expected error code(s) {}, got {} from wasm.",
                        expected.join(", "),
                        err.code
                    ),
                ));
                false
            }
        }
    }
}

fn check_expected_native_error(
    result: Result<(i64, String), birddisk_native::NativeError>,
    expected: &[String],
    case: &mut TestCase,
) -> bool {
    match result {
        Ok((result, stdout)) => {
            case.ok = false;
            case.native_result = Some(result);
            case.native_stdout = Some(stdout);
            case.diagnostics.push(expected_error_diagnostic(
                &case.path,
                format!(
                    "Expected error code(s) {}, but native succeeded.",
                    expected.join(", ")
                ),
            ));
            false
        }
        Err(err) => {
            let code = err.code.unwrap_or("E0400");
            if expected.iter().any(|expected_code| expected_code == code) {
                true
            } else {
                case.ok = false;
                case.diagnostics.push(expected_error_diagnostic(
                    &case.path,
                    format!(
                        "Expected error code(s) {}, got {} from native.",
                        expected.join(", "),
                        code
                    ),
                ));
                false
            }
        }
    }
}
