use super::diagnostics::{
    expected_error_diagnostic, harness_diagnostic, mismatch_diagnostic, output_expected_diagnostic,
    output_mismatch_diagnostic, runtime_diagnostic, runtime_spec_refs,
};
use super::harness::{
    collect_test_paths, companion_path, read_expected_error, read_expected_output, read_test_args,
    read_test_input,
};
use super::threading::wasm_threading_guard;
use serde::Serialize;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

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
    filters: &[String],
    jobs: Option<usize>,
    snapshot: bool,
    deterministic: bool,
) -> String {
    let mut report = TestReport {
        tool: birddisk_core::TOOL_NAME,
        version: birddisk_core::VERSION,
        ok: true,
        cases: Vec::new(),
        diagnostics: Vec::new(),
    };

    let paths = match collect_test_paths(dirs, tags, filters) {
        Ok(paths) => paths,
        Err(diag) => {
            report.ok = false;
            report.diagnostics.push(diag);
            return serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string());
        }
    };
    let job_count = jobs.unwrap_or(1).max(1);
    if job_count <= 1 || paths.len() <= 1 {
        for path in paths {
            let case = run_test_case(&path, engine, snapshot, deterministic);
            if !case.ok {
                report.ok = false;
            }
            report.cases.push(case);
        }
    } else {
        let queue: Arc<Mutex<VecDeque<(usize, String)>>> =
            Arc::new(Mutex::new(paths.into_iter().enumerate().collect()));
        let len = queue.lock().unwrap().len();
        let results: Arc<Mutex<Vec<Option<TestCase>>>> =
            Arc::new(Mutex::new((0..len).map(|_| None).collect()));
        let mut handles = Vec::new();
        let worker_count = job_count.min(results.lock().unwrap().len());
        for _ in 0..worker_count {
            let queue = Arc::clone(&queue);
            let results = Arc::clone(&results);
            let engine = engine;
            let deterministic = deterministic;
            let handle = std::thread::spawn(move || loop {
                let next = {
                    let mut guard = queue.lock().unwrap();
                    guard.pop_front()
                };
                let Some((idx, path)) = next else {
                    break;
                };
                let case = run_test_case(&path, engine, snapshot, deterministic);
                let mut out = results.lock().unwrap();
                if idx < out.len() {
                    out[idx] = Some(case);
                }
            });
            handles.push(handle);
        }
        for handle in handles {
            let _ = handle.join();
        }
        let mut out = results.lock().unwrap();
        for case in out.iter_mut() {
            if let Some(case) = case.take() {
                if !case.ok {
                    report.ok = false;
                }
                report.cases.push(case);
            }
        }
    }

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

fn run_test_case(
    path: &str,
    engine: Option<birddisk_core::Engine>,
    snapshot: bool,
    deterministic: bool,
) -> TestCase {
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

    if matches!(engine, Some(birddisk_core::Engine::Wasm)) {
        if let Some(diag) = wasm_threading_guard(path, &birddisk_core::ModuleConfig::default()) {
            case.ok = false;
            case.diagnostics.push(diag);
            if let Some(expected) = expected_error.as_ref() {
                if diagnostics_match(expected, &case.diagnostics) {
                    case.ok = true;
                } else {
                    case.diagnostics.push(expected_error_diagnostic(
                        path,
                        format!(
                            "Expected error code(s) {}, but wasm reported different codes.",
                            expected.join(", ")
                        ),
                    ));
                }
            }
            return case;
        }
    }
    let program = match birddisk_core::parse_and_typecheck(path) {
        Ok(mut program) => {
            birddisk_core::optimize_program(&mut program);
            program
        }
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
                let vm = birddisk_vm::eval_with_io_options(
                    &program,
                    &input,
                    &args,
                    birddisk_vm::VmOptions { deterministic },
                );
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
                let vm = birddisk_vm::eval_with_io_options(
                    &program,
                    &input,
                    &args,
                    birddisk_vm::VmOptions { deterministic },
                );
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
            let vm = birddisk_vm::eval_with_io_options(
                &program,
                &input,
                &args,
                birddisk_vm::VmOptions { deterministic },
            );
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
            let vm = birddisk_vm::eval_with_io_options(
                &program,
                &input,
                &args,
                birddisk_vm::VmOptions { deterministic },
            );
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
        if let (Some(wasm_result), Some(native_result)) = (case.wasm_result, case.native_result) {
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
        let snapshot_stdout = if snapshot {
            select_snapshot_stdout(&case, engine)
        } else {
            None
        };
        if let Some(expected) = expected_output.as_ref() {
            if let Some(vm_stdout) = case.vm_stdout.as_ref() {
                if vm_stdout != expected {
                    if snapshot {
                        if let Some(value) = snapshot_stdout.as_deref() {
                            if let Err(err) = write_snapshot_stdout(path, value) {
                                case.ok = false;
                                case.diagnostics
                                    .push(harness_diagnostic(path, err, "E0501"));
                            }
                        }
                    } else {
                        case.ok = false;
                        case.diagnostics
                            .push(output_expected_diagnostic(path, "vm", expected, vm_stdout));
                    }
                }
            }
            if let Some(wasm_stdout) = case.wasm_stdout.as_ref() {
                if wasm_stdout != expected {
                    if snapshot {
                        if let Some(value) = snapshot_stdout.as_deref() {
                            if let Err(err) = write_snapshot_stdout(path, value) {
                                case.ok = false;
                                case.diagnostics
                                    .push(harness_diagnostic(path, err, "E0501"));
                            }
                        }
                    } else {
                        case.ok = false;
                        case.diagnostics.push(output_expected_diagnostic(
                            path,
                            "wasm",
                            expected,
                            wasm_stdout,
                        ));
                    }
                }
            }
            if let Some(native_stdout) = case.native_stdout.as_ref() {
                if native_stdout != expected {
                    if snapshot {
                        if let Some(value) = snapshot_stdout.as_deref() {
                            if let Err(err) = write_snapshot_stdout(path, value) {
                                case.ok = false;
                                case.diagnostics
                                    .push(harness_diagnostic(path, err, "E0501"));
                            }
                        }
                    } else {
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
        } else if snapshot {
            if let Some(value) = snapshot_stdout.as_deref() {
                if let Err(err) = write_snapshot_stdout(path, value) {
                    case.ok = false;
                    case.diagnostics
                        .push(harness_diagnostic(path, err, "E0501"));
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

fn select_snapshot_stdout(
    case: &TestCase,
    engine: Option<birddisk_core::Engine>,
) -> Option<String> {
    match engine {
        Some(birddisk_core::Engine::Vm) => case.vm_stdout.clone(),
        Some(birddisk_core::Engine::Wasm) => case.wasm_stdout.clone(),
        Some(birddisk_core::Engine::Native) => case.native_stdout.clone(),
        None => case
            .vm_stdout
            .clone()
            .or_else(|| case.wasm_stdout.clone())
            .or_else(|| case.native_stdout.clone()),
    }
}

fn write_snapshot_stdout(path: &str, stdout: &str) -> Result<(), String> {
    let output_path = companion_path(path, "stdout");
    std::fs::write(&output_path, stdout).map_err(|err| err.to_string())
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
