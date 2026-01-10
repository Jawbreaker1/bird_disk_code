use serde::Serialize;
use std::collections::HashSet;
use std::env;
use std::path::{Path, PathBuf};
use std::process;

const HELP: &str = "\
BirdDisk compiler (POC)

Usage:
  birddisk <command> [options]

Commands:
  fmt <file|dir>
  check <file|dir> [--json]
  run <file> [--engine vm|wasm|native] [--json] [--emit wat|wasm|obj|exe] [--out <file>] [--stdin <file>] [--stdout <file>] [--report <file>]
  test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>]

Options:
  -h, --help     Show this help message
  --version      Show version information
";

const FMT_HELP: &str = "\
Usage:
  birddisk fmt <file|dir>

Options:
  -h, --help     Show this help message
";

const CHECK_HELP: &str = "\
Usage:
  birddisk check <file|dir> [--json]

Options:
  --json         Emit JSON diagnostics
  -h, --help     Show this help message
";

const RUN_HELP: &str = "\
Usage:
  birddisk run <file> [--engine vm|wasm|native] [--json] [--emit wat|wasm|obj|exe] [--out <file>] [--stdin <file>] [--stdout <file>] [--report <file>]

Options:
  --engine       Execution engine (vm, wasm, or native)
  --json         Emit JSON output
  --emit         Emit compiled output (wat, wasm, obj, or exe)
  --out          Output file for --emit
  --stdin        Read stdin from file
  --stdout       Write stdout to file (JSON still printed to stdout)
  --report       Write JSON report to file (stdout becomes program output unless --json is set)
  -h, --help     Show this help message
";

const TEST_HELP: &str = "\
Usage:
  birddisk test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>]

Options:
  --json         Emit JSON output
  --engine       Execution engine (vm, wasm, or native)
  --dir          Directory to scan for .bd files (repeatable)
  --tag          Filter tests by tag (repeatable)
  -h, --help     Show this help message
";

#[derive(Debug, PartialEq, Eq)]
enum Command {
    Fmt { path: String },
    Check { path: String, json: bool },
    Run {
        path: String,
        engine: birddisk_core::Engine,
        json: bool,
        emit: Option<EmitFormat>,
        out: Option<String>,
        stdin: Option<String>,
        stdout: Option<String>,
        report: Option<String>,
    },
    Test {
        json: bool,
        engine: Option<birddisk_core::Engine>,
        dirs: Vec<String>,
        tags: Vec<String>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum EmitFormat {
    Wat,
    Wasm,
    Obj,
    Exe,
}

#[derive(Serialize)]
struct TestCase {
    path: String,
    ok: bool,
    vm_result: Option<i64>,
    wasm_result: Option<i64>,
    native_result: Option<i64>,
    vm_stdout: Option<String>,
    wasm_stdout: Option<String>,
    native_stdout: Option<String>,
    diagnostics: Vec<birddisk_core::Diagnostic>,
}

#[derive(Serialize)]
struct TestReport {
    tool: &'static str,
    version: &'static str,
    ok: bool,
    cases: Vec<TestCase>,
    diagnostics: Vec<birddisk_core::Diagnostic>,
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() || matches!(args[0].as_str(), "-h" | "--help") {
        print!("{HELP}");
        return;
    }

    if args[0] == "--version" {
        println!("birddisk 0.1.0");
        return;
    }

    if args[0] == "fmt" && contains_help_flag(&args[1..]) {
        print!("{FMT_HELP}");
        return;
    }
    if args[0] == "check" && contains_help_flag(&args[1..]) {
        print!("{CHECK_HELP}");
        return;
    }
    if args[0] == "run" && contains_help_flag(&args[1..]) {
        print!("{RUN_HELP}");
        return;
    }
    if args[0] == "test" && contains_help_flag(&args[1..]) {
        print!("{TEST_HELP}");
        return;
    }

    let command = match parse_command(&args) {
        Ok(command) => command,
        Err(message) => {
            eprintln!("error: {message}");
            eprintln!();
            eprintln!("{HELP}");
            process::exit(2);
        }
    };

    if let Err(message) = execute(command) {
        eprintln!("error: {message}");
        process::exit(2);
    }
}

fn contains_help_flag(args: &[String]) -> bool {
    args.iter().any(|arg| matches!(arg.as_str(), "-h" | "--help"))
}

fn parse_command(args: &[String]) -> Result<Command, String> {
    match args[0].as_str() {
        "fmt" => parse_fmt(&args[1..]),
        "check" => parse_check(&args[1..]),
        "run" => parse_run(&args[1..]),
        "test" => parse_test(&args[1..]),
        other => Err(format!("unknown command '{other}'")),
    }
}

fn parse_fmt(args: &[String]) -> Result<Command, String> {
    let parsed =
        parse_path_and_flags(
            args,
            ParseConfig::new(
                true, false, false, false, false, false, false, false, false, false,
            ),
        )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for fmt".to_string())?;
    Ok(Command::Fmt { path })
}

fn parse_check(args: &[String]) -> Result<Command, String> {
    let parsed =
        parse_path_and_flags(
            args,
            ParseConfig::new(
                true, false, true, false, false, false, false, false, false, false,
            ),
        )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for check".to_string())?;
    Ok(Command::Check {
        path,
        json: parsed.json,
    })
}

fn parse_run(args: &[String]) -> Result<Command, String> {
    let parsed =
        parse_path_and_flags(
            args,
            ParseConfig::new(true, true, true, true, true, false, false, true, true, true),
        )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for run".to_string())?;
    if parsed.emit.is_some() && parsed.json {
        return Err("cannot combine --emit with --json".to_string());
    }
    if parsed.emit.is_none() && parsed.out.is_some() {
        return Err("--out requires --emit".to_string());
    }
    if parsed.emit.is_some()
        && (parsed.stdin.is_some() || parsed.stdout.is_some() || parsed.report.is_some())
    {
        return Err("--stdin/--stdout/--report are not supported with --emit".to_string());
    }
    if !parsed.json
        && parsed.report.is_none()
        && (parsed.stdin.is_some() || parsed.stdout.is_some())
    {
        return Err("--stdin/--stdout require --json or --report".to_string());
    }
    Ok(Command::Run {
        path,
        engine: parsed.engine.unwrap_or(birddisk_core::Engine::Vm),
        json: parsed.json,
        emit: parsed.emit,
        out: parsed.out,
        stdin: parsed.stdin,
        stdout: parsed.stdout,
        report: parsed.report,
    })
}

fn parse_test(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(false, true, true, false, false, true, true, false, false, false),
    )?;
    if parsed.path.is_some() {
        return Err("unexpected path for test".to_string());
    }
    Ok(Command::Test {
        json: parsed.json,
        engine: parsed.engine,
        dirs: parsed.dirs,
        tags: parsed.tags,
    })
}

#[derive(Clone, Copy)]
struct ParseConfig {
    allow_path: bool,
    allow_engine: bool,
    allow_json: bool,
    allow_emit: bool,
    allow_out: bool,
    allow_dir: bool,
    allow_tag: bool,
    allow_stdin: bool,
    allow_stdout: bool,
    allow_report: bool,
}

impl ParseConfig {
    fn new(
        allow_path: bool,
        allow_engine: bool,
        allow_json: bool,
        allow_emit: bool,
        allow_out: bool,
        allow_dir: bool,
        allow_tag: bool,
        allow_stdin: bool,
        allow_stdout: bool,
        allow_report: bool,
    ) -> Self {
        Self {
            allow_path,
            allow_engine,
            allow_json,
            allow_emit,
            allow_out,
            allow_dir,
            allow_tag,
            allow_stdin,
            allow_stdout,
            allow_report,
        }
    }
}

struct ParsedArgs {
    path: Option<String>,
    engine: Option<birddisk_core::Engine>,
    json: bool,
    emit: Option<EmitFormat>,
    out: Option<String>,
    dirs: Vec<String>,
    tags: Vec<String>,
    stdin: Option<String>,
    stdout: Option<String>,
    report: Option<String>,
}

fn parse_path_and_flags(args: &[String], config: ParseConfig) -> Result<ParsedArgs, String> {
    let mut path = None;
    let mut engine = None;
    let mut json = false;
    let mut emit = None;
    let mut out = None;
    let mut dirs = Vec::new();
    let mut tags = Vec::new();
    let mut stdin = None;
    let mut stdout = None;
    let mut report = None;
    let mut iter = args.iter();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--json" => {
                if !config.allow_json {
                    return Err("unexpected --json".to_string());
                }
                json = true;
            }
            "--engine" => {
                if !config.allow_engine {
                    return Err("unexpected --engine".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --engine".to_string())?;
                engine = Some(parse_engine(value)?);
            }
            "--emit" => {
                if !config.allow_emit {
                    return Err("unexpected --emit".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --emit".to_string())?;
                emit = Some(parse_emit(value)?);
            }
            "--out" => {
                if !config.allow_out {
                    return Err("unexpected --out".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --out".to_string())?;
                out = Some(value.to_string());
            }
            "--dir" => {
                if !config.allow_dir {
                    return Err("unexpected --dir".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --dir".to_string())?;
                dirs.push(value.to_string());
            }
            "--tag" => {
                if !config.allow_tag {
                    return Err("unexpected --tag".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --tag".to_string())?;
                tags.push(value.to_string());
            }
            "--stdin" => {
                if !config.allow_stdin {
                    return Err("unexpected --stdin".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --stdin".to_string())?;
                stdin = Some(value.to_string());
            }
            "--stdout" => {
                if !config.allow_stdout {
                    return Err("unexpected --stdout".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --stdout".to_string())?;
                stdout = Some(value.to_string());
            }
            "--report" => {
                if !config.allow_report {
                    return Err("unexpected --report".to_string());
                }
                let value = iter
                    .next()
                    .ok_or_else(|| "missing value for --report".to_string())?;
                report = Some(value.to_string());
            }
            flag if flag.starts_with('-') => {
                return Err(format!("unknown option '{flag}'"));
            }
            value => {
                if !config.allow_path {
                    return Err(format!("unexpected argument '{value}'"));
                }
                if path.is_some() {
                    return Err("multiple paths provided".to_string());
                }
                path = Some(value.to_string());
            }
        }
    }

    Ok(ParsedArgs {
        path,
        engine,
        json,
        emit,
        out,
        dirs,
        tags,
        stdin,
        stdout,
        report,
    })
}

fn parse_engine(value: &str) -> Result<birddisk_core::Engine, String> {
    match value {
        "vm" => Ok(birddisk_core::Engine::Vm),
        "wasm" => Ok(birddisk_core::Engine::Wasm),
        "native" => Ok(birddisk_core::Engine::Native),
        _ => Err(format!(
            "invalid engine '{value}' (expected 'vm', 'wasm', or 'native')"
        )),
    }
}

fn parse_emit(value: &str) -> Result<EmitFormat, String> {
    match value {
        "wat" => Ok(EmitFormat::Wat),
        "wasm" => Ok(EmitFormat::Wasm),
        "obj" => Ok(EmitFormat::Obj),
        "exe" => Ok(EmitFormat::Exe),
        _ => Err(format!(
            "invalid emit format '{value}' (expected 'wat', 'wasm', 'obj', or 'exe')"
        )),
    }
}

fn execute(command: Command) -> Result<(), String> {
    match command {
        Command::Fmt { path } => birddisk_core::fmt(&path),
        Command::Check { path, json } => {
            if json {
                println!("{}", birddisk_core::check_json(&path));
                Ok(())
            } else {
                Err("check not implemented (use --json for stub output)".to_string())
            }
        }
        Command::Run {
            path,
            engine,
            json,
            emit,
            out,
            stdin,
            stdout,
            report,
        } => {
            if let Some(format) = emit {
                return emit_compiled(&path, engine, format, out);
            }
            if json || report.is_some() {
                let input = match stdin {
                    Some(path) => std::fs::read_to_string(&path)
                        .map_err(|err| format!("unable to read --stdin file '{path}': {err}"))?,
                    None => String::new(),
                };
                let run_report = run_report(&path, engine, &input);
                let report_json =
                    serde_json::to_string_pretty(&run_report).unwrap_or_else(|_| "{}".to_string());
                if let Some(path) = report.as_deref() {
                    std::fs::write(path, &report_json)
                        .map_err(|err| format!("unable to write --report file '{path}': {err}"))?;
                }
                if json {
                    println!("{report_json}");
                }
                if let Some(output) = run_report.stdout.as_deref() {
                    if let Some(path) = stdout {
                        std::fs::write(&path, output).map_err(|err| {
                            format!("unable to write --stdout file '{path}': {err}")
                        })?;
                    } else if report.is_some() && !json {
                        print!("{output}");
                    }
                }
                Ok(())
            } else {
                Err("run not implemented (use --json or --report)".to_string())
            }
        }
        Command::Test {
            json,
            engine,
            dirs,
            tags,
        } => {
            if json {
                println!("{}", run_tests_json(engine, &dirs, &tags));
                Ok(())
            } else {
                Err("test is JSON-only for now".to_string())
            }
        }
    }
}

fn run_report(
    path: &str,
    engine: birddisk_core::Engine,
    input: &str,
) -> birddisk_core::RunReport {
    match birddisk_core::parse_and_typecheck(path) {
        Ok(program) => match engine {
            birddisk_core::Engine::Vm => match birddisk_vm::eval_with_io(&program, input) {
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
            birddisk_core::Engine::Wasm => match birddisk_wasm::run_with_io(&program, input) {
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
            birddisk_core::Engine::Native => match birddisk_native::run_with_io(&program, input) {
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
                        Vec::new(),
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

fn run_tests_json(
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

fn collect_bd_files(
    dir: &std::path::Path,
    paths: &mut Vec<String>,
) -> Result<(), String> {
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
                let vm = birddisk_vm::eval_with_io(&program, &input);
                if !check_expected_vm_error(vm, expected, &mut case) {
                    return case;
                }
            }
            Some(birddisk_core::Engine::Wasm) => {
                let wasm = birddisk_wasm::run_with_io(&program, &input);
                if !check_expected_wasm_error(wasm, expected, &mut case) {
                    return case;
                }
            }
            Some(birddisk_core::Engine::Native) => {
                let native = birddisk_native::run_with_io(&program, &input);
                if !check_expected_native_error(native, expected, &mut case) {
                    return case;
                }
            }
            None => {
                let vm = birddisk_vm::eval_with_io(&program, &input);
                let wasm = birddisk_wasm::run_with_io(&program, &input);
                let vm_ok = check_expected_vm_error(vm, expected, &mut case);
                let wasm_ok = check_expected_wasm_error(wasm, expected, &mut case);
                if !vm_ok || !wasm_ok {
                    return case;
                }
            }
        }
        return case;
    }

    match engine {
        Some(birddisk_core::Engine::Vm) => {
            let vm = birddisk_vm::eval_with_io(&program, &input);
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
            let wasm = birddisk_wasm::run_with_io(&program, &input);
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
            let native = birddisk_native::run_with_io(&program, &input);
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
                        Vec::new(),
                    ));
                }
            }
        }
        None => {
            let vm = birddisk_vm::eval_with_io(&program, &input);
            let wasm = birddisk_wasm::run_with_io(&program, &input);

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
                case.diagnostics
                    .push(output_mismatch_diagnostic(path, "vm", "wasm", vm_stdout, wasm_stdout));
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

fn diagnostics_match(
    expected: &[String],
    diagnostics: &[birddisk_core::Diagnostic],
) -> bool {
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

fn emit_compiled(
    path: &str,
    engine: birddisk_core::Engine,
    format: EmitFormat,
    out: Option<String>,
) -> Result<(), String> {
    let program = birddisk_core::parse_and_typecheck(path)
        .map_err(|_| "emit failed; run `birddisk check --json` for diagnostics".to_string())?;
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
                let bytes = birddisk_native::emit_object(&program)
                    .map_err(|err| err.message)?;
                std::fs::write(&out_path, bytes).map_err(|err| err.to_string())?;
                Ok(())
            }
            EmitFormat::Exe => {
                let out_path = out.unwrap_or_else(|| default_exe_path(path));
                let bytes = birddisk_native::emit_object(&program)
                    .map_err(|err| err.message)?;
                build_native_executable(&bytes, path, &out_path)
            }
            _ => Err("emit format not supported for --engine native".to_string()),
        },
        _ => Err("emit is only supported for --engine wasm or native".to_string()),
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

fn build_native_executable(obj_bytes: &[u8], source_path: &str, out_path: &str) -> Result<(), String> {
    let work_dir = native_work_dir()?;
    std::fs::create_dir_all(&work_dir).map_err(|err| err.to_string())?;
    let stem = Path::new(source_path)
        .file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or("birddisk");
    let obj_path = work_dir.join(format!("{stem}.o"));
    std::fs::write(&obj_path, obj_bytes).map_err(|err| err.to_string())?;

    let wrapper_path = work_dir.join(format!("{stem}_wrapper.rs"));
    std::fs::write(&wrapper_path, native_wrapper_source()).map_err(|err| err.to_string())?;

    let target_dir = target_profile_dir()?;
    let deps_dir = target_dir.join("deps");
    let runtime_rlib = find_runtime_rlib(&deps_dir)?;
    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());

    if let Some(parent) = Path::new(out_path).parent() {
        if !parent.as_os_str().is_empty() {
            std::fs::create_dir_all(parent).map_err(|err| err.to_string())?;
        }
    }

    let status = process::Command::new(rustc)
        .arg("--edition=2021")
        .arg(&wrapper_path)
        .arg("-o")
        .arg(out_path)
        .arg("--extern")
        .arg(format!("birddisk_native_runtime={}", runtime_rlib.display()))
        .arg("-L")
        .arg(&deps_dir)
        .arg("-C")
        .arg(format!("link-arg={}", obj_path.display()))
        .status()
        .map_err(|err| err.to_string())?;

    if status.success() {
        Ok(())
    } else {
        Err("native link failed".to_string())
    }
}

fn native_wrapper_source() -> String {
    let entry = birddisk_native::NATIVE_MAIN_SYMBOL;
    format!(
        "use std::io::Read;\n\nextern \"C\" {{\n    fn {entry}(rt: *mut birddisk_native_runtime::Runtime) -> i64;\n}}\n\nfn main() {{\n    let mut input = String::new();\n    let _ = std::io::stdin().read_to_string(&mut input);\n    let mut runtime = birddisk_native_runtime::Runtime::new();\n    runtime.set_input(&input);\n    let _result = unsafe {{ {entry}(&mut runtime) }};\n    if let Some(err) = runtime.take_error() {{\n        eprintln!(\"runtime error {{}}: {{}}\", err.code, err.message);\n        std::process::exit(1);\n    }}\n    let output = runtime.take_output();\n    print!(\"{{}}\", output);\n}}\n"
    )
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
    let entries = std::fs::read_dir(deps_dir).map_err(|err| err.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if let Some(name) = path.file_name().and_then(|name| name.to_str()) {
            if name.starts_with("libbirddisk_native_runtime-") && name.ends_with(".rlib") {
                return Ok(path);
            }
        }
    }
    Err("unable to locate birddisk_native_runtime rlib".to_string())
}

fn mismatch_diagnostic(path: &str, vm_result: i64, wasm_result: i64) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0500",
        severity: "error",
        message: format!(
            "VM/WASM mismatch: vm={vm_result}, wasm={wasm_result}."
        ),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Differential test failure.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

fn output_mismatch_diagnostic(
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
            "VM/WASM output mismatch: {left_engine}='{left}', {right_engine}='{right}'."
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

fn output_expected_diagnostic(
    path: &str,
    engine: &str,
    expected: &str,
    actual: &str,
) -> birddisk_core::Diagnostic {
    birddisk_core::Diagnostic {
        code: "E0502",
        severity: "error",
        message: format!(
            "Output mismatch ({engine}): expected='{expected}', got='{actual}'."
        ),
        file: path.to_string(),
        span: default_span(),
        trace: Vec::new(),
        notes: vec!["Output does not match .stdout fixture.".to_string()],
        spec_refs: Vec::new(),
        fixits: Vec::new(),
        help: None,
    }
}

fn expected_error_diagnostic(path: &str, message: impl Into<String>) -> birddisk_core::Diagnostic {
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

fn harness_diagnostic(path: &str, message: String, code: &'static str) -> birddisk_core::Diagnostic {
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

fn test_harness_diagnostic(message: impl Into<String>) -> birddisk_core::Diagnostic {
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

fn default_span() -> birddisk_core::Span {
    birddisk_core::Span::new(
        birddisk_core::Position::new(1, 1),
        birddisk_core::Position::new(1, 1),
    )
}

fn runtime_diagnostic(
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

fn runtime_spec_refs(code: &str) -> Vec<String> {
    match code {
        "E0402" => vec!["SPEC.md#6-4-binary-operators".to_string()],
        "E0403" => vec!["SPEC.md#8-4-indexing".to_string()],
        _ => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cmd(args: &[&str]) -> Result<Command, String> {
        let args = args.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        parse_command(&args)
    }

    #[test]
    fn parse_run_engine_json_any_order() {
        let command = cmd(&["run", "--engine", "wasm", "main.bd", "--json"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: "main.bd".to_string(),
                engine: birddisk_core::Engine::Wasm,
                json: true,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: None,
            }
        );
    }

    #[test]
    fn parse_run_with_emit_wat() {
        let command = cmd(&["run", "--engine", "wasm", "main.bd", "--emit", "wat"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: "main.bd".to_string(),
                engine: birddisk_core::Engine::Wasm,
                json: false,
                emit: Some(EmitFormat::Wat),
                out: None,
                stdin: None,
                stdout: None,
                report: None,
            }
        );
    }

    #[test]
    fn parse_run_with_emit_wasm_out() {
        let command = cmd(&[
            "run",
            "--engine",
            "wasm",
            "main.bd",
            "--emit",
            "wasm",
            "--out",
            "main.wasm",
        ])
        .unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: "main.bd".to_string(),
                engine: birddisk_core::Engine::Wasm,
                json: false,
                emit: Some(EmitFormat::Wasm),
                out: Some("main.wasm".to_string()),
                stdin: None,
                stdout: None,
                report: None,
            }
        );
    }

    #[test]
    fn parse_run_rejects_emit_with_json() {
        let err = cmd(&[
            "run",
            "--engine",
            "wasm",
            "main.bd",
            "--emit",
            "wat",
            "--json",
        ])
        .unwrap_err();
        assert!(err.contains("cannot combine"));
    }

    #[test]
    fn parse_run_rejects_out_without_emit() {
        let err = cmd(&["run", "main.bd", "--out", "main.wasm"]).unwrap_err();
        assert!(err.contains("--out requires --emit"));
    }

    #[test]
    fn parse_run_with_stdin_stdout() {
        let command = cmd(&[
            "run",
            "--json",
            "main.bd",
            "--stdin",
            "input.txt",
            "--stdout",
            "output.txt",
        ])
        .unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: "main.bd".to_string(),
                engine: birddisk_core::Engine::Vm,
                json: true,
                emit: None,
                out: None,
                stdin: Some("input.txt".to_string()),
                stdout: Some("output.txt".to_string()),
                report: None,
            }
        );
    }

    #[test]
    fn parse_run_rejects_stdin_without_json() {
        let err = cmd(&["run", "main.bd", "--stdin", "input.txt"]).unwrap_err();
        assert!(err.contains("--stdin/--stdout require --json or --report"));
    }

    #[test]
    fn parse_run_rejects_stdout_with_emit() {
        let err = cmd(&[
            "run",
            "main.bd",
            "--emit",
            "wat",
            "--stdout",
            "out.txt",
        ])
        .unwrap_err();
        assert!(err.contains("--stdin/--stdout/--report are not supported with --emit"));
    }

    #[test]
    fn parse_run_with_report() {
        let command = cmd(&["run", "main.bd", "--report", "report.json"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: "main.bd".to_string(),
                engine: birddisk_core::Engine::Vm,
                json: false,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: Some("report.json".to_string()),
            }
        );
    }

    #[test]
    fn parse_check_requires_path() {
        let err = cmd(&["check", "--json"]).unwrap_err();
        assert!(err.contains("missing path"));
    }

    #[test]
    fn parse_test_disallows_path() {
        let err = cmd(&["test", "extra.bd"]).unwrap_err();
        assert!(err.contains("unexpected argument"));
    }

    #[test]
    fn parse_test_with_dir_and_tag() {
        let command = cmd(&["test", "--json", "--dir", "examples", "--tag", "loop"]).unwrap();
        assert_eq!(
            command,
            Command::Test {
                json: true,
                engine: None,
                dirs: vec!["examples".to_string()],
                tags: vec!["loop".to_string()],
            }
        );
    }

    #[test]
    fn parse_test_with_engine() {
        let command = cmd(&["test", "--json", "--engine", "vm"]).unwrap();
        assert_eq!(
            command,
            Command::Test {
                json: true,
                engine: Some(birddisk_core::Engine::Vm),
                dirs: Vec::new(),
                tags: Vec::new(),
            }
        );
    }
}
