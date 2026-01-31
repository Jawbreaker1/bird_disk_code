pub(crate) const HELP: &str = "\
BirdDisk compiler (POC)

Usage:
  birddiskc <command> [options]

Commands:
  fmt <file|dir>
  check <file|dir> [--json]
  lint <file|dir> [--json]
  doc [<file|dir>] [--out <file>]
  build [<file|dir>] [--engine vm|wasm|native] [--emit wat|wasm|obj|exe] [--out <file>] [--require-tests]
  run [<file|dir>] [--engine vm|wasm|native] [--json] [--emit wat|wasm|obj|exe] [--out <file>] [--stdin <file>] [--stdout <file>] [--report <file>]
  test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>] [--require-tests]

Options:
  -h, --help     Show this help message
  --version      Show version information
";

pub(crate) const FMT_HELP: &str = "\
Usage:
  birddiskc fmt <file|dir>

Options:
  -h, --help     Show this help message
";

pub(crate) const CHECK_HELP: &str = "\
Usage:
  birddiskc check <file|dir> [--json]

Options:
  --json         Emit JSON diagnostics
  -h, --help     Show this help message
";

pub(crate) const LINT_HELP: &str = "\
Usage:
  birddiskc lint <file|dir> [--json]

Options:
  --json         Emit JSON diagnostics
  --require-tests Enforce test coverage (opt-in)
  -h, --help     Show this help message
";

pub(crate) const DOC_HELP: &str = "\
Usage:
  birddiskc doc [<file|dir>] [--out <file>]

Options:
  --out          Write docs to file instead of stdout
  -h, --help     Show this help message
";

pub(crate) const RUN_HELP: &str = "\
Usage:
  birddiskc run [<file|dir>] [--engine vm|wasm|native] [--json] [--emit wat|wasm|obj|exe] [--out <file>] [--stdin <file>] [--stdout <file>] [--report <file>] [-- <args>...]

Options:
  --engine       Execution engine (vm, wasm, or native)
  --json         Emit JSON output
  --emit         Emit compiled output (wat, wasm, obj, or exe)
  --out          Output file for --emit
  --stdin        Read stdin from file
  --             Pass remaining arguments to std::env::args()
  --stdout       Write stdout to file (JSON still printed to stdout)
  --report       Write JSON report to file (stdout becomes program output unless --json is set)
  -h, --help     Show this help message
Notes:
  If <file|dir> is omitted, `birddiskc run` uses `birddisk.json` (manifest entry).
";

pub(crate) const BUILD_HELP: &str = "\
Usage:
  birddiskc build [<file|dir>] [--engine vm|wasm|native] [--emit wat|wasm|obj|exe] [--out <file>] [--require-tests]

Options:
  --engine       Compilation engine (wasm or native)
  --emit         Emit compiled output (wat, wasm, obj, or exe)
  --out          Output file for --emit
  --require-tests Enforce test coverage (opt-in)
  -h, --help     Show this help message
Notes:
  If <file|dir> is omitted, `birddiskc build` uses `birddisk.json` (manifest entry).
";

pub(crate) const TEST_HELP: &str = "\
Usage:
  birddiskc test [--json] [--engine vm|wasm|native] [--dir <path>] [--tag <tag>] [--require-tests]

Options:
  --json         Emit JSON output
  --engine       Execution engine (vm, wasm, or native)
  --dir          Directory to scan for .bd files (repeatable)
  --tag          Filter tests by tag (repeatable)
  --require-tests Enforce test coverage (opt-in)
  -h, --help     Show this help message
";

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Command {
    Fmt { path: String },
    Check { path: String, json: bool },
    Lint { path: String, json: bool, require_tests: bool },
    Doc { path: Option<String>, out: Option<String> },
    Build {
        path: Option<String>,
        engine: birddisk_core::Engine,
        emit: Option<EmitFormat>,
        out: Option<String>,
        require_tests: bool,
    },
    Run {
        path: Option<String>,
        engine: birddisk_core::Engine,
        json: bool,
        emit: Option<EmitFormat>,
        out: Option<String>,
        stdin: Option<String>,
        stdout: Option<String>,
        report: Option<String>,
        args: Vec<String>,
    },
    Test {
        json: bool,
        engine: Option<birddisk_core::Engine>,
        dirs: Vec<String>,
        tags: Vec<String>,
        require_tests: bool,
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum EmitFormat {
    Wat,
    Wasm,
    Obj,
    Exe,
}

pub(crate) fn contains_help_flag(args: &[String]) -> bool {
    args.iter().any(|arg| matches!(arg.as_str(), "-h" | "--help"))
}

pub(crate) fn parse_command(args: &[String]) -> Result<Command, String> {
    match args[0].as_str() {
        "fmt" => parse_fmt(&args[1..]),
        "check" => parse_check(&args[1..]),
        "lint" => parse_lint(&args[1..]),
        "doc" => parse_doc(&args[1..]),
        "build" => parse_build(&args[1..]),
        "run" => parse_run(&args[1..]),
        "test" => parse_test(&args[1..]),
        other => Err(format!("unknown command '{other}'")),
    }
}

fn parse_fmt(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, false, false, false, false, false, false, false, false, false, false, false),
    )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for fmt".to_string())?;
    Ok(Command::Fmt { path })
}

fn parse_check(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, false, true, false, false, false, false, false, false, false, false, false),
    )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for check".to_string())?;
    Ok(Command::Check {
        path,
        json: parsed.json,
    })
}

fn parse_lint(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, false, true, false, false, false, false, false, false, false, false, true),
    )?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for lint".to_string())?;
    Ok(Command::Lint {
        path,
        json: parsed.json,
        require_tests: parsed.require_tests,
    })
}

fn parse_doc(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, false, false, false, true, false, false, false, false, false, false, false),
    )?;
    Ok(Command::Doc {
        path: parsed.path,
        out: parsed.out,
    })
}

fn parse_build(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, true, false, true, true, false, false, false, false, false, false, true),
    )?;
    if parsed.emit.is_none() && parsed.out.is_some() {
        return Err("--out requires --emit".to_string());
    }
    Ok(Command::Build {
        path: parsed.path,
        engine: parsed.engine.unwrap_or(birddisk_core::Engine::Native),
        emit: parsed.emit,
        out: parsed.out,
        require_tests: parsed.require_tests,
    })
}

fn parse_run(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(true, true, true, true, true, false, false, true, true, true, true, false),
    )?;
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
    if !parsed.json && parsed.report.is_none() && (parsed.stdin.is_some() || parsed.stdout.is_some())
    {
        return Err("--stdin/--stdout require --json or --report".to_string());
    }
    Ok(Command::Run {
        path: parsed.path,
        engine: parsed.engine.unwrap_or(birddisk_core::Engine::Vm),
        json: parsed.json,
        emit: parsed.emit,
        out: parsed.out,
        stdin: parsed.stdin,
        stdout: parsed.stdout,
        report: parsed.report,
        args: parsed.args,
    })
}

fn parse_test(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(
        args,
        ParseConfig::new(false, true, true, false, false, true, true, false, false, false, false, true),
    )?;
    if parsed.path.is_some() {
        return Err("unexpected path for test".to_string());
    }
    Ok(Command::Test {
        json: parsed.json,
        engine: parsed.engine,
        dirs: parsed.dirs,
        tags: parsed.tags,
        require_tests: parsed.require_tests,
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
    allow_args: bool,
    allow_require_tests: bool,
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
        allow_args: bool,
        allow_require_tests: bool,
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
            allow_args,
            allow_require_tests,
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
    args: Vec<String>,
    require_tests: bool,
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
    let mut arg_values = Vec::new();
    let mut require_tests = false;
    let mut iter = args.iter();

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--" => {
                if !config.allow_args {
                    return Err("unexpected --".to_string());
                }
                arg_values.extend(iter.cloned());
                break;
            }
            "--json" => {
                if !config.allow_json {
                    return Err("unexpected --json".to_string());
                }
                json = true;
            }
            "--require-tests" => {
                if !config.allow_require_tests {
                    return Err("unexpected --require-tests".to_string());
                }
                require_tests = true;
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
        args: arg_values,
        require_tests,
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
