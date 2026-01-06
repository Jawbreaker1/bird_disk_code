use std::env;
use std::process;

const HELP: &str = "\
BirdDisk compiler (POC)

Usage:
  birddisk <command> [options]

Commands:
  fmt <file|dir>
  check <file|dir> [--json]
  run <file> [--engine vm|wasm] [--json] [--emit wat]
  test [--json]

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
  birddisk run <file> [--engine vm|wasm] [--json] [--emit wat]

Options:
  --engine       Execution engine (vm or wasm)
  --json         Emit JSON output
  --emit         Emit compiled output (wat)
  -h, --help     Show this help message
";

const TEST_HELP: &str = "\
Usage:
  birddisk test [--json]

Options:
  --json         Emit JSON output
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
    },
    Test { json: bool },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum EmitFormat {
    Wat,
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
    let parsed = parse_path_and_flags(args, ParseConfig::new(true, false, false, false))?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for fmt".to_string())?;
    Ok(Command::Fmt { path })
}

fn parse_check(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(args, ParseConfig::new(true, false, true, false))?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for check".to_string())?;
    Ok(Command::Check {
        path,
        json: parsed.json,
    })
}

fn parse_run(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(args, ParseConfig::new(true, true, true, true))?;
    let path = parsed
        .path
        .ok_or_else(|| "missing path for run".to_string())?;
    if parsed.emit.is_some() && parsed.json {
        return Err("cannot combine --emit with --json".to_string());
    }
    Ok(Command::Run {
        path,
        engine: parsed.engine.unwrap_or(birddisk_core::Engine::Vm),
        json: parsed.json,
        emit: parsed.emit,
    })
}

fn parse_test(args: &[String]) -> Result<Command, String> {
    let parsed = parse_path_and_flags(args, ParseConfig::new(true, false, true, false))?;
    if parsed.path.is_some() {
        return Err("unexpected path for test".to_string());
    }
    Ok(Command::Test { json: parsed.json })
}

#[derive(Clone, Copy)]
struct ParseConfig {
    allow_path: bool,
    allow_engine: bool,
    allow_json: bool,
    allow_emit: bool,
}

impl ParseConfig {
    fn new(allow_path: bool, allow_engine: bool, allow_json: bool, allow_emit: bool) -> Self {
        Self {
            allow_path,
            allow_engine,
            allow_json,
            allow_emit,
        }
    }
}

struct ParsedArgs {
    path: Option<String>,
    engine: Option<birddisk_core::Engine>,
    json: bool,
    emit: Option<EmitFormat>,
}

fn parse_path_and_flags(args: &[String], config: ParseConfig) -> Result<ParsedArgs, String> {
    let mut path = None;
    let mut engine = None;
    let mut json = false;
    let mut emit = None;
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
    })
}

fn parse_engine(value: &str) -> Result<birddisk_core::Engine, String> {
    match value {
        "vm" => Ok(birddisk_core::Engine::Vm),
        "wasm" => Ok(birddisk_core::Engine::Wasm),
        _ => Err(format!(
            "invalid engine '{value}' (expected 'vm' or 'wasm')"
        )),
    }
}

fn parse_emit(value: &str) -> Result<EmitFormat, String> {
    match value {
        "wat" => Ok(EmitFormat::Wat),
        _ => Err(format!("invalid emit format '{value}' (expected 'wat')")),
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
        } => {
            if let Some(format) = emit {
                return emit_compiled(&path, engine, format);
            }
            if json {
                println!("{}", run_json(&path, engine));
                Ok(())
            } else {
                Err("run not implemented (use --json for stub output)".to_string())
            }
        }
        Command::Test { json } => {
            if json {
                println!("{}", birddisk_core::test_json());
                Ok(())
            } else {
                Err("test not implemented (use --json for stub output)".to_string())
            }
        }
    }
}

fn run_json(path: &str, engine: birddisk_core::Engine) -> String {
    let report = match birddisk_core::parse_and_typecheck(path) {
        Ok(program) => match engine {
            birddisk_core::Engine::Vm => match birddisk_vm::eval(&program) {
                Ok(result) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                    )],
                },
            },
            birddisk_core::Engine::Wasm => match birddisk_wasm::run(&program) {
                Ok(result) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: true,
                    result: Some(result),
                    diagnostics: Vec::new(),
                },
                Err(err) => birddisk_core::RunReport {
                    tool: birddisk_core::TOOL_NAME,
                    version: birddisk_core::VERSION,
                    ok: false,
                    result: None,
                    diagnostics: vec![runtime_diagnostic(
                        path,
                        err.message,
                        err.code,
                        runtime_spec_refs(err.code),
                    )],
                },
            },
        },
        Err(diagnostics) => birddisk_core::RunReport {
            tool: birddisk_core::TOOL_NAME,
            version: birddisk_core::VERSION,
            ok: false,
            result: None,
            diagnostics,
        },
    };

    serde_json::to_string_pretty(&report).unwrap_or_else(|_| "{}".to_string())
}

fn emit_compiled(
    path: &str,
    engine: birddisk_core::Engine,
    format: EmitFormat,
) -> Result<(), String> {
    if engine != birddisk_core::Engine::Wasm {
        return Err("emit is only supported for --engine wasm".to_string());
    }
    let program = birddisk_core::parse_and_typecheck(path)
        .map_err(|_| "emit failed; run `birddisk check --json` for diagnostics".to_string())?;
    match format {
        EmitFormat::Wat => {
            let wat = birddisk_wasm::emit_wat(&program).map_err(|err| err.message)?;
            println!("{wat}");
            Ok(())
        }
    }
}

fn runtime_diagnostic(
    path: &str,
    message: String,
    code: &'static str,
    spec_refs: Vec<String>,
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
        notes: vec!["Runtime error".to_string()],
        spec_refs,
        fixits: Vec::new(),
        help: None,
    }
}

fn runtime_spec_refs(code: &str) -> Vec<String> {
    match code {
        "E0402" => vec!["SPEC.md#6-4-binary-operators".to_string()],
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
    fn parse_check_requires_path() {
        let err = cmd(&["check", "--json"]).unwrap_err();
        assert!(err.contains("missing path"));
    }

    #[test]
    fn parse_test_disallows_path() {
        let err = cmd(&["test", "extra.bd"]).unwrap_err();
        assert!(err.contains("unexpected path"));
    }
}
