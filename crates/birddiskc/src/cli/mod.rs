pub(crate) mod args;
pub(crate) mod build;
pub(crate) mod diagnostics;
pub(crate) mod manifest;
pub(crate) mod run;
pub(crate) mod test;

use args::Command;
use diagnostics::format_diagnostics_human;
use std::io::{IsTerminal, Read};

pub(crate) fn execute(command: Command) -> Result<(), String> {
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
        Command::Build {
            path,
            engine,
            emit,
            out,
        } => {
            let context = manifest::resolve_project_context(path.as_deref())?;
            let format = match emit {
                Some(format) => format,
                None => build::default_build_emit(engine)?,
            };
            build::emit_compiled(&context.entry, &context.config, engine, format, out)
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
            args,
        } => {
            let context = manifest::resolve_project_context(path.as_deref())?;
            let path = context.entry;
            let config = context.config;
            if let Some(format) = emit {
                return build::emit_compiled(&path, &config, engine, format, out);
            }
            if json || report.is_some() {
                let input = match stdin {
                    Some(path) => std::fs::read_to_string(&path)
                        .map_err(|err| format!("unable to read --stdin file '{path}': {err}"))?,
                    None => String::new(),
                };
                let run_report = run::run_report(&path, &config, engine, &input, &args);
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
            } else if engine == birddisk_core::Engine::Vm {
                let input = if std::io::stdin().is_terminal() {
                    String::new()
                } else {
                    let mut buf = String::new();
                    std::io::stdin()
                        .read_to_string(&mut buf)
                        .map_err(|err| format!("unable to read stdin: {err}"))?;
                    buf
                };
                let program = birddisk_core::parse_and_typecheck_with_config(&path, &config)
                    .map_err(|diags| format_diagnostics_human(&diags))?;
                match birddisk_vm::eval_with_io_streaming(
                    &program,
                    &input,
                    &args,
                    std::io::stdin().is_terminal(),
                ) {
                    Ok(_) => Ok(()),
                    Err(err) => {
                        let diag = diagnostics::runtime_diagnostic(
                            &path,
                            err.message,
                            err.code,
                            diagnostics::runtime_spec_refs(err.code),
                            err.trace,
                        );
                        Err(format_diagnostics_human(&[diag]))
                    }
                }
            } else {
                let input = if std::io::stdin().is_terminal() {
                    String::new()
                } else {
                    let mut buf = String::new();
                    std::io::stdin()
                        .read_to_string(&mut buf)
                        .map_err(|err| format!("unable to read stdin: {err}"))?;
                    buf
                };
                let run_report = run::run_report(&path, &config, engine, &input, &args);
                if let Some(output) = run_report.stdout.as_deref() {
                    print!("{output}");
                }
                if run_report.ok {
                    Ok(())
                } else if run_report.diagnostics.is_empty() {
                    Err("run failed (use --json for diagnostics)".to_string())
                } else {
                    Err(format_diagnostics_human(&run_report.diagnostics))
                }
            }
        }
        Command::Test {
            json,
            engine,
            dirs,
            tags,
        } => {
            if json {
                println!("{}", test::run_tests_json(engine, &dirs, &tags));
                Ok(())
            } else {
                Err("test is JSON-only for now".to_string())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::args::{parse_command, Command, EmitFormat};
    use super::{build, diagnostics, manifest, run};
    use std::{env, fs, process};

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
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Wasm,
                json: true,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: None,
                args: Vec::new(),
            }
        );
    }

    #[test]
    fn parse_run_with_emit_wat() {
        let command = cmd(&["run", "--engine", "wasm", "main.bd", "--emit", "wat"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Wasm,
                json: false,
                emit: Some(EmitFormat::Wat),
                out: None,
                stdin: None,
                stdout: None,
                report: None,
                args: Vec::new(),
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
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Wasm,
                json: false,
                emit: Some(EmitFormat::Wasm),
                out: Some("main.wasm".to_string()),
                stdin: None,
                stdout: None,
                report: None,
                args: Vec::new(),
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
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Vm,
                json: true,
                emit: None,
                out: None,
                stdin: Some("input.txt".to_string()),
                stdout: Some("output.txt".to_string()),
                report: None,
                args: Vec::new(),
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
        let err = cmd(&["run", "main.bd", "--emit", "wat", "--stdout", "out.txt"]).unwrap_err();
        assert!(err.contains("--stdin/--stdout/--report are not supported with --emit"));
    }

    #[test]
    fn parse_run_with_report() {
        let command = cmd(&["run", "main.bd", "--report", "report.json"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Vm,
                json: false,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: Some("report.json".to_string()),
                args: Vec::new(),
            }
        );
    }

    #[test]
    fn parse_run_collects_args_after_dash_dash() {
        let command = cmd(&["run", "main.bd", "--json", "--", "alpha", "beta"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Vm,
                json: true,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: None,
                args: vec!["alpha".to_string(), "beta".to_string()],
            }
        );
    }

    #[test]
    fn parse_run_allows_args_without_path() {
        let command = cmd(&["run", "--json", "--", "alpha"]).unwrap();
        assert_eq!(
            command,
            Command::Run {
                path: None,
                engine: birddisk_core::Engine::Vm,
                json: true,
                emit: None,
                out: None,
                stdin: None,
                stdout: None,
                report: None,
                args: vec!["alpha".to_string()],
            }
        );
    }

    #[test]
    fn parse_build_with_emit() {
        let command = cmd(&["build", "main.bd", "--emit", "obj"]).unwrap();
        assert_eq!(
            command,
            Command::Build {
                path: Some("main.bd".to_string()),
                engine: birddisk_core::Engine::Native,
                emit: Some(EmitFormat::Obj),
                out: None,
            }
        );
    }

    #[test]
    fn resolve_project_context_from_manifest_dir() {
        let mut root = env::temp_dir();
        root.push(format!("birddisk_manifest_{}", std::process::id()));
        let src_dir = root.join("src");
        let dep_dir = root.join("deps").join("util");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&dep_dir).unwrap();
        fs::write(src_dir.join("main.bd"), "rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        fs::write(
            dep_dir.join("math.bd"),
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n",
        )
        .unwrap();
        let manifest = "{\n  \"name\": \"demo\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"src/main.bd\",\n  \"deps\": { \"util\": \"deps/util\" }\n}\n";
        fs::write(root.join(manifest::manifest_filename()), manifest).unwrap();

        let context = manifest::resolve_project_context(Some(root.to_str().unwrap())).unwrap();
        assert!(context.entry.ends_with("src/main.bd"));
        assert_eq!(context.config.project_root.as_ref().unwrap(), &root);
        assert!(context.config.dep_roots.contains_key("util"));

        let _ = fs::remove_dir_all(&root);
    }

    #[test]
    fn resolve_project_context_from_manifest_with_versioned_dep() {
        let mut root = env::temp_dir();
        root.push(format!("birddisk_manifest_versioned_{}", std::process::id()));
        let src_dir = root.join("src");
        let dep_dir = root.join("deps").join("util");
        fs::create_dir_all(&src_dir).unwrap();
        fs::create_dir_all(&dep_dir).unwrap();
        fs::write(src_dir.join("main.bd"), "rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        fs::write(
            dep_dir.join("math.bd"),
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n",
        )
        .unwrap();
        let manifest = "{\n  \"name\": \"demo\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"src/main.bd\",\n  \"deps\": { \"util\": { \"path\": \"deps/util\", \"version\": \"0.1.0\" } }\n}\n";
        fs::write(root.join(manifest::manifest_filename()), manifest).unwrap();

        let context = manifest::resolve_project_context(Some(root.to_str().unwrap())).unwrap();
        assert!(context.entry.ends_with("src/main.bd"));
        assert_eq!(context.config.project_root.as_ref().unwrap(), &root);
        assert!(context.config.dep_roots.contains_key("util"));

        let _ = fs::remove_dir_all(&root);
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

    #[test]
    fn native_wrapper_exposes_json_and_result_toggles() {
        assert!(build::native_wrapper_contains_toggles());
    }

    #[test]
    fn native_aot_json_trace_smoke() {
        if env::var("BIRDDISK_RUN_NATIVE_AOT_TEST").is_err() {
            return;
        }
        let source = "rule boom() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n\nrule main() -> i64:\n  yield boom().\nend\n";
        let mut path = env::temp_dir();
        path.push(format!("birddisk_native_trace_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp source");
        let path_str = path.to_string_lossy().to_string();

        let program = birddisk_core::parse_and_typecheck(&path_str).expect("parse program");
        let obj = birddisk_native::emit_object(&program).expect("emit object");
        let layout = birddisk_native::layout_for_program(&program).expect("layout");
        let trace = birddisk_native::trace_for_program(&program).expect("trace");

        let mut exe_path = env::temp_dir();
        exe_path.push(format!("birddisk_native_trace_{}.exe", std::process::id()));
        let exe_str = exe_path.to_string_lossy().to_string();
        build::build_native_executable(&obj, &path_str, &exe_str, &layout, &trace)
            .expect("build native exe");

        let output = process::Command::new(&exe_path)
            .env("BIRDDISK_JSON", "1")
            .output()
            .expect("run native exe");
        let stdout = String::from_utf8_lossy(&output.stdout);
        let parsed: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse json");
        assert_eq!(parsed["ok"], false);
        assert_eq!(parsed["diagnostics"][0]["code"], "E0403");
        assert_eq!(parsed["diagnostics"][0]["trace"][0]["function"], "boom");
        assert_eq!(parsed["diagnostics"][0]["trace"][1]["function"], "main");
        assert_eq!(parsed["diagnostics"][0]["trace"][0]["file"], path_str);
        assert!(parsed["diagnostics"][0]["trace"][0]["source"]
            .as_str()
            .unwrap_or("")
            .contains("rule boom"));

        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&exe_path);
    }

    #[test]
    fn native_aot_try_catch_smoke() {
        if env::var("BIRDDISK_RUN_NATIVE_AOT_TEST").is_err() {
            return;
        }
        let source = "import std::io.\nimport std::string.\n\nrule safe_div(divisor: i64) -> i64:\n  when divisor == 0:\n    throw \"division by zero\".\n  otherwise:\n    yield 100 / divisor.\n  end\nend\n\nrule main() -> i64:\n  try:\n    set value: i64 = safe_div(0).\n    std::io::print(std::string::concat(\"value=\", std::string::from_i64(value))).\n    std::io::print(\"\\n\").\n    yield 0.\n  catch message:\n    std::io::print(std::string::concat(\"error: \", message)).\n    std::io::print(\"\\n\").\n    yield 1.\n  end\nend\n";
        let mut path = env::temp_dir();
        path.push(format!("birddisk_native_try_catch_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp source");
        let path_str = path.to_string_lossy().to_string();

        let program = birddisk_core::parse_and_typecheck(&path_str).expect("parse program");
        let obj = birddisk_native::emit_object(&program).expect("emit object");
        let layout = birddisk_native::layout_for_program(&program).expect("layout");
        let trace = birddisk_native::trace_for_program(&program).expect("trace");

        let mut exe_path = env::temp_dir();
        exe_path.push(format!("birddisk_native_try_catch_{}.exe", std::process::id()));
        let exe_str = exe_path.to_string_lossy().to_string();
        build::build_native_executable(&obj, &path_str, &exe_str, &layout, &trace)
            .expect("build native exe");

        let output = process::Command::new(&exe_path)
            .env("BIRDDISK_JSON", "1")
            .output()
            .expect("run native exe");
        let stdout = String::from_utf8_lossy(&output.stdout);
        let parsed: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse json");
        assert_eq!(parsed["ok"], true);
        assert_eq!(parsed["result"], 1);
        assert_eq!(parsed["stdout"], "error: division by zero\n");

        let _ = fs::remove_file(&path);
        let _ = fs::remove_file(&exe_path);
    }

    #[test]
    fn human_runtime_error_format_smoke() {
        let source = "rule boom() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n\nrule main() -> i64:\n  yield boom().\nend\n";
        let mut path = env::temp_dir();
        path.push(format!("birddisk_human_diag_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp source");
        let path_str = path.to_string_lossy().to_string();

        let report = run::run_report(
            &path_str,
            &birddisk_core::ModuleConfig::default(),
            birddisk_core::Engine::Vm,
            "",
            &[],
        );
        assert!(!report.ok);
        let output = diagnostics::format_diagnostics_human(&report.diagnostics);
        assert!(output.contains("error[E0403]"));
        assert!(output.contains("stack trace"));

        let _ = fs::remove_file(&path);
    }
}
