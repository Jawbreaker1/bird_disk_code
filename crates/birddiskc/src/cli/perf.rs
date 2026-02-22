use super::diagnostics::{perf_harness_diagnostic, runtime_diagnostic, runtime_spec_refs};
use super::harness::{collect_test_paths, read_test_args, read_test_input};
use birddisk_core::ast::Program;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, Instant};

pub(crate) const DEFAULT_BASELINE_PATH: &str = "tests/perf/perf_baseline.json";
const DEFAULT_ITERATIONS: usize = 10;
const DEFAULT_WARMUP: usize = 3;
const DEFAULT_MAX_REGRESSION_PCT: f64 = 15.0;

#[derive(Serialize)]
pub(crate) struct PerfCase {
    pub(crate) path: String,
    pub(crate) ok: bool,
    pub(crate) mean_ms: f64,
    pub(crate) min_ms: f64,
    pub(crate) max_ms: f64,
    pub(crate) baseline_ms: Option<f64>,
    pub(crate) regression_pct: Option<f64>,
    pub(crate) diagnostics: Vec<birddisk_core::Diagnostic>,
}

#[derive(Serialize)]
pub(crate) struct PerfReport {
    pub(crate) tool: &'static str,
    pub(crate) version: &'static str,
    pub(crate) ok: bool,
    pub(crate) engine: &'static str,
    pub(crate) iterations: usize,
    pub(crate) warmup: usize,
    pub(crate) max_regression_pct: f64,
    pub(crate) cases: Vec<PerfCase>,
    pub(crate) diagnostics: Vec<birddisk_core::Diagnostic>,
}

#[derive(Serialize, Deserialize)]
struct PerfBaseline {
    engine: String,
    iterations: usize,
    cases: Vec<PerfBaselineCase>,
}

#[derive(Serialize, Deserialize)]
struct PerfBaselineCase {
    path: String,
    mean_ms: f64,
}

#[derive(Clone, Copy)]
struct PerfStats {
    mean_ms: f64,
    min_ms: f64,
    max_ms: f64,
}

pub(crate) fn run_perf_report(
    engine: birddisk_core::Engine,
    dirs: &[String],
    tags: &[String],
    filters: &[String],
    baseline_path: Option<&str>,
    iterations: Option<usize>,
    warmup: Option<usize>,
    max_regression: Option<f64>,
) -> PerfReport {
    let mut report = PerfReport {
        tool: birddisk_core::TOOL_NAME,
        version: birddisk_core::VERSION,
        ok: true,
        engine: engine_label(engine),
        iterations: iterations.unwrap_or(DEFAULT_ITERATIONS).max(1),
        warmup: warmup.unwrap_or(DEFAULT_WARMUP),
        max_regression_pct: max_regression.unwrap_or(DEFAULT_MAX_REGRESSION_PCT),
        cases: Vec::new(),
        diagnostics: Vec::new(),
    };

    let roots = if dirs.is_empty() {
        default_perf_dirs()
    } else {
        dirs.to_vec()
    };
    let paths = match collect_test_paths(&roots, tags, filters) {
        Ok(paths) => paths,
        Err(diag) => {
            report.ok = false;
            report.diagnostics.push(diag);
            return report;
        }
    };

    let baseline = match baseline_path {
        Some(path) => match read_baseline(path) {
            Ok(baseline) => Some((path.to_string(), baseline)),
            Err(err) => {
                report.ok = false;
                report.diagnostics.push(perf_harness_diagnostic(path, err));
                None
            }
        },
        None => None,
    };

    let baseline_map = baseline.as_ref().map(|(_, baseline)| {
        baseline
            .cases
            .iter()
            .map(|case| (case.path.clone(), case.mean_ms))
            .collect::<HashMap<_, _>>()
    });

    if let Some((path, baseline)) = baseline.as_ref() {
        if baseline.engine != report.engine {
            report.ok = false;
            report.diagnostics.push(perf_harness_diagnostic(
                path,
                format!(
                    "Baseline engine '{}' does not match requested engine '{}'",
                    baseline.engine, report.engine
                ),
            ));
        }
    }

    for path in paths {
        let case = run_perf_case(
            &path,
            engine,
            report.iterations,
            report.warmup,
            report.max_regression_pct,
            baseline_map.as_ref(),
        );
        if !case.ok {
            report.ok = false;
        }
        report.cases.push(case);
    }

    report
}

pub(crate) fn write_baseline(
    path: &str,
    report: &PerfReport,
    engine: birddisk_core::Engine,
    iterations: usize,
) -> Result<(), String> {
    let cases: Vec<PerfBaselineCase> = report
        .cases
        .iter()
        .map(|case| PerfBaselineCase {
            path: case.path.clone(),
            mean_ms: case.mean_ms,
        })
        .collect();
    let baseline = PerfBaseline {
        engine: engine_label(engine).to_string(),
        iterations,
        cases,
    };
    let json = serde_json::to_string_pretty(&baseline)
        .map_err(|err| format!("unable to serialize perf baseline: {err}"))?;
    if let Some(parent) = std::path::Path::new(path).parent() {
        std::fs::create_dir_all(parent)
            .map_err(|err| format!("unable to create baseline dir '{path}': {err}"))?;
    }
    std::fs::write(path, json).map_err(|err| format!("unable to write baseline '{path}': {err}"))
}

pub(crate) fn format_perf_report(report: &PerfReport) -> String {
    let mut output = String::new();
    output.push_str(&format!(
        "Performance ({})\niterations: {}  warmup: {}\n",
        report.engine, report.iterations, report.warmup
    ));
    for case in &report.cases {
        output.push_str(&format!(
            "- {}: mean {:.2}ms (min {:.2}ms, max {:.2}ms)",
            case.path, case.mean_ms, case.min_ms, case.max_ms
        ));
        if let Some(baseline) = case.baseline_ms {
            output.push_str(&format!(" baseline {:.2}ms", baseline));
        }
        if let Some(regression) = case.regression_pct {
            output.push_str(&format!(" Δ{regression:.2}%"));
        }
        if !case.ok {
            output.push_str(" [FAIL]");
        }
        output.push('\n');
    }
    output
}

fn run_perf_case(
    path: &str,
    engine: birddisk_core::Engine,
    iterations: usize,
    warmup: usize,
    max_regression: f64,
    baseline: Option<&HashMap<String, f64>>,
) -> PerfCase {
    let mut case = PerfCase {
        path: path.to_string(),
        ok: true,
        mean_ms: 0.0,
        min_ms: 0.0,
        max_ms: 0.0,
        baseline_ms: None,
        regression_pct: None,
        diagnostics: Vec::new(),
    };

    let input = match read_test_input(path) {
        Ok(input) => input,
        Err(err) => {
            case.ok = false;
            case.diagnostics.push(perf_harness_diagnostic(path, err));
            return case;
        }
    };
    let args = match read_test_args(path) {
        Ok(args) => args,
        Err(err) => {
            case.ok = false;
            case.diagnostics.push(perf_harness_diagnostic(path, err));
            return case;
        }
    };

    let program = match birddisk_core::parse_and_typecheck(path) {
        Ok(mut program) => {
            birddisk_core::optimize_program(&mut program);
            program
        }
        Err(diagnostics) => {
            case.ok = false;
            case.diagnostics = diagnostics;
            return case;
        }
    };

    for _ in 0..warmup {
        if let Err(diag) = run_once(&program, engine, &input, &args, path) {
            case.ok = false;
            case.diagnostics.push(diag);
            return case;
        }
    }

    let mut samples = Vec::with_capacity(iterations);
    for _ in 0..iterations {
        let started = Instant::now();
        if let Err(diag) = run_once(&program, engine, &input, &args, path) {
            case.ok = false;
            case.diagnostics.push(diag);
            return case;
        }
        let elapsed = started.elapsed();
        samples.push(elapsed);
    }

    let stats = compute_stats(&samples);
    case.mean_ms = stats.mean_ms;
    case.min_ms = stats.min_ms;
    case.max_ms = stats.max_ms;

    if let Some(baseline) = baseline {
        match baseline.get(path) {
            Some(value) => {
                case.baseline_ms = Some(*value);
                if *value <= 0.0 {
                    case.ok = false;
                    case.diagnostics.push(perf_harness_diagnostic(
                        path,
                        "Baseline mean must be > 0".to_string(),
                    ));
                    return case;
                }
                let regression = ((case.mean_ms - value) / value) * 100.0;
                case.regression_pct = Some(regression);
                if regression > max_regression {
                    case.ok = false;
                    case.diagnostics.push(perf_harness_diagnostic(
                        path,
                        format!("Regression {regression:.2}% exceeds max {max_regression:.2}%"),
                    ));
                }
            }
            None => {
                case.ok = false;
                case.diagnostics.push(perf_harness_diagnostic(
                    path,
                    "No baseline entry found for case".to_string(),
                ));
            }
        }
    }

    case
}

fn run_once(
    program: &Program,
    engine: birddisk_core::Engine,
    input: &str,
    args: &[String],
    path: &str,
) -> Result<(), birddisk_core::Diagnostic> {
    match engine {
        birddisk_core::Engine::Vm => match birddisk_vm::eval_with_io(program, input, args) {
            Ok(_) => Ok(()),
            Err(err) => Err(runtime_diagnostic(
                path,
                err.message,
                err.code,
                runtime_spec_refs(err.code),
                err.trace,
            )),
        },
        birddisk_core::Engine::Wasm => match birddisk_wasm::run_with_io(program, input, args) {
            Ok(_) => Ok(()),
            Err(err) => Err(runtime_diagnostic(
                path,
                err.message,
                err.code,
                runtime_spec_refs(err.code),
                err.trace,
            )),
        },
        birddisk_core::Engine::Native => match birddisk_native::run_with_io(program, input, args) {
            Ok(_) => Ok(()),
            Err(err) => Err(runtime_diagnostic(
                path,
                err.message,
                err.code.unwrap_or("E0400"),
                runtime_spec_refs(err.code.unwrap_or("E0400")),
                err.trace,
            )),
        },
    }
}

fn compute_stats(samples: &[Duration]) -> PerfStats {
    let mut min: f64 = f64::MAX;
    let mut max: f64 = 0.0;
    let mut sum: f64 = 0.0;
    for sample in samples {
        let ms = sample.as_secs_f64() * 1000.0;
        min = min.min(ms);
        max = max.max(ms);
        sum += ms;
    }
    let mean = if samples.is_empty() {
        0.0
    } else {
        sum / samples.len() as f64
    };
    PerfStats {
        mean_ms: mean,
        min_ms: if min == f64::MAX { 0.0 } else { min },
        max_ms: max,
    }
}

fn read_baseline(path: &str) -> Result<PerfBaseline, String> {
    let contents =
        std::fs::read_to_string(path).map_err(|err| format!("unable to read baseline: {err}"))?;
    serde_json::from_str(&contents).map_err(|err| format!("invalid baseline JSON: {err}"))
}

fn default_perf_dirs() -> Vec<String> {
    if std::path::Path::new("tests/perf").exists() {
        vec!["tests/perf".to_string()]
    } else {
        vec!["tests".to_string()]
    }
}

fn engine_label(engine: birddisk_core::Engine) -> &'static str {
    match engine {
        birddisk_core::Engine::Vm => "vm",
        birddisk_core::Engine::Wasm => "wasm",
        birddisk_core::Engine::Native => "native",
    }
}

#[cfg(test)]
mod tests {
    use super::{compute_stats, PerfBaseline};
    use std::time::Duration;

    #[test]
    fn compute_stats_basic() {
        let samples = vec![
            Duration::from_millis(10),
            Duration::from_millis(20),
            Duration::from_millis(30),
        ];
        let stats = compute_stats(&samples);
        assert!((stats.mean_ms - 20.0).abs() < 0.01);
        assert!((stats.min_ms - 10.0).abs() < 0.01);
        assert!((stats.max_ms - 30.0).abs() < 0.01);
    }

    #[test]
    fn parse_baseline() {
        let json = r#"{
  "engine": "native",
  "iterations": 5,
  "cases": [
    { "path": "tests/perf/sum.bd", "mean_ms": 1.23 }
  ]
}"#;
        let baseline: PerfBaseline = serde_json::from_str(json).unwrap();
        assert_eq!(baseline.engine, "native");
        assert_eq!(baseline.iterations, 5);
        assert_eq!(baseline.cases.len(), 1);
        assert_eq!(baseline.cases[0].path, "tests/perf/sum.bd");
    }
}
