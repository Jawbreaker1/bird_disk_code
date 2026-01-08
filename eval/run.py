#!/usr/bin/env python3
import argparse
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path


def load_tasks(path: Path):
    try:
        data = json.loads(path.read_text())
    except json.JSONDecodeError as exc:
        raise SystemExit(f"invalid JSON in {path}: {exc}")
    if not isinstance(data, list):
        raise SystemExit(f"tasks file must be a JSON array: {path}")
    return data


def safe_filename(task_id: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_-]+", "_", task_id).strip("_") or "task"


def java_class_name(task_id: str) -> str:
    base = re.sub(r"[^a-zA-Z0-9_]+", "", safe_filename(task_id))
    if not base or not base[0].isalpha():
        base = f"Task{base}"
    return f"Task{base}"


def parse_json_output(stdout: str, stderr: str, task_id: str):
    raw = stdout.strip() or stderr.strip()
    if not raw:
        raise ValueError("empty output")
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise ValueError(f"invalid JSON output: {exc}")


def run_birddisk_case(case, expect, birddisk, tmpdir: Path, task_id: str):
    kind = case.get("kind")
    source = case.get("input")

    if kind not in {"check", "run"}:
        return False, f"unsupported kind '{kind}'"
    if not isinstance(source, str) or not source:
        return False, "missing or empty input"

    file_name = f"{safe_filename(task_id)}.bd"
    file_path = tmpdir / file_name
    file_path.write_text(source)

    cmd = [birddisk, kind, str(file_path), "--json"]
    if kind == "run":
        engine = case.get("engine") or "vm"
        cmd.extend(["--engine", engine])

    try:
        proc = subprocess.run(cmd, capture_output=True, text=True)
    except FileNotFoundError:
        raise SystemExit(f"birddisk not found: {birddisk}")

    try:
        result = parse_json_output(proc.stdout, proc.stderr, task_id)
    except ValueError as exc:
        return False, str(exc)

    return validate_expect(expect, result)


def run_python_case(case, expect, tmpdir: Path, task_id: str):
    source = case.get("input")
    if not isinstance(source, str) or not source:
        return False, "missing or empty input"
    file_name = f"{safe_filename(task_id)}.py"
    file_path = tmpdir / file_name
    file_path.write_text(source)

    cmd = ["python3", str(file_path)]
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True)
    except FileNotFoundError:
        return False, "python3 not found"

    return validate_process_expect(expect, proc)


def run_js_case(case, expect, tmpdir: Path, task_id: str):
    source = case.get("input")
    if not isinstance(source, str) or not source:
        return False, "missing or empty input"
    file_name = f"{safe_filename(task_id)}.js"
    file_path = tmpdir / file_name
    file_path.write_text(source)

    cmd = ["node", str(file_path)]
    try:
        proc = subprocess.run(cmd, capture_output=True, text=True)
    except FileNotFoundError:
        return False, "node not found"

    return validate_process_expect(expect, proc)


def run_java_case(case, expect, tmpdir: Path, task_id: str):
    source = case.get("input")
    if not isinstance(source, str) or not source:
        return False, "missing or empty input"

    wrap = case.get("wrap", False)
    class_name = case.get("class")
    if wrap:
        class_name = java_class_name(task_id)
        body_lines = source.splitlines() or [""]
        body = "\n".join(f"    {line}" for line in body_lines)
        source = (
            f"public class {class_name} {{\n"
            "  public static void main(String[] args) throws Exception {\n"
            f"{body}\n"
            "  }\n"
            "}\n"
        )
    elif not class_name:
        return False, "missing java class name (use 'class' or 'wrap')"

    file_path = tmpdir / f"{class_name}.java"
    file_path.write_text(source)

    try:
        compile_proc = subprocess.run(
            ["javac", str(file_path)],
            capture_output=True,
            text=True,
            cwd=tmpdir,
        )
    except FileNotFoundError:
        return False, "javac not found"

    if compile_proc.returncode != 0:
        return False, f"javac failed: {compile_proc.stderr.strip()}"

    try:
        run_proc = subprocess.run(
            ["java", "-cp", str(tmpdir), class_name],
            capture_output=True,
            text=True,
            cwd=tmpdir,
        )
    except FileNotFoundError:
        return False, "java not found"

    return validate_process_expect(expect, run_proc)

def validate_expect(expect, result):
    if "ok" in expect:
        if result.get("ok") is not expect["ok"]:
            return False, f"ok mismatch (expected {expect['ok']})"

    if "result" in expect:
        if result.get("result") != expect["result"]:
            return False, f"result mismatch (expected {expect['result']})"

    if "stdout" in expect:
        if result.get("stdout") != expect["stdout"]:
            return False, "stdout mismatch"

    if "codes" in expect:
        diag_codes = [d.get("code") for d in result.get("diagnostics", [])]
        for code in expect["codes"]:
            if code not in diag_codes:
                return False, f"missing diagnostic code {code}"

    return True, ""


def validate_process_expect(expect, proc):
    ok = proc.returncode == 0
    stdout = proc.stdout

    if "ok" in expect:
        if ok is not expect["ok"]:
            return False, f"ok mismatch (expected {expect['ok']})"

    if "stdout" in expect:
        if stdout != expect["stdout"]:
            return False, "stdout mismatch"

    if "result" in expect:
        stripped = stdout.strip()
        try:
            value = int(stripped, 10)
        except ValueError:
            return False, "unable to parse result from stdout"
        if value != expect["result"]:
            return False, f"result mismatch (expected {expect['result']})"

    return True, ""


def resolve_cases(task):
    cases = task.get("cases")
    if cases:
        return cases
    kind = task.get("kind")
    source = task.get("input")
    if kind or source:
        return [
            {
                "lang": "birddisk",
                "kind": kind,
                "input": source,
                "engine": task.get("engine"),
            }
        ]
    return []


def main():
    parser = argparse.ArgumentParser(description="Run minimal BirdDisk eval tasks")
    parser.add_argument(
        "--birddisk",
        default="birddisk",
        help="Path to birddisk CLI (default: birddisk)",
    )
    parser.add_argument(
        "--tasks",
        default=str(Path(__file__).with_name("tasks.json")),
        help="Path to tasks.json",
    )
    args = parser.parse_args()

    tasks_path = Path(args.tasks)
    tasks = load_tasks(tasks_path)

    passed = 0
    with tempfile.TemporaryDirectory() as tmp:
        tmpdir = Path(tmp)
        total = 0
        for task in tasks:
            task_id = task.get("id", "(missing id)")
            expect = task.get("expect", {})
            cases = resolve_cases(task)
            if not cases:
                print(f"FAIL {task_id}: missing cases")
                total += 1
                continue
            for case in cases:
                total += 1
                lang = case.get("lang", "birddisk")
                case_id = f"{task_id}:{lang}"
                case_expect = case.get("expect", expect)
                if lang == "birddisk":
                    ok, message = run_birddisk_case(
                        case, case_expect, args.birddisk, tmpdir, case_id
                    )
                elif lang == "python":
                    ok, message = run_python_case(case, case_expect, tmpdir, case_id)
                elif lang in {"javascript", "js", "node"}:
                    ok, message = run_js_case(case, case_expect, tmpdir, case_id)
                elif lang == "java":
                    ok, message = run_java_case(case, case_expect, tmpdir, case_id)
                else:
                    ok, message = False, f"unsupported language '{lang}'"
                if ok:
                    passed += 1
                    print(f"PASS {case_id}")
                else:
                    print(f"FAIL {case_id}: {message}")

    print(f"\n{passed}/{total} cases passed")
    sys.exit(0 if total > 0 and passed == total else 1)


if __name__ == "__main__":
    main()
