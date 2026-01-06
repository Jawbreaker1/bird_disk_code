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


def parse_json_output(stdout: str, stderr: str, task_id: str):
    raw = stdout.strip() or stderr.strip()
    if not raw:
        raise ValueError("empty output")
    try:
        return json.loads(raw)
    except json.JSONDecodeError as exc:
        raise ValueError(f"invalid JSON output: {exc}")


def run_task(task, birddisk, tmpdir: Path):
    task_id = task.get("id", "(missing id)")
    kind = task.get("kind")
    source = task.get("input")

    if kind not in {"check", "run"}:
        return False, f"unsupported kind '{kind}'"
    if not isinstance(source, str) or not source:
        return False, "missing or empty input"

    file_name = f"{safe_filename(task_id)}.bd"
    file_path = tmpdir / file_name
    file_path.write_text(source)

    cmd = [birddisk, kind, str(file_path), "--json"]
    if kind == "run":
        engine = task.get("engine", "vm")
        cmd.extend(["--engine", engine])

    try:
        proc = subprocess.run(cmd, capture_output=True, text=True)
    except FileNotFoundError:
        raise SystemExit(f"birddisk not found: {birddisk}")

    try:
        result = parse_json_output(proc.stdout, proc.stderr, task_id)
    except ValueError as exc:
        return False, str(exc)

    expect = task.get("expect", {})
    if "ok" in expect:
        if result.get("ok") is not expect["ok"]:
            return False, f"ok mismatch (expected {expect['ok']})"

    if "result" in expect:
        if result.get("result") != expect["result"]:
            return False, f"result mismatch (expected {expect['result']})"

    if "codes" in expect:
        diag_codes = [d.get("code") for d in result.get("diagnostics", [])]
        for code in expect["codes"]:
            if code not in diag_codes:
                return False, f"missing diagnostic code {code}"

    return True, ""


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
        for task in tasks:
            task_id = task.get("id", "(missing id)")
            ok, message = run_task(task, args.birddisk, tmpdir)
            if ok:
                passed += 1
                print(f"PASS {task_id}")
            else:
                print(f"FAIL {task_id}: {message}")

    total = len(tasks)
    print(f"\n{passed}/{total} tasks passed")
    sys.exit(0 if passed == total else 1)


if __name__ == "__main__":
    main()
