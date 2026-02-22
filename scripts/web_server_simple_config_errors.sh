#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

EXE="/tmp/web_server_simple_native_exe"
CONF="examples/web_server_simple/web_server_simple.conf"
BACKUP="/tmp/web_server_simple.conf.backup.$$"

cp "$CONF" "$BACKUP"
restore_conf() {
  cp "$BACKUP" "$CONF"
  rm -f "$BACKUP"
}
trap restore_conf EXIT

cargo run -p birddiskc -- build examples/web_server_simple/main.bd --engine native --emit exe --out "$EXE" >/tmp/web_server_simple_build.log 2>&1

run_expect_fail() {
  local name="$1"
  local conf_text="$2"
  local expected="$3"
  local out_file="/tmp/web_server_simple_cfg_${name}.out"

  printf '%s\n' "$conf_text" >"$CONF"

  set +e
  "$EXE" >"$out_file" 2>&1
  local status=$?
  set -e

  if [[ "$status" -eq 0 ]]; then
    echo "expected failure for case: $name" >&2
    cat "$out_file" >&2 || true
    exit 1
  fi

  if ! grep -Eiq "$expected" "$out_file"; then
    echo "failure output for case '$name' did not match /$expected/" >&2
    cat "$out_file" >&2 || true
    exit 1
  fi
}

run_expect_fail \
  "missing_port" \
  "host 127.0.0.1
max_requests 200" \
  "Missing config key: port"

run_expect_fail \
  "invalid_port" \
  "host 127.0.0.1
port not_a_number
max_requests 200" \
  "runtime error|to_i64|i64"

run_expect_fail \
  "invalid_max_requests" \
  "host 127.0.0.1
port 18080
max_requests nope" \
  "runtime error|to_i64|i64"

run_expect_fail \
  "invalid_mode" \
  "host 127.0.0.1
port 18080
max_requests 200
mode weird" \
  "Invalid mode:"

run_expect_fail \
  "invalid_workers" \
  "host 127.0.0.1
port 18080
max_requests 200
mode threaded
workers 0" \
  "Invalid workers"

echo "web_server_simple config error-path checks ok"
