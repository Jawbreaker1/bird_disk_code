#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

EXE="/tmp/web_server_simple_native_exe"
LOG="/tmp/web_server_simple_smoke.log"
CONF="examples/web_server_simple/web_server_simple.conf"

host="$(awk '$1=="host"{print $2; exit}' "$CONF")"
port="$(awk '$1=="port"{print $2; exit}' "$CONF")"

if [[ -z "${host:-}" || -z "${port:-}" ]]; then
  echo "missing host/port in $CONF" >&2
  exit 1
fi

base="http://${host}:${port}"

cargo run -p birddiskc -- build examples/web_server_simple/main.bd --engine native --emit exe --out "$EXE" >/tmp/web_server_simple_build.log 2>&1

: > "$LOG"
"$EXE" >"$LOG" 2>&1 &
pid=$!

cleanup() {
  if kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
}
trap cleanup EXIT

for _ in $(seq 1 100); do
  if curl -fsS "$base/health" >/tmp/web_server_simple_health.out 2>/dev/null; then
    break
  fi
  sleep 0.1
done

if ! curl -fsS "$base/health" >/tmp/web_server_simple_health.out 2>/dev/null; then
  echo "server did not become ready on $base" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 1
fi

expect_status() {
  local method="$1"
  local path="$2"
  local expected="$3"
  local body_file="/tmp/web_server_simple_body_$(echo "$method$path" | tr -cs 'a-zA-Z0-9' '_')"
  local got
  got="$(curl -s -X "$method" -o "$body_file" -w '%{http_code}' "$base$path")"
  if [[ "$got" != "$expected" ]]; then
    echo "expected $method $path -> $expected, got $got" >&2
    cat "$body_file" >&2 || true
    exit 1
  fi
}

expect_contains() {
  local path="$1"
  local needle="$2"
  local body
  body="$(curl -fsS "$base$path")"
  if [[ "$body" != *"$needle"* ]]; then
    echo "response for $path did not contain: $needle" >&2
    exit 1
  fi
}

expect_header_contains() {
  local path="$1"
  local header="$2"
  local expected="$3"
  local line
  line="$(curl -sD - -o /dev/null "$base$path" | tr -d '\r' | awk -v h="$header" 'BEGIN{IGNORECASE=1} tolower($0) ~ "^"tolower(h)":" {print; exit}')"
  if [[ "$line" != *"$expected"* ]]; then
    echo "header $header for $path missing expected value: $expected" >&2
    echo "got: $line" >&2
    exit 1
  fi
}

expect_status GET "/" 200
expect_status GET "/features" 200
expect_status GET "/about" 200
expect_status GET "/style.css" 200
expect_status GET "/app.js" 200
expect_status GET "/api/status" 200
expect_status GET "/missing" 404
post_body_file="/tmp/web_server_simple_post_features.out"
post_code="$(curl -s -X POST -o "$post_body_file" -w '%{http_code}' "$base/features")"
if [[ "$post_code" != "405" ]]; then
  echo "expected POST /features -> 405, got $post_code" >&2
  cat "$post_body_file" >&2 || true
  exit 1
fi

expect_contains "/" "BirdDisk Native Site"
expect_contains "/features" "What this demo proves"
expect_contains "/about" "About this server"
expect_contains "/api/status" "\"ok\":true"
expect_contains "/api/status" "\"served\":"
expect_contains "/style.css" ":root {"
expect_contains "/app.js" "fetch(\"/api/status\")"
if ! grep -q "404" /tmp/web_server_simple_body_GET_missing_; then
  echo "GET /missing body missing expected 404 marker" >&2
  cat /tmp/web_server_simple_body_GET_missing_ >&2 || true
  exit 1
fi
if ! grep -q "method not allowed" "$post_body_file"; then
  echo "POST /features body missing expected text" >&2
  cat "$post_body_file" >&2 || true
  exit 1
fi

expect_header_contains "/style.css" "Content-Type" "text/css"
expect_header_contains "/app.js" "Content-Type" "text/javascript"
expect_header_contains "/api/status" "Content-Type" "application/json"

shutdown_body="$(curl -fsS "$base/shutdown")"
if [[ "$shutdown_body" != *"shutting down"* ]]; then
  echo "/shutdown response did not contain expected text" >&2
  exit 1
fi

wait "$pid"
trap - EXIT

if ! grep -q "served total:" "$LOG"; then
  echo "server log missing served total line" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 1
fi

echo "web_server_simple smoke ok ($base)"
