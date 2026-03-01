#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

EXE="/tmp/web_server_simple_native_exe"
LOG="/tmp/web_server_simple_threaded_stress.log"
CONF="examples/web_server_simple/web_server_simple.conf"
BACKUP="/tmp/web_server_simple_threaded_stress.conf.backup.$$"
WORKDIR="/tmp/web_server_simple_threaded_stress.$$"
pid=""

FEATURE_REQUESTS=120
ABOUT_REQUESTS=80
CSS_REQUESTS=40
JS_REQUESTS=20
LOAD_TOTAL=$((FEATURE_REQUESTS + ABOUT_REQUESTS + CSS_REQUESTS + JS_REQUESTS))
# One successful /health probe + one POST /features + one /shutdown request.
EXPECTED_SERVED_TOTAL=$((LOAD_TOTAL + 3))

mkdir -p "$WORKDIR"
cp "$CONF" "$BACKUP"
cleanup() {
  if [[ -n "${pid:-}" ]] && kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
  cp "$BACKUP" "$CONF"
  rm -f "$BACKUP"
  rm -rf "$WORKDIR"
}
trap cleanup EXIT

host="$(awk '$1=="host"{print $2; exit}' "$CONF")"
port="$(awk '$1=="port"{print $2; exit}' "$CONF")"

if [[ -z "${host:-}" || -z "${port:-}" ]]; then
  echo "missing host/port in $CONF" >&2
  exit 1
fi

cat > "$CONF" <<EOF
# birdisk web server config
host $host
port $port
max_requests $((EXPECTED_SERVED_TOTAL + 40))
mode threaded
workers 8
EOF

base="http://${host}:${port}"

cargo run -p birddiskc -- build examples/web_server_simple/main.bd --engine native --emit exe --out "$EXE" >/tmp/web_server_simple_build.log 2>&1

: > "$LOG"
"$EXE" >"$LOG" 2>&1 &
pid=$!

ready=0
for _ in $(seq 1 120); do
  if curl -fsS "$base/health" >/dev/null 2>&1; then
    ready=1
    break
  fi
  sleep 0.1
done

if [[ "$ready" -ne 1 ]]; then
  echo "threaded stress server did not become ready on $base" >&2
  tail -n 100 "$LOG" >&2 || true
  exit 1
fi

pids=""
req_id=0

enqueue_request() {
  local path="$1"
  local expected_code="$2"
  local body_check="$3"
  local prefix="$4"
  local id="$5"

  local body_file="${WORKDIR}/${prefix}_${id}.body"
  local code_file="${WORKDIR}/${prefix}_${id}.code"

  (
    code="$(curl -sS -o "$body_file" -w '%{http_code}' "$base$path" || echo "000")"
    printf '%s\n' "$code" > "$code_file"
  ) &
  pids="$pids $!"

  printf '%s|%s|%s|%s|%s\n' "$prefix" "$id" "$expected_code" "$body_check" "$body_file" >> "${WORKDIR}/manifest.txt"
}

for i in $(seq 1 "$FEATURE_REQUESTS"); do
  req_id=$((req_id + 1))
  enqueue_request "/features" "200" "What this demo proves" "features" "$req_id"
done
for i in $(seq 1 "$ABOUT_REQUESTS"); do
  req_id=$((req_id + 1))
  enqueue_request "/about" "200" "About this server" "about" "$req_id"
done
for i in $(seq 1 "$CSS_REQUESTS"); do
  req_id=$((req_id + 1))
  enqueue_request "/style.css" "200" "--bg-a:" "css" "$req_id"
done
for i in $(seq 1 "$JS_REQUESTS"); do
  req_id=$((req_id + 1))
  enqueue_request "/app.js" "200" "fetch(\"/api/status\")" "js" "$req_id"
done

wait_fail=0
for child in $pids; do
  if ! wait "$child"; then
    wait_fail=1
  fi
done
if [[ "$wait_fail" -ne 0 ]]; then
  echo "one or more concurrent requests failed to execute" >&2
  exit 1
fi

while IFS='|' read -r prefix id expected_code body_check body_file; do
  code_file="${WORKDIR}/${prefix}_${id}.code"
  if [[ ! -f "$code_file" ]]; then
    echo "missing code file for ${prefix}_${id}" >&2
    exit 1
  fi
  code="$(tr -d '\r\n' < "$code_file")"
  if [[ "$code" != "$expected_code" ]]; then
    echo "unexpected status for ${prefix}_${id}: got $code expected $expected_code" >&2
    cat "$body_file" >&2 || true
    exit 1
  fi
  if ! grep -q -- "$body_check" "$body_file"; then
    echo "response body check failed for ${prefix}_${id}: missing '$body_check'" >&2
    cat "$body_file" >&2 || true
    exit 1
  fi
done < "${WORKDIR}/manifest.txt"

post_code="$(curl -s -X POST -o "${WORKDIR}/post_features.body" -w '%{http_code}' "$base/features")"
if [[ "$post_code" != "405" ]]; then
  echo "expected POST /features -> 405 in threaded stress mode, got $post_code" >&2
  cat "${WORKDIR}/post_features.body" >&2 || true
  exit 1
fi

shutdown_body="$(curl -fsS "$base/shutdown")"
if [[ "$shutdown_body" != *"shutting down"* ]]; then
  echo "/shutdown response did not contain expected text in threaded stress mode" >&2
  exit 1
fi

wait "$pid"
pid=""

served_total="$(awk '/served total:/{val=$3} END{print val}' "$LOG")"
if [[ -z "${served_total:-}" ]]; then
  echo "threaded stress server log missing served total line" >&2
  tail -n 100 "$LOG" >&2 || true
  exit 1
fi
if [[ "$served_total" != "$EXPECTED_SERVED_TOTAL" ]]; then
  echo "unexpected served total: got $served_total expected $EXPECTED_SERVED_TOTAL" >&2
  tail -n 120 "$LOG" >&2 || true
  exit 1
fi

echo "web_server_simple threaded stress ok ($base), served total $served_total"
