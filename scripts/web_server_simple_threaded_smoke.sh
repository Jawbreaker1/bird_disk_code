#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

EXE="/tmp/web_server_simple_native_exe"
LOG="/tmp/web_server_simple_threaded.log"
CONF="examples/web_server_simple/web_server_simple.conf"
BACKUP="/tmp/web_server_simple_threaded.conf.backup.$$"
pid=""

cp "$CONF" "$BACKUP"
cleanup() {
  if [[ -n "${pid:-}" ]] && kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
    wait "$pid" 2>/dev/null || true
  fi
  cp "$BACKUP" "$CONF"
  rm -f "$BACKUP"
}
trap cleanup EXIT

host="$(awk '$1=="host"{print $2; exit}' "$CONF")"
port="$(awk '$1=="port"{print $2; exit}' "$CONF")"
max_requests="$(awk '$1=="max_requests"{print $2; exit}' "$CONF")"

if [[ -z "${host:-}" || -z "${port:-}" || -z "${max_requests:-}" ]]; then
  echo "missing host/port/max_requests in $CONF" >&2
  exit 1
fi

cat > "$CONF" <<EOF
# birdisk web server config
host $host
port $port
max_requests $max_requests
mode threaded
workers 4
EOF

base="http://${host}:${port}"

cargo run -p birddiskc -- build examples/web_server_simple/main.bd --engine native --emit exe --out "$EXE" >/tmp/web_server_simple_build.log 2>&1

: > "$LOG"
"$EXE" >"$LOG" 2>&1 &
pid=$!

for _ in $(seq 1 100); do
  if curl -fsS "$base/health" >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

if ! curl -fsS "$base/health" >/dev/null 2>&1; then
  echo "threaded server did not become ready on $base" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 1
fi

# Issue concurrent requests to exercise threaded stream workers.
pids=""
for i in $(seq 1 24); do
  out="/tmp/web_server_simple_threaded_features_${i}.out"
  (
    curl -fsS "$base/features" >"$out"
  ) &
  pids="$pids $!"
done

for child in $pids; do
  wait "$child"
done

for i in $(seq 1 24); do
  out="/tmp/web_server_simple_threaded_features_${i}.out"
  if ! grep -q "What this demo proves" "$out"; then
    echo "threaded features response $i missing expected content" >&2
    cat "$out" >&2 || true
    exit 1
  fi
done

code_post="$(curl -s -X POST -o /tmp/web_server_simple_threaded_post.out -w '%{http_code}' "$base/features")"
if [[ "$code_post" != "405" ]]; then
  echo "expected POST /features -> 405 in threaded mode, got $code_post" >&2
  cat /tmp/web_server_simple_threaded_post.out >&2 || true
  exit 1
fi

shutdown_body="$(curl -fsS "$base/shutdown")"
if [[ "$shutdown_body" != *"shutting down"* ]]; then
  echo "/shutdown response did not contain expected text in threaded mode" >&2
  exit 1
fi

wait "$pid"
if ! grep -q "served total:" "$LOG"; then
  echo "threaded server log missing served total line" >&2
  tail -n 80 "$LOG" >&2 || true
  exit 1
fi

echo "web_server_simple threaded smoke ok ($base)"
