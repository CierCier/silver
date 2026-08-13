#!/usr/bin/env bash
# tests/perf/run_perf.sh — three-way HTTP client throughput comparison.
#
# Starts the fast Go server (tests/perf/http_server.go on 127.0.0.1:18099),
# then runs each client for the same 15-second window and prints a table:
#
#   tests/perf/run_perf.sh              # run all four (Silver needs agc)
#   tests/perf/run_perf.sh silver       # just the Silver HttpClient
#   tests/perf/run_perf.sh raw          # just the Silver raw-socket ceiling
#   tests/perf/run_perf.sh go           # just the Go client
#   tests/perf/run_perf.sh rust         # just the Rust client
#
# All clients use the same workload: one keep-alive connection, sequential
# GETs, full body read per request. The server is deliberately trivial so the
# client is the measured side.
set -u
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PORT=18099
URL="http://127.0.0.1:$PORT/"

command -v go >/dev/null 2>&1 || { echo "go not available" >&2; exit 1; }
command -v rustc >/dev/null 2>&1 || { echo "rustc not available" >&2; exit 1; }

# Clear any lingering server from a previous run, then build and start.
pkill -f silver_http_perf_server 2>/dev/null
sleep 0.2
CGO_ENABLED=0 go build -o /tmp/silver_http_perf_server "$ROOT/tests/perf/http_server.go" || exit 1
/tmp/silver_http_perf_server &
SERVER_PID=$!
trap 'kill "$SERVER_PID" 2>/dev/null; wait "$SERVER_PID" 2>/dev/null' EXIT
for _ in $(seq 1 50); do
    if curl -s -o /dev/null --max-time 1 "$URL"; then break; fi
    sleep 0.1
done

run_one() {
    local name=$1
    shift
    echo "== $name =="
    "$@" || echo "  (failed)"
}

case "${1:-all}" in
    silver)
        # Build and run the Silver HttpClient (15s).
        "$ROOT/target/debug/agc" "$ROOT/tests/http_perf_test.ag" -o /tmp/silver_http_perf 2>/dev/null || exit 1
        run_one silver /tmp/silver_http_perf
        ;;
    raw)
        # Silver raw-socket ceiling (no client machinery).
        "$ROOT/target/debug/agc" "$ROOT/tests/perf/client_raw.ag" -o /tmp/silver_http_perf_raw 2>/dev/null || exit 1
        run_one "silver raw" /tmp/silver_http_perf_raw
        ;;
    go)
        run_one go env CGO_ENABLED=0 go run "$ROOT/tests/perf/client_go.go"
        ;;
    rust)
        rustc -O "$ROOT/tests/perf/client_rust.rs" -o /tmp/silver_http_perf_rust || exit 1
        run_one rust /tmp/silver_http_perf_rust
        ;;
    all)
        "$ROOT/target/debug/agc" "$ROOT/tests/http_perf_test.ag" -o /tmp/silver_http_perf 2>/dev/null || exit 1
        run_one silver /tmp/silver_http_perf
        "$ROOT/target/debug/agc" "$ROOT/tests/perf/client_raw.ag" -o /tmp/silver_http_perf_raw 2>/dev/null || exit 1
        run_one "silver raw" /tmp/silver_http_perf_raw
        run_one go env CGO_ENABLED=0 go run "$ROOT/tests/perf/client_go.go"
        rustc -O "$ROOT/tests/perf/client_rust.rs" -o /tmp/silver_http_perf_rust || exit 1
        run_one rust /tmp/silver_http_perf_rust
        ;;
    *)
        echo "usage: $0 [silver|raw|go|rust|all]" >&2
        exit 2
        ;;
esac
