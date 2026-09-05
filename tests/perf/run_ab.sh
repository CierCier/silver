#!/usr/bin/env bash
# tests/perf/run_ab.sh — Apache Bench (ab) benchmark for the stdlib HTTP and
# HTTPS servers (std.net.http HttpServer.serve_conn / serve_tls_conn).
#
# Builds and starts the thread-per-connection bench servers, then runs the
# same request matrix against each and prints a summary table:
#
#   tests/perf/run_ab.sh              # HTTP + HTTPS matrix
#   tests/perf/run_ab.sh http         # HTTP only
#   tests/perf/run_ab.sh https        # HTTPS only (requires libssl; run from
#                                     # a checkout whose tests/certs/ exists)
#
# Requires: ab (apache2-utils), target/{debug}/{agc} built, repo certs.
set -u
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

AGC="target/debug/agc"
[ -x "$AGC" ] || AGC="target/release/agc"
command -v ab >/dev/null 2>&1 || { echo "ab not found (apt-get install apache2-utils)" >&2; exit 1; }
[ -x "$AGC" ] || { echo "agc not built (cargo build -p agc)" >&2; exit 1; }

mode="${1:-all}"
OUT=/tmp/ab_results
mkdir -p "$OUT"

start_server() {
    local bin=$1 log=$2
    pkill -x "$(basename "$bin")" 2>/dev/null
    sleep 0.3
    (setsid "$bin" > "$log" 2>&1 &)
    sleep 0.5
}

summarize() {
    local label=$1 file=$2
    printf "%-14s %8s %10s %12s %10s\n" "$label" \
        "$(awk '/Complete requests/{print $3}' "$file")" \
        "$(awk '/^Failed requests/{print $3}' "$file")" \
        "$(awk '/Requests per second/{print $4}' "$file")" \
        "$(awk '/Time per request/{print $4; exit}' "$file")"
}

run_matrix() {
    local scheme=$1 port=$2 prefix=$3
    echo "== $scheme matrix (keep-alive unless noted) =="
    printf "%-14s %8s %10s %12s %10s\n" "case" "complete" "failed" "req/s" "ms/req"
    for C in 1 8 64; do
        local N=$(( C == 64 ? 20000 : 50000 ))
        ab -k -n "$N" -c "$C" "$scheme://127.0.0.1:$port/plaintext" > "$OUT/${prefix}_k_c$C.txt" 2>&1
        summarize "$prefix -k c$C" "$OUT/${prefix}_k_c$C.txt"
    done
    # Connection churn: a fresh connection per request (no keep-alive).
    ab -n 2000 -c 8 "$scheme://127.0.0.1:$port/plaintext" > "$OUT/${prefix}_churn_c8.txt" 2>&1
    summarize "$prefix churn c8" "$OUT/${prefix}_churn_c8.txt"
}

if [ "$mode" = http ] || [ "$mode" = all ]; then
    "$AGC" -O2 tests/perf/bench_server_http.ag -o /tmp/bench_http 2>/dev/null || exit 1
    start_server /tmp/bench_http /tmp/bench_http.log
    run_matrix http 18095 http
    pkill -x bench_http 2>/dev/null
fi

if [ "$mode" = https ] || [ "$mode" = all ]; then
    "$AGC" -O2 tests/perf/bench_server_https.ag -o /tmp/bench_https 2>/dev/null || exit 1
    start_server /tmp/bench_https /tmp/bench_https.log
    # Known issue: concurrent TLS connections trigger a heap corruption in the
    # stdlib TLS layer (server aborts with glibc "malloc(): unaligned tcache
    # chunk detected"), so expect c>1 runs to report large failure counts.
    run_matrix https 18443 https
    pkill -x bench_https 2>/dev/null
fi
