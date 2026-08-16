#!/usr/bin/env bash
# Integration test harness for the Silver compiler (agc).
#
# Builds agc, then compiles and runs every tests/*.ag file. A test fails if
# it does not compile, or if the produced binary exits with an unexpected
# status. Run from anywhere; the script always operates from the repo root.
#
# Usage:
#   bash tests/run_tests.sh              # run all tests
#   bash tests/run_tests.sh vec_test     # run a single test by name
#   bash tests/run_tests.sh vec_test.ag  # (with or without the .ag suffix)
#   bash tests/run_tests.sh --compare FILE  # run, then diff run times vs FILE
#
# Every run appends a tab-separated metrics row (test, compile_ms, run_ms,
# peak_mem_kb) to bench/metrics.tsv; the summary lists the slowest tests.
# Compare two runs (e.g. before/after a perf change) with --compare.
#
# Dependencies: bash, coreutils, cargo (plus the toolchain agc itself needs).
set -u

# Baseline metrics file for --compare; also the destination of every run.
COMPARE_BASELINE=""
for arg in "$@"; do
    if [ "$arg" = "--compare" ]; then
        COMPARE_BASELINE="${2:-}"
        if [ -z "$COMPARE_BASELINE" ] || [ ! -f "$COMPARE_BASELINE" ]; then
            echo "error: --compare requires an existing metrics file" >&2
            exit 1
        fi
        shift 2
        break
    fi
    if [ "$arg" = "--" ]; then
        shift
        break
    fi
    break
done

# ---------------------------------------------------------------------------
# Skip list. One test name (without .ag) per line, each with a comment
# explaining why it is skipped. Currently empty: every test in tests/ is
# expected to compile and run.
SKIP_TESTS="
"

# Some tests intentionally exit with a nonzero status. Return the expected
# exit code for a test name; defaults to 0.
expected_exit() {
    case "$1" in
        # These tests verify raw Linux syscall support by invoking
        # sys_exit(42) directly (see plan/runtime-migration.md, Phase 1),
        # so the expected exit code is 42, not 0.
        syscall_test) echo 42 ;;
        syscall_wrapper_test) echo 42 ;;
        # main returns a + b + c + d = 1 + 2 + 3 + 1 = 7; the harness
        # treats the exit status as the expected code.
        static_volatile_test) echo 7 ;;
        # A failing @assert prints its message to stderr and aborts (128+6).
        assert_fail_test) echo 134 ;;
        *) echo 0 ;;
    esac
}

# Some tests require specific compiler flags. Return extra flags for a test
# name; defaults to empty.
test_specific_flags() {
    case "$1" in
        static_link_test) echo "--static-runtime" ;;
        thread_test) echo "--static-runtime" ;;
        launch_wait_test) echo "--static-runtime" ;;
        channel_test) echo "--static-runtime" ;;
        guard_test) echo "--static-runtime" ;;
        launch_send_test) echo "--static-runtime" ;;
        tls_test|http2_tls_test) [ -n "$SILVER_OPENSSL_LIB" ] && echo "-L $SILVER_OPENSSL_LIB" ;;
        module_import_test) echo "-I $MODLIB_DIR" ;;
        cfg_test) echo "--cfg cfg_test_flag=1,cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1" ;;
        ternary_test) echo "--cfg cpu.sse41=1" ;;
        target_feature_test) echo "--cfg cpu.avx2=1" ;;
        cfg_derived_test) echo "-O2" ;;
        *) echo "" ;;
    esac
}

# Tests compiled with --leak-check: allocator leak/double-free/overflow
# detection must report zero leaks or the test fails. Grow this list as
# leaks get fixed; the goal is to enable it for every test.
LEAK_CHECK_TESTS="
memory_pentest
alloc_validity_test
string_test
vec_test
mem_test
memmove_scalar_test
channel_test
memory_stress
http_test
cookie_test
assignment_drop_test
field_predrop_test
temp_operator_test
enum_move_test
enum_cascade_test
"

# True when a test is compiled with --leak-check.
leak_check_enabled() {
    case "$LEAK_CHECK_TESTS" in
        *"$1"*) return 0 ;;
        *) return 1 ;;
    esac
}


# Some tests are expected to fail at compile time (e.g., type errors).
# Return 0 (success) if compilation failure is the expected outcome.
expected_compile_failure() {
    case "$1" in
        enum_arity_error_test) return 0 ;;
        inherent_drop_error_test) return 0 ;;
        static_volatile_negative_test) return 0 ;;
        target_feature_error_test) return 0 ;;
        borrow_origin_escape_error_test) return 0 ;;
        enum_move_in_error_test) return 0 ;;
        launch_wait_error_test) return 0 ;;
        launch_send_error_test) return 0 ;;
        *) return 1 ;;
    esac
}
# Kill runaway test binaries after this many seconds.
RUN_TIMEOUT_SECS=120

# ---------------------------------------------------------------------------
# Timing and metrics helpers
#
# TIME_FMT for \time(1) — written to a file via -o, then read back.
# The \ operator bypasses the shell built-in to reach GNU time.
TIME_FMT='real=%e\tuser=%U\tsys=%S\tcpu=%P\tmem=%M'

# Column widths for the results table.
COL_NAME=26
COL_TIME=8
COL_CPU=7
COL_MEM=8

# Aggregate accumulators (units below).
total_compile_real_ms=0
total_run_real_ms=0
total_compile_cpu_pct=0
total_run_cpu_pct=0
peak_mem_kb=0
test_count=0

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1
AGC="$ROOT/target/debug/agc"

is_skipped() {
    while IFS= read -r line; do
        # Strip inline comments starting with '#'
        local clean="${line%%#*}"
        # Trim leading and trailing whitespace
        clean="${clean#"${clean%%[![:space:]]*}"}"
        clean="${clean%"${clean##*[![:space:]]}"}"
        if [ -n "$clean" ] && [ "$clean" = "$1" ]; then
            return 0
        fi
    done <<< "$SKIP_TESTS"
    return 1
}

# Format milliseconds as a short human string: "1.23s", "0.45s", "12.3s".
fmt_ms() {
    local ms=$1
    if [ "$ms" -ge 10000 ]; then
        awk "BEGIN { printf \"%.1fs\", $ms/1000 }"
    elif [ "$ms" -ge 1000 ]; then
        awk "BEGIN { printf \"%.2fs\", $ms/1000 }"
    else
        awk "BEGIN { printf \"%.3fs\", $ms/1000 }"
    fi
}

# Format KB to human string: "1.5MB", "128KB", etc.
fmt_mem() {
    local kb=$1
    if [ "$kb" -ge 1048576 ]; then
        awk "BEGIN { printf \"%.1fGB\", $kb/1048576 }"
    elif [ "$kb" -ge 1024 ]; then
        awk "BEGIN { printf \"%.1fMB\", $kb/1024 }"
    else
        printf "%dKB" "$kb"
    fi
}

# Run a command under \time, writing timing data to a file for later parsing.
# Usage: run_timed <step> <logfile> <cmd...>
# On return, global variables ${step}_real_ms, ${step}_cpu_pct, ${step}_mem_kb
# are set.
run_timed() {
    local step=$1
    local logfile=$2
    shift 2
    local rundir=""
    if [ "$1" = "-C" ]; then
        rundir="$2"
        shift 2
    fi
    local tfile
    tfile="$(mktemp)"
    # Run the timed command in a subshell only when a working directory is
    # requested; the timing file is parsed below in the caller's scope so
    # the measured variables survive.
    if [ -n "$rundir" ]; then
        (cd "$rundir" && \time -o "$tfile" -f "$TIME_FMT" "$@" >"$logfile" 2>&1)
    else
        \time -o "$tfile" -f "$TIME_FMT" "$@" >"$logfile" 2>&1
    fi
    local rc=$?
    # Parse fields: tab-separated key=value pairs from TIME_FMT.
    local line
    line="$(cat "$tfile")"
    rm -f "$tfile"
    local saved_ifs="$IFS"
    IFS=$'\t'
    for field in $line; do
        case "$field" in
            real=*)
                local val="${field#real=}"
                eval "${step}_real_ms=\$(awk 'BEGIN { printf \"%d\", $val * 1000 }')"
                ;;
            cpu=*)
                local val="${field#cpu=}"
                # Strip trailing % for arithmetic
                val="${val%\%}"
                eval "${step}_cpu_pct=\$val"
                ;;
            mem=*)
                local val="${field#mem=}"
                eval "${step}_mem_kb=\$val"
                ;;
        esac
    done
    IFS="$saved_ifs"
    # Ensure defaults if parsing failed.
    eval "[ -z \"\${${step}_real_ms+x}\" ] && ${step}_real_ms=0"
    eval "[ -z \"\${${step}_cpu_pct+x}\" ] && ${step}_cpu_pct=0"
    eval "[ -z \"\${${step}_mem_kb+x}\" ] && ${step}_mem_kb=0"
    return $rc
}

echo "== Building agc =="
if ! cargo build -p agc; then
    echo "error: failed to build agc" >&2
    exit 1
fi
if [ ! -x "$AGC" ]; then
    echo "error: agc binary not found at $AGC" >&2
    exit 1
fi

# Collect the tests to run.
METRICS_FILE="$ROOT/bench/metrics.tsv"
CURRENT_FILE="$ROOT/bench/current.tsv"
mkdir -p "$ROOT/bench"
printf '# run %s\n' "$(date +'%F %T')" > "$CURRENT_FILE"

tests=()
if [ "$#" -ge 1 ]; then
    name="${1##*/}"
    name="${name%.ag}"
    if [ ! -f "$ROOT/tests/$name.ag" ]; then
        echo "error: no such test: tests/$name.ag" >&2
        exit 1
    fi
    tests=("$ROOT/tests/$name.ag")
else
    for t in "$ROOT"/tests/*.ag; do
        tests+=("$t")
    done
fi

WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

# tls_test links and loads the OpenSSL shared libraries and needs node to
# serve HTTPS. Skip it (and its server) when either is unavailable.
SILVER_OPENSSL_LIB=""
if command -v openssl >/dev/null 2>&1; then
    # NixOS: the openssl package dir's lib (the -bin wrapper has no lib).
    for d in /nix/store/*openssl-*/lib; do
        if [ -f "$d/libssl.so" ] || [ -f "$d/libssl.so.3" ]; then
            SILVER_OPENSSL_LIB="$d"
            break
        fi
    done
    if [ -z "$SILVER_OPENSSL_LIB" ] && ldconfig -p 2>/dev/null | grep -q 'libssl\.so'; then
        SILVER_OPENSSL_LIB="$(ldconfig -p | awk '/libssl\.so/{print $NF; exit}' | xargs dirname)"
    fi
fi
if [ -z "$SILVER_OPENSSL_LIB" ] || ! command -v node >/dev/null 2>&1; then
    SKIP_TESTS="$SKIP_TESTS
tls_test
http2_tls_test"
fi
if ! command -v go >/dev/null 2>&1; then
    SKIP_TESTS="$SKIP_TESTS
http_perf_test"
fi
if ! command -v node >/dev/null 2>&1; then
    SKIP_TESTS="$SKIP_TESTS
http2_test
websocket_test
sse_test"
fi

# Start the node HTTPS server once, before the loop, if tls_test will run.
TLS_NODE_PID=""
if ! is_skipped tls_test; then
    (cd "$ROOT" && exec node tests/tls_server.js >"$WORKDIR/tls_node.log" 2>&1) &
    TLS_NODE_PID=$!
    for _ in $(seq 1 50); do
        if grep -q TLS_NODE_READY "$WORKDIR/tls_node.log" 2>/dev/null; then
            break
        fi
        sleep 0.1
    done
fi

# Start the node HTTP/2 server once, before the loop, if http2_test will run.
H2_NODE_PID=""
if ! is_skipped http2_test; then
    (cd "$ROOT" && exec node tests/h2_server.js >"$WORKDIR/h2_node.log" 2>&1) &
    H2_NODE_PID=$!
    for _ in $(seq 1 50); do
        if grep -q H2C_NODE_READY "$WORKDIR/h2_node.log" 2>/dev/null; then break; fi
        sleep 0.1
    done
fi

# Start the node WebSocket/SSE server once, before the loop, if websocket_test
# or sse_test will run.
WS_NODE_PID=""
if ! is_skipped websocket_test || ! is_skipped sse_test; then
    (cd "$ROOT" && exec node tests/ws_server.js >"$WORKDIR/ws_node.log" 2>&1) &
    WS_NODE_PID=$!
    for _ in $(seq 1 50); do
        if grep -q WS_NODE_READY "$WORKDIR/ws_node.log" 2>/dev/null; then break; fi
        sleep 0.1
    done
fi

# Start the loopback HTTP perf server once, before the loop, if http_perf_test
# will run. The Go server is ready when it accepts connections (checked by
# the perf test itself via the warmup request); give it a moment to bind.
PERF_SERVER_PID=""
if ! is_skipped http_perf_test; then
    CGO_ENABLED=0 go build -o "$WORKDIR/perf_server" "$ROOT/tests/perf/http_server.go" || true
    (cd "$ROOT" && exec "$WORKDIR/perf_server" >"$WORKDIR/perf_server.log" 2>&1) &
    PERF_SERVER_PID=$!
    for _ in $(seq 1 50); do
        if grep -q PERF_SERVER_READY "$WORKDIR/perf_server.log" 2>/dev/null; then break; fi
        sleep 0.1
    done
fi

# module_import_test consumes a precompiled module: emit module_lib.agm +
# module_lib.o into a dedicated directory first, then the consumer resolves
# `import module_lib;` via -I and auto-links the sibling object.
MODLIB_DIR="$WORKDIR/modlib"
if ! is_skipped module_import_test; then
    mkdir -p "$MODLIB_DIR"
    if ! (cd "$MODLIB_DIR" && "$AGC" --emit=module "$ROOT/tests/modules/module_lib.ag" >"$WORKDIR/modlib.emit.log" 2>&1); then
        echo "error: failed to emit module for module_import_test" >&2
        cat "$WORKDIR/modlib.emit.log" >&2
        exit 1
    fi
    if [ ! -f "$MODLIB_DIR/module_lib.agm" ] || [ ! -f "$MODLIB_DIR/module_lib.o" ]; then
        echo "error: module emit did not produce module_lib.agm/o" >&2
        exit 1
    fi
fi

passed=0
failed=0
skipped=0
failed_names=""

echo "== Running integration tests =="
# Print table header.
printf '%-*s  %*s  %*s  %*s  %*s\n' \
    "$COL_NAME" "TEST" \
    "$COL_TIME" "COMPILE" \
    "$COL_TIME" "RUN" \
    "$COL_CPU" "CPU" \
    "$COL_MEM" "MEM"
printf '%-*s  %*s  %*s  %*s  %*s\n' \
    "$COL_NAME" "----" \
    "$COL_TIME" "-------" \
    "$COL_TIME" "---" \
    "$COL_CPU" "---" \
    "$COL_MEM" "----"
for t in "${tests[@]}"; do
    name="${t##*/}"
    name="${name%.ag}"

    if is_skipped "$name"; then
        printf '  SKIP  %-*s\n' "$COL_NAME" "$name"
        skipped=$((skipped + 1))
        continue
    fi

    bin="$WORKDIR/$name"
    compile_log="$WORKDIR/$name.compile.log"
    run_log="$WORKDIR/$name.run.log"

    # ------- Compile step -------
    extra_flags="$(test_specific_flags "$name")"
    if leak_check_enabled "$name"; then
        extra_flags="$extra_flags --leak-check"
    fi
    compile_real_ms=0; compile_cpu_pct=0; compile_mem_kb=0
    # shellcheck disable=SC2086
    if ! run_timed compile "$compile_log" "$AGC" "$t" -o "$bin" \
        --cfg "cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1" $extra_flags; then
        if expected_compile_failure "$name"; then
            printf '  PASS  %-*s  (expected compile error)\n' "$COL_NAME" "$name"
            passed=$((passed + 1))
        else
            printf '  FAIL  %-*s  (compile error)\n' "$COL_NAME" "$name"
            sed 's/^/    /' "$compile_log"
            failed=$((failed + 1))
            failed_names="$failed_names $name"
        fi
        continue
    fi
    # Fail if a test expected to fail at compile-time unexpectedly compiled
    if expected_compile_failure "$name"; then
        printf '  FAIL  %-*s  (unexpectedly compiled)\n' "$COL_NAME" "$name"
        failed=$((failed + 1))
        failed_names="$failed_names $name"
        continue
    fi

# Some tests require stdin input. If this function returns 0 for a test name,
# its stdout is piped into the test binary as stdin.
test_stdin() {
    case "$1" in
        scanner_test) printf '\357\273\2773\n10 -20 caf\303\251\nlast line\n' ;;
        scanner_wide_test) printf -- '-5 -300 70000 -9000000000 170141183460469231731687303715884105727 255 65000 4000000000 18446744073709551615 340282366920938463463374607431768211455 300 -40000 256 -1 3.5 -2.25 1e3 true false hello\n' ;;
        *) return 1 ;;
    esac
}

    # ------- Run step -------
    run_dir="$WORKDIR/$name.rundir"
    mkdir -p "$run_dir"
    # tls/http2-tls tests read the committed certs via relative paths.
    if [ "$name" = "tls_test" ] || [ "$name" = "http2_tls_test" ]; then
        run_dir="$ROOT"
    fi
    run_real_ms=0; run_cpu_pct=0; run_mem_kb=0
    if test_stdin "$name" > /dev/null 2>&1; then
        run_timed run "$run_log" -C "$run_dir" timeout "$RUN_TIMEOUT_SECS" "$bin" < <(test_stdin "$name")
    else
        # Feed EOF stdin so tests that read stdin (e.g. Scanner.stdin())
        # cannot block forever on an interactive terminal.
        run_timed run "$run_log" -C "$run_dir" timeout "$RUN_TIMEOUT_SECS" "$bin" < /dev/null
    fi
    exit_code=$?

    # ------- Post-run checks -------
    status="PASS"

    # For static_link_test, verify the binary is truly static.
    if [ "$name" = "static_link_test" ]; then
        if ldd "$bin" 2>&1 | head -1 | grep -q "not a dynamic executable"; then
            : # good
        else
            printf '  FAIL  %-*s  (binary is not static)\n' "$COL_NAME" "$name"
            failed=$((failed + 1))
            failed_names="$failed_names $name"
            continue
        fi
    fi

    want="$(expected_exit "$name")"
    if [ "$exit_code" -ne "$want" ]; then
        if [ "$exit_code" -eq 124 ]; then
            printf '  FAIL  %-*s  (timed out after %ss)\n' "$COL_NAME" "$name" "$RUN_TIMEOUT_SECS"
        else
            printf '  FAIL  %-*s  (exit %s, expected %s)\n' "$COL_NAME" "$name" "$exit_code" "$want"
        fi
        sed 's/^/    /' "$run_log"
        failed=$((failed + 1))
        failed_names="$failed_names $name"
        continue
    fi

    # ------- Display metrics -------
    ctime="$(fmt_ms "$compile_real_ms")"
    rtime="$(fmt_ms "$run_real_ms")"
    # Average CPU across both phases (weighted by real time).
    total_real=$(( compile_real_ms + run_real_ms ))
    avg_cpu=0
    if [ "$total_real" -gt 0 ]; then
        avg_cpu=$(( (compile_real_ms * compile_cpu_pct + run_real_ms * run_cpu_pct) / total_real ))
    fi
    peak_mem=$(( compile_mem_kb > run_mem_kb ? compile_mem_kb : run_mem_kb ))
    mem_str="$(fmt_mem "$peak_mem")"

    # Metrics: append a TSV row (history + current-run file) and track the
    # slowest run times.
    printf '%s\t%s\t%s\t%s\n' "$name" "$compile_real_ms" "$run_real_ms" "$peak_mem" >> "$METRICS_FILE"
    printf '%s\t%s\t%s\t%s\n' "$name" "$compile_real_ms" "$run_real_ms" "$peak_mem" >> "$CURRENT_FILE"
    if [ "$run_real_ms" -gt 0 ]; then
        slot=0
        while [ "$slot" -lt 5 ]; do
            if [ -z "${slow_run_ms[$slot]:-}" ] || [ "$run_real_ms" -gt "${slow_run_ms[$slot]}" ]; then
                # Shift the tail down.
                tail=4
                while [ "$tail" -gt "$slot" ]; do
                    slow_run_ms[$tail]="${slow_run_ms[$((tail - 1))]:-}"
                    slow_name[$tail]="${slow_name[$((tail - 1))]:-}"
                    tail=$((tail - 1))
                done
                slow_run_ms[$slot]="$run_real_ms"
                slow_name[$slot]="$name"
                break
            fi
            slot=$((slot + 1))
        done
    fi

    printf '  PASS  %-*s  %*s  %*s  %3d%%  %*s\n' \
        "$COL_NAME" "$name" \
        "$COL_TIME" "$ctime" \
        "$COL_TIME" "$rtime" \
        "$avg_cpu" \
        "$COL_MEM" "$mem_str"
    passed=$((passed + 1))

    # ------- Accumulate -------
    total_compile_real_ms=$(( total_compile_real_ms + compile_real_ms ))
    total_run_real_ms=$(( total_run_real_ms + run_real_ms ))
    total_compile_cpu_pct=$(( total_compile_cpu_pct + compile_cpu_pct ))
    total_run_cpu_pct=$(( total_run_cpu_pct + run_cpu_pct ))
    if [ "$peak_mem" -gt "$peak_mem_kb" ]; then
        peak_mem_kb=$peak_mem
    fi
    test_count=$(( test_count + 1 ))
done

if [ -n "$TLS_NODE_PID" ]; then
    kill "$TLS_NODE_PID" 2>/dev/null
    wait "$TLS_NODE_PID" 2>/dev/null
fi
if [ -n "$PERF_SERVER_PID" ]; then
    kill "$PERF_SERVER_PID" 2>/dev/null
    wait "$PERF_SERVER_PID" 2>/dev/null
fi
if [ -n "$H2_NODE_PID" ]; then
    kill "$H2_NODE_PID" 2>/dev/null
    wait "$H2_NODE_PID" 2>/dev/null
fi
if [ -n "$WS_NODE_PID" ]; then
    kill "$WS_NODE_PID" 2>/dev/null
    wait "$WS_NODE_PID" 2>/dev/null
fi

# ---- Slowest tests (run time) ----
echo
echo "== Slowest tests =="
slot=0
while [ "$slot" -lt 5 ]; do
    if [ -n "${slow_name[$slot]:-}" ]; then
        printf '  %-24s %6s ms\n' "${slow_name[$slot]}" "${slow_run_ms[$slot]}"
    fi
    slot=$((slot + 1))
done
if [ -z "${slow_name[0]:-}" ]; then
    echo "  (no timed runs)"
fi

# ---- Before/after comparison (--compare FILE) ----
if [ -n "$COMPARE_BASELINE" ]; then
    echo
    echo "== Before/after (run time, ms) =="
    awk -F'\t' '
        NR == FNR {
            if ($1 ~ /^#/ || $1 == "name") next
            base[$1] = $3 + 0
            next
        }
        {
            if ($1 ~ /^#/ || $1 == "name") next
            cur = $3 + 0
            b = base[$1] + 0
            delta = cur - b
            rows[++n] = sprintf("%s\t%d\t%d\t%+d", $1, b, cur, delta)
        }
        END {
            rows[0] = "TEST\tBEFORE\tAFTER\tDELTA"
            for (i = 0; i <= n; i++) print rows[i]
        }
    ' "$COMPARE_BASELINE" "$CURRENT_FILE" | sort -t"$(printf '\t')" -k4 -n
fi

# ---- Summary ----
echo
echo "== Summary =="
echo "passed:   $passed"
echo "failed:   $failed"
echo "skipped:  $skipped"

if [ "$test_count" -gt 0 ]; then
    total_real_ms=$(( total_compile_real_ms + total_run_real_ms ))
    avg_cpu_all=$(( (total_compile_cpu_pct + total_run_cpu_pct) / (test_count * 2) ))
    echo ""
    printf "  Total compile time:  %s\n" "$(fmt_ms "$total_compile_real_ms")"
    printf "  Total run time:      %s\n" "$(fmt_ms "$total_run_real_ms")"
    printf "  Total wall time:     %s\n" "$(fmt_ms "$total_real_ms")"
    printf "  Average CPU:         %d%%\n" "$avg_cpu_all"
    printf "  Peak memory:         %s\n" "$(fmt_mem "$peak_mem_kb")"
fi

if [ "$failed" -gt 0 ]; then
    echo ""
    echo "failing tests:$failed_names"
    exit 1
fi
exit 0
