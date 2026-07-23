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
#
# Dependencies: bash, coreutils, cargo (plus the toolchain agc itself needs).
set -u

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
        *) echo 0 ;;
    esac
}

# Some tests require specific compiler flags. Return extra flags for a test
# name; defaults to empty.
test_specific_flags() {
    case "$1" in
        static_link_test) echo "--static-runtime" ;;
        *) echo "" ;;
    esac
}


# Some tests are expected to fail at compile time (e.g., type errors).
# Return 0 (success) if compilation failure is the expected outcome.
expected_compile_failure() {
    case "$1" in
        enum_arity_error_test) return 0 ;;
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
    local tfile
    tfile="$(mktemp)"
    \time -o "$tfile" -f "$TIME_FMT" "$@" >"$logfile" 2>&1
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
    compile_real_ms=0; compile_cpu_pct=0; compile_mem_kb=0
    # shellcheck disable=SC2086
    if ! run_timed compile "$compile_log" "$AGC" "$t" -o "$bin" $extra_flags; then
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

    # ------- Run step -------
    run_dir="$WORKDIR/$name.rundir"
    mkdir -p "$run_dir"
    run_real_ms=0; run_cpu_pct=0; run_mem_kb=0
    (cd "$run_dir" && run_timed run "$run_log" timeout "$RUN_TIMEOUT_SECS" "$bin")
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
