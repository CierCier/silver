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

# Kill runaway test binaries after this many seconds.
RUN_TIMEOUT_SECS=120
# ---------------------------------------------------------------------------

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT" || exit 1
AGC="$ROOT/target/debug/agc"

is_skipped() {
    for skip in $SKIP_TESTS; do
        if [ "$skip" = "$1" ]; then
            return 0
        fi
    done
    return 1
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
for t in "${tests[@]}"; do
    name="${t##*/}"
    name="${name%.ag}"

    if is_skipped "$name"; then
        printf 'SKIP %s\n' "$name"
        skipped=$((skipped + 1))
        continue
    fi

    bin="$WORKDIR/$name"
    compile_log="$WORKDIR/$name.compile.log"
    run_log="$WORKDIR/$name.run.log"

    # Compile from the repo root: agc resolves `import std.*` relative to
    # the current working directory (the package root).
    if ! "$AGC" "$t" -o "$bin" >"$compile_log" 2>&1; then
        printf 'FAIL %s (compile error)\n' "$name"
        sed 's/^/    /' "$compile_log"
        failed=$((failed + 1))
        failed_names="$failed_names $name"
        continue
    fi

    # Run each test in its own scratch directory so tests that create
    # files (e.g. file_test) cannot litter the repository.
    run_dir="$WORKDIR/$name.rundir"
    mkdir -p "$run_dir"
    (cd "$run_dir" && timeout "$RUN_TIMEOUT_SECS" "$bin") >"$run_log" 2>&1
    exit_code=$?

    want="$(expected_exit "$name")"
    if [ "$exit_code" -eq "$want" ]; then
        printf 'PASS %s\n' "$name"
        passed=$((passed + 1))
    else
        if [ "$exit_code" -eq 124 ]; then
            printf 'FAIL %s (timed out after %ss)\n' "$name" "$RUN_TIMEOUT_SECS"
        else
            printf 'FAIL %s (exit %s, expected %s)\n' "$name" "$exit_code" "$want"
        fi
        sed 's/^/    /' "$run_log"
        failed=$((failed + 1))
        failed_names="$failed_names $name"
    fi
done

echo
echo "== Summary =="
echo "passed:  $passed"
echo "failed:  $failed"
echo "skipped: $skipped"
if [ "$failed" -gt 0 ]; then
    echo "failing tests:$failed_names"
    exit 1
fi
exit 0
