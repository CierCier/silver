# Silver Integration Tests

This directory contains integration tests for the Silver compiler (`agc`) and
the standard library. Each `tests/*.ag` file is a standalone program that is
compiled and executed by the test harness.

## Test Files

| File | Description |
|------|-------------|
| `align_test.ag` | Struct layout and alignment checks |
| `box_test.ag` | Tests for `std.mem` Box smart pointer |
| `cascade_drop_test.ag` | Nested/cascading `Drop` behavior |
| `defer_test.ag` | `defer` block execution order |
| `file_test.ag` | Tests for `std.file` (File handle, read/write, seek, utilities) |
| `io_test.ag` | Tests for `std.io` (printf, puts, putchar, etc.) |
| `map_test.ag` | Tests for `std.map` HashMap |
| `mem_test.ag` | Tests for `std.mem` (alloc, free, memset, memcpy, memmove, realloc) |
| `memory_pentest.ag` | Definitive RAII/move/drop-flag regression suite |
| `memory_stress.ag` | Allocation-heavy stress test |
| `optional_test.ag` | Tests for `std.optional` (Optional and Result types) |
| `rc_test.ag` | Tests for `std.mem` Rc smart pointer |
| `str_eq_test.ag` | String equality semantics |
| `str_key_map_test.ag` | HashMap with string keys |
| `string_test.ag` | Tests for `std.string` (String object with dynamic memory) |
| `syscall_test.ag` | Raw inline-asm syscall (`sys_exit(42)`) |
| `syscall_wrapper_test.ag` | `std.sys.syscall` wrappers (`sys_exit(42)`) |
| `vec_test.ag` | Tests for `std.vec` (Vec_i32, Vec_i64, Vec_f64, Vec_ptr) |

## Running Tests

The harness is `tests/run_tests.sh`. It builds `agc` (debug profile), then
compiles every `tests/*.ag` into a temporary directory and executes it,
printing a `PASS`/`FAIL` line per test and a final summary. It exits nonzero
if any test fails.

```bash
# Run all integration tests (from the repo root, or anywhere)
bash tests/run_tests.sh

# Run a single test by name (the .ag suffix is optional)
bash tests/run_tests.sh vec_test
```

A test fails when it does not compile, or when the produced binary exits with
an unexpected status. Most tests are expected to exit `0`; per-test expected
exit codes are declared in `expected_exit` at the top of `run_tests.sh`
(e.g. the syscall tests intentionally exit `42` via `sys_exit`). Tests that
must be skipped entirely can be listed in `SKIP_TESTS`, with a comment
explaining why; the list is currently empty.

Compiler unit tests live in the `agc` crate and are run separately:

```bash
cargo test -p agc
```

Both suites run in CI on every push and pull request
(`.github/workflows/ci.yml`).

## Test Structure

Most tests use the shared harness `std.test` (`std/test.ag`). Import it instead
of writing ad-hoc assertion helpers:

```silver
import std.test;
// Import module being tested

i32 main() {
    test_start("My Tests");
    assert_true(1 + 1 == 2, "basic arithmetic");
    assert_eq_i64(42, compute_answer(), "answer check");
    return done();  // returns number of failed tests
}
```

The full assertion suite: `assert_true`, `assert_false`, `check`, `assert_eq_i64`,
`assert_eq_i32`, `assert_eq_str`. See `std/test.ag` for details.

Some older tests use a local `printf`-based pattern and do not import `std.test`.
This is legacy — new tests should use `std.test`.

By default, tests return exit code 0 on success and 1 on failure. However, some
special test cases (like syscall tests) may intentionally exit with a different
status if configured in the harness's `expected_exit` function.

## Adding New Tests

1. Create a new `.ag` file in this directory following the pattern above.
2. The harness discovers `tests/*.ag` automatically; no registration needed.
3. If the test intentionally exits with a nonzero status, add its expected
   exit code to `expected_exit` in `run_tests.sh` with a comment.
4. Run `bash tests/run_tests.sh <name>` to verify it passes.
