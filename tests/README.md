# Silver tests

Run from the repository root inside `nix develop`:

```sh
cargo test -p agc
python3 -m unittest discover -s tests -p 'test_runner.py'
python3 tests/run_tests.py --release --no-tui --jobs 4
python3 tests/run_tests.py --release --no-tui memory_pentest cascade_drop_test
bash tests/selfhost/run_native.sh --release --no-tui --jobs 4
```

The integration runner discovers `tests/*.ag`, compiles each program, and checks
its exit status. Name filters select a subset. `--compiler /path/to/agc` uses an
existing stage1 compiler. `--timeout N` bounds execution; `--compile-timeout N`
bounds compiler processes, allowing more time for large fixtures. The runner
reports compilation and execution times separately.

`--release` selects Cargo's release profile for the compiler. It does not
optimize each test program. Fixture flags come from `// compile_flags: ...`
comments and the few platform-specific settings in `run_tests.py`.

## Keep cases focused

A case needs an observable result that fails for a plausible defect.
Check values, diagnostics, ownership effects, or output artifacts. An empty
program, an unconditional success, and printing a measurement are insufficient.

Keep a regression's smallest reproducer when it exercises a distinct compiler
path. Put shared library behavior in its module's existing fixture. Extend
`algo_sort_test.ag`, `bytes_test.ag`, or `collections_heap_test.ag` rather than
adding another broad modernization checklist. Unit tests for finite rule sets
should use explicit input/expected-result tables instead of repeated setup.

Most fixtures use `std.test` and return `done()`, or return a nonzero status
directly on a mismatch. Both `@assert` and `std.test` checks depend on debug cfg.
A fixture compiled with `-O2` must use explicit failure returns or explicitly
enable debug cfg when release cfg itself is not being tested.

## Negative cases and special exits

Declare the intended diagnostic in the fixture:

```silver
// error_contains: returned reference does not outlive
i64* escape() {
    i64 local = 5;
    return &local;
}
i32 main() { return 0; }
```

Repeat `error_contains` for independently required diagnostics. All substrings
must occur on compiler error lines. A crash, timeout, unrelated error, or
successful compile fails the case. Avoid matching incidental formatting or
source positions.

A runtime fixture can declare `// expected_exit: 42`. Existing syscall,
backtrace, and static-storage fixtures also have expected statuses in the
runner. Target-specific skip dictionaries explain unsupported cases.

The optional Rust bridge fixture needs a native artifact:

```sh
cargo build -p silver-ffi
SILVER_FFI_LIBRARY_DIR="$PWD/target/debug" python3 tests/run_tests.py rust_ffi_test
```

## Performance workloads

Timed workloads live in `tests/perf/` and are excluded from correctness discovery:

```sh
bash tests/perf/run_perf.sh silver
target/release/agc tests/perf/memcpy_bench.ag -o /tmp/silver-memcpy-bench
/tmp/silver-memcpy-bench
target/release/agc tests/perf/mem_growth_watch.ag -o /tmp/silver-mem-growth
/tmp/silver-mem-growth
```

The HTTP comparison needs Go and a built `target/debug/agc`. The memory-growth
watch intentionally allocates about 1 GiB and runs for about 30 seconds.
Correctness coverage for memory intrinsics stays in
`adversarial_mem_intrinsics_test.ag` and `mem_test.ag`; HTTP buffering, streaming,
and pooling stay in `http_test.ag`, `stream_test.ag`, and `pool_test.ag`.
