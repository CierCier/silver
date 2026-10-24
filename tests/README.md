# Standard-library tests

The suite contains 114 runnable Silver programs and is capped at 124. Tests
exercise stdlib APIs, observable results, failure behavior, and resource cleanup.
Language features are exercised by the library and compiler builds. Dedicated
compiler unit, parser/parity, and self-host test suites are retired.

Run from the repository root inside `nix develop`:

```sh
cargo build --workspace
python3 tests/run_tests.py --no-tui --jobs 4
target/debug/agc build silver.toml --bin agc --no-cache -o /tmp/agc-stage1
target/debug/agc build silver.toml --bin aglsp --no-cache -o /tmp/aglsp-stage1
python3 tests/run_tests.py --compiler /tmp/agc-stage1 --no-tui --jobs 4
```

The runner discovers `tests/*.ag`, compiles each program, and checks its exit
status. Name filters select a subset. `--compiler` uses an existing compiler;
`--release` selects the release Cargo compiler without disabling test-program
assertions. `--timeout` and `--compile-timeout` bound execution and compilation.
Compilation and execution times are reported separately.

Use `--verbose` to print separate compile/run times on each result line. Timeout
failures retain elapsed time, the command, and partial stdout/stderr; their time
counts toward the summary. Redirect output to a local log when investigating.
A compile timeout is a wall-clock budget failure, not proof of a source-size
limit or a compiler hang. Large HTTP fixtures may exceed the default 300 seconds
under parallel load even when an isolated compile succeeds. Preserve the initial
failure, then retry affected fixtures with `--jobs 1` and unchanged limits before
attributing the result to compiler behavior. A passing subset retry does not
replace the full-suite gate.

For native compiler phase timings, compile the affected fixture directly with
`--profile --verbose`, preserving its flags and recording the compiler hash.
Use `--no-cache` to measure actual code generation rather than object-cache
reuse, and record it as a distinct diagnostic run. Verbose profiles separate
IR generation, verification, optimization, and object emission; all-job totals
can exceed wall time because jobs overlap. Also record the exact optimization
flag: native stage1's omitted `-O` uses LLVM's `Default` machine-codegen level,
whereas explicit `-O0` uses `None` (see the [compiler guide](../docs/compiler-guide.md)).

Extend an existing module fixture instead of adding another test program for
each regression. Check values, bytes, error variants, or cleanup. Printing
success, an unconditional assertion, or compiling alone does not prove an API
contract. Runtime failures must produce a nonzero exit status.

Bounds, enum tags, and indirect C callbacks are covered by `process_test`,
`slice_syntax_test`, `display_test`, `collection_drop_glue_test`, and
`rust_ffi_test`. Run the changed fixtures with both compiler stages and explicit
`-O0`/`-O2`; keep `--no-cache` when verifying changed code generation. New bounds
and callback checks retain explicit failure exits when assertions are disabled.
Stage0 supports wider unit-enum tags than native stage1; temporary boundary
probes must distinguish stage-specific expected rejections from shared behavior.

Most fixtures use `std.test` and return `done()`, or use `@assert` and explicit
failure returns. Assertions depend on debug cfg, so do not optimize test
programs in a way that removes their checks.

Allocator and unwrap failures use separate processes and declare
`// expected_exit: 134`. The syscall wrapper fixture expects exit 42.
Fixture options use `// compile_flags: ...` comments. There is no compiler
diagnostic-fixture mode.

The same suite runs against stage0 and native stage1. `--target wasm32-wasip1`
uses the Node WASI runner. Platform skip dictionaries describe unavailable
stdlib facilities, including Linux sockets, threads, terminals, and DMA.

TLS, HTTP/2 TLS, WebSocket, and SSE cases use the Node service helpers and
certificates in this directory. The optional Rust FFI case needs
`cargo build -p silver-ffi`; `cargo build --workspace` already supplies it.
Missing optional tools or platform support are reported as skips.
