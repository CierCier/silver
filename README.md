# Silver

Silver is a statically typed systems programming language with an LLVM backend,
automatic resource cleanup, ownership and borrow checks, OS threads, and a
freestanding runtime. The Rust stage0 compiler bootstraps the Silver stage1
compiler. Their supported commands and checking coverage differ; see the
[compiler reference](docs/compiler-guide.md).

## Example

```silver
import std.io;

struct Point {
    f64 x;
    f64 y;
}

i64 compute(i64 val) {
    return val * 2;
}

i32 main() {
    Point p = { .x = 10.0, .y = 20.0 };
    @println("Point: ({}, {})", p.x, p.y);

    Task<i64> task = launch compute(21);
    i64 answer = wait task;
    @println("Computed answer: {}", answer);

    return 0;
}
```

## Features

- **Resource management**: Automatic drop cleanup on scope exit via compiler-managed drop flags. Owned values transfer implicitly or with `move`; see the [ownership rules](SYNTAX.md#8-memory-management--ownership-model) and [container cleanup tests](tests/collection_drop_glue_test.ag).
- **Borrowing**: References (`&T`, `&mut T`) carry borrow-origin checks. Raw pointers (`T*`) remain available for manual control and FFI. [Iterator tests](tests/iterator_ergonomics_test.ag) exercise borrowing through stdlib APIs; complete typed control-flow analysis remains open.
- **Concurrency**: OS tasks use `launch` and explicit `wait`, with a Send check on arguments. A dropped task handle does not join the task. See [task semantics](SYNTAX.md#9-concurrency--threading) and [guard tests](tests/guard_test.ag).
- **Freestanding runtime**: Pure-Silver runtime (`std/rt/`) with no required C standard library dependency.
- **Tooling and diagnostics**: Stage0 provides DWARF debug information and allocation leak tracking. Stage1 builds the [AGLSP language server](bin/aglsp); its native option limits are listed in the [compiler guide](docs/compiler-guide.md#native-options).

The [syntax reference](SYNTAX.md) documents tuple values and destructuring,
slices, selective imports, receiver shorthand, and the Optional/Result APIs,
with links to their implementations and runtime fixtures.

## Installation

### Nix

Nix provides reproducible builds with LLVM 22, the compiler, and standard library sysroots:

```bash
# Install to profile
nix profile install github:CierCier/silver

# Or run directly without installing
nix run github:CierCier/silver -- run path/to/file.ag

# Enter a development shell
nix develop
```

### Prebuilt binaries

Download prebuilt archives from the [Releases](https://github.com/CierCier/silver/releases) page. Extract the archive and add the binary directory to your `PATH`.

Prebuilt binaries require LLVM 22 runtime libraries (`libLLVM-22` or `libclang`) and a system linker (`cc`, `clang`, or `ld.lld`).

### Building from source

Requirements:
- Rust toolchain supporting edition 2024
- LLVM 22 development headers and libraries (`inkwell` / `llvm-sys`)
- System C toolchain and linker (`cc`, `clang`, or `ld.lld`)

```bash
git clone https://github.com/CierCier/silver.git
cd silver

# Build compiler in release mode
cargo build --release -p agc

# Or install to ~/.local with
./scripts/install.sh
```

## Usage

### Compile and run

```bash
# Compile to an executable
agc path/to/file.ag -o out

# Compile and run, forwarding program arguments after --
agc run path/to/file.ag -- input.txt

# Frontend type checking only
agc check path/to/file.ag

# Stage0: run with allocation leak tracking
agc run path/to/file.ag --leak-check
```

### Modules and packaging

```bash
# Emit a precompiled module (.agm)
agc path/to/file.ag --emit=module -o path/to/file.agm

# Stage0: emit a shared packaged module
agc path/to/file.ag --emit=module --shared -o path/to/file.agm
```

### Native targets

The Silver stage1 compiler builds native programs on x86_64 Linux using LLVM's
default host target. An explicit `--target TRIPLE` must match that triple
exactly. Unsupported native targets fail before source compilation or output
creation. Source and workspace `check` commands can use another target's cfg
values; `lex`, `parse`, and `ast` accept `--target` but dump the unfiltered
source. Neither frontend check nor dump establishes native support for that
target. The Rust stage0 compiler retains its separate cross-compilation paths.

See the [compiler target contract](docs/compiler-guide.md#native-target-contract)
for the scope of this restriction.

### Running tests

```bash
# Build bootstrap tools and the optional Rust FFI bridge
cargo build --workspace

# Run standard-library behavior tests
python3 tests/run_tests.py --no-tui --jobs 4

# Build the Silver compiler and language server
target/debug/agc build silver.toml --bin agc --no-cache -o /tmp/agc-stage1
target/debug/agc build silver.toml --bin aglsp --no-cache -o /tmp/aglsp-stage1

# Run the same stdlib suite with the native Silver compiler
python3 tests/run_tests.py --compiler /tmp/agc-stage1 --no-tui --jobs 4
```

The suite contains 114 stdlib programs and is capped at 124. Extend an existing
module test for new behavior. Compiler builds and stdlib use exercise language
features; separate compiler unit, parity, and self-host test suites are retired.

## Repository layout

- `bootstrap/stage0/agc/`: Rust bootstrap compiler (stage0): driver, frontend, and LLVM backend, using [Elise](https://github.com/CierCier/elise) for parsing
- `bootstrap/stage0/agsm/`: Source maps and module artifact generator
- `bootstrap/stage0/ffi-rust/`: Optional Rust implementation behind Silver's versioned C ABI
- `bootstrap/aglsp/`: Rust Language Server Protocol server
- `bin/agc/`: Stage1 self-hosting compiler driver in Silver; see the [compiler guide](docs/compiler-guide.md)
- `bin/aglsp/`: Silver language-server binary package
- `bin/agc/src/agsm.ag`: AGSM command integrated into the stage1 compiler driver
- `libs/agc/`: Reusable stage1 compiler frontend library (Silver)
- `silver.toml`: Silver workspace manifest for the self-host root
- `std/`: Standard library (allocators, collections, I/O, networking, runtime)
- `examples/`: Sample programs
- `tests/`: Stdlib behavior tests and their runner
- `docs/`: Language specifications and standard protocol designs

## Documentation

- [Syntax Specification](SYNTAX.md)
- [Agent Working Guide](AGENTS.md)
- [Contributing](CONTRIBUTING.md)
- [Compiler Architecture Reference](docs/compiler-guide.md)
- [C ABI and FFI](docs/rust-ffi.md)
- [Standard Library Protocols](docs/standards/README.md)

## License

MIT
