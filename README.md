# Silver

Silver is a statically typed systems programming language with an LLVM backend. It provides deterministic resource management, compile-time borrow checks, structured concurrency, and a freestanding runtime.

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

- **Resource management**: Automatic drop cleanup on scope exit via compiler-managed drop flags. Ownership transfers explicitly with `move`.
- **Borrowing**: References (`&T`, `&mut T`) cannot escape their stack frames. Raw pointers (`T*`) remain available for manual control and FFI.
- **Concurrency**: Structured task spawning with `launch` and `wait`. The compiler enforces that non-thread-safe types cannot cross task boundaries.
- **Freestanding runtime**: Pure-Silver runtime (`std/rt/`) with no required C standard library dependency.
- **Tooling and diagnostics**: DWARF debug information by default, built-in allocation leak tracking (`--leak-check`), crash backtraces, and Language Server Protocol (`aglsp`) support.

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
- Rust toolchain with Cargo 1.75+
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

# Compile and run in one step
agc run path/to/file.ag [args...]

# Frontend type checking only
agc check path/to/file.ag

# Run with allocation leak tracking
agc --leak-check run path/to/file.ag
```

### Modules and packaging

```bash
# Emit a precompiled module (.agm)
agc path/to/file.ag --emit=module -o path/to/file.agm

# Emit a shared packaged module
agc path/to/file.ag --emit=module --shared -o path/to/file.agm
```

### Running tests

```bash
# Run compiler unit and parser parity tests
cargo test -p agc

# Run integration test suite
python3 tests/run_tests.py --no-tui

# Build stage1 with stage0 and run frontend parity gates
tests/selfhost/run_stage.sh --include-std

# Exercise the full Linux command surface through stage1's compatibility seam
bash tests/selfhost/run_native.sh --no-tui --jobs 4
```

## Repository layout

- `bootstrap/stage0/agc/`: Rust bootstrap compiler (stage0): driver, frontend, and LLVM backend, using [Elise](https://github.com/CierCier/elise) for parsing
- `bootstrap/stage0/agsm/`: Source maps and module artifact generator
- `bootstrap/stage0/ffi-rust/`: Optional Rust implementation behind Silver's versioned C ABI
- `bootstrap/aglsp/`: Rust Language Server Protocol server
- `bin/agc/`: Stage1 self-hosting compiler driver (Silver); see `docs/selfhost-plan.txt`
- `bin/aglsp/`: Silver language-server binary package and future LSP port target
- `libs/agc/`: Reusable stage1 compiler frontend library (Silver)
- `silver.toml`: Silver workspace manifest for the self-host root
- `std/`: Standard library (allocators, collections, I/O, networking, runtime)
- `examples/`: Sample programs
- `tests/`: Unit, ownership, and integration test suites
- `docs/`: Language specifications and standard protocol designs

## Documentation

- [Syntax Specification](SYNTAX.md)
- [Compiler Architecture Guide](AGENTS.md)
- [C ABI and FFI](docs/rust-ffi.md)
- [Standard Library Protocols](docs/standards/README.md)

## License

MIT
