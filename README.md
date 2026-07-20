# Silver — A Systems Programming Language Experiment

Silver is an open-source, statically typed systems programming language built on LLVM.
It explores a pragmatic design space between C-level control and modern ergonomics:
deterministic destruction, zero-cost iterators, first-class RAII, and a module system
with packaged compilation. The compiler (`agc`) implements parsing, semantic analysis,
type checking, and LLVM-based code generation, all from a single self-hosted pipeline.

The project currently includes:
- `agc`, the Silver compiler
- `silver_runtime`, the runtime crate
- `std`, the standard library sources used for bootstrapping
- `vendor`, imports/includes for third-party libraries
- `bootstrap`, generated compiler and standard library artifacts for local development

Silver is still under active development. Expect sharp edges, incomplete features, and regular iteration on syntax, semantics, and tooling.

## Features

- **Mutable by default, `const` for immutability** — variables are mutable unless declared
  `const`. Constness propagates through pointers: `&x` on a `const` variable yields an
  immutable pointer. The compiler enforces immutability in assignments, field writes, and
  pointer dereferences.
- **LLVM code generation** — compiles to native code via LLVM with optimizations,
  debug info, and target-specific back ends.
- **Deterministic destruction** — RAII-style resource management with `drop` trait and
  guaranteed cleanup at scope exit.
- **Zero-cost iterators** — `for x in iter` compiles to explicit state-machine loops
  with no hidden allocation or dispatch.
- **Module system** — packaged `.agm` artifacts with metadata for dependency resolution;
  supports both source imports and precompiled module imports.
- **Bootstrap pipeline** — the compiler compiles itself and the standard library through
  a cached bootstrap cycle.
- **Memory safety primitives** — `Box<T>`, `Vec<T>`, slices, and a borrow-checker-light
  ownership model, all testable via a memory-pentest suite.

## Repository Layout

- `agc/` - compiler sources
- `silver_runtime/` - runtime crate
- `std/` - standard library sources
- `vendor/` - third-party library headers
- `bootstrap/` - generated bootstrap compiler and stdlib outputs
- `examples/` - sample Silver programs
- `tests/` - project test inputs and supporting material
- `docs/` - project documentation, currently the runtime migration plan (`docs/runtime-migration.md`)

## Getting Started

Prerequisites:
- Rust toolchain with Cargo
- LLVM 22 compatible development environment for `inkwell`
- a working system C toolchain for linking

A minimal Silver program:

```silver
import std.io;

i32 main() {
    println("Hello, world!");
    return 0;
}
```

Build the compiler:

```bash
cargo build -p agc
```

Run the test suite:

```bash
cargo test -p agc
```

Refresh the bootstrap toolchain and stdlib artifacts:

```bash
bash ./update-bootstrap.sh
```

Create a Linux release bundle:

```bash
bash ./scripts/create-linux-bundle.sh 2026-03-27
```

Tag, build, and publish a GitHub release locally:

```bash
bash ./scripts/release-local.sh v0.1.0
```

Compile a Silver source file:

```bash
cargo run -p agc -- path/to/file.ag
```

Emit a packaged module:

```bash
cargo run -p agc -- path/to/file.ag --emit=module -o path/to/file.agm
```

Emit a shared packaged module:

```bash
cargo run -p agc -- path/to/file.ag --emit=module --shared -o path/to/file.agm
```

## Contributing

Contributions are welcome. Please read `CONTRIBUTING.md` before opening a pull request.

Please also read `CODE_OF_CONDUCT.md` before participating in discussions or reviews.

## License

This project is available under the MIT License. See `LICENSE` for details.
