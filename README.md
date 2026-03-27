# Silver

Silver is an experimental systems programming language and compiler toolchain built in this repository.

The project currently includes:
- `agc`, the Silver compiler
- `silver_runtime`, the runtime crate
- `std`, the standard library sources used for bootstrapping
- `bootstrap`, generated compiler and standard library artifacts for local development

Silver is still under active development. Expect sharp edges, incomplete features, and regular iteration on syntax, semantics, and tooling.

## Current Focus

The compiler currently supports:
- parsing, semantic analysis, and LLVM-based code generation
- a bootstrap standard library flow
- module packaging through `.agm` metadata plus compiled artifacts
- source imports and packaged module imports

Recent work has focused on making the module system usable end to end, including packaged metadata, import resolution, and bootstrap generation.

## Repository Layout

- `agc/` - compiler sources
- `silver_runtime/` - runtime crate
- `std/` - standard library sources
- `bootstrap/` - generated bootstrap compiler and stdlib outputs
- `examples/` - sample Silver programs
- `tests/` - project test inputs and supporting material
- `docs/` - design notes and related documentation

## Getting Started

Prerequisites:
- Rust toolchain with Cargo
- LLVM 21 compatible development environment for `inkwell`
- a working system C toolchain for linking

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

Contributions are welcome. If you want to help, please read `CONTRIBUTING.md` before opening a pull request.

Please also read `CODE_OF_CONDUCT.md` before participating in discussions or reviews.

## License

This project is available under the MIT License. See `LICENSE` for details.
