# Silver

A small, C-shaped systems language. It has scope-based cleanup, compile-time
checks for ownership and threads, and a runtime that does not need libc. There
is no garbage collector.

[![CI](https://github.com/CierCier/silver/actions/workflows/ci.yml/badge.svg)](https://github.com/CierCier/silver/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

> **Status: v0.3.0, pre-1.0.** The language and standard library still change.
> Read [How Silver compares](#how-silver-compares) and
> [Pros and cons](#pros-and-cons) before you pick it for real work.

## A first look

```silver
import std.io;
import std.string;

enum Shape {
    Circle(f64);
    Rect(f64, f64);
}

f64 area(Shape s) {
    return match s {
        Circle(r)  : 3.14159 * r * r,
        Rect(w, h) : w * h,
    };
}

i32 main() {
    // Owned values are dropped when their scope ends; `move` hands one over.
    String name = String.from_str("silver");
    String owner = move name;
    defer { @println("cleanup runs last, in reverse order"); }

    Shape disc = Shape.Circle(2.0);
    Shape box = Shape.Rect(3.0, 4.0);
    @println("{} owns shapes of area {} and {}", owner, area(disc), area(box));

    let double = (i32 value) => value * 2;
    @println("double(21) = {}", double(21));
    return 0;
}
```

```text
silver owns shapes of area 12.56636 and 12
double(21) = 42
cleanup runs last, in reverse order
```

### What the compiler rejects

These are real diagnostics, copied from the compiler.

```silver
String b = move a;
@println("{}", a);
```
```text
error: use of moved value 'a'
note: value explicitly moved here
```

```silver
i64* first(i64 count) {
    i64 local = count;
    return &local;
}
```
```text
error: returned reference does not outlive the function (it borrows a local
value; return a reference to a global or a reference parameter instead)
```

```silver
void bump(&shared i64 total) {
    total += 1;
}
```
```text
error: `total` is shared between threads: read or change it inside `lock total { ... }`
```

### Threads the compiler checks

```silver
import std.io;
import std.thread;
import std.sync;

void add(&shared i64 total, i64 amount) {
    lock total { total += amount; }
}

i32 main() {
    shared i64 total = 0;

    // Tasks borrow `total`; the compiler joins them before the borrow ends.
    Task<void> a = launch add(&total, 5);
    Task<void> b = launch add(&total, 7);
    wait a;
    wait b;

    i64 result = 0;
    lock total { result = total; }
    @println("total = {}", result);   // total = 12
    return 0;
}
```

`launch` starts an OS thread and returns a `Task<T>`. Everything that crosses the
thread boundary must be `Send`, and a task that borrows a local is joined before
the borrow ends. See [concurrency](SYNTAX.md#9-concurrency--threading).

## How Silver compares

| | **Silver** | C | Rust | Zig | Go |
|---|---|---|---|---|---|
| **Memory** | Scope-based `Drop`, tracked moves, no GC | Manual `malloc`/`free` | Ownership and lifetimes, no GC | Manual, explicit allocators, `defer` | Garbage collected |
| **Borrow checking** | `&T`/`&mut T` are borrow origins; returned references are checked against them. No lifetime annotations | None | Full, with lifetimes | None | Not needed (GC) |
| **Unchecked code** | Raw pointers (`T*`) anywhere. There is no `unsafe` marker | Everything | `unsafe` blocks | Everything | `unsafe` package |
| **Threads** | `launch`/`wait` on 1:1 OS threads, a `Send` check, `shared T` with `lock` | `pthread`, no checks | `Send`/`Sync` traits | `std.Thread`, no checks | Goroutines and channels |
| **Errors** | `Result<T, E>` and `Optional<T>`; no exceptions | Return codes, `errno` | `Result` and `Option` | Error unions | Multiple returns |
| **Generics** | Monomorphized, trait bounds with `where` | None (macros, `void*`) | Monomorphized traits | `comptime` | Type parameters |
| **Runtime** | Pure-Silver runtime, direct Linux syscalls, no libc required | libc | libc (default) | Optional libc | Own runtime |
| **Declarations** | Type before name, `let` for inferred locals, no `fn` | Type before name | `fn`, type after name | `fn`, type after name | `func`, type after name |

What this means in practice:

- **Against C:** you keep the declaration style and the manual feel, but the
  compiler tracks who owns a value, runs cleanup for you, and rejects dangling
  returns and use after move.
- **Against Rust:** Silver aims at the same class of mistakes with a smaller
  model. There are no lifetime parameters, and the checker is less complete than
  Rust's, so you get fewer guarantees and less ceremony.
- **Against Zig:** cleanup is automatic through `Drop` instead of written by hand
  with `defer`, though `defer` is there too.
- **Against Go:** no garbage collector and no scheduler. Threads are OS threads,
  and the compiler checks what you share between them.

## Pros and cons

### Pros

- **Deterministic cleanup.** Drops run at scope exit in a defined order, and
  moves are tracked per field, so a partly moved struct drops only what it still
  owns.
- **Real compile-time errors for the common mistakes:** use after move, returning
  a reference to a local, using a shared value outside its lock, and sending a
  non-`Send` value to a thread.
- **Threading syntax that matches the checks.** `shared T`, `lock`, and borrowed
  `launch` are built in, so the compiler can tell you what to change.
- **Small footprint.** The runtime is written in Silver and talks to Linux
  directly. A program that prints, allocates, and formats strings builds to a
  6.8 KiB WebAssembly module.
- **Familiar syntax.** If you read C, you can read Silver. Enums carry data,
  `match` is an expression, and lambdas capture without a heap environment.
- **A broad standard library.** Collections, strings, JSON, files, processes,
  terminals, sockets, TLS, HTTP/1.1 and HTTP/2, and WebSocket.
- **Interop and low-level control.** Call C, call into Rust, write inline
  assembly, and select code with `#[cfg]`.
- **Editor support.** A language server, a tree-sitter grammar, and Zed and
  VS Code extensions.

### Cons

- **Pre-1.0.** Expect breaking changes to syntax and standard library APIs.
- **Two compilers that do not match.** A Rust bootstrap compiler (stage0) builds
  the Silver compiler (stage1). They cover different features, and stage1 cannot
  rebuild itself yet: its borrow checker rejects part of its own source. See the
  [compiler guide](docs/compiler-guide.md).
- **Weaker guarantees than Rust.** Borrow checking is limited to the borrow-origin
  rules. Raw pointers are unchecked and not marked `unsafe`. Taking two locks in
  different orders can deadlock, and the compiler does not check it.
- **Narrow platform support.** Stage1 builds native programs for x86_64 Linux
  only. aarch64 Linux and WebAssembly (`wasm32-wasip1`) are stage0-only, and
  WebAssembly has no threads. The Windows port is a design and partial
  implementation, not a supported target.
- **Small ecosystem.** The package manifest takes local-path and Git
  dependencies, and the repository documents no package registry.
- **Known gaps.** A process can have at most 64 live tasks, lambdas are not
  allowed inside generic functions, user macros are not hygienic in stage1, and
  the complex number types are incomplete.
- **Heavy to build from source.** It needs LLVM 22 and a Rust toolchain that
  supports edition 2024.

## Compiler status

| | Stage0 (Rust) | Stage1 (Silver) |
|---|---|---|
| Role | Bootstraps stage1; full command set | Self-hosting compiler, language server |
| Native targets | x86_64 and aarch64 Linux | x86_64 Linux |
| WebAssembly | `wasm32-wasip1` | No |
| Debugging aids | DWARF, allocation leak tracking | `-g` is accepted but no DWARF is generated yet |

The [compiler guide](docs/compiler-guide.md) lists what each stage supports and
the [syntax reference](SYNTAX.md) marks where they differ.

## Installation

### Nix

Nix provides reproducible builds with LLVM 22, the compiler, and standard library
sysroots:

```bash
# Install to profile
nix profile install github:CierCier/silver

# Or run directly without installing
nix run github:CierCier/silver -- run path/to/file.ag

# Enter a development shell
nix develop
```

Nix support files live in [`nix/`](nix/); only `flake.nix` and `flake.lock` stay
at the repository root. For the legacy entry points, run
`nix-build nix -A silver` or `nix-shell nix/shell.nix` from the root. Both use the
Nixpkgs revision pinned in `flake.lock`.

### Prebuilt binaries

Download an archive from the [Releases](https://github.com/CierCier/silver/releases)
page, extract it, and add its `bin/` directory to your `PATH`.

The binaries link LLVM statically, so you do not need LLVM installed. You do need:

- x86_64 Linux with glibc 2.39 or newer
- a system linker on `PATH`: `cc`, `clang`, or `ld.lld`

### Building from source

You need a Rust toolchain that supports edition 2024, LLVM 22 development headers
and libraries, and a C toolchain with a linker (`cc`, `clang`, or `ld.lld`).

```bash
git clone https://github.com/CierCier/silver.git
cd silver

# Build the compiler in release mode
cargo build --release -p agc

# Install the compiler, the language server, and the standard library
./scripts/install.sh
```

## Usage

```bash
# Compile to an executable
agc path/to/file.ag -o out

# Compile and run, forwarding program arguments after --
agc run path/to/file.ag -- input.txt

# Type-check only
agc check path/to/file.ag

# Create a new package (silver.toml + src/main.ag)
agc init myapp

# Build the package or workspace in the current directory
agc build

# Stage0: run with allocation leak tracking
agc run path/to/file.ag --leak-check
```

Executables default to `./a.out` (package builds to `./<target name>`), and
`--emit-llvm -o -` streams LLVM IR to stdout.

### Modules and packaging

```bash
# Emit a precompiled module (.agm)
agc path/to/file.ag --emit=module -o path/to/file.agm

# Stage0: emit a shared packaged module
agc path/to/file.ag --emit=module --shared -o path/to/file.agm
```

### Native targets

Stage1 builds native programs for the host target on x86_64 Linux. An explicit
`--target TRIPLE` must match it exactly, and unsupported targets fail before any
output is created. `check` accepts another target's `cfg` values, which does not
make that target supported. Stage0 also cross-compiles for aarch64 Linux
(`--target aarch64-unknown-linux-gnu`); `agc run` and the test runner execute
those binaries under `qemu-aarch64` on an x86_64 host. See the
[compiler target contract](docs/compiler-guide.md#native-target-contract).

## Testing

```bash
# Run every suite: Rust tests, the stdlib suite with stage0 (native and, under
# qemu, the other Linux architecture) and stage1, and the language-server suite.
# Keeps going after a failure and exits non-zero if any step failed.
nix run .#check
```

To run suites one at a time:

```bash
# Build the bootstrap tools and the optional Rust FFI bridge
cargo build --workspace

# Standard-library behavior tests with stage0
python3 tests/run_tests.py --no-tui --jobs 4

# Build the Silver compiler and language server, then test them
target/debug/agc build silver.toml --bin agc --no-cache -o /tmp/agc-stage1
target/debug/agc build silver.toml --bin aglsp --no-cache -o /tmp/aglsp-stage1
python3 scripts/check_lsp.py --server /tmp/aglsp-stage1

# The same stdlib suite with the native Silver compiler
python3 tests/run_tests.py --compiler /tmp/agc-stage1 --no-tui --jobs 4
```

The stdlib suite has 118 programs and is capped at 124. Extend an existing module
test for new behavior. Compiler builds and standard library use exercise language
features; separate compiler unit, parity, and self-host test suites are retired.

## Repository layout

- `bootstrap/stage0/agc/`: Rust bootstrap compiler (stage0): driver, frontend, and LLVM backend, using [Elise](https://github.com/CierCier/elise) for parsing
- `bootstrap/stage0/agsm/`: Source maps and module artifact generator
- `bootstrap/stage0/ffi-rust/`: Optional Rust implementation behind Silver's versioned C ABI
- `bin/agc/`: Stage1 self-hosting compiler driver in Silver
- `bin/aglsp/`: Language-server binary; see [native LSP setup](docs/language-server.md) and the [protocol support ledger](docs/lsp-protocol.md)
- `libs/agc/`: Reusable stage1 compiler frontend library (Silver)
- `std/`: Standard library (allocators, collections, I/O, networking, runtime)
- `tools/`: Editor support: a [Zed extension](tools/zed), a [VS Code extension](tools/vscode), and the [tree-sitter grammar](tools/tree-sitter-silver) they share; see the [tools guide](tools/README.md)
- `examples/`: Sample programs
- `tests/`: Stdlib behavior tests and their runner
- `docs/`: Language specifications and standard protocol designs
- `silver.toml`: Silver workspace manifest for the self-host root

## Documentation

- [Syntax specification](SYNTAX.md)
- [Compiler architecture reference](docs/compiler-guide.md)
- [C ABI and FFI](docs/rust-ffi.md)
- [WebAssembly target](docs/wasm-target.md)
- [Standard library protocols](docs/standards/README.md)
- [Contributing](CONTRIBUTING.md)
- [Agent working guide](AGENTS.md)

## License

MIT
