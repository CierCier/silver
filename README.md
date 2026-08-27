# Silver — A Modern Systems Programming Language

Silver is a statically typed, LLVM-backed systems programming language designed to occupy the sweet spot between raw C-level control and modern ergonomic safety.

It combines **deterministic RAII destruction**, **compile-time escape-checked borrowing**, **first-class structured concurrency (`launch`/`wait`)**, and **zero-cost abstractions** on top of a **pure freestanding static runtime** — with no garbage collector and no libc dependency.

---

## Design

### 1. Deterministic Resource Management (RAII) & Ownership
- **Automatic Field Destruction**: Scoped cleanup via compiler-generated drop flags. Destructors and struct field cleanup cascade automatically without explicit manual drop chains.
- **Explicit Moves (`move`)**: Ownership transfers are declared explicitly with `move x`, invalidating the source drop flag to prevent double destruction.
- **Enforced Borrowing (`&T`, `&mut T`)**: References serve as borrow origins. The compiler statically enforces that returned references only derive from valid origins and never escape stack-local frames.
- **Unchecked Escape Hatch (`T*`)**: Raw pointers remain available for FFI, heap-backed abstractions, and manual memory arithmetic without borrow constraints.

### 2. First-Class Concurrency & Static Send Gate
- **1:1 OS Thread Tasks**: Spawn background tasks with `launch f(args...)`, which returns a typed `Task<T>` handle. Arguments are moved across the thread boundary.
- **Explicit Joining**: Join tasks using `wait task` to move out and consume results.
- **Compile-Time Send Gate**: The compiler inspects types moved into tasks, disallowing un-Send data (such as non-atomic `Rc<T>`, raw pointers `T*`, or borrows `&T`) from crossing thread boundaries.
- **Synchronization Primitives**: Pure-Silver RAII `Mutex<T>` / `Guard<T>`, low-level futex `RawMutex`, unbounded MPSC `Channel<T>`, `WaitGroup`, and atomic operations (`std.atomic`).

### 3. Freestanding Static Runtime
- Applications compile to native binaries running on a pure-Silver runtime (`std/rt/`, `std/sys/entry`).
- Freestanding execution without libc bloat or hidden runtime dispatch.

### 4. Zero-Cost Ergonomics
- **Monomorphized Generics**: Full compile-time monomorphization of generic functions, structs, and traits.
- **Algebraic Data Types & Match**: Enums with unit, tuple, and struct variants paired with expression-level `match` pattern matching and tag-aware payload cleanup.
- **Zero-Cost Iterators**: `for x in iter` lowers directly into state-machine loops via `Iterator` and `IntoIterator` traits.
- **Operator Overloading**: Double-underscored method protocols (`__add`, `__index_get`, `__index_set`, etc.) backed by `std.ops`.

### 5. Developer Tooling & Safety Diagnostics
- **Built-in Leak Checker (`--leak-check`)**: Tracks allocations and resolves exact allocation sites (function, file, and line) if memory remains unreleased at exit.
- **Runtime Backtraces**: Built-in DWARF-backed backtrace walker reporting exact function names, source lines, and arguments on crashes/assertions.
- **DWARF by Default**: Native debug information emission for seamless GDB/LLDB debugging and variable inspection.
- **LSP Support (`aglsp`)**: Language server providing real-time diagnostics, symbol outlines, inlay hints, and go-to-definition.

---

## Language Tour

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
    // RAII and formatted printing
    Point p = { .x = 10.0, .y = 20.0 };
    @println("Point: ({}, {})", p.x, p.y);

    // First-class structured concurrency
    Task<i64> task = launch compute(21);

    // Explicit join consuming the task handle
    i64 answer = wait task;
    @println("Computed answer: {}", answer);

    return 0;
}
```

---

## Repository Layout

- `bin/` — Executable binaries and CLI drivers:
  - `bin/agc/` — Reference Silver compiler (`agc` CLI driver and LLVM backend, powered by [Elise](https://github.com/CierCier/elise))
  - `bin/aglsp/` — Silver Language Server Protocol implementation (`aglsp`)
  - `bin/agsm/` — Source maps and module artifact generator (`agsm`)
- `std/` — Standard library sources (memory management, concurrency, I/O, collections, runtime)
- `examples/` — Sample Silver programs
- `tests/` — Test suites including language unit tests, memory pentests, and integration tests
- `docs/` — Language specifications and architecture design docs
- `vendor/` — Third-party library headers and bindings (e.g. `vendor.gfx`)
- `bootstrap/` — Generated compiler and standard library artifacts

---

## Getting Started

### Prerequisites
- **Rust Toolchain** with Cargo (1.75+)
- **LLVM 22** development environment for `inkwell`
- A working system C toolchain / linker (`cc` or `ld.lld`)

### Building the Compiler

```bash
cargo build -p agc
```

### Running Tests

```bash
cargo test -p agc
```

### Compiling & Running Programs

Compile a source file to an executable:
```bash
cargo run -p agc -- path/to/file.ag -o out
```

Fast frontend-only type checking (`agc check`):
```bash
cargo run -p agc -- check path/to/file.ag
```

Compile and run directly in one step (`agc run`):
```bash
cargo run -p agc -- run path/to/file.ag [args...]
```

Run with leak checking enabled:
```bash
cargo run -p agc -- --leak-check run path/to/file.ag
```

### Modules & Packaging

Emit a precompiled module artifact (`.agm`):
```bash
cargo run -p agc -- path/to/file.ag --emit=module -o path/to/file.agm
```

Emit a shared packaged module:
```bash
cargo run -p agc -- path/to/file.ag --emit=module --shared -o path/to/file.agm
```


---

## Contributing

Contributions are welcome! Please review [CONTRIBUTING.md](CONTRIBUTING.md) and [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) before submitting pull requests.

For detailed technical specifications of the compiler architecture and pipeline, see [AGENTS.md](AGENTS.md) and [SYNTAX.md](SYNTAX.md).

## License

This project is available under the MIT License. See [LICENSE](LICENSE) for details.
