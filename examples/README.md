# Silver Examples

Each file demonstrates one or more Silver language features.
Build with: `agc examples/<name>.ag -o <name> && ./<name>`

## Getting Started

| File | What it shows |
|------|---------------|
| `hello.ag` | Minimal working program — imports, `i32 main()`, `println()` |
| `fizzbuzz.ag` | `for-in` ranges, `if/else`, modulo, `printf()` |
| `control_flow.ag` | `while` loops, `break`/`continue`, iteration patterns |

## Data & Types

| File | What it shows |
|------|---------------|
| `data_types.ag` | `struct` with generics, `enum`, `Optional<T>`, `impl` blocks |
| `traits.ag` | `trait` definitions, `impl Trait for Type`, method dispatch |
| `casting.ag` | Numeric casts `(f64)x`, struct `cast` operators |
| `array_init.ag` | Positional/designated array init, multi-dimensional arrays |
| `struct_attributes.ag` | `#[packed]`, `#[align(n)]`, `#[repr(C)]`, `@size(T)` |

## Standard Library

| File | What it shows |
|------|---------------|
| `strings.ag` | `String` type — `push`, `push_str`, `clone`, `equals`, `clear` |
| `file_io.ag` | `File` open/read/write/close, `file_delete`, error handling |

## Advanced Features

| File | What it shows |
|------|---------------|
| `iterators.ag` | `for i in 0..n` range loops, nested loops, accumulation |
| `ownership.ag` | `move` ownership transfer, `drop` destructors, scope-based teardown |
| `defer_cleanup.ag` | `defer` blocks — LIFO release order, cleanup before early return |
| `associated_fn_values.ag` | Function-typed trait members (`handler: (i32) -> i32`) with static dispatch |
| `regression_demo.ag` | Larger program — linear/logistic regression with structs, impls, `std.math` |

## Graphics (requires raylib)

| File | What it shows |
|------|---------------|
| `gfx_raylib.ag` | 3D rendering with raylib — camera, rotation, drawing |

## Language Quick Reference

- **Entry point**: `i32 main()` or `i32 main(i32 argc, char** argv)`
- **Output**: `println(str)` for simple lines, `printf(fmt, ...)` for formatted
- **Variables**: `i32 x = 42;` — type annotations required for locals
- **Structs**: `struct Name { Type field; ... }` — fields end with `;`
- **Enums**: `enum Name { Variant; Variant(Type); ... }` — variants end with `;`
- **Methods**: `impl Type { fn method(Type self, ...) { ... } }`
  - Non-mutating: `Type self` as first param, called as `obj.method()`
  - Mutating: `Type* self`, called as `obj.method()`
  - Static: no `self` param, called as `Type.method()`
- **Generics**: `struct Foo<T> { T val; }` — works on structs, enums, impls
- **for-in**: `for i in 0..10 { ... }` — range-based, integer types
- **Traits**: `trait Name { fn method(Type self) -> Ret; }` with `impl Trait for Type { ... }`
- **Casts**: explicit numeric: `(f64) x`; struct: `impl Type { cast Target(Type self) { ... } }`
- **C interop**: `extern "C" { fn name(params); }` — link with `#[link(lib)]`
- **Comptime**: `comptime expr` — evaluate at compile time
- **Builtin macros**: `@size(T)` — type size in bytes
- **Struct attributes**: `#[packed]`, `#[align(n)]`, `#[repr(C)]`
- **Array init**: `{ 1, 2, 3 }`, `{ [2] = 10, [4] = 20 }`, `{ 1, 2, [4] = 9, 10 }`
- **Multi-dim arrays**: `i32 grid[2][3] = { { 1, 2, 3 }, { 4, 5, 6 } };`
