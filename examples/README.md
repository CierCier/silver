# Silver Examples

These `.ag` files are meant to be a living “syntax spec by example”.

## Conventions used in these examples

- **Modules**: prefer `import <module>;` (e.g. `import std.io;`).
- **Entry point**: prefer `i32 main(...) { ... }` and `return 0;` on success.
- **Output**: prefer builtin `println("...")` for simple lines, and builtin `printf("...", ...)` for formatted output.
- **Structs**:
  - `struct Name { Type field; ... }`
  - Fields end with `;`.
  - Optional attributes: `#[packed]`, `#[repr(C)]`, `#[align(n)]`.
- **Initializers**:
  - Positional: `{ 1, 2 }`
  - Named: `{ .x = 1, .y = 2 }` (missing fields default to 0)
  - Array designators: `{ [2] = 10, [4] = 20 }`
- **Enums**:
  - `enum Name { Variant; Variant(Type payload); ... }`
  - Variants end with `;`.
- **Methods / impl**:
  - `impl Type { ... }`
  - Static method: no `self` parameter (e.g. `Type<T>.new(...)`).
  - Instance method (non-mutating): first parameter is `Type self`.
  - Instance method (mutating): first parameter is `Type* self`.
  - Instance calls use `obj.method(...)`; static calls use `Type<T>.method(...)`.
- **Traits**:
  - `trait Name { fn method(Type self) -> Type; }`
  - `trait Derived: Base { ... }` for super-traits.
  - `impl Trait for Type where Type: Bound { ... }` for trailing where-clauses.
  - Multiple type params: `trait Pair<A, B> { ... }` / `impl Pair<i32, i64> for i32 { ... }`.
  - See `examples/traits.ag` for `Point`, `Vec3`, and `Pair` examples.
- **Closures**: `list.each(|Type a, Type b| { ... });`
- **Casts**: explicit numeric casts use C-style syntax: `(f64) x`.
- **Inline blocks**:
  - `asm("...")` for inline assembly.

If you want to introduce a new syntax feature, add or update an example here first so it stays documented.
