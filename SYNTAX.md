# Silver Language Syntax Specification

This reference describes Silver syntax and behavior implemented by the compiler
and standard library. Stage0 and stage1 do not support every feature equally.
The notes below identify known limits, and the evidence index links the parser,
stage1 source, standard-library modules, and representative fixtures.

---

## Table of Contents

1. [Lexical Structure & Tokens](#1-lexical-structure--tokens)
2. [Type System & Type Expressions](#2-type-system--type-expressions)
3. [Top-Level Declarations (Items)](#3-top-level-declarations-items)
4. [Statements & Block Scopes](#4-statements--block-scopes)
5. [Expression Grammar & Taxonomy](#5-expression-grammar--taxonomy)
6. [Operator Precedence & Associativity](#6-operator-precedence--associativity)
7. [Operator Overloading & Custom Protocols](#7-operator-overloading--custom-protocols)
8. [Memory Management & Ownership Model](#8-memory-management--ownership-model)
9. [Concurrency & Threading](#9-concurrency--threading)
10. [Standard Library & Patterns](#10-standard-library--patterns)

### Evidence by area

These links are representative evidence. Parser acceptance does not by itself
prove type checking, code generation, or runtime behavior. Stage0 lives under
`bootstrap/stage0`; stage1's Silver implementation lives under `libs/agc`.

| Area | Implementation | Stdlib examples |
|---|---|---|
| Tokens and literals | [stage0 lexer](bootstrap/stage0/agc/src/lexer.rs), [stage1 lexer](libs/agc/src/frontend/lexer.ag) | [byte formatting](tests/format_bytes_test.ag) |
| Types, expressions, and patterns | [stage0 parser](bootstrap/stage0/agc/src/parser/prt_parser.rs), [stage1 type checker](libs/agc/src/frontend/typecheck_expr.ag) | [slices](tests/slice_syntax_test.ag) |
| Declarations and imports | [stage0 import loader](bootstrap/stage0/agc/src/parser/import_hook.rs) | [stdlib re-exports](tests/std_shim_test.ag) |
| Functions, callbacks, and generics | [stage1 monomorphization](libs/agc/src/frontend/monomorph.ag) | [Result callbacks](tests/result_combinators_test.ag), [checked conversions](tests/checked_conversion_test.ag) |
| Traits, operators, and casts | [operator traits](std/ops.ag), [stage1 lowering](libs/agc/src/frontend/lower_expr.ag) | [comparison](tests/compare_trait_test.ag), [sequences](tests/sequence_traits_test.ag) |
| Control flow, matching, and defers | [stage1 statement codegen](libs/agc/src/backend/llvm/stmt.ag) | [iterators](tests/iterator_ergonomics_test.ag), [guards](tests/guard_test.ag) |
| Macros, attributes, cfg, and static values | [stage1 cfg](libs/agc/src/frontend/cfg.ag), [stage0 parser](bootstrap/stage0/agc/src/parser/prt_parser.rs) | [JSON serialization](tests/json_test.ag), [formatting](tests/fmt_test.ag) |
| Ownership and borrowing | [stage0 borrow checker](bootstrap/stage0/agc/src/semantic/borrow_check.rs), [stage1 ownership](libs/agc/src/frontend/ownership.ag) | [container cleanup](tests/collection_drop_glue_test.ag), [Box](tests/box_test.ag) |
| Threads and channels | [stage1 Send check](libs/agc/src/frontend/send.ag), [channels](std/channel.ag) | [channels](tests/channel_test.ag), [bounded channels](tests/channel_bounded_test.ag) |
| Result and optional values | [Optional](std/optional.ag), [Result](std/result.ag), [syscall errors](std/sys/result.ag) | [structured errors](tests/structured_error_test.ag), [Result cleanup](tests/unwrap_or_err_drop_test.ag) |
| Allocation, formatting, FFI, and assembly | [allocator](std/mem/alloc.ag), [display](std/display.ag), [stage1 codegen](libs/agc/src/backend/llvm/generator.ag) | [allocation](tests/allocation_bounds_test.ag), [display](tests/display_test.ag), [Rust FFI](tests/rust_ffi_test.ag), [syscalls](tests/syscall_wrapper_test.ag) |

---

## 1. Lexical Structure & Tokens

Silver source files use UTF-8 encoding and the `.ag` extension.

### Comments

```silver
// Single-line comment

/* Multi-line block comment
   spanning multiple lines */
```

### Identifiers

Identifiers match `[a-zA-Z_][a-zA-Z0-9_]*`.

```silver
i32 my_variable_1 = 100;
f64 _internalCount = 3.14;
```

### Keywords

These are hard keywords — they cannot be used as identifiers:

| Keyword | Description | Keyword | Description |
|---|---|---|---|
| `struct` | Struct definition | `enum` | Enum definition |
| `impl` | Implementation block | `trait` | Trait definition |
| `mut` | Mutable modifier | `const` | Constant declaration (enforced immutability) |
| `static` | Static local / internal-linkage global | `volatile` | Volatile load/store qualifier |
| `if` | Conditional branch | `else` | Alternative branch |
| `while` | Condition loop | `for` | Counter or iterator loop |
| `in` | Iterator element | `break` | Loop termination (with optional value) |
| `continue` | Loop iteration skip | `return` | Early return |
| `defer` | Scope-exit deferred action | `import` | Module import directive |
| `comptime` | Compile-time evaluation | `launch` | Spawn a detached thread, returning a `Task<T>` |
| `wait` | Join a `Task`, moving out its result | `cast` | Type cast declaration/expr |
| `move` | Ownership transfer | `extern` | External symbol binding |
| `private` | Private visibility modifier | `asm` | Inline assembly block |
| `macro` | Macro declaration | `true` / `false` | Boolean literals |
| `void` | Empty/unit return type | `match` | Match expression |
| `type` | Type alias / associated type | `Self` | Implementing type keyword |

**Not keywords**: `pub` (items are public by default; `private` restricts visibility), `fn`.
`let` is a keyword introducing inferred local bindings (`let x = expr;`).

### Literals

```silver
// Integers — decimal, binary, octal, and hexadecimal
i32 dec = 42;
i64 bin = 0b1010;
i64 oct = 0o755;
i64 hex = 0x1A2F;
i64 grouped = 1_000_000;

// Floating-point
f64 pi = 3.1415926535;
f64 sci = 1.0e-5;

// Complex numbers (lexed but type-level support incomplete)
c64 c1 = 3.5i;

// Strings & Characters
str greeting = "Hello, Silver!\n";
char ch = 'A';
char esc = '\n';

// Booleans
bool active = true;
bool disabled = false;
```

Underscores may separate digits in integer and floating-point literals. Binary
(`0b` or `0B`), octal (`0o` or `0O`), and hexadecimal (`0x` or `0X`) prefixes
select the integer radix.

String escapes include newline, carriage return, tab, NUL, backslash, quotation
mark, apostrophe, two-digit hexadecimal bytes (`\xNN`), and Unicode scalar
values (`\u{...}`). Hex escapes contribute raw bytes, so the complete string
must remain valid UTF-8. Character literals support the single-character
escapes, but not the string-only hexadecimal or Unicode forms.

---

## 2. Type System & Type Expressions

Silver features a static type system with primitives, pointers, static arrays,
generics, and algebraic data types.

### Primitive Types

| Group | Types | Description |
|---|---|---|
| Signed Integers | `i8`, `i16`, `i32`, `i64`, `i128` | 2's complement |
| Unsigned Integers | `u8`, `u16`, `u32`, `u64`, `u128` | Unsigned |
| IEEE-754 Floats | `f32`, `f64`, `f80` | Single, double, extended precision |
| Complex Numbers | `c32`, `c64`, `c80` | Lexer tokens exist; type-level support incomplete |
| String & Character | `str`, `char` | NUL-terminated byte pointer, 32-bit codepoint |
| Boolean & Void | `bool`, `void` | `true`/`false`, unit return |

`char` is a 32-bit Unicode scalar and `char*` points at `char` values. Byte
buffers use `u8`/`i8`, and a `u8*`/`i8*` never converts to a `char*` (or the
reverse) without an explicit cast: the element widths differ, so an implicit
conversion would let a `char` store write four bytes into a one-byte buffer.
C byte strings therefore use `u8*` or `str`, not `char*`.

### References & Borrowing

References are the **enforced** indirection mechanism.  A parameter declared
`&T` or `&mut T` is a **borrow origin**: the escape checker proves that any
pointer a function returns derives only from its `&`-marked parameters or
from independent data (globals, heap values, raw-pointer pointees).
Returning a reference to a local is a compile error.

```silver
i64 read(&Pair p) { return *p.method(); }   // &T borrow origin
void update(&mut Pair p) { p.set(42); }     // &mut T borrow origin
Pair* borrow_first(&Pair a, &Pair b) { ... } // returned ref may borrow a or b
```

Methods can write the first receiver parameter as `self`, `&self`, or
`&mut self`. These forms mean by-value, shared-borrow, and mutable-borrow
receivers. The raw-pointer shorthand is `*self`.

### Raw Pointers

Raw pointers (`T*`, `const T*`, `mut T*`) are the **unchecked escape
hatch**: non-owning views that are never automatically dropped and create
**no** borrow constraints.

```silver
i32* p_mut;         // raw pointer to i32 (no borrow check)
const i32* p_const; // raw pointer to const i32 (read-only target)
i32** pp_mut;       // double raw pointer
```

Pointer and reference field access auto-derefs: `p.x` is equivalent to `*(p).x`.

Prefer `&T`/`&mut T` whenever the callee borrows caller-owned data; reserve
raw pointers for FFI, manual address arithmetic, and state owned by the
callee (heap-backed).

### Arrays, tuples, and slices

Fixed-size array declarations put `[N]` after the variable name. Array literals
use braces.

```silver
i32 fixed_arr[10];      // 10-element array of i32
u8 buf[512];            // local array
DnsCacheEntry cache[64]; // global array (size must be an integer literal)
i32 values[3] = { 1, 2, 3 };
```

Tuple types and tuple literals use square brackets. A comma separates tuple
elements. A bracket type with one element and no comma is a slice type.

```silver
[i32, str] pair = [42, "answer"];
[i32] view = values[1:];
```

The standard library also exposes the named `Slice<T>` type in
[`std/slice.ag`](std/slice.ag). Indexing uses `value[index]`. Slicing uses
`value[start:end]`; either bound may be omitted, as in `value[:]` or
`value[2:]`. Slicing arrays, vectors, strings, and slices produces a borrowed
slice view.

### Compound & Generic Types

```silver
Vec<i32> numbers;
HashMap<str, f64> scores;
Optional<str> name;
```

### Tuples

Tuple types and values use square brackets. Elements can have different types;
constant indices select individual elements.

```silver
[i32, str] pair = [7, "seven"];
i32 number = pair[0];
let [value, label] = pair;
```

Parentheses group expressions; `(i32, str)` and `(7, "seven")` are not tuple syntax.

### Function Types

Function types use `ReturnType(ParamTypes...)` syntax.

```silver
bool(i32, f64) predicate;   // function pointer: takes i32, f64 → returns bool
void() callback;             // function pointer: no params → void
```

---

## 3. Top-Level Declarations (Items)

### Module Imports

```silver
// Whole-module imports bring all public module items into scope.
import std.io;

// Select names and give a function a local alias.
import std.io.file { println as print_line, STDOUT };
```

Selective imports can name exported items and rename them with `as`. Whole-module
imports remain supported and can coexist with selective imports.

Imports inline `.ag` source modules or ingest `.agm` binary artifacts.
Named selections are non-restrictive: other public exports remain available.
Function aliases introduce a local name for an imported function; they do not
create a module namespace.

### Type Aliases

```silver
type Distance = f64;
type Handler = void(i32);
```

### Global & Constant Variables

```silver
str g_app_name = "SilverApp";      // global variable
const f64 PI = 3.1415926535;       // const (enforced immutability)
```

`mut i32 g_counter` is **not** valid — `mut` cannot start a type declaration (variables are mutable by default).

### Functions

Silver uses **C-style syntax** for top-level functions: the return type precedes
the function name.  There is no `fn` keyword for top-level functions. Items are
public by default; use `private` to restrict visibility.

```silver
// Standard function with generics and where clause
T max<T>(T a, T b) where T: Lt<T> {
    if (a < b) { return b; }
    return a;
}

// C-style return type
i32 main() { return 0; }

// Void return (private to module)
private void log_message(str msg) {
    @println("[LOG] {}", msg);
}

// External variadic function
extern "C" i32 printf(const u8* fmt, ...);
```

### Struct Definitions

Fields are separated by `;` and are public by default. Prefix a field with
`private` to restrict its visibility. Struct items are public by default; use
`private struct ...` to restrict the item's visibility.

```silver
struct Point<T> {
    T x;
    T y;
}

struct Buffer {
    i32* data;
    i32 length;
    i32 capacity;
}
```

> **Note**: Fields on struct pointers and struct references can be directly accessed
> without dereferencing (`s.f` is the same as `*(s).f`).

### Enum Definitions

Variants are separated by `;` (**not** `,`).  Discriminants use `=` (not `:`).
All three variant kinds are supported.

```silver
// Unit enum with discriminants
enum SYSCALL {
    READ = 0;
    WRITE = 1;
    OPEN = 2;
    CLOSE = 3;
}

// Algebraic data type
enum Shape {
    Circle(f64);                          // tuple variant
    Rectangle { f64 width; f64 height; }  // struct variant
    Point;                                // unit variant
}
```

### Trait Declarations

Traits define interface contracts, associated types, and optional default method
bodies.  Methods use **C-style syntax**: return type before name.

```silver
trait Display {
    void fmt(*self, BufWriter* out);
}

trait Iterator<Self> {
    type Item;
    Optional<Item> next(Self* self);
}
```

> **Note**: `Self` is a keyword referencing the implementing type in trait and method declarations.

### Implementation Blocks (`impl`)

```silver
struct Vector2 { f64 x; f64 y; }

// Inherent implementation
impl Vector2 {
    Vector2 new(f64 x, f64 y) {
        Vector2 v = { .x = x, .y = y };
        return move v;
    }

    f64 magnitude(Vector2* self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Custom cast block
impl Vector2 {
    cast f64(Vector2 self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Trait implementation
impl Display for Vector2 {
    void fmt(*self, BufWriter* out) { out.write_str("Vector2"); }
}
```

### External Declarations (`extern`)

```silver
extern "C" f32 sinf(f32 x);

extern "C" {
    i32 open(const u8* path, i32 flags);
    i32 close(i32 fd);
    i32 errno;    // mutable extern variables not supported (no `mut` keyword)
}
```

Supported linkage specs: `"C"`, `"Silver"`, `"system"`, `"Rust"`, `"cdecl"`,
`"stdcall"`, `"fastcall"`.

C string parameters use `const u8*` (or `str`): Silver's `char` is a 32-bit
codepoint, so `char*` addresses `char` values and cannot alias a byte buffer.
The C ABI is unchanged — both spellings are pointers — but a Silver call
passes a `str` only to a `const u8*` parameter.

### Attributes & Metadata

```silver
#[link("pthread")]
#[link("m")]

#[link_name("native_c_pow")]
extern "C" f64 c_pow(f64 base, f64 exp);

#[volatile]           // never dead-code-eliminated (globaldce root)
i32 runtime_shim() { return 0; }
```

`#[volatile]` on a top-level function or global keeps the item alive even
when nothing references it: it survives dead-code elimination at `-O2+`
(the standard-library `memcmp`/`bcmp` shims use it, because LLVM introduces
calls to them only after DCE has run). It is unrelated to the `volatile`
type qualifier, which makes loads/stores LLVM-volatile.

`@attr(...)` is **not** attribute syntax — `@` is for expression macro calls
(`@println`, `@size`, etc.).

### Conditional Compilation (`#[cfg]` / `@cfg` / top-level `if`)

Platform and build configuration gate through one key space:

- Derived: `debug`/`release` (opt level), `arch.<arch>` / `os.<os>`
  (target triple or host), `cpu.<feature>` (native x86_64 probe only).
- Custom: `--cfg "key=val,..."` (repeatable, AND-composed).

```silver
#[cfg(os.linux)]
i32 linux_only() { return 1; }

i32 v() {
    if (@cfg(os.linux)) { return 1; } else { return 2; }
    // absent key -> false (dead branch pruned before typeck);
    // present cpu.* -> runtime probe global (g_has_<feat>).
}
```

Top-level `if (@cfg(...))` blocks compose per-platform files. They expand
before import lowering, so dead-branch imports are never resolved:

```silver
if (@cfg(os.linux)) {
    import cfg_platform.linux;
} else if (@cfg(os.windows)) {
    import cfg_platform.windows;
} else {
    import cfg_platform.windows; // fallback
}
```

Conditions support `!`, `&&`, `||`. `cpu.*` is runtime-only and cannot
gate items (top-level use is an error). `#[cfg(...)]` on an `import`
likewise skips resolution when rejected.

### Macro Definitions

Stage1 currently expands positional user macros, including a final variadic
parameter. A variadic binding supports `.len` and constant nonnegative integer
indexing. Statement macros may also use `for item in values { ... }` to unroll
their body once per argument in a final variadic binding; an empty binding
produces no iterations. This is not general iteration or variadic splicing.
Expression macros support a direct `return` expression or an `if`/`else` whose
branches each return an expression; statement macros must not contain `return`.
Nested calls are supported. Stage0 renames local bindings; stage1 does not
provide full hygienic renaming. Macro arguments substitute at each use, so
repeated parameters repeat evaluation and unused arguments do not run.
Both compilers reject incorrect value-argument counts. See the
[macro contract](docs/macros.md) for evaluation rules and stage limits.
Built-in compiler macros include `@print`, `@println`,
`@eprint`, `@eprintln`, `@fprint`, `@sprint`, `@format`, `@size`, `@align`,
`@hash`, `@json`, `@from_json`, `@memcpy`, `@memset`, and `@memmove`.

```silver
macro swap(a, b) {
    a = a + b;
    b = a - b;
    a = a - b;
}
```

---

## 4. Statements & Block Scopes

### Variable Declarations

Silver uses **C-style syntax**: type before name. An initializer-less binding
declares the variable with its annotated type.
```silver
i32 x = 42;           // C-style declaration (idiomatic)
f64 y;                // uninitialized (allowed; drop-flag caveats apply)
```

**Inferred bindings**: `let name = expr;` declares a local whose type is
inferred from the initializer expression (including generic call results).
An initializer is required — `let x;` is an error; annotate instead.
```silver
let n = compute();    // type inferred from compute()'s return type
let b = move owned;   // move transfers ownership, as with annotated lets
```

Tuple destructuring uses square brackets. Use `_` to ignore an element.

```silver
let [x, y] = [10, 20];
let [first, _, last] = [1, 2, 3];
```

The parenthesized form `let (a, b) = ...` is not supported.

### Assignments & Compound Assignments

All compound assignment operators are supported:

```silver
x = 100;
*ptr = 42;
arr[0] = 5;
point.x = 3.14;

count += 1;
sub   -= 5;
total *= 2;
div   /= 4;
rem   %= 3;
```

### Defer Cleanup Statements

`defer` postpones a statement or block until the enclosing scope exits.
Defers fire in **LIFO** order, including before early returns.

```silver
{
    i32 fd = open("file.txt", 0);
    defer close(fd);

    void* buf = malloc(1024);
    defer { free(buf); }   // fires BEFORE close(fd)

    if (error_condition) { return -1; }  // defers fire here
}
```

### Control Flow Statements

```silver
return;
return result_value;

break;               // loop exit
break value;          // break with value (AST support; semantic support TBD)
continue;             // skip iteration
```

### Block Statements

```silver
{
    i32 inner_var = 10;
    // scope exit drops local resources
}
```

---

## 5. Expression Grammar & Taxonomy

### Primary & Path Expressions

```silver
my_var;
```

### Literals & Aggregate Initializers

```silver
// Primitives
42;
3.14;
"Hello";

// Designated struct initializer
Point p = { .x = 10.0, .y = 20.0 };

// Positional struct initializer
Point p2 = { 10.0, 20.0 };

// Array initializer
i32 arr[3] = { 1, 2, 3 };

// Tuple initializer
[i32, str] entry = [100, "OK"];
```

Array and struct initializers use braces. Bracket literals such as `[1, 2, 3]`
construct tuples, not fixed-size arrays. Parenthesized tuple literals are not
supported.

### Binary Expressions

| Category | Operators | Example |
|---|---|---|
| Arithmetic | `+`, `-`, `*`, `/`, `%` | `a + b * c` |
| Bitwise | `&`, `\|`, `^`, `<<`, `>>` | `(mask & 0xFF) << 4` |
| Comparison | `==`, `!=`, `<`, `>`, `<=`, `>=` | `x >= 0 && x < length` |
| Logical | `&&`, `\|\|` | `is_valid && !is_expired` |
| Range | `..` | `0..10` |


Numeric literal operands can take their type from context. For example,
`value << 1` works with a `u64 value` in both stages. Use `(Type)expression`
when an explicit numeric conversion is needed; broader coercion and diagnostic
parity remains part of the stage1 checking work.

### Unary & Postfix Expressions

```silver
+val;       // unary plus
-val;       // unary minus
!flag;      // logical NOT
~mask;      // bitwise NOT
&var;       // address-of
*ptr;       // pointer dereference
++i;        // prefix increment
--i;        // prefix decrement
i++;        // postfix increment
i--;        // postfix decrement
```

### Ownership & Move Expressions

```silver
Buffer b1 = create_buffer();
Buffer b2 = move b1;    // b1's drop flag cleared; only b2 drops
```

**References** are created with `&var` (address-of). A `&` expression passed as an
argument or receiver is a **borrow**: the escape checker verifies it does not escape
its source (see [§8](#8-memory-management--ownership-model)).
```silver
Buffer* r1 = &b2;     // raw pointer view of b2 (auto-derefs on field access)
void take(&Buffer b) { ... }
take(&b2);            // borrow of b2 — escape-checked
```

`ref mut` is not a distinct syntax. `&T` and `&mut T` are valid reference
types and `&expr` / `&mut expr` produce reference expressions.

### Cast & Conversion Expressions

```silver
f64 float_val = (f64)int_val;     // primitive numeric cast
void* raw_ptr = (void*)buffer;    // pointer cast
f64 magnitude = (f64)p;           // custom struct cast (triggers cast block)
```

Cast syntax is `(Type)expression`. Pointer casts and primitive type names are
unambiguous. A bare user-defined type name in `(Type)expression` can be
ambiguous with a parenthesized expression; use a pointer cast, a fully
qualified/function type, or an implemented `cast` block when the parser cannot
disambiguate it. The `cast` declaration form defines a user conversion; it is
not a prefix cast operator.

### Compile-Time (`comptime`) Expressions

```silver
const i32 BLOCK_SIZE = comptime (1024 * 64);
i32 folded_val = comptime (i32)3.99;   // → 3
```

### Function Calls & Method Invocations

```silver
i32 res = add(10, 20);           // direct call
f64 dist = point.magnitude();    // method call (passes self pointer)
Vector2 v = Vector2.new(1, 2);   // static method (no self param)
```

### Lambdas

Both compilers support lambda expressions. Run the
[lambda example](examples/lambdas.ag):

```bash
cargo run -p agc -- run examples/lambdas.ag
```

An expression body returns its result automatically. A block body needs an
explicit `return` to produce a value; a block without a return produces `void`.
`return` exits the lambda. A `match` expression can be the entire body.

```silver
let double = |i32 value| value * 2;
let absolute = |i32 value| {
    if (value < 0) { return -value; }
    return value;
};
let classify = |i32 value| match value { 0: "zero", _: "nonzero" };
let answer = || 42;
```

Parameters use type-before-name order. `&T` and `&mut T` parameters borrow;
`move T name` explicitly declares a by-value parameter with Silver's usual
copy or move behavior. An expected function type can supply omitted parameter
types, including an unambiguous non-generic named callback parameter:

```silver
i32(i32) triple = |value| value * 3;
let length = |&String value| value.len();
let consume = |move String value| value.len();
```

Outer locals are captured automatically. Reads use shared borrows, writes use
mutable borrows, and consuming a non-Copy value moves it into the lambda when
the lambda is created. Copy values retain ordinary copy semantics. Captures
do not clone values or allocate a heap environment. Conflicting outside access
is rejected while a capture borrow is still needed. A lambda that consumes an
owned capture can be called once; an uncalled lambda drops its owned captures.

```silver
String text = String.from_str("silver");
let length = || text.len();
@println("length={}, text={}", length(), text);

let take = || move text;
String received = take();
```

A lambda without captures can be stored in a function pointer. A capturing
lambda has its own generated type; use an inferred `let` binding. This initial
implementation supports typed parameters and function-type context inference;
it does not infer an untyped local lambda's parameters from later calls.
Lambdas inside generic functions or impls are not supported yet. Map integration
is outside this change.

### Field Access & Indexing

```silver
f64 x = point.x;          // direct field access
Point* p_ptr = &point;
f64 y = p_ptr.y;          // auto-deref: equivalent to (*p_ptr).y
i32 item = arr[0];        // array/container index (may invoke __index_get)
arr[1] = 100;             // index write (may invoke __index_set)
```

> **Note**: Fields on struct pointers and struct references can be directly accessed
> without dereferencing (`s.f` is the same as `*(s).f`).

### Control Flow Expressions

#### If-Else

```silver
if (a > b) { return a; } else { return b; }
```

#### Loops

```silver
while (i < 10) { i += 1; }

for (i32 i = 0; i < 10; i += 1) {   // C-style for
    @println("i = {}", i);
}

for item in container {              // for-in (lowers to IntoIterator+Iterator)
    @println("item = {}", item);
}
```

#### Match

`match` evaluates an expression and selects an arm by pattern. It is an
**expression**: every arm yields a value of the same type.

```silver
enum Shape { Circle(f64); Rect(f64, f64); }

f64 area(Shape s) {
    return match s {
        Circle(r)  : 3.14159 * r * r,
        Rect(w, h) : w * h,
    };
}
```

Arm syntax is `pattern : value` separated by commas. Variant patterns use
just the variant name (`Circle(r)`, `None`) — the enum type is inferred from
the scrutinee. Patterns bind payload values (`r` in `Circle(r)`), and a
wildcard `_` arm can be used as a catch-all:

### Inline Assembly (`asm`)

```silver
asm("nop");

asm("syscall", [num, arg1]);   // string, operand list in [...]
```

The asm string is passed directly to LLVM inline assembly.  Operands are
positional — `{}` in the string has no special meaning.
### Macro Invocation Expressions

Builtin macros use `@name(...)` syntax:

```silver
@print("Value: {}", x);
@println("Formatted {} {}", val1, val2);
@eprintln("Error: {}", err_msg);
String formatted = @format("Coordinate: ({}, {})", point.x, point.y);

i64 struct_size = @size(Vector2);
i64 struct_align = @align(Vector2);
u64 obj_hash = @hash(my_object);

@memcpy(dst_ptr, src_ptr, bytes_count);
@memset(dst_ptr, 0, bytes_count);
@memmove(dst_ptr, src_ptr, bytes_count);
```

> **Note**: Only `@name(...)` syntax works.  `name!(...)` syntax is **not**
> parsed — the `!` token is only used as logical NOT.

`{}` placeholders dispatch through the `Display` trait (`void fmt(*self,
BufWriter* out)`). Types without a `Display` impl fall back to a comptime
debug expansion: structs print as `Name { field: value, ... }` and unit
enums print their variant name.

---

## 6. Operator Precedence & Associativity

Actual precedence as implemented in the Pratt parser (`prt_parser.rs`):

| Priority | Operators | Associativity |
|---|---|---|
| 1 (highest) | `()` `[]` `.` `++` `--` (postfix) | Left-to-Right |
| 2 | `+` `-` `!` `~` `*` `&` `++` `--` `move` `comptime` `(Type)` | Right-to-Left |
| 3 | `*` `/` `%` | Left-to-Right |
| 4 | `+` `-` | Left-to-Right |
| 5 | `<<` `>>` | Left-to-Right |
| 6 | `&` (bitwise) | Left-to-Right |
| 7 | `^` | Left-to-Right |
| 8 | `\|` | Left-to-Right |
| 9 | `..` `==` `!=` `<` `>` `<=` `>=` | Left-to-Right |
| 10 | `&&` | Left-to-Right |
| 11 | `\|\|` | Left-to-Right |
| 12 | `=` `+=` `-=` `*=` `/=` `%=` | Right-to-Left |

> **Changes from previous version**: `::` removed (not an expression
> operator).  `..=` removed (does not exist).  Range `..` is at the relational
> level, not a separate priority.

---

## 7. Operator Overloading & Custom Protocols

Silver maps operators to double-underscore methods.  Implement these in `impl`
blocks or via standard traits from `std/ops.ag`.

| Operator | Method | Trait |
|---|---|---|
| `a + b` | `__add(Self, Other) -> Self` | `Add<A, B>` |
| `a - b` | `__sub(Self, Other) -> Self` | `Sub<A, B>` |
| `a * b` | `__mul(Self, Other) -> Self` | `Mul<A, B>` |
| `a / b` | `__div(Self, Other) -> Self` | `Div<A, B>` |
| `a % b` | `__mod(Self, Other) -> Self` | `Mod<A, B>` |
| `-a` | `__neg(Self) -> Self` | `Neg<A>` |
| `!a` | `__not(Self) -> Self` | — |
| `~a` | `__bitnot(Self) -> Self` | — |
| `a == b` | `__eq(Self, Self) -> bool` | `Eq<A>` |
| `a != b` | `__ne(Self, Self) -> bool` | `Ne<A>` |
| `a < b` | `__lt(Self, Self) -> bool` | `Lt<A>` |
| `a > b` | `__gt(Self, Self) -> bool` | `Gt<A>` |
| `a <= b` | `__le(Self, Self) -> bool` | `Le<A>` |
| `a >= b` | `__ge(Self, Self) -> bool` | `Ge<A>` |
| `a & b` | `__bitand(Self, Self) -> Self` | — |
| `a \| b` | `__bitor(Self, Self) -> Self` | — |
| `a ^ b` | `__bitxor(Self, Self) -> Self` | — |
| `a << b` | `__shl(Self, Shift) -> Self` | — |
| `a >> b` | `__shr(Self, Shift) -> Self` | — |
| `c[i]` (read) | `__index_get(Self*, i64) -> Item` | `IndexedAccess<Container>` |
| `c[i] = v` (write) | `__index_set(Self*, i64, Item)` | `IndexedAccess<Container>` |
| `(Target) val` | `cast Target(Self) -> Target` | Custom `cast` block |

### Iterator Protocol

`for item in container` lowers to the `IntoIterator` and `Iterator` protocol:

For `Vec<T>`, `for item in values` consumes the vector, while
`for item in &values` and `for item in values.iter()` borrow its storage.
Borrowed iteration preserves the vector's contents. A local `Vec<T>*` used
as the iterable also remains usable after the loop.

```silver
struct RangeIter {
    i32 current;
    i32 end;
}

impl Iterator<RangeIter> for RangeIter {
    type Item = i32;
    Optional<i32> next(RangeIter* self) {
        if (self.current < self.end) {
            Optional<i32> res = Optional<i32>.some(self.current);
            self.current += 1;
            return res;
        }
        return Optional<i32>.none();
    }
}
```

---

## 8. Memory Management & Ownership Model

Silver uses a stack-machine resource tracking architecture with explicit move
semantics and drop flags.

### Core Memory Rules

1. **RAII with auto field drops**: The compiler automatically drops struct
   fields that implement `Drop`, after the struct's own `drop()` method returns.
   `drop()` methods should **not** explicitly call `drop()` on fields — doing so
   causes a double-drop.  `drop()` is for cleaning up non-field resources
   (pointers, file descriptors).

2. **Move semantics**: `move x` clears the source variable's drop flag.  At
   scope exit, destructors only fire if the drop flag is `true (1)`.

3. **Borrows are enforced, raw pointers are not**: A parameter of type
   `&T` or `&mut T` is a borrow origin.  The escape checker proves that any
   pointer a function returns derives only from its `&`-marked parameters or
   from independent data (globals, heap values, raw-pointer pointees);
   returning a reference to a local is a compile error, as is storing a
   reference to a local into a global.  Raw pointers (`T*`, `const T*`,
   `mut T*`) create no borrow constraints and are never automatically
   dropped — they are non-owning escape hatches for FFI and address
   arithmetic.

4. **Defer stack**: `defer` statements execute in **LIFO** order at scope exit,
   including before `return`.

### 8.1 Implicit Copy vs Move and Place Granularity

`TypeProperties {is_copy, needs_drop}` is the single source of truth:

- **Copy** (bitwise, `copy_from` — no invalidation): primitives (`bool`, `i8`/`i16`/`i32`/`i64`/`u8`…/`f32`/`f64`/`char`), raw pointers (`T*`), and structs/enums whose every field/variant payload is Copy. `T x = y` copies.
- **Move** (destructive, `move_out` — invalidates source): `String`, `Vec<T>`, `HashMap<K,V>`, `HashSet<K>`, `Deque<T>`, `Box<T>`, `Rc<T>`, `File`, etc., and any struct containing them. `T x = y` moves; use `T x = move y` to be explicit. LSP shows implicit `move`/`copy` inlay hints before the source place.

```silver
String a = String.from_str("hello");
String b = a;          // implicit move — LSP shows `move` before `a`, `a` is uninitialized after
String c = move b;     // explicit move — no inlay hint (already `move`)
i64 x = 42;
i64 y = x;             // copy — LSP shows `copy` before `x`, `x` stays live
```

Moves are field-granular via `Place {local, projections}` with `is_prefix_of`/`overlaps`:

An owned field cannot be consumed while any containing value has its own
`Drop` implementation. This includes explicit moves, implicit transfers,
by-value calls, returns, and explicit field drops. The containing destructor
must retain its owned fields. Move the whole containing value instead, or
borrow the field. Copy fields can still be copied from a value with `Drop`.

```silver
struct Zoo { String cage; String keeper; }
Zoo zoo;
zoo.cage = String.from_str("a");
zoo.keeper = String.from_str("b");
String cage = move zoo.cage; // moves only `zoo.cage`; `zoo.keeper` stays live
zoo.cage = String.from_str("c"); // reinit — `zoo.cage` is live again
// move zoo  would invalidate both fields; `zoo.cage` vs `zoo.keeper` are disjoint
```

`v[i]` is an `Index` place. Index is conservative: `v` overlaps `v[i]`, and `v[i]` overlaps `v[j]` (different indices) — borrow/move of `v[i]` conflicts with `v[j]` and with `v`.

```silver
Vec<String> v; v.push(String.from_str("a")); v.push(String.from_str("b"));
String s = move v[0]; // conservatively moves the Index place; later v[1] access is rejected
// v[0] = String.from_str("c"); // reinit `v[0]` before indexed access
```

Borrow origins interact with places: `&x` vs `move x` conflicts; `&x.a` vs `move x.b` is allowed (disjoint fields); `&mut x.a` vs `move x.a` conflicts.

## 9. Concurrency & Threading

Threads are 1:1 OS threads running on the pure-Silver static runtime
(no libc; `--static-runtime` is the default and only mode).

### Launching Tasks

`launch f(args...)` spawns a thread that calls `f`, returning a `Task<T>`
handle where `T` is `f`'s return type.  Every argument is **moved** into
 the child thread.

```silver
Task<i64> t = launch compute(21);   // Task<i64>: the callee's return type
Task<void> u = launch worker();     // void tasks are fine
```

The handle is a plain value with no `Drop`: dropping an unwaited `Task`
does **not** join it — the thread keeps running and is reaped by the
exit-time join-all.  Joining is always explicit:

```silver
i64 r = wait t;          // join, move out the result, consume the handle
defer wait t;            // join on every exit path of the scope
```

A second `wait` on the same handle is a move error.

The result type `T` need not be `Send`: `wait` joins the worker before exposing
the result to the caller. Launch arguments still must be `Send` because they
are accessed concurrently with the launching thread.

### Send gate

Because launch arguments are moved across the thread boundary, every
argument type must be **Send**.  The check is a structural field walk:

- **Send**: primitives (`i64`, `str`, `bool`, …), function values, `Task<T>`
  handles, arrays, `Optional<T>`, tuples; owned containers (`Vec<T>`,
  `Box<T>`, `String`, `Bytes`, `HashMap<K, V>`) iff their owned type
  arguments are Send; structs/enums whose fields/variant payloads are all
  Send (generic parameters substituted from the instantiation).
- **Not Send** (compile error at the launch site): `Rc<T>` (shared,
  non-atomic refcount), GC heap `Handle`s, raw pointers (`T*`), references
  (`&T`/`&mut T`), slices, and any struct holding an unmarked raw pointer.

### Synchronization primitives

- **`Mutex<T>`** + **`Guard<T>`** (RAII): `g.lock()` returns a guard granting
  access to the value (`get`/`set`/`get_ptr`) and unlocking the underlying
  `RawMutex` when the guard drops — at scope exit, on early return, or when
  moved.  A zero-initialized global `Mutex<T>` is a valid unlocked mutex
  holding a zero value.
- **`RawMutex`**: the low-level futex lock (`lock`/`unlock`/`try_lock`)
  underneath `Mutex<T>` and `Channel<T>`.
- **`Channel<T>`**: thread-safe FIFO with unbounded (`Channel<T>.new()`) and
  bounded (`Channel<T>.bounded(cap)`) modes. `send(T)` moves a value into the
  queue; bounded sends block when the queue is full. `recv()` blocks until it
  can return `Some(value)`, or until a closed, drained channel returns `None`.
  `send()` returns false if the channel is closed. `close()` wakes blocked
  senders and receivers. Call `destroy()` after use to free the queue buffer.
- **`WaitGroup`**: `add(n)` / `done()` / `wait_all()` — a counter that
  blocks `wait_all()` until `done()` brings it to zero.
- **Atomics**: the `__atomic_*` intrinsic layer (`__atomic_load_i64`,
  `__atomic_fetch_add_i64`, …) wrapped by `std.atomic` value types
  (`AtomicI8`/`AtomicI32`/`AtomicI64`/`AtomicBool`).

All futex-based primitives are seq_cst by default.

### Runtime model

- Threads are 1:1 OS threads registered in a fixed-size (64-slot) registry;
  a detached task keeps its slot until a waiter or the exit-time join-all
  reaps it.
- The runtime entry (`std.sys.entry`) is imported automatically and provides
  `_start`; every binary links statically against the pure-Silver runtime —
  there is no libc, and `--dynamic-runtime` no longer exists.

---

## 10. Standard Library & Patterns

### Error Handling

Silver uses explicit return values for recoverable failures. `Optional<T>` and
`Result<T, E>` are payload enums; inspect them with `match` or their predicate
methods before extracting a value. There is no exception mechanism.

| Type | Module | Representation |
|---|---|---|
| `Optional<T>` | `std.optional` | `None` or `Some(T)` |
| `Result<T, E>` | `std.result` | `Ok(T)` or `Err(E)` |
| `Error` | `std.error` | `i64 code`, borrowed `str message` |
| `TypeResult` | `std.rt.types` | `bool ok`, `TypeId id`, `i32 err`, `String msg` |

Typed syscall wrappers in `std.sys.result` return `Result<i64, Error>`.
Their error codes are positive errno values. The former `SysResult` type is
no longer used.

`Optional.is_some()` / `is_none()` and `Result.is_ok()` / `is_err()` borrow the
container. `unwrap()`, `unwrap_err()`, and `unwrap_or(fallback)` consume it and
transfer the selected value. `unwrap()` and `unwrap_err()` abort when called
on the wrong variant. `Result.get_error()` is a consuming alias for
`unwrap_err()` and aborts on `Ok(T)`.

The `value ? fallback` operator also selects a fallback when an `Optional` or
`Result` has no success value; it does not propagate errors to the caller.

`map` and `and_then` consume `Optional` or `Result` and call a function only
on the success/present variant. `Result.map_err` transforms only the error.
Payloads move into callbacks; unselected variants pass through unchanged.
See [stdlib contracts](docs/stdlib-contracts.md) for signatures and examples.

Unrecoverable errors call `abort()` from `std.mem.memory`.

### Output

| Mechanism | How |
|---|---|
| `@print`, `@println`, `@eprintln` | Compiler builtin macro with `{}` formatting. Preferred. |
| `println(str)`, `eprintln(str)` | Plain functions in `std.io`, single NUL-terminated string. |

Formatting `String` preserves its full byte length, including embedded NULs.
Formatting `str` stops at NUL. Character formatting writes UTF-8, replacing
invalid Unicode scalars with U+FFFD. `BufWriter.write_u8` writes a raw byte.

### Checked conversions and hardware DMA

`std.convert.TryFrom<i128>` provides explicit checked integer narrowing and
Unicode scalar conversion through `Result`. It does not change the language's
permissive implicit numeric conversions or ordinary casts.

On Linux, `std.dma.DmaBuffer` owns a DMA-BUF descriptor, supports explicit
DMA-heap allocation and fd import, and brackets scoped CPU access with kernel
coherency ioctls. Device submission and fence waits remain driver-specific.
See [stdlib contracts](docs/stdlib-contracts.md) for ownership and error behavior.

### Building strings

`String.push_display(&value)` appends the `Display` representation of a borrowed
value. It calls the formatter once and leaves the source value available to the
caller. Custom types use the same `Display` implementation as `{}` formatting.
For signed decimal integers, `String.from_i64(value)` constructs a new string.

```silver
import std.string;

i32 main() {
    String message = String.from_str("count=");
    i64 count = 42;
    message.push_display(&count);
    return message.equals("count=42") && count == 42 ? 0 : 1;
}
```

The append preserves the bytes emitted by the formatter, including embedded NUL
bytes. Use `len()` and `as_slice()` for byte content; `to_str()` exposes a
NUL-terminated view. Formatting may allocate when its output exceeds the scratch
buffer or the destination's remaining capacity.

### Borrowing and comparing sequences

`AsSlice<T>` in `std.slice` provides `Slice<T> as_slice(&self)` for generic
borrowers. Vec, Slice, String, and Bytes implement it. Slice, Vec, and Bytes
support content equality and lexicographic comparison through the operator
traits. Ordering requires an element `Compare` implementation. See
[the sequence API](docs/stdlib-sequences.md) for ownership, examples, and hashing
limits.

### Memory Allocation

The allocator (`std.mem.alloc`) provides three tiers:

1. **Libc shims**: `malloc`, `calloc`, `realloc`, `free` — for compatibility.
2. **Frozen C-ABI**: `silver_rt_alloc`, `silver_rt_realloc`, etc.
3. **Typed generic**: `alloc<T>()`, `alloc<T>(count)` — aborts on OOM. Preferred.

### Receiver Convention

| Receiver form | Meaning |
|---|---|
| `&self` / `&mut self` | Shared or mutable borrow of caller-owned state; the borrow is escape-checked |
| `*self` | Raw-pointer receiver; creates no borrow constraint |
| `self` | By-value receiver; follows normal copy or move rules |
| `T* self` | Explicit raw-pointer parameter form |

Constructors return by value: `Vec<T> new() { ... return move v; }`.
Prefer `&T`/`&mut T` receivers for methods that inspect or mutate
caller-owned state; keep `T*` for methods whose state lives on the heap or
behind an FFI boundary.

### Test Framework

```silver
import std.test;

i32 main() {
    test_start("My Tests");
    assert_true(1 + 1 == 2, "basic arithmetic");
    assert_eq_i64(42, answer(), "answer check");
    return done();  // returns failure count
}
```

See `std/test.ag` for: `assert_true`, `assert_false`, `check`, `assert_eq_i64`,
`assert_eq_i32`, `assert_eq_str`, `done`.
