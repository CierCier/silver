# Silver Language Syntax Specification

This document is the authoritative reference for the Silver systems programming
language (`agc`) as implemented by the compiler — every claim is verified against
the compiler source.  Features not yet implemented are explicitly marked as
**Planned** or omitted.

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
9. [Standard Library & Patterns](#9-standard-library--patterns)

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

Identifiers match `[a-zA-Z_][a-zA-Z0-9_]*`.  The identifier `type` is a **soft
keyword** — it behaves as an identifier but is recognized specially at the start
of a top-level item (type alias) or inside a trait (associated type).

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
| `let` | Local variable binding | `mut` | Mutable modifier |
| `const` | Constant declaration (parsed, not enforced) | `if` | Conditional branch |
| `else` | Alternative branch | `while` | Condition loop |
| `for` | Counter or iterator loop | `in` | Iterator element |
| `break` | Loop termination (with optional value) | `continue` | Loop iteration skip |
| `return` | Early return | `defer` | Scope-exit deferred action |
| `import` | Module import directive | `comptime` | Compile-time evaluation |
| `cast` | Type cast declaration/expr | `move` | Ownership transfer |
| `ref` | Reference (aliasing) | `extern` | External symbol binding |
| `pub` | Public visibility modifier | `private` | Private visibility modifier |
| `asm` | Inline assembly block | `macro` | Macro declaration |
| `true` / `false` | Boolean literals | `void` | Empty/unit return type |

**Not keywords** (treated as identifiers): `Self`, `type` (soft keyword), `where`
(not used).  There is no `match` or `fn` keyword.

### Literals

```silver
// Integers — decimal and hexadecimal
i32 dec = 42;
i64 hex = 0x1A2F;

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

> **Note**: Octal (`0o755`) and binary (`0b101010`) integer literals are **not**
> supported.  The lexer has no token paths for them.

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

### Pointers

Pointers are the primary indirection mechanism.

```silver
i32* p_mut;         // mutable pointer to i32
const i32* p_const; // pointer to const i32 (read-only target)
i32** pp_mut;       // double pointer
```

Pointer field access auto-derefs: `p.x` is equivalent to `(*p).x`.

### Arrays

Fixed-size arrays use postfix `[N]` syntax.

```silver
i32[10] fixed_arr;      // 10-element array of i32
u8 buf[512];            // local array
DnsCacheEntry cache[64]; // global array (size must be an integer literal)
```

Slices exist as the `Slice<T>`
library type in `std/slice.ag`.

### Compound & Generic Types

```silver
Vec<i32> numbers;
Map<str, f64> scores;
Optional<str> name;
```

### Function Types

Function types use `ReturnType(ParamTypes...)` syntax.  Tuple types `(T1, T2)`
are **not** supported — parens group a single expression.

```silver
bool(i32, f64) predicate;   // function pointer: takes i32, f64 → returns bool
void() callback;             // function pointer: no params → void
```

---

## 3. Top-Level Declarations (Items)

### Module Imports

```silver
// Single module import
import std.io;
// import parts of a module
import std.io { print, println as pln };

// Module path import with alias
import path.to.graphics as gfx;
```

Imports inline `.ag` source modules or ingest `.agm` binary artifacts.

### Type Aliases

`type` is a soft keyword — recognized at item-start position only.

```silver
type Distance = f64;
pub type Handler = void(i32);
```

### Global & Constant Variables

```silver
str g_app_name = "SilverApp";      // global variable
pub const f64 PI = 3.1415926535;   // const (parsed; immutability not enforced)
```

`pub mut i32 g_counter` is **not** valid — `mut` cannot start a type declaration.

### Functions

Silver uses **C-style syntax** for top-level functions: the return type precedes
the function name.  There is no `fn` keyword for top-level functions.

```silver
// Standard function with generics and where clause
pub T max<T>(T a, T b) where T: Lt<T> {
    if (a < b) { return b; }
    return a;
}

// C-style return type
i32 main() { return 0; }

// Void return
pub void log_message(str msg) {
    @println("[LOG] {}", msg);
}

// External variadic function
pub extern "C" i32 printf(const char* fmt, ...);
```

### Struct Definitions

Fields are separated by `;`.  Field visibility modifiers (`pub`) on individual
fields are **not** parsed — all fields are implicitly private.

```silver
pub struct Point<T> {
    T x;
    T y;
}

struct Buffer {
    i32* data;
    i32 length;
    i32 capacity;
}
```

### Enum Definitions

Variants are separated by `;` (**not** `,`).  Discriminants use `=` (not `:`).
All three variant kinds are supported.

```silver
// Unit enum with discriminants
pub enum SYSCALL {
    READ = 0;
    WRITE = 1;
    OPEN = 2;
    CLOSE = 3;
}

// Algebraic data type
pub enum Shape {
    Circle(f64);                          // tuple variant
    Rectangle { f64 width; f64 height; }  // struct variant
    Point;                                // unit variant
}
```

### Trait Declarations

Traits define interface contracts, associated types, and optional default method
bodies.  Methods use **C-style syntax**: return type before name.

```silver
pub trait Display {
    str to_string(Self* self);
}

pub trait Iterator<Self> {
    type Item;
    Optional<Item> next(Self* self);
}
```

> **Note**: `Self` is a regular identifier conventionally naming the
> implementing type.  It has no special substitution behavior.

### Implementation Blocks (`impl`)

```silver
struct Vector2 { f64 x; f64 y; }

// Inherent implementation
impl Vector2 {
    pub Vector2 new(f64 x, f64 y) {
        Vector2 v = { .x = x, .y = y };
        return move v;
    }

    pub f64 magnitude(Vector2* self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Custom cast block
impl Vector2 {
    pub cast f64(Vector2 self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Trait implementation
impl Display for Vector2 {
    fn to_string(Vector2* self) -> str { return "Vector2"; }
}
```

### External Declarations (`extern`)

```silver
extern "C" f32 sinf(f32 x);

extern "C" {
    i32 open(const char* path, i32 flags);
    i32 close(i32 fd);
    i32 errno;    // mutable extern variables not supported (no `mut` keyword)
}
```

Supported linkage specs: `"C"`, `"Silver"`, `"system"`, `"Rust"`, `"cdecl"`,
`"stdcall"`, `"fastcall"`.

### Attributes & Metadata

```silver
#[link("pthread")]
#[link("m")]

#[link_name("native_c_pow")]
extern "C" f64 c_pow(f64 base, f64 exp);
```

`@attr(...)` is **not** attribute syntax — `@` is for expression macro calls
(`@println`, `@size`, etc.).

### Macro Definitions

Macro definitions parse but are **not expanded** — only built-in macros
(`@println`, `@size`, `@align`, `@hash`, `@memcpy`, `@memset`, `@memmove`) work.

```silver
macro swap(a, b) {
    let temp = a;
    a = b;
    b = temp;
}
```

---

## 4. Statements & Block Scopes

### Variable Declarations

Silver uses **C-style syntax**: type before name.  The `let` keyword is reserved
(recognized by the lexer) but the bootstrap parser does **not** parse `let`
statements — only C-style declarations work.

```silver
i32 x = 42;           // C-style declaration (idiomatic)
f64 y;                // uninitialized (allowed; drop-flag caveats apply)
```

Pattern destructuring (`let (a, b) = ...`) is **not** supported.

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
i32[3] arr = { 1, 2, 3 };
```

> **Note**: `[1, 2, 3]` bracket syntax and `(100, "OK")` tuple literals are
> **not** supported.  Array and struct initialization uses `{ ... }` braces.

### Binary Expressions

| Category | Operators | Example |
|---|---|---|
| Arithmetic | `+`, `-`, `*`, `/`, `%` | `a + b * c` |
| Bitwise | `&`, `\|`, `^`, `<<`, `>>` | `(mask & 0xFF) << 4` |
| Comparison | `==`, `!=`, `<`, `>`, `<=`, `>=` | `x >= 0 && x < length` |
| Logical | `&&`, `\|\|` | `is_valid && !is_expired` |
| Range | `..` | `0..10` |

> **Note**: `..=` (inclusive range) does **not** exist.  Range `..` is at the
> relational precedence level (alongside `<`/`>`), not a separate level.

All binary operands must have matching types — e.g., `u64 << 8` fails;
use `(u64)8`.

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

`->` is **not** an expression operator — it is used only for function pointer
return types (`void() callback`).

### Ownership & Move Expressions

```silver
Buffer b1 = create_buffer();
Buffer b2 = move b1;    // b1's drop flag cleared; only b2 drops
```

**Reference aliases** are created with `&var` (address-of).  The `ref` keyword
is reserved by the lexer but the bootstrap parser does **not** parse `ref` as
an expression — use `&` to create pointers.

```silver
Buffer* r1 = &b2;     // pointer to b2 (auto-derefs on field access)
```

`ref mut` is **not** a distinct syntax.

### Cast & Conversion Expressions

```silver
f64 float_val = (f64)int_val;     // primitive numeric cast
void* raw_ptr = (void*)buffer;    // pointer cast
f64 magnitude = (f64)p;           // custom struct cast (triggers cast block)
```

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

### Field Access & Indexing

```silver
f64 x = point.x;          // direct field access
Point* p_ptr = &point;
f64 y = p_ptr.y;          // auto-deref: equivalent to (*p_ptr).y
i32 item = arr[0];        // array/container index (may invoke __index_get)
arr[1] = 100;             // index write (may invoke __index_set)
```

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

#### Match (Planned)

`match` is **not implemented**.  An `ExpressionKind::Match` AST node exists but
is not produced by the parser — no `match` keyword exists.  Use `if`/`else`
chains or the `Optional`/`Result` methods instead.

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

i64 struct_size = @size(Vector2);
i64 struct_align = @align(Vector2);
u64 obj_hash = @hash(my_object);

@memcpy(dst_ptr, src_ptr, bytes_count);
@memset(dst_ptr, 0, bytes_count);
@memmove(dst_ptr, src_ptr, bytes_count);
```

> **Note**: Only `@name(...)` syntax works.  `name!(...)` syntax is **not**
> parsed — the `!` token is only used as logical NOT.

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

> **Changes from previous version**: `->`, `ref`, `::` removed (not expression
> operators).  `..=` removed (does not exist).  Range `..` is at the relational
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

3. **Pointer immunity**: Raw pointers (`T*`) and reference aliases (`ref T`)
   are never automatically dropped — they are non-owning views.

4. **Defer stack**: `defer` statements execute in **LIFO** order at scope exit,
   including before `return`.

---

## 9. Standard Library & Patterns

### Error Handling

Silver uses tagged structs — there is no exception mechanism.

| Type | Module | Fields |
|---|---|---|
| `Optional<T>` | `std.optional` | `bool present`, `T thing` |
| `Result<T, E>` | `std.optional` | `bool ok`, `T value`, `E error` |
| `SysResult` | `std.sys.result` | `bool ok`, `i64 value`, `i64 errno` |
| `TypeResult` | `std.rt.types` | `bool ok`, `TypeId id`, `i32 err`, `String msg` |

Unrecoverable errors call `abort()` from `std.mem.memory`.

### Output

| Mechanism | How |
|---|---|
| `@print`, `@println`, `@eprintln` | Compiler builtin macro with `{}` formatting. Preferred. |
| `println(str)`, `eprintln(str)` | Plain functions in `std.io`, single NUL-terminated string. |

### Memory Allocation

The allocator (`std.mem.alloc`) provides three tiers:

1. **Libc shims**: `malloc`, `calloc`, `realloc`, `free` — for compatibility.
2. **Frozen C-ABI**: `silver_rt_alloc`, `silver_rt_realloc`, etc.
3. **Typed generic**: `alloc<T>()`, `alloc<T>(count)` — aborts on OOM. Preferred.

### Receiver Convention

| Receiver | When |
|---|---|
| `T* self` | Mutating or inspecting state |
| `T self` | Consuming transfer or copy semantics |

Constructors return by value: `pub Vec<T> new() { ... return move v; }`.

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
