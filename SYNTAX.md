# Silver Language Syntax Specification

This document provides an exhaustive reference of the Silver systems programming language (`agc`) syntax, lexical structure, type system, top-level items, statements, expressions, operator precedence, protocol hooks, and memory ownership semantics.

---

## Table of Contents

1. [Lexical Structure & Tokens](#1-lexical-structure--tokens)
   - [Comments](#comments)
   - [Identifiers](#identifiers)
   - [Keywords](#keywords)
   - [Literals](#literals)
2. [Type System & Type Expressions](#2-type-system--type-expressions)
   - [Primitive Types](#primitive-types)
   - [Pointers & References](#pointers--references)
   - [Arrays & Slices](#arrays--slices)
   - [Compound & Generic Types](#compound--generic-types)
   - [Function & Tuple Types](#function--tuple-types)
3. [Top-Level Declarations (Items)](#3-top-level-declarations-items)
   - [Module Imports](#module-imports)
   - [Type Aliases](#type-aliases)
   - [Global & Constant Variables](#global--constant-variables)
   - [Functions & Variadics](#functions--variadics)
   - [Struct Definitions](#struct-definitions)
   - [Enum Definitions](#enum-definitions)
   - [Trait Declarations](#trait-declarations)
   - [Implementation Blocks (`impl`)](#implementation-blocks-impl)
   - [External Declarations (`extern`)](#external-declarations-extern)
   - [Attributes & Metadata](#attributes--metadata)
   - [Macro Definitions](#macro-definitions)
4. [Statements & Block Scopes](#4-statements--block-scopes)
   - [Variable Declarations](#variable-declarations)
   - [Assignments & Compound Assignments](#assignments--compound-assignments)
   - [Defer Cleanup Statements](#defer-cleanup-statements)
   - [Control Flow Statements](#control-flow-statements)
   - [Block Statements](#block-statements)
5. [Expression Grammar & Taxonomy](#5-expression-grammar--taxonomy)
   - [Primary & Path Expressions](#primary--path-expressions)
   - [Literals & Aggregate Initializers](#literals--aggregate-initializers)
   - [Binary Expressions](#binary-expressions)
   - [Unary & Postfix Expressions](#unary--postfix-expressions)
   - [Ownership & Move Expressions](#ownership--move-expressions)
   - [Cast & Conversion Expressions](#cast--conversion-expressions)
   - [Compile-Time (`comptime`) Expressions](#compile-time-comptime-expressions)
   - [Function Calls & Method Invocations](#function-calls--method-invocations)
   - [Field Access & Indexing](#field-access--indexing)
   - [Control Flow Expressions](#control-flow-expressions)
   - [Inline Assembly (`asm`)](#inline-assembly-asm)
   - [Macro Invocation Expressions](#macro-invocation-expressions)
6. [Operator Precedence & Associativity](#6-operator-precedence--associativity)
7. [Operator Overloading & Custom Protocols](#7-operator-overloading--custom-protocols)
8. [Memory Management & Ownership Model](#8-memory-management--ownership-model)

---

## 1. Lexical Structure & Tokens

Silver source code files use UTF-8 encoding and standard `.ag` extension.

### Comments

```silver
// Single-line comment running to end of line

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

| Keyword | Description | Keyword | Description |
|---|---|---|---|
| `struct` | Struct definition | `enum` | Enum definition |
| `impl` | Implementation block | `trait` | Trait definition |
| `fn` | Function definition | `let` | Local variable binding |
| `mut` | Mutable binding/parameter | `const` | Constant declaration |
| `if` | Conditional branch | `else` | Alternative branch |
| `while` | Condition loop | `for` | Counter or iterator loop |
| `in` | Iterator sequence element | `break` | Loop termination |
| `continue` | Loop iteration skip | `return` | Early return from function |
| `defer` | Scope-exit deferred action | `import` | Module import directive |
| `comptime` | Compile-time evaluation | `cast` | Type cast declaration/expr |
| `move` | Ownership transfer expression | `ref` | Reference creation |
| `extern` | External symbol binding | `pub` | Public visibility modifier |
| `private` | Private visibility modifier | `asm` | Inline assembly block |
| `macro` | Macro declaration | `type` | Type alias declaration |
| `where` | Trait bound constraint | `true` | Boolean true literal |
| `false` | Boolean false literal | `void` | Empty/unit return type |

### Literals

```silver
// Integers (Decimal, Hexadecimal, Binary, Octal)
i32 dec = 42;
i32 hex = 0x1A2F;
i32 bin = 0b101010;
i32 oct = 0o755;

// Floating-Point
f64 pi = 3.1415926535;
f64 sci = 1.0e-5;

// Complex Numbers
c64 c1 = 3.5i;
c64 c2 = 1.0 + 2.0i;

// Strings & Characters
str greeting = "Hello, Silver!\n";
char ch = 'A';
char esc = '\n';

// Booleans
bool active = true;
bool disabled = false;
```

---

## 2. Type System & Type Expressions

Silver features a static type system combining native primitives, raw pointers, explicit references, static arrays, slices, generics, and algebraic data types.

### Primitive Types

| Group | Types | Description |
|---|---|---|
| Signed Integers | `i8`, `i16`, `i32`, `i64`, `i128` | 2's complement signed integers |
| Unsigned Integers | `u8`, `u16`, `u32`, `u64`, `u128` | Unsigned integers |
| IEEE-754 Floats | `f32`, `f64`, `f80` | Single, double, and extended precision floats |
| Complex Numbers | `c32`, `c64`, `c80` | Complex floating-point primitives |
| Strings & Characters | `str`, `char` | UTF-8 string view and 32-bit unicode char |
| Boolean & Void | `bool`, `void` | `true`/`false` boolean and void type |

### Pointers & References

Pointers explicitly manage memory addresses. References declare non-owning references.

```silver
// Mutable pointer
i32* p_mut;

// Const pointer (read-only target)
const i32* p_const;

// Double pointer
i32** pp_mut;

// Reference types
ref i32 r_immut;
ref mut i32 r_mut;
```

### Arrays & Slices

```silver
// Fixed-size array type: [Type; N] or Type[N]
i32[10] fixed_arr;
[f64; 4] vec4;

// Slice type (view over sequence)
[i32] slice_view;
```

### Compound & Generic Types

```silver
// Generic named type
Vec<i32> numbers;
Map<str, f64> scores;
Optional<str> name;

// Namespace-qualified named type
std::io::FileHandle handle;
```

### Function & Tuple Types
// Function pointer signature: ReturnType(ParamTypes...)
bool(i32, f64) predicate;
void() callback;

// Tuple type: (T1, T2, ...)
(i32, str, bool) tuple_val;
```

---

## 3. Top-Level Declarations (Items)

Silver programs consist of top-level items declared at file scope.

### Module Imports

Imports inline `.ag` source modules or ingest precompiled binary `.agm` library artifacts.

```silver
// Single module import
import std.io;

// Qualified selective import with aliasing
import std.io::{println, print as custom_print};

// Module path import with alias
import path.to.graphics as gfx;
```

### Type Aliases

```silver
type Distance = f64;
pub type Handler = void(i32);
```

### Global & Constant Variables

```silver
// Global variable (mutable or immutable)
pub mut i32 g_counter = 0;
str g_app_name = "SilverApp";

// Const variable (evaluated at compile time)
pub const f64 PI = 3.141592653589793;
```

### Functions & Variadics

Functions can be written using standard keyword syntax or C-style type-prefix return syntax.

```silver
// Standard generic function with trait bounds
pub fn max<T>(T a, T b) -> T where T: Lt<T> {
    if (a < b) {
        return b;
    }
    return a;
}

// C-style return type signature (supported idiom)
i32 main() {
    return 0;
}

// Explicit void return
pub void log_message(str msg) {
    @println("[LOG] {}", msg);
}

// External variadic function
pub extern "C" i32 printf(const char* fmt, ...);
```

### Struct Definitions

Structs declare memory layout. Struct fields are stored inline without automated recursive destruction unless explicit `Drop` trait methods are provided.

```silver
pub struct Point<T> {
    pub T x;
    pub T y;
}

struct Buffer {
    i32* data;
    i32 length;
    i32 capacity;
}
```

### Enum Definitions

Silver enums support unit variants with explicit discriminants, tuple variants, and struct variants.

```silver
// Simple enum with explicit discriminants
pub enum SYSCALL {
    READ = 0,
    WRITE = 1,
    OPEN = 2,
    CLOSE = 3,
}

// Algebraic Data Type (ADT) enum
pub enum Shape {
    Circle(f64),                           // Tuple variant
    Rectangle { f64 width, f64 height },  // Struct variant
    Point,                                // Unit variant
}
```

### Trait Declarations

Traits define interface contracts, associated types, and default method bodies.

```silver
pub trait Display {
    fn to_string(Self* self) -> str;
}

pub trait Iterator<Self> {
    type Item;
    Optional<Item> next(Self* self);
}
```

### Implementation Blocks (`impl`)

Implementation blocks declare inherent methods, static constructors, custom cast conversions, and trait implementations.

```silver
struct Vector2 {
    f64 x;
    f64 y;
}

// Inherent implementation block
impl Vector2 {
    // Static constructor method
    pub Vector2 new(f64 x, f64 y) {
        Vector2 v = { .x = x, .y = y };
        return move v;
    }

    // Instance pointer method
    pub f64 magnitude(Vector2* self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Custom Cast implementation block
impl Vector2 {
    pub cast f64(Vector2 self) {
        return sqrt(self.x * self.x + self.y * self.y);
    }
}

// Trait implementation block
impl Display for Vector2 {
    fn to_string(Vector2* self) -> str {
        return "Vector2";
    }
}
```

### External Declarations (`extern`)

External declarations bind native C or system library routines and global variables.

```silver
// Single external function declaration
extern "C" f32 sinf(f32 x);

// External block with linkage specification
extern "C" {
    i32 open(const char* path, i32 flags);
    i32 close(i32 fd);
    mut i32 errno;
}
```

Supported Linkage Specs: `"C"`, `"Silver"`, `"system"`, `"Rust"`, `"cdecl"`, `"stdcall"`, `"fastcall"`.

### Attributes & Metadata

Attributes attach metadata to top-level items or program units using `#[attr(...)]` or `@attr(...)` syntax.

```silver
// Global program link attribute
#[link("pthread")]
#[link("m")]

// Function symbol alias attribute
#[link_name("native_c_pow")]
extern "C" f64 c_pow(f64 base, f64 exp);
```

### Macro Definitions

```silver
macro swap(a, b) {
    let temp = a;
    a = b;
    b = temp;
}
```

---

## 4. Statements & Block Scopes

Statements execute within function bodies and block scopes.

### Variable Declarations

```silver
// Explicit let declaration with type annotation
let mut count: i32 = 0;

// Type inference let binding
let msg = "Hello";

// C-style variable declaration syntax
i32 x = 42;
f64 y; // uninitialized local

// Pattern destructuring assignment
let (a, b) = get_pair();
```

### Assignments & Compound Assignments

```silver
x = 100;
*ptr = 42;
arr[0] = 5;
point.x = 3.14;

// Compound operators
count += 1;
sub -= 5;
total *= 2;
div /= 4;
rem %= 3;
```

### Defer Cleanup Statements

`defer` postpones execution of a statement or block until the current scope exits. Defer blocks execute in strict **Last-In, First-Out (LIFO)** order.

```silver
{
    i32 fd = open("file.txt", 0);
    defer close(fd); // Executed at block exit

    void* buf = malloc(1024);
    defer {
        free(buf); // Executed BEFORE close(fd)
    }

    if (error_condition) {
        return -1; // Defers fire automatically prior to return!
    }
}
```

### Control Flow Statements

```silver
// Early Return
return;
return result_value;

// Loop Break & Continue
break;
break break_value;
continue;
```

### Block Statements

Blocks create child scopes for scope-bound drop flag lifetime tracking.

```silver
{
    i32 inner_var = 10;
    // scope exit destroys local resources
}
```

---

## 5. Expression Grammar & Taxonomy

Silver treats almost all syntactic operations as expressions that return values.

### Primary & Path Expressions

```silver
// Identifiers and Scope Paths
my_var;
std::io::stdin;
```

### Literals & Aggregate Initializers

```silver
// Primitives
42;
3.14;
"Hello";

// C-style Designated Struct Initializer
Point p = { .x = 10.0, .y = 20.0 };

// Positional Initializer
Point p2 = { 10.0, 20.0 };

// Array & Tuple Literals
i32[3] arr = [1, 2, 3];
(i32, str) pair = (100, "OK");
```

### Binary Expressions

| Category | Operators | Example Syntax |
|---|---|---|
| Arithmetic | `+`, `-`, `*`, `/`, `%` | `a + b * c` |
| Bitwise | `&`, `\|`, `^`, `<<`, `>>` | `(mask & 0xFF) << 4` |
| Comparison | `==`, `!=`, `<`, `>`, `<=`, `>=` | `x >= 0 && x < length` |
| Logical | `&&`, `\|\|` | `is_valid && !is_expired` |
| Range | `..`, `..=` | `0..10`, `1..=100` |

### Unary & Postfix Expressions

```silver
+val;       // Unary plus
-val;       // Unary minus (negation)
!flag;      // Logical NOT
~mask;      // Bitwise NOT
&var;       // Address-of
*ptr;       // Pointer dereference
++i;        // Prefix increment
--i;        // Prefix decrement
i++;        // Postfix increment
i--;        // Postfix decrement
```

### Ownership & Move Expressions

The `move` keyword explicitly transfers ownership of a variable, clearing its stack frame drop flag to zero (`0`) so the original location will not execute a destructor on scope exit.

```silver
Buffer b1 = create_buffer();

// Move transfers resource ownership from b1 to b2
Buffer b2 = move b1; 
```

The `ref` keyword explicitly captures non-owning references:
```silver
ref Buffer r1 = ref b2;
ref mut Buffer r2 = ref mut b2;
```

### Cast & Conversion Expressions

Silver supports both C-style explicit cast parentheses and the `cast(...)` operator keyword.

```silver
// Primitive numeric casts
i32 int_val = 42;
f64 float_val = (f64)int_val;

// Pointer conversions
void* raw_ptr = (void*)buffer;
i32* int_ptr = (i32*)raw_ptr;

// Custom struct cast invocation (triggers `impl T { cast TargetType(T self) }`)
Point p = { .x = 3.0, .y = 4.0 };
f64 magnitude = (f64)p;
```

### Compile-Time (`comptime`) Expressions

`comptime` expressions force evaluation during compilation.

```silver
const i32 BLOCK_SIZE = comptime (1024 * 64);
i32 folded_val = comptime (i32)3.99; // folded to 3 at compile time
```

### Function Calls & Method Invocations

```silver
// Direct function call
i32 res = add(10, 20);

// Instance method call (passes receiver pointer automatically)
f64 dist = point.magnitude();

// Associated static method call
Vector2 v = Vector2.new(1.0, 2.0);
```

### Field Access & Indexing

```silver
// Direct field access
f64 x = point.x;

// Automatic pointer field access dereference
Point* p_ptr = &point;
f64 y = p_ptr.y; // equivalent to (*p_ptr).y

// Array/Slice/Container Indexing
i32 item = arr[0];
arr[1] = 100; // invokes __index_set if overloaded
```

### Control Flow Expressions

#### If-Else Expressions

```silver
i32 max_val = if (a > b) { return a; } else { return b; };
```

#### Loop Expressions

```silver
// Condition While Loop
while (i < 10) {
    i = i + 1;
}

// C-style For Loop
for (i32 i = 0; i < 10; i = i + 1) {
    @println("i = {}", i);
}

// Iterator For-In Loop
for item in container {
    @println("item = {}", item);
}
```

#### Match Expressions

`match` matches pattern arms against values or enum variants.

```silver
match shape {
    Shape::Circle(radius) => {
        @println("Circle with radius {}", radius);
    },
    Shape::Rectangle { width, height } => {
        @println("Rectangle {} x {}", width, height);
    },
    Shape::Point => {
        @println("Point");
    },
    _ => {
        @println("Other shape");
    }
}
```

### Inline Assembly (`asm`)

Inline assembly allows embedding target architecture assembly instructions directly into function bodies.

```silver
// Simple inline assembly
asm("nop");

// Inline assembly with input operand expressions
i32 syscall_num = 1;
asm("mov x0, {}", [syscall_num]);
```

### Macro Invocation Expressions

Builtin macros use `@name(...)` or `name!(...)` syntax.

```silver
// Formatting & Printing Macros
@print("Value: {}", x);
@println("Formatted {} {}", val1, val2);
@eprintln("Error: {}", err_msg);

// Memory & Layout Introspection Macros
i64 struct_size = size!(Vector2);
i64 struct_align = align!(Vector2);
u64 obj_hash = hash!(my_object);

// Low-level Memory Operations
memcpy!(dst_ptr, src_ptr, bytes_count);
memset!(dst_ptr, 0, bytes_count);
memmove!(dst_ptr, src_ptr, bytes_count);
```

---

## 6. Operator Precedence & Associativity

The following table summarizes Silver operator precedence from highest (1) to lowest (14).

| Priority | Operator | Description | Associativity |
|---|---|---|---|
| 1 | `()` `[]` `.` `->` `::` `++` `--` (postfix) | Postfix & Field access | Left-to-Right |
| 2 | `+` `-` `!` `~` `*` `&` `++` `--` `move` `ref` `comptime` `(Type)` | Unary prefix & Casts | Right-to-Left |
| 3 | `*` `/` `%` | Multiplicative | Left-to-Right |
| 4 | `+` `-` | Additive | Left-to-Right |
| 5 | `<<` `>>` | Bitwise Shift | Left-to-Right |
| 6 | `&` | Bitwise AND | Left-to-Right |
| 7 | `^` | Bitwise XOR | Left-to-Right |
| 8 | `\|` | Bitwise OR | Left-to-Right |
| 9 | `..` `..=` | Range Operators | Left-to-Right |
| 10 | `==` `!=` `<` `>` `<=` `>=` | Relational & Equality | Left-to-Right |
| 11 | `&&` | Logical AND | Left-to-Right |
| 12 | `\|\|` | Logical OR | Left-to-Right |
| 13 | `=` `+=` `-=` `*=` `/=` `%=` | Assignment | Right-to-Left |
| 14 | `,` `;` | Expression separators | Left-to-Right |

---

## 7. Operator Overloading & Custom Protocols

Silver maps primitive operators to named internal functions. Custom types implement these double-underscore methods in `impl` blocks or standard traits to overload operators.

### Operator Protocol Mapping Table

| Operator | Internal Protocol Method | Standard Trait |
|---|---|---|
| `a + b` | `fn __add(Self self, Other b) -> Self` | `Add<A, B>` |
| `a - b` | `fn __sub(Self self, Other b) -> Self` | `Sub<A, B>` |
| `a * b` | `fn __mul(Self self, Other b) -> Self` | `Mul<A, B>` |
| `a / b` | `fn __div(Self self, Other b) -> Self` | `Div<A, B>` |
| `a % b` | `fn __mod(Self self, Other b) -> Self` | `Mod<A, B>` |
| `-a` | `fn __neg(Self self) -> Self` | `Neg<A>` |
| `!a` | `fn __not(Self self) -> Self` | — |
| `~a` | `fn __bitnot(Self self) -> Self` | — |
| `a == b` | `fn __eq(Self self, Self b) -> bool` | `Eq<A>` |
| `a != b` | `fn __ne(Self self, Self b) -> bool` | `Ne<A>` |
| `a < b` | `fn __lt(Self self, Self b) -> bool` | `Lt<A>` |
| `a > b` | `fn __gt(Self self, Self b) -> bool` | `Gt<A>` |
| `a <= b` | `fn __le(Self self, Self b) -> bool` | `Le<A>` |
| `a >= b` | `fn __ge(Self self, Self b) -> bool` | `Ge<A>` |
| `a & b` | `fn __bitand(Self self, Self b) -> Self` | — |
| `a \| b` | `fn __bitor(Self self, Self b) -> Self` | — |
| `a ^ b` | `fn __bitxor(Self self, Self b) -> Self` | — |
| `a << b` | `fn __shl(Self self, Shift b) -> Self` | — |
| `a >> b` | `fn __shr(Self self, Shift b) -> Self` | — |
| `c[i]` (read) | `fn __index_get(Self* self, i64 i) -> Item` | `IndexedAccess<Container>` |
| `c[i] = v` (write) | `fn __index_set(Self* self, i64 i, Item v) -> void` | `IndexedAccess<Container>` |
| `(TargetType) val` | `cast TargetType(Self self) -> TargetType` | Custom `cast` block |

### Iterator Protocol Example

`for item in container` syntax lowers to the `IntoIterator` and `Iterator` protocol methods:

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
            self.current = self.current + 1;
            return res;
        }
        return Optional<i32>.none();
    }
}
```

---

## 8. Memory Management & Ownership Model

Silver employs a stack-machine resource tracking architecture based on **explicit move semantics** and **drop flags**.

### Core Memory Rules

1. **RAII Teardown**: Local stack-allocated value variables are tracked by LLVM code generation using a 1-bit boolean drop flag (`{var}.drop`).
2. **Move Invalidation**: `move x` sets `{x}.drop = 0`. Upon scope exit, destructors only execute if `{var}.drop == 1`.
3. **Explicit Field Cleanups**: Struct destructors must explicitly invoke `drop()` on inner managed resource fields; field destruction is **not** automatically recursive.
4. **Pointer Immunity**: Raw pointers (`T*`) and reference views (`ref T`) do not own resources and are never automatically dropped.
5. **Defer Stack Execution**: Defer statements are pushed onto a LIFO execution stack for the current scope depth and fire before any function return or block exit.
