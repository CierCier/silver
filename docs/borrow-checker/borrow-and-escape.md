# Borrowing & Escape Analysis

Silver provides first-class reference types (`&T` and `&mut T`) combined with static escape analysis to prevent dangling references without runtime garbage collection.

---

## 1. Reference Types & Syntax

Silver distinguishes between owned values, raw pointers, and tracked references:

| Type Syntax | Meaning | Ownership | Mutability | Nullable? |
|---|---|---|---|---|
| `T` | Owning Value | Yes (dropped on exit) | Value mutable if binding is mutable | No |
| `&T` | Immutable Reference | No (borrowed view) | Read-only | No |
| `&mut T` | Mutable Reference | No (borrowed view) | Read-write | No |
| `T*` | Raw Pointer | No (manual management) | Read-write | Yes (`(T*)0`) |

### Example Usage

```silver
struct Point {
    f64 x;
    f64 y;
}

// Immutable borrow: caller retains ownership, callee reads
f64 length_squared(&Point p) {
    return p.x * p.x + p.y * p.y;
}

// Mutable borrow: caller retains ownership, callee mutates
void translate(&mut Point p, f64 dx, f64 dy) {
    p.x = p.x + dx;
    p.y = p.y + dy;
}
```

---

## 2. The Escape Checker (`semantic/escape_check.rs`)

The escape checker verifies that **references never outlive the data they point to**.

### Borrow Origin Classification (`Source`)

Every reference expression is tagged with a borrow origin:

1. **`Source::Local` (Stack-Bound)**:
   - Created by borrowing a function-local variable (`&local`) or a by-value parameter (`&val_param`).
   - **Constraint**: Must not escape the function frame.
2. **`Source::Escapable { origins }` (Caller-Bound)**:
   - Created by borrowing from an incoming reference parameter (`&ref_param`, `&mut ref_param`, or `&self` / `&mut self`).
   - Carries the set of parameter names / indices from which it derived.
   - **Constraint**: May be returned to the caller because the caller owns the underlying referent.
3. **Independent Origins**:
   - Dereferences of raw pointers (`(*ptr)`), globals (`&GLOBAL_VAR`), and heap allocations.
   - **Constraint**: Allowed to escape as they outlive the local function frame.
4. **`Source::Opaque`**:
   - Conservative fallback for complex unclassified expressions.

```mermaid
flowchart TD
    Borrow[Borrow Expression '&expr'] --> Check{What is 'expr'?}
    Check -->|Local stack variable| Local[Source::Local\nCANNOT ESCAPE]
    Check -->|Reference Parameter| Escapable[Source::Escapable\nBound to Caller Lifetime]
    Check -->|Global or Pointer Pointee| Indep[Independent Origin\nCan Safely Escape]
```

---

## 3. Escape Rules & Invariants

### Rule 1: Local Stack References Cannot Escape via `return`
A reference to a local stack variable cannot be returned:

```silver
i64* bad_borrow() {
    i64 local = 42;
    return &local; // COMPILE ERROR: cannot return reference to local variable 'local'
}
```

### Rule 2: References Cannot Escape via Global Assignment
Assigning a local borrow into a global variable is rejected:

```silver
i64* GLOBAL_PTR;

void bad_escape() {
    i64 x = 100;
    GLOBAL_PTR = &x; // COMPILE ERROR: cannot store reference to local variable in global
}
```

### Rule 3: Parameter Origin Propagation
Returning a reference derived from an input parameter is valid and records the parameter origin for caller-side validation:

```silver
struct Pair {
    i64 left;
    i64 right;
}

// Valid: &pair is a caller-owned parameter
i64* get_left(&Pair pair) {
    return &pair.left;
}

// Method receiver propagation:
impl Pair {
    i64* right_ref(&Pair self) {
        return &self.right;
    }
}
```

### Rule 4: Field & Index Borrow Transparency
Borrowing a field (`&p.field`) or an array element (`&arr[i]`) preserves the borrow origin of the parent container:
- `&local_struct.field` $\rightarrow$ `Source::Local`
- `&param_ref.field` $\rightarrow$ `Source::Escapable { "param_ref" }`

---

## 4. Active Borrow Conflict Checker (`semantic/borrow_check.rs`)

The active borrow checker enforces the fundamental aliasing and mutability rule:

$$\text{For any memory location } P \text{ at any program point}: \quad (\text{Any number of } \&P) \oplus (\text{Exactly one } \&\text{mut } P)$$

A borrow is **active** from its creation until the last use of the reference
binding that holds it, at which point the loan expires (see §5). This is
checked at statement granularity, so code after the borrow's last use is
unconstrained.

### Key Rules & Invariants

1. **Shared Borrows (`&T`)**:
   - Multiple active shared borrows (`&x`, `&p.left`) may coexist simultaneously.
   - Taking `&mut x` while any shared borrow of `x` is active is rejected with a multi-span error.
2. **Exclusive Borrows (`&mut T`)**:
   - Only one active mutable borrow of a path can exist at a time.
   - Taking `&x` or another `&mut x` while `&mut x` is active is rejected.
   - Reading or mutating `x` directly while `&mut x` is active is rejected.
3. **Disjoint Field Borrows**:
   - Borrows on disjoint struct fields (`&mut p.left` and `&mut p.right` or `&mut n.pair.left` and `&mut n.pair.right`) target independent paths and are permitted simultaneously.
   - Borrowing a parent path (`&p`) conflicts with borrows on child fields (`&mut p.left`).
4. **Move Protection**:
   - Moving a variable (`move x`) or field (`move p.left`) while any borrow is active on that path is rejected with `cannot move out of '...' because it is borrowed`.
5. **Raw Pointer Exemption**:
   - Raw C-style pointers (`T*`) are unchecked views that bypass borrow checking, preserving zero-overhead C FFI and low-level allocator ergonomics.
6. **Reference Parameter & Reborrow Ergonomics**:
   - Passing an existing mutable reference `m: &mut T` automatically reborrows (`&mut *m`), suspending `m` until the callee/reborrow finishes.
   - Method receivers (`&mut Self self`) provide exclusive access to mutate fields without self-conflicting.

---

## 5. Non-Lexical Lifetimes (NLL): Loans Expire at Last Use

A borrow's loan ends at the statement boundary *after* its binding's last
use, not at the end of the enclosing block. Short-lived references no longer
need artificial `{ ... }` sub-blocks to release the root for later mutation or
moves:

```silver
struct Point { i64 x; i64 y; }

i64 read_x(&Point p) {
    return p.x;
}

void example() {
    Point pt;
    pt.x = 10;
    pt.y = 20;

    &Point r = &pt;
    i64 v = read_x(r);   // last use of 'r' — the loan ends here

    pt.x = 100;          // OK — 'r' is no longer live, no { ... } needed
}
```

### How Last-Use Is Tracked

The checker walks each block statement-by-statement and records which
reference bindings the statement uses (any expression position: reads, field
access, call/method-call arguments, derefs, moves, control-flow conditions,
macro arguments). A loan whose borrower was last used in a previous statement
is removed before the next statement is checked, so later code sees no
conflict.

### Boundary Cases

1. **Unused reference bindings stay live for the whole block**:
   `&Point r = &pt;` followed by `pt.x = 100;` is still rejected, because the
   checker never observes a last use — `r` may still be read later.

   ```silver
   &Point r = &pt;       // never used again
   pt.x = 100;           // COMPILE ERROR: cannot assign to 'pt.x' because it is borrowed
   ```

2. **Reference parameters never expire**: a `&T` / `&mut T` parameter's loan
   lives for the entire function — the caller still holds the borrow, so the
   callee cannot assume it ended.

3. **Reborrow chains survive expiry**: the source binding's loan is kept
   alive by the statement that reborrows from it, so the chain stays tracked:

   ```silver
   &Point r = &pt;
   read_x(r);            // last direct use of 'r'
   &Point m = &*r;       // reborrow keeps the loan on 'pt' alive through 'm'
   pt.x = 100;           // COMPILE ERROR: 'm' is still live (used below)
   read_x(m);            // 'm' is used after the mutation, so the loan had not expired
   ```

4. **Conflicts before the last use are unchanged**: taking `&mut pt` while a
   shared `r` still has uses ahead of it is still rejected, as are
   mutations/moves of a root whose borrow has not yet expired.
