# Ownership & Move Semantics

Silver pairs explicit ownership transfers with a compile-time drop-flag stack and a flow-sensitive use-after-move checker. This document grounds each claim in the current compiler source.

> **Scope**: no behavior change — Phase 0 freeze documentation. For borrow escape, see `borrow-and-escape.md`; for `Send`, see `thread-safety-send.md`. Index at `borrow-checker/README.md` is unchanged.

---

## 1. What Is an Owning Value?

A value **owns** a resource when its type — or any type transitively contained in it — implements `Drop`.

```silver
struct FileHandle {
    i32 fd;
}

impl Drop<FileHandle> for FileHandle {
    void drop(FileHandle* self) {
        if (self.fd >= 0) { sys_close(self.fd); }
    }
}
```

Only owning values are tracked. The checker computes this once per program:

* `Facts::build` in `bin/agc/src/semantic/move_check.rs:156-215` collects `drop_owners` (every `impl Drop<T>`) and `struct_fields` (field type maps), plus `value_receivers` and `value_args`.
* `MoveChecker::is_tracked` at `move_check.rs:330-346` returns `true` for a type if it is a `Task` handle, a direct `drop_owners` member, or a struct whose any field is transitively `is_tracked`. Generic-typed bindings (`T x` inside a generic) are *not* tracked in v1 (`move_check.rs:18-20`).
* Pointer and reference types are **never** owning — `Facts::is_view_type` at `move_check.rs:217-223` matches `TypeKind::Pointer(_)` and `TypeKind::Reference(_)` (i.e. `T*`, `&T`, `&mut T`). `AGENTS.md §6.2` calls these "non-owning views … never automatically dropped."

```silver
FileHandle h;
h.fd = sys_open("/tmp/a", 0);
FileHandle* p = &h;   // view — no drop flag, never moved
&FileHandle r = &h;   // view — same
```

Function pointers and `str` primitives follow the same rule.

**Automatic field cascading** (`AGENTS.md §6.1`): after a struct's `drop(self)` returns, the compiler cascades to drop each `Drop`-typed field automatically. `drop` bodies only free non-field resources (heap buffers, fds) — do not manually `drop` fields.

**Per-field flags** (`AGENTS.md §6.6`, `codegen/llvm_ir/scope.rs:754-820`): each `Drop`-typed field `f` of a struct local gets its own `i1` flag (distinct from the variable's top-level `.drop` flag), initialized `false` ("no live value yet"), set to `true` on field assignment / struct init / by-value params. Scope-exit cascade checks per-field flags, so uninitialized fields are never destructed; overwriting `x.f = y` releases the old `f`.

**Enum payloads** (`AGENTS.md §6.7`, `scope.rs:551-752`): `Res.Ok(move i)` — constructing a variant with an owned payload *requires* `move`; a bare `Res.Ok(i)` is a compile error. Enums **without** a `Drop` impl get a tag-aware payload cascade at scope exit (active variant's `Drop` payload is dropped, so a never-unwrapped `Result<Owned,E>` does not leak). Extract with `move` binding — `match r { Ok(move v): v, ... }` — which zeroes the enum slot; a plain copy binding double-frees because the cascade still runs. Enums **with** a `Drop` impl manage payloads in `drop` and get no cascade.

---

## 2. The Drop-Flag Stack Machine

Silver implements `AGENTS.md §6.3-6.4`: a deferred drop stack, not inline `drop` calls.

### 2.1 Flag allocation

For each tracked local or by-value parameter (`stmt.rs:128-161`, `scope.rs:754-817`):

```llvm
%x.drop = alloca i1, align 1
store i1 1, ptr %x.drop          ; live
; per-field flags, when applicable:
%x.f.drop = alloca i1
store i1 0, %x.f.drop            ; no live value yet
```

Parameter scope registers flags via `register_drop_flag` (`scope.rs:758-817`) and marks field flags live for the caller's transferred value.

Struct field drops are cascaded fields as separate deferred entries in the same scope; enum payload cascades use `register_enum_payload_cascade` (`scope.rs:682-752`) guarded by the enum's own `.drop` flag.

### 2.2 Deferred stack (`defers`)

Codegen maintains `defers: Vec<Vec<DeferredEntry>>` — a stack of scopes (`codegen/llvm_ir/mod.rs:132-133`, `codegen/llvm_ir/scope.rs:307-315`):

```rust
pub struct DeferredEntry<'ctx> { pub action: DeferAction<'ctx>, pub flag: Option<PointerValue<'ctx>> }
pub enum DeferAction<'ctx> { Statement(Stmt), DropCall(fn_name, var_ptr), EnumPayloadDrop(ty, var_ptr) }
```

* `push_scope()` / `pop_scope()` at `scope.rs:307-315` bracket every `{ }` block and loop iteration scope.
* Registering a `Drop` variable pushes `DeferredEntry { action: DropCall, flag: Some(.drop) }` onto `defers.last_mut()` (`scope.rs:745-750`).
* Plain `defer { … }` pushes `DeferAction::Statement` with `flag: None` (`codegen/llvm_ir/generate.rs:1582-1588`) — always runs.

### 2.3 Scope exit: guarded emission

`LlvmIrGenerator::emit_defers(levels)` at `scope.rs:317-383`:

1. Slices `defers[total-levels ..]` and iterates scopes outer→inner, entries inner→outer (`rev()`).
2. For each entry with `Some(flag_ptr)`, emits a conditional branch:
   ```llvm
   %flag = load i1, ptr %x.drop
   br i1 %flag, label %defer.run, label %defer.after
   defer.run:
     call void @FileHandle$drop(ptr %x)
     br label %defer.after
   defer.after:
   ```
   (`scope.rs:332-352`, `358-365`). If `flag` is `None`, the action runs unconditionally.
3. `generate.rs:1512-1524` fires `emit_defers(1)` on normal block fall-through; function epilogue fires `emit_defers(defers.len())` (`stmt.rs:186`); `break`/`continue` fire only loop levels (`stmt.rs:300-304`, `generate.rs:1642-1663`).

### 2.4 Move invalidation clears flags

Executing a move clears the flag so the deferred drop becomes a no-op:

* `move x` in expression codegen: `scope.rs:395-425`, `expr.rs:1247-1259` → `store i1 0, ptr %x.drop` plus `clear_field_flags("x")`.
* `move x.field` → `clear_field_flags_for_path("x","field")` at `scope.rs:406-425`.
* Transfer to by-value param or `launch` child: `codegen/llvm_ir/call.rs:254-256`, `tasks.rs:197`, `expr.rs:1251-1258`.
* By-value method receiver consumed: `call.rs:852-854`.
* Bare return: `generate.rs:1607-1616`.

Snippet — the runtime effect of `move`:

```silver
FileHandle a; a.fd = sys_open("/tmp/a", 0);
FileHandle b = move a;   // codegen: store i1 0, %a.drop  — a's defer now skipped
// a is dead here
```

---

## 3. Explicit Move (`move x`)

`AGENTS.md §6.3` — marking a transfer with `move x` invalidates the caller's local.

```silver
FileHandle f1; f1.fd = sys_open("/tmp/x", 0);
consume(move f1);          // f1 moved, drop flag cleared
// f1.fd;                  // error: use of moved value 'f1'

struct Pair { Owned left; Owned right; }
Pair p; p.left = Owned.new(1); p.right = Owned.new(2);
Owned a = move p.left;     // partial move — only p.left flag cleared
// p.left;                 // error: use of moved field 'p.left'
Owned b = move p.right;    // ok — p.right still live
```

What `move x` means, precisely:

* **Static**: `move_check.rs:564-626` — `ExpressionKind::Move(inner)` calls `expr_root_and_path(inner)` (`move_check.rs:130-147`, helper that walks `Identifier` / `FieldAccess` chains and returns `(root_name, "a.b.c")`). If the path is untracked (view-typed), the checker recurses into the inner expression and *does not* record a move. Otherwise:
  * whole-variable `move x` → `VarState::mark_moved(span, "value explicitly moved here")` at `move_check.rs:55-60`, `594`.
  * field `move x.f.g` → `VarState::mark_field_moved(path, span, reason)` at `move_check.rs:62-71`, `617-621`.
  * Already-moved / partially-moved sources are diagnosed at the `move` site itself (`move_check.rs:571-592`, `599-615`).

* **Dynamic**: codegen `expr.rs:1247-1260` / `scope.rs:395-425` clears the root's `i1` flag (and field cascade flags) so deferred drops are skipped, then returns the value bits by copy. The callee's copy carries its own flag set to `1` and will be dropped at callee exit.

The string `"value explicitly moved here"` is the note attached to the move site; `move_check.rs:440-453` and `diagnostics::render` display it as `note: value explicitly moved here`.

---

## 4. Implicit Transfers (No `move` Keyword Required)

Four additional transfer forms are treated identically to `move` — they invalidate the source's flag without a `move` token.

### 4.1 Bare return `return x;`

```silver
FileHandle make() {
    FileHandle h; h.fd = sys_open("/tmp/b", 0);
    return h;          // implicit move — no `move` needed
}
FileHandle g = make();
```

* Checker: `StatementKind::Return(Some(expr))` at `move_check.rs:501-512` — a bare `Identifier` return `return x;` calls `var.mark_moved(span, "value moved by return")`. Field returns like `return x.field;` do **not** move (`_ => check_expr`, comment at `move_check.rs:503-504`).
* Codegen: `generate.rs:1591-1617` evaluates the return value into a temporary **first**, then clears the source's flag (`store i1 0, %x.drop` + `clear_field_flags`), then `emit_defers(len)` for all pending scopes, then `ret`. Evaluating first avoids Bug A (use-after-free where defers would destroy the value before it is copied out). The saved value is `cast_value_to_ast_type`-adjusted to the declared return type.

### 4.2 By-value arguments

```silver
void process(FileHandle f) {}          // f is by-value
FileHandle h; h.fd = sys_open("/tmp/c", 0);
process(move h);        // explicit
// or, when callee param is by-value, the arg-position itself is the transfer:
void sink(FileHandle f);
sink(h);                // h is moved into f even without `move` keyword at call site?  checker case depends on call form
```

* `Facts::value_args` at `move_check.rs:161-162`, populated `move_check.rs:196-209` for every non-view param (`!is_view_type`).
* At `Call` (`move_check.rs:669-702`): for each arg position `i` where `facts.value_args.contains((fn_name,i))`, if the arg is a trackable `(root,path)`, the checker `mark_moved`/`mark_field_moved` with reason `"value moved into function parameter"` (the call's `check_expr(function,…)` still runs). Otherwise it recurses normally.
* Codegen mirrors it: `call.rs:254-256` / `905-907` → `clear_drop_flag_of(argument)` when `arg_drops` is true, and trampolines for `launch` at `tasks.rs:197`.

### 4.3 By-value method receiver

```silver
impl FileHandle { void consume(Self self) {} }
FileHandle h; h.fd = sys_open("/tmp/d", 0);
h.consume();            // h consumed — Self is by-value
```

* `Facts::value_receivers` at `move_check.rs:159-160`, `190-194` — every `impl` method with `MethodKind::InstanceValue`.
* `MethodCall` at `move_check.rs:629-668`: `is_consuming = method=="drop" || value_receiver` ; when true and the receiver is a trackable `(root,path)`, it marks `root` (or field path) moved with `"value consumed by method call"` without recursing into the receiver. `call.rs:852-854` clears the flag.

### 4.4 Explicit `v.drop()`

```silver
Owned o = Owned.new(42);
o.drop();               // consumes o — flag cleared, destructor runs now
// o;                   // error: use of moved value 'o'
```

* Also `is_consuming` at `move_check.rs:634` (`method.name == "drop"`), same path as above. Calling a type's destructor directly is treated as an explicit move of the receiver.

### 4.5 Thread primitives

* `launch f(args…)` at `move_check.rs:703-760`: every launch argument is moved (`"value moved into thread launch"`). The checker also diagnoses a launch of an already-moved variable at the arg site.
* `wait t` at `move_check.rs:762-787`: `wait task_handle` consumes the `Task` handle; a second `wait` on the same identifier is a use-after-move (`"task handle consumed by wait"`). `is_tracked` explicitly includes `Task` (`move_check.rs:336`).

---

## 5. Pointer / Reference Exemption

Because `Facts::is_view_type` classifies `T*` and `&T`/`&mut T` as views (`move_check.rs:217-223`), none of the transfer logic applies to them — at any level:

* `Mix` struct `MovedField` example: constructing `Moves::Ok(move i)` requires `move` but the `FieldAccess` / `move` check at `move_check.rs:566-567` consults `is_path_tracked` (`367-381` → `get_field_type` + `is_tracked`). If the leaf `path` type is `i32*` / `&T`, tracking returns false and the `move` does **not** consume the root.
* In `Move` exprs, by-value arg transfers, consuming receivers, and `launch`/`wait`, the code first gates on `is_path_tracked` / `is_view_type`. View-typed variables never enter `State` at all (`declare` at `move_check.rs:383-404` only inserts when `is_tracked` holds), so they have no `VarState` and cannot be reported moved.

In words: views borrow; only `T` values (structs/enums with `Drop`) own.

```silver
Owned o = Owned.new(1);
Owned* p = &o;          // borrow — no flag, no move
mut_ref(&o);            // &mut borrow — same
// o still live; *p is a view; moving *p would be an error only if tracked
```

---

## 6. Flow-Sensitive Move Checker

`semantic/move_check.rs:1-17` — per-function, per-path dataflow before codegen, with Rust-like "moved on any fall-through path is poison after the join."

### 6.1 Lattice

`VarState` at `move_check.rs:37-43`:

```rust
pub struct VarState {
    pub level: u8,                          // 0 = Live, 1 = PartiallyMoved, 2 = FullyMoved
    pub move_span: Option<Span>,            // site for note span
    pub move_reason: Option<&'static str>,  // e.g. "value explicitly moved here"
    pub moved_fields: FxHashMap<String, (Span, &'static str)>, // dotted paths of moved fields
}
```

Transitions at `move_check.rs:45-84`:

* `mark_moved(span, reason)` → `level=2`, `moved_fields.clear()` (whole move supersedes any partial).
* `mark_field_moved(path,span,reason)` → `level=1` (if not already `2`), inserts `"a.b"` into `moved_fields`.
* `mark_field_reinitialized(path)` → removes `path` and any `"path.*"` subpaths; if `moved_fields` becomes empty, resets to `level=0` (`Live`). Used on `p.left = new_val;`.
* `is_field_moved(path)` at `move_check.rs:94-108` walks `path` upward (`rfind('.')`) so moving `p.left` poisons `p.left.inner`.
* `merge_with` at `move_check.rs:111-127` takes the max `level` and unions `moved_fields`; if any moved field survives the union, `level` is promoted to `1`.

### 6.2 Root-and-path splitting

`expr_root_and_path(expr)` at `move_check.rs:130-147`:

```rust
fn expr_root_and_path(expr: &Expression) -> Option<(String, String)> {
    // walks FieldAccess chains:  a.b.c  ->  ("a", "b.c")
    // Identifier           -> ("x", "")
    // anything else        -> None
}
```

Callee helpers `get_field_type` (`348-365`) and `is_path_tracked` (`367-381`) resolve the dotted path against `struct_fields` via `var_types: FxHashMap<String, Type>` (shadow-aware type map maintained alongside `State`). Untouched paths = plain `check_expr` walk.

### 6.3 State & scopes

`State = FxHashMap<String, VarState>` (`149`), `ScopeEntry = (name, Option<VarState>, Option<Type>)` (`153`). `var_types` mirrors `State` for type queries. `declare` (`383-404`) pushes `State::new_live()` and records shadowing; `pop_scope` (`407-429`) restores; `check_function` (`310-324`) seeds tracked params as live and walks the body block; `check_program` (`274-302`) iterates `Function` and `Impl::Function` / `Cast` bodies.

### 6.4 Per-path control flow

Every expression that can affect liveness clones and merges `State`:

* **`if` / ternary** at `move_check.rs:880-913` and `885-914`-style `If` expr: clones `then_state` / `else_state`; `statement_terminates` (`238-248`) / `block_terminates` (`250-252`) / `expression_terminates` (`254-272`) identify branches that never fall through (`return`/`break`/`continue`). Terminating branches contribute `VarState::default()` (level 0) to the merge, so `if (c) { return move x; } else {}` does **not** poison `x` after the `if`. Non-terminating branches `merge_with` into the base state — moved on any fall-through path = moved after.
* **`while` / `for` / `for-in`** at `915-1010`, `943-1010`: fixpoint loop up to 8 iterations (`for _ in 0..8`) accumulating `level = max` across iterations; a terminating body contributes `default()`. This is why "cannot move out of 'x' in a loop" emerges naturally — a value moved on iteration 1 is poison on iteration 2's entry.
* **`match`** at `1011-1039`: each arm is an independent path from pre-match `State`; only arms where `!expression_terminates(&arm.body)` contribute. Guards containing `move` are errors (`pattern_has_move` at `1090-1099`).
* **`Binary Assign`** at `820-856`: RHS is visited first; if LHS is `x` (empty path) and `x` is tracked, it resets that name to `VarState::new_live()` (the reassignment resurrection). If LHS is `x.f` and `x` is not fully moved, `mark_field_reinitialized(path)`; assigning to a field of a fully-moved `x` is an error.

The same pattern repeats for `Binary` non-assign, `Call`, `StructLiteral`, `Array`/`Tuple`, `Block`, etc. — all structural, no interprocedural analysis.

### 6.5 Diagnostics

`MoveError` at `move_check.rs:29-35`: `message` + `span` for the error plus `note_span` + `note_message` for the move origin. Helpers `error` / `error_with_note` (`431-453`) attach catalog strings from `diagnostics::messages`:

* `msg::use_of_moved_value(name)` — `"use of moved value 'x'"`
* `"use of partially moved value 'x'"`
* `"use of moved field 'x.f'"`
* `"cannot move already partially moved value 'x'"`
* `"cannot move out of payload in a match arm with a guard"`

Reasons:

* `msg::note_value_explicitly_moved()` — "`value explicitly moved here`" (`move x`)
* `msg::note_value_moved_into_param()` — parameter transfers
* `msg::note_value_consumed_by_method()` — consuming receivers / `drop()`
* `msg::note_value_moved_by_return()` — `return x`
* `msg::note_value_moved_into_launch()` — `launch`
* `msg::note_task_handle_consumed()` — `wait`

Rendered by the centralized diagnostic pipeline (`AGENTS.md §7`) as:

```
error: app.ag:15:5: use of moved value 'handle'
15 |     handle.read();
   |     ^^^^^^
note: app.ag:12:12: value explicitly moved here
12 |     sink(move handle);
   |          ^^^^^^^^^^^
```

with source-line caret alignment (`diagnostics::render`).

---

## 7. Re-Initialization

Assigning a new whole value to a moved binding resurrects it — both statically and at runtime.

```silver
Owned o = Owned.new(1);
consume(move o);     // o is FullyMoved
o = Owned.new(2);    // ok — o is Live again; flag set to 1, field flags refreshed
consume(move o);     // consume new value; flag cleared again
```

* Checker: `Binary::Assign` at `move_check.rs:830-834` — `state.insert(root_name, VarState::new_live())` for a whole-variable reassignment of a tracked name, erasing move state. Overwriting a live variable does not trigger a diagnostic; field assigns only fail if the container is `FullyMoved`.
* Codegen: prior value is pre-dropped guarded by its flag, then the new assignment sets the variable's `i1` to `1` and initializes per-field flags (`scope.rs:xxx` and `AGENTS.md §6.6`).

Field-level resurrection:

```silver
Pair p; p.left = Owned.new(1); p.right = Owned.new(2);
Owned a = move p.left;    // p is PartiallyMoved, moved_fields={left}
p.left = Owned.new(9);    // p.left re-initialized — removed from map; p partially moved via right? no — only left was moved
// if left was the only moved field, p returns to Live
Owned b = move p.left;    // ok after reinit
```

`mark_field_reinitialized` at `move_check.rs:73-84` removes `path` and all `path.*` entries; if `moved_fields` becomes empty, `level` resets to `0`.

Whole moves poison all fields (`mark_moved` clears `moved_fields`, `is_fully_moved` makes `is_field_moved` return the whole-move span at `move_check.rs:95-96`).

---

## 8. Return & Exit Handling (Bug A / Scope Defer Order)

`AGENTS.md §6.5` and `generate.rs:1591-1630`:

When emitting `return expr;` the compiler **evaluates the return expression first** into an SSA temporary, **then** clears the returned local's drop flag (if it is a bare identifier — implicit move), **then** snapshots and runs `emit_defers(defers.len())` (which lowers each `DropCall`/`EnumPayloadDrop`/`Statement` guard behind `load i1 %x.drop`), and only then `ret`. This avoids Bug A where defers would destroy a local before the return value copy completes. Function parameters receive their own drop flags and are dropped at function exit unless moved away.

`AGENTS.md §6.5 Bug C` (known limitation): locals declared but not initialized still have `.drop = 1` by default; if the type's `drop` dereferences a field without a null check, zero-initialized memory may be freed spuriously. Mitigate by null-guarding `drop` bodies (`if (self.data != 0) free(…);`).

Loop breaks/continues similarly unwind only up to `loop_defers_base` (`stmt.rs:300-304`, `generate.rs:1642-1663`).

---

## 9. Stdlib Contracts: Owned Elements Are Leaked Unless Drained

Silver's container `Drop` implementations free the **buffer**, not the **elements**. This is deliberate: the type system has no per-element drop-iteration.

### `Vec<T>` — `std/mem/vec.ag:9-16`, `174-185`, `204-272`

```silver
import std.mem.vec;
Vec<String> v = Vec<String>.new();
v.push(String.from("hi"));   // push copies bits into buffer; caller's by-value param is dropped at callee exit
// Do NOT index-copy owned elements:
String s = v.get(0);         // shallow copy — double-free on scope exit
String* p = v.get_ptr(0);    // ok — read via pointer
String owned = v.take(0);    // ok — removes slot (shifts tail), returns owned value
```

Header comment at `vec.ag:9-16`: "Drop does NOT call per-element destructors … For owned-element types, use `get_ptr` for read-only access and `take` to extract values before the Vec goes out of scope." `Vec<T>::drop` at `vec.ag:174-185` is `if (self.data != 0) { free(self.data); }` only. `get` at `vec.ag:78-81` documents the shallow-copy trap; `take` at `vec.ag:132-141` shifts the tail left and shrinks `len` so the slot is vacated; `drain` ownership transfer is mirrored for iterators: `VecIter` owns the buffer after `into_iter` clears the source Vec (`vec.ag:202-272`), and `VecIter::drop` frees only the buffer, never elements.

### `HashMap<K,V>` — `std/map.ag:309-333`, `381-412`, `425-438`

```silver
import std.map;
HashMap<String, Owned> m = HashMap<String, Owned>.new();
m.insert(String.from("k"), Owned.new(1));

// Leaks — tombstones slot, does not drop K or V:
m.erase(move k);             // doc: "does NOT call per-element destructors — owned K/V leaked" (map.ag:309-311)

// Returns ownership — correct for Drop types:
Optional<Pair<String,Owned>> pr = m.remove(move k);   // move k + move v out (map.ag:323-333)

// Clear leaks unless drained:
m.clear();                   // frees ctrl/keys/values arrays only (map.ag:383-384)
// Drain to reclaim:
HashMapDrain<String,Owned> d = m.drain();  // moves buffers into drain (map.ag:399-412)
while (true) { Optional<Pair<String,Owned>> o = d.next(); if (o.is_none()) { break; } o.unwrap().first.drop(); o.unwrap().second.drop(); }
```

`HashMap::erase` at `map.ag:312-320` and `HashMap::clear` at `map.ag:385-396` / `HashMap::drop` at `map.ag:425-438` carry the same NOTE as `Vec`: "Does NOT call per-element destructors — … Use `remove` (which returns the owned pair) or `drain` …". `drain` at `map.ag:398-412` nulls the map and yields `HashMapDrain` (`map.ag:671-682`) whose `Drop` (`678-691`) only frees buffers, not elements; iteration at `map.ag:692-735` / `set.ag:509-525` yields shallow copies of `K`/`V`.

`HashSet<K>` mirrors this at `collections/set.ag:7-8`, `246-273`, `322-360`, `466-502`.

**Rule**: for owned `T`, drain with `take`/`remove`/`drain` + manual `drop` before the container goes out of scope; `clear`/`erase` without draining leaks under `--leak-check` (the allocator reports `leak ptr=… allocated at …` via `AGENTS.md §7.3`).

---

## 10. Quick Reference

| Transfer | Syntax | Checker reason | Flag action | Citation |
|---|---|---|---|---|
| Explicit move | `move x` / `move x.f` | `value explicitly moved here` | `store i1 0, %x.drop` / field flag | `move_check.rs:564-626`, `scope.rs:395-425` |
| Bare return | `return x;` | `value moved by return` | same, before `emit_defers` | `move_check.rs:501-512`, `generate.rs:1599-1616` |
| By-value param | `f(x)` where `param: T` | `value moved into function parameter` | `clear_drop_flag_of(arg)` | `move_check.rs:669-702`, `call.rs:254-256` |
| Consuming method | `x.method()` where `self: Self` | `value consumed by method call` | same | `move_check.rs:629-668`, `call.rs:852-854` |
| Explicit `drop` | `x.drop()` | same as consuming | same | `move_check.rs:634` |
| Thread | `launch f(x)` / `wait t` | `moved into thread launch` / `task handle consumed` | same / same | `move_check.rs:703-787`, `tasks.rs:197` |
| View | `T*`, `&T`, `&mut T` | _never consumed_ | none | `move_check.rs:217-223`, `AGENTS.md §6.2` |

Diagnostics are multi-span with a `note: … moved here` pointing to the originating span (`move_check.rs:29-35`, `440-453`); uninitialized locals start with `.drop = 1`, so guard `drop` bodies against zeroed fields (`AGENTS.md §6.5 Bug C`).

---

*Sources*: `bin/agc/src/semantic/move_check.rs` (VarState `37-128`, `expr_root_and_path` `130-147`, latch `merge_with` `111-127`, `check_program`/`check_function` `274-324`, terminates helpers `238-272`), `bin/agc/src/codegen/llvm_ir/scope.rs` (`defers` stack `307-383`, flag helpers `395-425`, `register_drop_flag` `758-817`, enum cascade `551-752`), `bin/agc/src/codegen/llvm_ir/generate.rs:1582-1630` (defer push + return ordering), `bin/agc/src/codegen/llvm_ir/call.rs:254-256`, `tasks.rs:197` (launch), `AGENTS.md §6.1-6.7` (invariants), `std/mem/vec.ag:9-16,78-81,132-141,174-185` and `std/map.ag:309-333,381-438` (leaked-element contracts).

---

## 11. Phase 1 Migration Note — String Paths → `semantic::place::Place` (scaffolding only)

> **No behavior change in this phase; comments only.** The checker strings described above are the current authoritative representation.

Phase 1 introduces a structured `Place` type in `bin/agc/src/semantic/place.rs` (`semantic::place::Place { local: LocalId, projections: Vec<Projection> }` where `Projection::{Field(FieldId), TupleField(usize), Index, Deref}` mirrors Silver syntax, not a Rust clone) alongside the existing string logic. Helpers are pure and comparable (`Place::overlaps`, prefix checks, etc.).

* `move_check.rs`: `VarState::moved_fields: FxHashMap<String,_>` + `expr_root_and_path` → `Place` keys (`Field` projections); `is_field_moved`/`mark_field_moved`/`mark_field_reinitialized` string walks (`rfind('.')`, `starts_with`) will move to `Place`-based prefix/overlap helpers. See header, `VarState::moved_fields`, `mark_field_moved`, `is_field_moved`, `expr_root_and_path` TODOs.
* `borrow_check.rs`: `ActiveBorrow { root, path: String }` + `paths_overlap` + `extract_root_and_path` → `Place` keys; `paths_overlap` and `CallArgAccess` `(String,String)` joins will move to `Place::overlaps`/`is_prefix_of`. See module header, `RefVarInfo`/`ActiveBorrow`, `paths_overlap`, `extract_root_and_path`, `CallArgAccess` TODOs.

Until the cutover, `Place` coexists as parallel infrastructure; string paths remain the checked path. Follow-up phases will switch the checkers to `Place` and remove the string helpers.
