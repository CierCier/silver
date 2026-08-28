# Copy vs Move: Current POD / Owned Split

> **Freeze note (Phase 0, 2026-08-28, branch `move-semantics`)** — This document
> is a *read-only snapshot* of the compiler as it exists today. It describes
> the current `Copy` vs `needs_drop` split, how `T x = y` behaves, and where
> the per-subsystem `is_copy`/`has_drop` heuristics live. No behavior is
> changed here; the central `TypeProperties` query that will unify these
> checks is deferred to Phase 5.

Silver has no `Copy` trait or `POD` annotation in source. Whether a value
is trivially copyable or owns resources is inferred from one fact: **does
its type (transitively) implement `Drop`?** That single predicate drives
the drop-flag machine described in `AGENTS.md §6` and in
`docs/borrow-checker/ownership-and-moves.md`.

---

## 1. The split

| Predicate | Meaning in Silver | Runtime consequence |
|---|---|---|
| **Copy / POD** — `!needs_drop(T)` | Bitwise copy is sound; no destructor to run. | No `i1` drop flag, no field flags, no deferred drop. `T x = y` leaves both live. |
| **Owned / `needs_drop(T)`** — `has_drop(T)` | Value owns heap, fd, or other resource; destructor must run exactly once. | Stack slot gets an `i1` flag (`{name}.drop`) plus per-field flags. Moves clear the flag; scope exit checks it. |

`needs_drop` is transitive: a struct is owned if it implements `Drop`
directly **or** any field (recursively) is owned. An array/tuple is owned
if any element is owned. A pointer or reference (`T*`, `&T`, `&mut T`) is
never owned — it is a non-owning view (`AGENTS.md §6.2`).

```silver
// Copy — no Drop in the closure
struct Point { f64 x; f64 y; }
Point a = { .x = 1.0, .y = 2.0 };
Point b = a;          // bitwise copy, both live

// Owned — Drop in the closure
String s = String.from_str("hi");
String t = move s;    // s.drop flag cleared to 0, t owns the buffer
// use(s);            // ERROR: use of moved value 's'
```

---

## 2. Table: Copy vs `needs_drop` in the current tree

### 2.1 Always Copy (no `Drop` impl anywhere in std today)

| Type | Why Copy | Code ref |
|---|---|---|
| `bool` | primitive, never has `Drop` | `scope.rs:934-938` skips `Pointer/Reference` only; primitives fall through to `get_drop_function_name → None` |
| `i8` `i16` `i32` `i64` `i128` | primitive | same |
| `u8` `u16` `u32` `u64` `u128` | primitive | same |
| `f32` `f64` | primitive | same |
| `char`, `str` (as primitive) | primitive | same |
| `T*`, `T&` views | view types are excluded explicitly | `typeck.rs:739`, `move_check.rs:218-222`, `scope.rs:934-938`, `scope.rs:913-918` |
| `T[N]` where `T: Copy` | array recursion returns false | `typeck.rs:740`, `move_check.rs:332` |
| `(A,B,…)` where all `A,B: Copy` | tuple recursion returns false | `typeck.rs:741`, `move_check.rs:333` |
| `struct S` where every field `T: Copy` | no field needs drop | `move_check.rs:338-342`, `scope.rs:820-901` (no flag emitted) |
| `enum E` with no `Drop` and no `Drop` payload | tag-only, no payload cascade | `scope.rs:682-752` returns early when `!has_drop_payload` |

### 2.2 Always `needs_drop` (owns resources, has `Drop`)

| Type | Resource | Drop impl | Code ref |
|---|---|---|---|
| `String` | `u8*` heap buffer | `std/string.ag:557` | `std/string.ag` |
| `Vec<T>` | `T*` heap buffer | `std/mem/vec.ag:174` | `std/mem/vec.ag` |
| `VecIter<T>` | owned buffer after `into_iter` | `std/mem/vec.ag:263` | — |
| `HashMap<K,V>` | `ctrl*`, `keys*`, `vals*` | `std/map.ag:425` | `std/map.ag` |
| `HashSet<K>` | same as `HashMap` | `std/collections/set.ag:466` | `std/collections/set.ag` |
| `Deque<T>` | ring buffer | `std/collections/deque.ag:194` | `std/collections/deque.ag` |
| `BinaryHeap<T>` | backing buffer | `std/collections/heap.ag:155` | `std/collections/heap.ag` |
| `Queue<T>` | backing buffer | `std/collections/queue.ag:133` | `std/collections/queue.ag` |
| `Bytes`, `BytesIntoIter` | `u8*` buffer | `std/bytes.ag:199,259` | `std/bytes.ag` |
| `Box<T>` | single heap slot | `std/mem/box.ag:35` | `std/mem/box.ag` |
| `Rc<T>` | ref-counted heap | `std/mem/rc.ag:66` | `std/mem/rc.ag` |
| `Arena` | arena base | `std/mem/arena.ag:44` | `std/mem/arena.ag` |
| `File`, `BufWriter` | fd / flush+free | `std/io/file.ag:216,584` | `std/io/file.ag` |
| `Scanner`, `ByteStream` | internal buf | `std/io/scanner.ag:647`, `std/io/byte_stream.ag:116` | `std/io/` |
| `Channel<T>` | `Vec<T>` + `RawMutex` + futex words | transitively via `Vec`/`RawMutex` | `std/channel.ag`, `std/sync.ag` |
| `RawMutex`, `Socket`, `TcpStream`, `TcpListener`, `UdpSocket`, `HttpConnection`, etc. | fd / futex | `std/sync.ag`, `std/net/*.ag` | `std/sync.ag`, `std/net/socket.ag:76` |
| `Task<T>` handles | join handle | special-cased as tracked | `move_check.rs:336`, `stmt.rs` / `scope.rs` |
| Any `struct S` containing a field of the above | transitive ownership | `move_check.rs:338-342` recursion | — |
| Any `enum` variant payload containing owned type (without own `Drop`) | tag-switched payload cascade | `scope.rs:555-676` | — |

> **Stdlib note:** `Vec<T>::drop`, `HashMap`/`HashSet`/`Deque` drops free the
> backing buffer only; they do **not** iterate per-element destructors
> (`std/mem/vec.ag:175` comment, `std/map.ag:426`). Pushing an owned element
> type into a `Vec` copies bits and leaves a dangling copy — see
> `docs/borrow-checker/current-limitations-and-roadmap.md` and the `Vec`
> header.

The trait itself is defined once, in `std/mem/drop.ag:7`:

```silver
trait Drop { void drop(Self* self); }
```

A type is considered to implement it when an `impl Drop<X> for X { … }`
item exists (generic impls like `impl<T> Drop<Vec<T>>` count — see
`typeck.rs:699-720`).

---

## 3. How each subsystem decides `Copy` today

There is **no central `TypeProperties` query**. Each pass recomputes the
same predicate from its own data structures. This is the Phase 5 unification
target (see §6).

### 3.1 `semantic/typeck.rs` — `type_has_drop_impl`

```rust
// bin/agc/src/semantic/typeck.rs:726-743
fn type_has_drop_impl(&self, ty: &ast::Type) -> bool {
    match ty.kind.as_ref() {
        TypeKind::Named(named) =>
            named.path.len() == 1 && self.drop_owner_bases.contains(&named.path[0].name),
        TypeKind::Pointer(_) | TypeKind::Reference(_) => false,
        TypeKind::Array(a)  => self.type_has_drop_impl(&a.element_type),
        TypeKind::Tuple(ts) => ts.iter().any(|t| self.type_has_drop_impl(t)),
        _ => false,
    }
}
```

* `drop_owner_bases: HashSet<String>` — populated in
  `collect_trait_impls` (`typeck.rs:699-720`) from every `impl Drop<X>`
  including generic templates (`impl<T> Drop<Vec<T>>` → inserts `"Vec"`).
  (The current code duplicates the insertion block — harmless, but a sign of
  the ad-hoc nature.)
* Base-name only (`"Vec"` not `"Vec<String>"`), so `Vec<i32>` and
  `Vec<String>` share the same answer.
* Bare type parameters inside a generic function body are **not** treated as
  owned (`typeck.rs:728-737` comment) to avoid false positives on concrete
  enum impls that still spell `T` in the payload list. Generic-template
  constructions are enforced at their instantiation sites instead — the
  `std` template bodies already use `move`.
* Used for one check today: `payload_must_be_moved` on enum construction
  (`typeck.rs:2041-2044`):
  `Res.Ok(move owned)` is required when the payload type has drop.

### 3.2 `semantic/move_check.rs` — `Facts` + `is_tracked`

```rust
// bin/agc/src/semantic/move_check.rs:155-165  Facts
struct Facts {
    drop_owners: FxHashSet<String>,              // base names with Drop
    value_receivers: FxHashSet<(String,String)>, // (owner, method) with InstanceValue
    value_args: FxHashSet<(String,usize)>,       // (fn, param idx) with value param
    struct_fields: FxHashMap<String, Vec<(String, Type)>>,
}
// bin/agc/src/semantic/move_check.rs:217-222
fn is_view_type(ty: &Type) -> bool {
    matches!(ty.kind.as_ref(), TypeKind::Pointer(_) | TypeKind::Reference(_))
}
// bin/agc/src/semantic/move_check.rs:330-345
fn is_tracked(&self, ty: &Type) -> bool {
    match ty.kind.as_ref() {
        TypeKind::Array(a)  => self.is_tracked(&a.element_type),
        TypeKind::Tuple(ts) => ts.iter().any(|t| self.is_tracked(t)),
        TypeKind::Named(named) => {
            let owner = Facts::owner_key(ty);
            (named.path.len()==1 && named.path[0].name=="Task")
                || self.facts.drop_owners.contains(&owner)
                || self.facts.struct_fields.get(&owner)
                    .is_some_and(|fields| fields.iter().any(|(_,fty)| self.is_tracked(fty)))
        }
        _ => self.facts.drop_owners.contains(&Facts::owner_key(ty)),
    }
}
```

* Built once per program in `Facts::build` by scanning `ItemKind::Struct`
  and `ItemKind::Impl` (recording `Drop` owners, `InstanceValue` receivers,
  and non-view `value_args`).
* `is_tracked` is **strictly broader** than `type_has_drop_impl`: it recurses
  into struct fields, so `struct Wrapper { String s; }` is tracked even though
  `Wrapper` itself has no direct `Drop` impl. It also special-cases `Task`
  handles (`wait` consumes them).
* Controls the move lattice: `declare` (`move_check.rs:383-404`) only
  inserts a `VarState::Live` entry when `is_tracked(ty)` is true. Copy types
  have no lattice entry — `move x` on a `Copy` is treated as a plain read
  (`move_check.rs:564-568`).

### 3.3 `codegen/llvm_ir/scope.rs` + friends — `get_drop_function_name` / `register_drop_flag`

```rust
// bin/agc/src/codegen/llvm_ir/scope.rs:526-548
pub(crate) fn get_drop_function_name(&mut self, ty: &Type)
    -> CodegenResult<Option<String>> {
    // 1. concrete Drop owners in drop_trait_impl_owners
    // 2. try_instantiate_generic_impl_method_for_type_filtered(ty,"drop",Some("Drop"))
    // 3. None
}
```

* `drop_trait_impl_owners: HashSet<String>` — populated during codegen
  from concrete `Drop` impls and from successful generic instantiations
  (`generate.rs:922`, `symbols.rs:1568`).
* `register_drop_flag` (`scope.rs:758-817`) decides whether a local/param
  gets an `i1` flag:
  * if `get_drop_function_name(ty).is_some()` → allocate `{name}.drop = 1`
    and register field cascade via `register_field_drops`.
  * else → try field cascade; if no field needs drop, delegate to
    `register_enum_payload_cascade` (`scope.rs:682-752`) for enums whose
    *payload* contains drop types (tag-switched drop).
  * pointers/references return early with no flag (`scope.rs:934-938`,
    `stmt.rs:934-938`).
* `register_field_drops` (`scope.rs:820-901`) walks `struct_fields` for the
  named type, recurses into nested structs, and allocates a per-field
  `i1` flag (`field.{name}.drop`, init `0` = "no live value yet") for each
  field whose type has a drop function. Declaration-order registration + LIFO
  defers gives correct drop order.
* Assignment overwrites call `emit_assignment_pre_drop` (`operators.rs:1481-1566`)
  which guards the old value's destructor with the flag — moved-from slots
  are not double-dropped, uninitialized fields are never dropped (`stmt.rs:900-971`,
  `AGENTS.md §6` Bug C fix).

**Divergence to watch:** `typeck` uses only direct `Drop` owners;
`move_check` adds transitive struct recursion and `Task`; codegen adds generic
instantiation and enum payload cascades. All three agree on the
`bool/i32/i64/f64/pointer/reference` core, but differ on nested generics and
payloads — exactly the gap a central query will close.

---

## 4. `T x = y` today

Silver has no implicit copy/move keyword on `=`. The statement

```silver
T x = y;
T x = move y;
x = y;
x = move y;
```

always emits a **bitwise copy** at the LLVM level (`builder.build_store`
in `stmt.rs:892` and `operators.rs:386`). What differs is the **drop-flag
protocol** on `y`:

| Form | `T: Copy` (POD) | `T: needs_drop` (owned) |
|---|---|---|
| `T x = y` | Copy both ways; `y` stays live, `x` gets no flag. Correct. | Bitwise copy, **both** flags remain `1` → **double-drop** (unsound). The compiler does not yet reject this; you must write `move`. |
| `T x = move y` | `move y` on a `Copy` is a no-op for the lattice (`move_check.rs:565-568`): `y` stays live, `x` is a copy. Harmless but unnecessary. | `move y` clears `y.drop` to `0` (`expr.rs:1244-1260`), `x` is initialized with `1` and its field flags set live (`stmt.rs:948-959`). Correct transfer. |
| `x = y` (whole-var overwrite) | `emit_assignment_pre_drop` sees no `drop_fn` for `T`, so no pre-drop; store copies bits. | `emit_assignment_pre_drop` (`operators.rs:1488-1515`) guard-drops the old `x` (if live), then stores `y`'s bits and marks `x`'s field flags live (`operators.rs:392-401`). But `y` still has `drop=1` → double-drop unless `move` was used. |
| `x = move y` | Same as Copy row above. | Pre-drop on old `x`, store, clear `y`'s flag(s) (`expr.rs:1256-1258`), mark `x` live. Correct. |
| `x.field = move y` | Parent is `Copy` → no flag changes. | Only the field's flag is cleared/set (`operators.rs:1518-1561`, `stmt.rs:834-848`); disjoint fields remain live (partial moves, `ownership-and-moves.md §5`). |

**Initialization vs re-initialization:**

* `let x;` without initializer leaves `x.drop = 0` and all field flags `0`
  (`stmt.rs:960-971`) — `AGENTS.md §6.6` — so a later `x = move y` does not
  double-drop garbage.
* `let mut x; x = move y;` in `move_check` resets `x` to `Live` on whole-var
  assignment (`move_check.rs:825-834`: `state.insert(name, Live)`), and codegen
  does the pre-drop guard which no-ops on `0`.

**Field moves** (`Owned a = move p.left`) clear only `p.left`'s flag
(`expr.rs:1257-1258`, `move_check.rs:597-623`). `p.right` stays live.
Re-assigning the field (`p.left = move v`) re-initializes it
(`move_check.rs:846-848`, `operators.rs:1529-1561`).

**Method/param moves:**

* `x.consume()` where `consume` has `InstanceValue` receiver moves `x`
  (`move_check.rs:629-664`, `codegen/call.rs:852-854` clears flag).
* `f(move a)` or `f(a)` where param is by-value (`!is_view_type`) moves `a`
  (`move_check.rs:669-701`, `codegen/call.rs:254-256`).

**Return** `return x;` with bare identifier moves `x` (flag cleared,
`move_check.rs:501-512`, `scope.rs` return handling) so the caller receives
ownership without the callee's defers running the destructor. `return move x;`
is equivalent. `return x.field;` is a view copy, not a move
(`move_check.rs:504` comment).

---

## 5. `explicit move always means MoveOut`

In Silver `move` is an **expression** (`Move(inner)`), not a modifier. Its
contract (freeze):

* **For `needs_drop` types** — `move x` and `move x.field` are `MoveOut`:
  the source's drop flag (or field flag) is cleared to `0` at the `move`
  site (`expr.rs:1244-1260`, `call.rs:256`, `generate.rs:1608`), and
  `move_check` transitions the source to `Moved`/`PartiallyMoved`
  (`move_check.rs:569-623`). Any later use of that path is a
  `use_of_moved_value` error with a secondary note pointing at the `move`
  (`AGENTS.md §7`, `ownership-and-moves.md §3`). This is true whether the
  `move` appears in `let T x = move y`, `x = move y`, `f(move y)`,
  `sink(move y)`, `return move y`, or `Res.Ok(move y)`.

* **For `Copy` types** — `move` is allowed but has **no move semantics**:
  `move_check` sees `!is_path_tracked` and falls through to a plain read
  (`move_check.rs:565-567`), and codegen's `lookup_variable(...).drop_flag`
  is `None` so no flag is cleared. The value is copied and the source
  remains live. This matches the test
  `copyable_values_are_not_tracked` (`move_check.rs:1215`):
  `i32 y = x; move x; z = x;` is not an error.

* **Payload rule** — constructing an enum variant with an owned payload
  requires `move` (`typeck.rs:2041-2044`): `Res.Ok(move s)` is ok,
  `Res.Ok(s)` is `payload_must_be_moved`. The enum payload cascade
  (`scope.rs:555-676`) guarantees the payload is dropped if the enum is
  never unwrapped.

In short: write `move` when you mean to transfer ownership; for `Copy` it
is redundant but not harmful, for owned types it is mandatory for soundness.
Future phases may make `T x = y` without `move` a hard error for owned
`T` (copy vs move disambiguation), but today the compiler only enforces it
via the double-drop hazard and the enum payload check.

---

## 6. Missing central `TypeProperties` query (Phase 5 gap)

All three predicates above — `type_has_drop_impl`, `is_tracked` /
`is_path_tracked`, `get_drop_function_name` + `register_field_drops` — encode
the same concept with different coverage and different data sources:

| Subsystem | Set / Map | File | Coverage |
|---|---|---|---|
| typeck | `drop_owner_bases: HashSet<String>` | `bin/agc/src/semantic/typeck.rs:49,699-720,726-743` | Direct `Drop` owners only; pointers excluded; arrays/tuples recursive; type params not tracked |
| move_check | `Facts { drop_owners, struct_fields, value_receivers, value_args }` | `bin/agc/src/semantic/move_check.rs:155-236,330-345` | Direct + transitive struct fields + `Task`; path-aware |
| codegen | `drop_trait_impl_owners: HashSet<String>` + `struct_fields` + `enum_payload_layouts` + generic instantiation | `bin/agc/src/codegen/llvm_ir/scope.rs:526-548,758-901`, `bin/agc/src/codegen/llvm_ir/mod.rs:153`, `bin/agc/src/codegen/llvm_ir/entry.rs:167`, `bin/agc/src/codegen/llvm_ir/generate.rs:922` | Direct + instantiation + field cascade + enum payload |

There is no `TypeProperties { is_copy, needs_drop, is_pod }` query that all
passes import. Consequences today:

* Adding a new owned type (e.g. `Mutex`, `Channel`) requires touching each
  table independently; a miss in `typeck` still allows `Res.Ok(chan)` without
  `move` even though `move_check` and codegen know it is owned.
* The base-name key (`"Vec"` vs `"Vec<String>"`) collapses generic args; a
  future `Copy` bound on type parameters will need per-instantiation
  answers, not base names.
* Pointer/reference exclusion is reimplemented three times (`is_view_type`,
  `type_has_drop_impl` arm, `register_drop_flag` early return,
  `is_pointer_or_reference`).

**Phase 5 plan (not in this freeze):** introduce a single
`TypeProperties` query (likely in `types/mod.rs` or a new
`semantic/type_properties.rs`) that answers `is_copy(ty)`,
`needs_drop(ty)`, and `is_pod(ty)` from a unified `TypeContext` /
`struct_fields` / `drop_impls` view, with substitution for generic args.
All three subsystems will then delegate to it. Until then this document is
the canonical inventory of the split.

See also `AGENTS.md §6` (invariants 2–6) and
`docs/borrow-checker/current-limitations-and-roadmap.md` for the broader
roadmap.

---

## 7. Quick reference: writing correct Silver today

```silver
// Copy types — plain assignment is fine
i32 a = 1;
i32 b = a;          // copy, both live
Point p = { .x = 1.0, .y = 2.0 };
Point q = p;        // copy

// Owned types — always use move
String s = String.from_str("hi");
String t = move s;  // s moved
// s.len();         // ERROR

String u;
u = move t;         // pre-drop on old u is no-op (u was uninit), t moved

Pair pr;
pr.left = String.from_str("a");
pr.right = String.from_str("b");
String a2 = move pr.left;   // partial move, pr.right still live
pr.left = String.from_str("c"); // re-init left

// Enum payloads — move required
Result<String, i32> r = Result<String,i32>.Ok(move u);

// Explicit move is always MoveOut for owned types
sink(move a2);      // a2 moved into callee (by-value param)
r.drop();           // explicit v.drop() also moves (move_check.rs:634)
```

---

*Refs: `bin/agc/src/semantic/typeck.rs:48-49,699-743,2041-2044`,
`bin/agc/src/semantic/move_check.rs:155-236,330-345,564-568,629-701,825-848,1215-1218`,
`bin/agc/src/codegen/llvm_ir/scope.rs:526-548,682-901,913-918,934-938`,
`bin/agc/src/codegen/llvm_ir/stmt.rs:900-971`,
`bin/agc/src/codegen/llvm_ir/expr.rs:1244-1260`,
`bin/agc/src/codegen/llvm_ir/operators.rs:1481-1603`,
`AGENTS.md §6`, `std/mem/drop.ag:7`, `std/string.ag:557`,
`std/mem/vec.ag:174`, `std/map.ag:425`, `std/collections/set.ag:466`,
`std/collections/deque.ag:194`, `std/io/file.ag:584`, `std/sync.ag`,
`std/channel.ag`.*
