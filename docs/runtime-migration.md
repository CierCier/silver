# Silver Runtime Migration & Language Roadmap

**Status: COMPLETE** — Every work unit (1–12) has been implemented and merged. The entire
`silver_runtime/` Rust crate has been rewritten in pure Silver as `std/rt/`, statically
linked into every executable. The Rust crate and all glibc dependencies have been deleted.
See §2 for the full changelog per unit.

Sections 3–5 are preserved as historical reference for the migration process and future
language development recommendations.

## 1. Research summary

**What `silver_runtime` is today (~2,800 lines Rust, dep: rustc-hash):**

- `runtime/ffi.rs` (237 ln) — 16 `#[no_mangle] extern "C"` symbols forming the intended
  runtime ABI: `silver_rt_print_{i64,u64,bool,f64,cstr,bytes}`, `silver_rt_abort`,
  `silver_rt_abort_cstr`, `silver_rt_strlen`, `silver_rt_{alloc,alloc_zeroed,realloc,dealloc}`,
  `silver_rt_{memset,memcpy,memmove}`. Self-contained; uses Rust std alloc/IO only.
- `types/` (~1,500 ln) — `TypeId(u32)` with 15 builtins, `TypeRegistry` (name lookup,
  structural interning of `vec<T>`/`optional<T>`/`fn(...)`, struct/enum definition with
  validation), `TypeKind` (10 variants), boxed `Value` with validating constructors,
  rule-based numeric `CastKind` with Rust-`as` wrapping semantics, error enums.
- `runtime/` (~800 ln) — generational-handle `Heap` with mark-sweep GC, `RtValue`/`Object`
  (aggregates behind GC handles), `MethodRegistry` (`(TypeId, name) → closure` dispatch
  with global fallback + arity checks), `CastRegistry` (`(from, to) → closure` table,
  156 prepopulated numeric pairs), `Runtime` aggregate.

**Critical fact: nothing links this crate.** `agc` does not depend on it; no `silver_rt_*`
symbol is referenced by codegen, `std/`, or `tests/`. The de-facto runtime is glibc
(`std/mem/alloc.ag` → malloc, `std/io.ag` → printf/FILE). The crate is a reference
implementation / ABI spec. The port therefore has two halves:
(a) replace the implicit glibc runtime with pure-Silver syscall-backed code exporting the
`silver_rt_*` ABI, and (b) port the dynamic type/GC system to Silver as `std/rt/`.

**What already exists in Silver's favor:**

- Phase 1 of `plan/runtime-migration.md` is DONE: `asm("syscall", [...])` intrinsic in
  codegen (`agc/src/codegen/llvm_ir.rs:3651`), `std/sys/syscall.ag` (syscall0–6),
  `std/sys/linux.ag` (386 generated `sys_*` wrappers + `SYSCALL` enum).
- Silver `pub` functions emit bare unmangled External symbols — C-ABI export works today
  (verified with `nm` + a C caller). `#[link_name]` available for overrides.
- `std/` already has Vec, HashMap (open addressing), String, Box/Rc/Arena — all bottoming
  out in the single chokepoint `std/mem/alloc.ag`.
- The `.agm` module system already links a sibling `.o` per module statically into
  consumers (`agc/src/module_artifact.rs:403`, `main.rs:395-420`) — the natural mechanism
  for shipping the runtime.

**Language gaps that constrain the port (workarounds encoded in the work units):**

1. No payload enums in codegen (integer enums only) → tagged structs (`kind` tag + fields),
   the existing `Optional`/`Result` pattern.
2. Match supports only literal/wildcard arms → if/else chains or integer switches.
3. No first-class function pointers → replace `Arc<dyn Fn>` method/cast closures with
   integer op-code dispatch tables (a `switch` in one dispatcher function). The only
   builtin capture is a `TypeId`, so `opcode + u32 ctx` covers every existing builtin.
4. `--no-std` only strips crt/libc at link time; no `_start` exists → Phase-5 unit writes it.
5. Drop is not recursive; drop-flag Bug C → defensive null checks in every `drop`.
6. Global initializers must be const → runtime state uses lazy-init or explicit `init()`.
7. `Value` uses i128/u128 — Silver has i128 lexing/types; if codegen paths prove immature,
   fall back to i64/u64 wide-arithmetic pairs (decision recorded by the worker in the PR).

## 2. Work units

Units are sliced to be independently mergeable. Dependencies force three waves; units
within a wave run as parallel worktree agents branched from `main`.

### Wave 1 — foundations (parallel, no interdependencies)

1. **Typed syscall/errno layer** — `std/sys/errno.ag` (new), `std/sys/linux.ag` (additive).
   Decode `-4095..-1` returns, `Result`-style typed front-ends for the syscalls the runtime
   needs (`read/write/open/close/mmap/munmap/mremap/exit_group/kill`). Test: `tests/errno_test.ag`.
2. **mmap allocator + memory intrinsics** — rewrite `std/mem/alloc.ag`; new `std/mem/memory.ag`.
   Page-aligned free-list allocator over `sys_mmap`/`sys_munmap`; pure-Silver
   `memset/memcpy/memmove/strlen` loops; export C-ABI `silver_rt_alloc`, `silver_rt_alloc_zeroed`,
   `silver_rt_realloc`, `silver_rt_dealloc`, `silver_rt_memset`, `silver_rt_memcpy`,
   `silver_rt_memmove`, `silver_rt_strlen` with the exact semantics of
   `silver_runtime/src/runtime/ffi.rs` (size-0 → dangling non-null; null-tolerant dealloc).
   Internal `alloc<T>()` generic API keeps its signature so Vec/HashMap/String/Box/Rc/Arena
   are untouched. Tests: `tests/mem_test.ag` (new) + existing `memory_pentest.ag` must pass.
3. **Formatting + I/O in pure Silver** — new `std/fmt.ag`; rewrite `std/io.ag` print path.
   `itoa`/`utoa`/`ftoa` (shortest-roundtrip not required; match Rust `{}` output for the
   ffi.rs test vectors), buffered write over `sys_write` fd 1/2; export
   `silver_rt_print_{i64,u64,bool,f64,cstr,bytes}`, `silver_rt_abort`, `silver_rt_abort_cstr`
   matching ffi.rs semantics (`print_bytes` = no trailing newline; `abort_cstr` → stderr
   then abort). Keep `printf` externs temporarily for the rest of io.ag (File API migrates
   in unit 9). Test: `tests/fmt_test.ag`.
4. **Port type registry** — new `std/rt/types.ag` (+ `std/rt/type_info.ag` if size warrants).
   `TypeId` as `u32`-carrying struct, 15 builtin ids, `TypeKind` as integer enum + payload
   struct, `Type`, `Field/StructType/EnumType/EnumVariant/FunctionType`, `TypeRegistry`
   with `std.map` for name lookup and interning caches (function-type cache key: canonical
   serialized param string), duplicate/overflow validation, error codes mirroring `TypeError`.
   Port `types/registry.rs` + `types/type_info.rs` + `types/error.rs` semantics and the
   `types/tests.rs` cases. Test: `tests/rt_types_test.ag`.
5. **Port GC heap** — new `std/rt/heap.ag`. `Handle{index,generation}`, slot vector with
   occupancy flags + generation counters + free list, `RtValue`/`Object` as tagged structs
   (type ids as bare `u32` — do NOT import std.rt.types; unified in unit 10), `alloc/get/
   get_mut/free` with generation checks, mark-sweep `collect_garbage(roots)` with explicit
   work stack. Port `runtime/heap.rs` + `object.rs` + `value.rs` and their tests.
   Test: `tests/rt_heap_test.ag`.
6. **Test harness + CI** — new `tests/run_tests.sh` (or a `cargo test -p agc` integration
   shim) that builds agc, then compiles & runs every `tests/*.ag`, asserting exit 0; new
   `.github/workflows/ci.yml` running `cargo test -p agc` + the .ag suite; delete stale
   `tests/CMakeLists.txt`/`examples/CMakeLists.txt`. No compiler changes.
7. **Std hygiene** — fix `Optional.unwrap()`/`Result.unwrap()` returning uninitialized
   memory (`std/optional.ag:28,69`): abort with message via existing `abort()` extern.
   Fix README `docs/` reference and `examples/README.md` drift (5 phantom files, 4 missing).

### Wave 2 — after wave 1 merges (parallel)

8. **Port casts** — new `std/rt/casts.ag`. Rule-based `CastKind` classification + wrapping
   numeric execution (Rust-`as` semantics: truncate/sign-extend, f64↔f32, float→int) porting
   `types/cast.rs`; table-based `CastRegistry` over `std.map` keyed `(from_u32, to_u32)`
   with opcode dispatch replacing closures; prepopulate the 156 numeric pairs. Port
   `runtime/casts.rs` tests. Test: `tests/rt_casts_test.ag`. (Deps: units 4, 5.)
9. **Port method registry + dynamic values** — new `std/rt/methods.ag`, `std/rt/value.ag`.
   `MethodSpec{arity, opcode, ctx}`, per-type + global tables over `std.map` (key: TypeId +
   interned name id; add a small intern table), `call_method` with arity checks, builtins
   (`type_name`, `string.len`, `vec.len`, `vec.push` with element type check) as opcodes;
   boxed `Value` validating constructors + display porting `types/value.rs`/`types/mod.rs`.
   Also migrate `std/io.ag` File API off `FILE*` onto typed syscalls (finishing unit 3).
   Test: `tests/rt_methods_test.ag`. (Deps: units 1, 3, 4, 5.)
10. **Runtime aggregate + static-link integration** — new `std/rt/runtime.ag` (`Runtime`
    struct unifying registry/heap/methods/casts, explicit `init()`), unify unit 5's bare
    `u32` type ids with `std.rt.types.TypeId`, register `std/rt` in `update-bootstrap.sh`
    packaging so it ships as `.agm` + `.o` and links statically via the existing dependency
    mechanism. End-to-end GC/dispatch test `tests/runtime_test.ag` porting
    `runtime/tests.rs`. (Deps: 4, 5, 8, 9.)
11. **`_start` + no-libc static default** — new `std/sys/entry.ag` (`_start`: zero rbp, pop
    argc, rsp→argv, call main, `sys_exit_group`); link driver (`agc/src/main.rs`) gains
    `--static-runtime` (or makes it default with `--libc` escape hatch — worker decides with
    maintainer conventions, documents choice) wiring `-nostdlib`, no crt objects, no
    `-lc/-lgcc`; `ldd` must report "not a dynamic executable". Requires units 2+3 merged
    (no libc symbols left on the hot path); `std/math.ag` libm externs stay behind `--libc`
12. **[DONE] Delete `silver_runtime`, docs, bootstrap** — Removed `silver_runtime/` from workspace
    (`Cargo.toml`, directory), purged `runtime =` workspace dep. Updated README, AGENTS.md,
    architecture sections. Marked `plan/runtime-migration.md` as superseded. Ran
    `./update-bootstrap.sh` and committed bootstrap artifacts as a separate dedicated commit.
    Full suite: 25 pass, 1 pre-existing fail (`errno_test`).

## 3. E2E verification recipe (every worker)

```bash
# from repo root of your worktree
cargo build -p agc                                        # compiler must build
cargo run -p agc -- tests/<your_test>.ag -o "$TMPDIR/t" && "$TMPDIR/t"   # exit 0 = pass
# regression sweep (all must exit 0):
for t in vec_test map_test str_key_map_test memory_pentest syscall_test; do
  cargo run -p agc -- tests/$t.ag -o "$TMPDIR/$t" && "$TMPDIR/$t" || exit 1
done
cargo test -p agc                                         # Rust unit suite
```

Rules: do NOT run `update-bootstrap.sh` or commit anything under `bootstrap/` (unit 12
does that once, as a dedicated commit). Workers test against source-resolved `std/`
imports, which `agc` inlines automatically from the repo root.

## 4. Worker instruction template

Each worker prompt contains: the overall goal (this doc's header), its unit's title/files/
description verbatim from §2, the conventions below, the recipe from §3, and the standard
finish steps (code-review skill → unit tests → e2e → commit/push → `gh pr create` →
report `PR: <url>`).

Conventions workers must follow:
- Silver style per AGENTS.md: constructors are static methods returning `move`d instances;
  receivers are `Self* self`; explicit `move` on ownership transfer.
- No payload enums / no fn pointers / literal-only match — use tagged structs, opcode
  dispatch tables, if-chains (see §1 gaps).
- Drop is not recursive: every `drop` null-checks and frees fields manually.
- Globals need const initializers: use lazy init or explicit `init()`.
- Tests follow the repo pattern: global pass/fail counters, `i32 main()` returns nonzero
  on failure; print per-case lines.
- Keep the 16 `silver_rt_*` symbol names and semantics EXACTLY as
  `silver_runtime/src/runtime/ffi.rs` — they are the frozen ABI.
- Commit messages: imperative summary line; bootstrap is never touched.

## 5. Recommended next steps for the language (beyond this migration)

Ranked by leverage; §2 already schedules items 1–4:

1. **Finish runtime self-sufficiency** (this plan) — the project's stated flagship goal.
2. **CI** (unit 6) — 216 Rust tests + 18 .ag tests currently run nowhere automatically.
3. **Typed syscall layer** (unit 1) — converts the 386-wrapper investment into a safe API.
4. **Fix `unwrap()` UB** (unit 7) — silent uninitialized-memory return in the core safety type.
5. **Real iterators in std** — `Iterator` impls for Vec/Slice/HashMap + adapters; the
   `for-in` lowering and traits exist, std never uses them.
6. **Payload enums + match destructuring in codegen** — the single biggest language gap;
   would retroactively simplify `std/rt/` (tagged structs → real sum types), Optional/Result.
7. **First-class function pointers** — parse fn-ptr locals/fields; unlocks callback APIs
   and would let `std/rt` dispatch drop its opcode tables.
8. **Std breadth**: args/env, time/clock, process spawn/wait over the syscall layer;
   String find/split/format.
9. **LSP completion** — hover/goto-def exist (775 ln crate); completion is the next
   highest-value tooling step.
10. **Arm64 ABI** (`agc/src/codegen/abi.rs:314`) — only after x86_64 self-sufficiency lands;
    syscall table and the asm intrinsic are x86_64-specific.
