# Silver: language review and six-month plan

Audit date: 2026-10-13. Planning window: 2026-10-13 through 2027-04-12.
Source baseline: `dae54069193ec6a3350a2424e9431052c313181e`, branch `bootstrap-migration`.

Live tracker: [GitHub project board](https://github.com/users/CierCier/projects/1/views/2), with [all chunks](https://github.com/users/CierCier/projects/1/views/1) and [timeline](https://github.com/users/CierCier/projects/1/views/3) views. Issues #31–#43 track C01–C13 with six milestones, priorities, dates, acceptance checklists and blocking relationships. C01 starts Ready; later chunks start in Backlog. Use issue status for execution progress; retain this document for the feature review and planning rationale.

## Recommended goal

Make Silver a dependable Linux systems language for command-line tools, compiler tooling, and small network services. By April, a developer should be able to install the native compiler, build a multi-package application, diagnose mistakes in the editor, and trust its documented ownership and cleanup behavior.

The language already has enough features to support this goal. The priority is making those features compose reliably. Preserve C-style declarations, concise receivers, inferred locals, explicit resource ownership, and useful generic APIs. Add syntax only when real programs demonstrate a need that existing constructs cannot meet cleanly.

This plan assumes one primary implementation stream. It contains 13 sequential two-week chunks, with the final chunk reserved for release validation and spillover. Dates are planning targets; failed correctness gates take precedence over the calendar.

## What this review establishes

This is a source and test-inventory audit, not a fresh certification run. I inspected the language reference, compiler architecture, both stage boundaries, native backend, package driver, standard-library families, fixtures, self-host gates, and CI configuration. I did not execute compiler or runtime suites during this documentation task. An implementation or test existing does not establish that every combination works.

The tree contains 123 standard-library Silver files, 33 compiler-library Silver files, 14 binary-package Silver files, 30 example Silver files, and 290 top-level integration fixtures. These are inventory counts, not passing-test counts.

The [self-host completion record](docs/selfhost-completion-record.md) reports an October 12 baseline of 609 Cargo tests, stage0 integration at 283 passed / 0 failed / 4 skipped, and stage1, stage2, and stage3 integration at 285 / 0 / 2 each. Stage2 and stage3 matched binary hashes in that local build. Those results predate the current HEAD and do not prove current or universal parity. The expanded generational gate exists in [check_stage2.py](tests/selfhost/check_stage2.py); full stage2/stage3 corpora remain opt-in in the checked-in workflow.

Use these status terms throughout the inventory:

| Status | Meaning |
| --- | --- |
| Present | Implementation and representative fixtures exist; broader guarantees still require tests. |
| Partial | A specific limitation or incomplete stage boundary is visible. |
| Missing | No supported implementation was established in the inspected language/compiler paths. |
| Unverified | Available evidence is insufficient; investigate before committing to a fix. |

## Immediate findings

| Priority | Finding | Evidence and action |
| --- | --- | --- |
| P0 | The syntax guide understates the implemented language. | Binary/octal literals, separators, square-bracket tuples, destructuring, slices, and selected import aliases have fixtures. Correct the guide before adding them to a feature backlog. |
| P0 | Stage1 compound assignment can evaluate a side-effecting target twice. | [lower_expr.ag](libs/agc/src/frontend/lower_expr.ag), compound-assignment lowering, constructs separate target and load expressions and explicitly records the divergence. Reproduce with an incrementing index or function-returned receiver, then lower the place once. This is a source finding, not a fresh runtime reproduction. |
| P0 | Target selection can disagree with code generation. | The [native driver](bin/agc/src/native_backend.ag) applies `--target` to cfg, while [LlvmGenerator.new](libs/agc/src/backend/llvm/generator.ag) selects `LLVMGetDefaultTargetTriple()`. Reject unsupported native targets until target, ABI, runtime, and linker agree. |
| P0 | `check` is not established as a complete semantic gate. | [typecheck_expr.ag](libs/agc/src/frontend/typecheck_expr.ag) can return from checking an unsupported lowered function; generic probing forwards selected diagnostics. Inventory these paths and require either complete checking or a useful unsupported diagnostic. |
| P0 | Safety behavior varies by API and build profile. | [Slice indexing](std/slice.ag) checks bounds under `@cfg(debug)`; [Vec access](std/mem/vec.ag) includes unconditional checks. Define and test the intended contract. Also correct `Slice.get` documentation, which promises Optional while returning `T` and aborting. |
| P1 | Stage1 enum layout fixes the tag to `i16`. | [generator.ag](libs/agc/src/backend/llvm/generator.ag) and [mono.ag](libs/agc/src/backend/llvm/mono.ag) select `LLVMInt16TypeInContext`. Establish a discriminant-range policy and test direct and indirect C ABI boundaries. |
| P1 | Native cache invalidation is too broad. | [native_object_cache_path](bin/agc/src/native_backend.ag) hashes every loaded source for each object. Make keys reflect actual dependencies without losing correctness. |
| P1 | CLI acceptance exceeds implemented behavior. | `--tree-shake` and `--no-tree-shake` are registered in [cli.ag](bin/agc/src/cli.ag), with no corresponding use found in stage1 compilation. Implement the promised behavior or report that it is unsupported. LLVM optimization is a separate mechanism. |
| P1 | Package and artifact independence is incomplete. | [workspace.ag](bin/agc/src/workspace.ag) rejects Git dependencies; [agsm.ag](bin/agc/src/agsm.ag) rejects foreign module generation. Prebuilt artifacts and local workspaces already work in supported cases. |
| P1 | Public status documents contradict newer evidence. | [stage1-checklist.md](stage1-checklist.md) predates full generational runs; [SYNTAX.md](SYNTAX.md) describes Optional/Result as structs and channels as unbounded MPSC. Source defines payload enums and bounded/unbounded channel operations. |

## Language feature review

| Feature | Current status | Missing, worth adding, or next focus |
| --- | --- | --- |
| Declarations and visibility | Present: C-style functions, typed locals, inferred `let`, public items by default, `private`. | Preserve this identity. Check visibility across source imports and artifacts rather than introducing another visibility spelling. |
| Receiver syntax | Present: `self`, `&self`, `&mut self`, explicit typed receivers, and raw-pointer receivers. [Fixture](tests/syntax_guards_test.ag). | Make method lookup and borrow behavior consistent across shorthand, explicit, generic, and callback contexts. |
| Literals | Present: decimal, hex, binary, octal, separators, string escapes, and wide-integer fixtures. [Fixture](tests/literals_and_reflection_test.ag). | Fix documentation; test malformed literals, overflow, and context-dependent defaults. Do not schedule binary/octal syntax as new work. |
| Numeric types | Present: signed/unsigned widths through 128 bits and float types in the type table. Complex types remain incomplete. | Specify overflow, shifts, narrowing, division, NaNs, and target-dependent `f80` behavior. Defer complex arithmetic until demanded by an application. |
| Strings and characters | Present: NUL-terminated `str`, owned length-bearing `String`, UTF-8 helpers, and `char`. | Make byte versus code-point behavior explicit. Audit embedded NUL handling and borrowed views through conversions. |
| Arrays | Present: fixed-size, nested, global, and aggregate initialization fixtures. | Cover bounds, size arithmetic, partial initialization, and element cleanup uniformly. |
| First-class slices | Present: `[T]`, `&str`, library `Slice<T>`, slicing and auto-borrow cases. [Fixture](tests/test_first_class_slices.ag). | Document the actual spelling and coercions. Prove owner lifetime, mutation, reallocation invalidation, and release bounds behavior. |
| Tuples and destructuring | Present: `[T, U]`, `[a, b]`, `let [a, b]`, nested tuples, generic returns, and partial-drop fixtures. [Fixture](tests/destructure_let_test.ag). | Preserve square-bracket syntax. Test nested ownership and wildcard cleanup; remove the obsolete blanket claim that tuples are unsupported. |
| Structs and layout | Present: generic structs, positional/designated initialization, packed/layout fixtures. | Specify private-field access and representation guarantees; prove imported and nested aggregate ABI behavior. |
| Enums | Present: unit/payload variants, discriminants, generics, matches, and destructor behavior. | Resolve tag-width limits and signed/large discriminants. State representation guarantees separately from C ABI conversion. |
| Pattern matching | Present: expression matches, payload binding, move binding, guards, wildcards, and exhaustiveness logic. [Guard fixture](tests/match_guard_test.ag). | Expand nested pattern, guarded exhaustiveness, unreachable-arm, and ownership checks. Do not assume general destructuring patterns from tuple-local support. |
| Control flow | Present: branches, ternaries, loops, `break`, `continue`, block expressions, and return. | Prioritize single evaluation and cleanup on every exit. Value-bearing `break` remains a contract to establish, not a promised feature. |
| Operators and casts | Present: numeric/bitwise/logical operators, overloading protocols, custom casts, indexing, and compound assignments. | Repair compound target evaluation; unify overload selection, conversions, and temporary lifetime. |
| Functions and callbacks | Present: free/inherent/trait functions, overloads, declared function pointers, and callback fields. [Fixture](tests/native_function_pointer_test.ag). | Complete supported function-value inference and returns, preserve C calling-convention metadata, and diagnose ambiguous overloads. |
| Generics and inference | Present: monomorphization, nested calls, method inference, generic containers, and tuple returns. | Resolve multiple arguments and expected return types consistently. Keep specialization deterministic and recursive failures bounded. |
| Traits and associated types | Present: impls, associated types, defaults/operator protocols, and inferred method/operator guard fixtures. | Define overlap/coherence and selection rules. Improve missing-bound diagnostics before adding specialization or trait objects. |
| Iteration | Present: owning and borrowed `for in`, iterator adapters, filtering, folding, zipping, and flattening. | Prove partial-consumption cleanup and borrow preservation across all collections. Improve inference before adding more adapter names. |
| Copy and move | Present: implicit transfer, explicit `move`, field projections, reinitialization, and Drop-ancestor rejection. | Finish typed control-flow analysis for joins, loops, aliases, indexed places, and nested temporaries. A passing fixture set is not a general ownership proof. |
| References and raw pointers | Present: borrow origins, conflict/escape checks, raw pointer arithmetic and FFI escape hatches. | Publish the exact safety boundary. Audit reference-to-pointer/view conversions; avoid claiming Rust-equivalent safety or race freedom. |
| Destructors and `defer` | Present: RAII, automatic struct-field cascade, enum payload cleanup, custom enum Drop, LIFO defer. | Establish exactly-once cleanup for all transfers, overwrites, early exits, and failed operations. Keep struct and custom-enum contracts distinct. |
| Optional and Result | Present: payload enums in [optional.ag](std/optional.ag) and [result.ag](std/result.ag), including move extraction. | Standardize recoverable errors and reduce repetitive handling. Do not redesign these as tagged structs. |
| Fallback/error syntax | Present: lazy `value ? fallback` for supported values. [Fixture](tests/unwrap_or_test.ag). | Automatic error propagation is a separate possible addition. Existing `?` means fallback, so any new spelling needs an explicit design decision. |
| Imports and aliases | Present: whole-module imports, selections, aliases, source/AGM paths. Selections are described as non-restrictive in the [fixture](tests/selective_import_test.ag). | Define namespace collisions, visibility, and whether selections restrict scope. Do not silently change semantics to match another language. |
| Constants, static, volatile, cfg | Present: const validation, static/global storage, volatile operations, item/expression/platform cfg. | Test shadowing, initialized storage, dead imports, target consistency, and build-profile differences. |
| User macros | Partial: positional/variadic expansion, selected expression control flow, statement expansion, argument-loop unrolling. | Add hygiene and expansion-site diagnostics before broadening macro expressiveness. Test evaluation count and capture collisions. |
| Built-ins and derives | Present: formatting, assertions, source-location/debug macros, size/alignment, hashing, memory operations, JSON synthesis. | Prefer reusable trait/library behavior. Document implicit allocations and cleanup; keep generated diagnostics tied to caller source. |
| Compile-time evaluation | Partial: token and stage0 folding support; general evaluator parity is not established. | Specify a small deterministic supported subset with limits. Defer a full compile-time execution language. |
| Threads and Send | Present: `launch`, consuming `wait`, structural Send checks, failure cleanup, OS threads. | Registry is fixed at 64 slots; unwaited tasks persist until reaped. Verify resource limits and result lifetimes. This is not lexical structured concurrency. |
| Closures, async, scoped borrowing | Missing as established general language features in inspected paths. | Consider closures or scoped tasks only after ownership/callback contracts hold. Defer an async scheduler and coroutine lowering. |
| Inline assembly and FFI | Present: assembly, extern declarations, link names, C ABI probes, optional Rust C-ABI bridge. | Make target restrictions explicit; test callbacks, aggregates, ownership transfer, and invalid signatures. Foreign syntax acceptance alone proves no ABI guarantee. |

## Standard library and runtime review

| Area | Current status | Next focus |
| --- | --- | --- |
| Allocation and smart pointers | Present: allocator, typed allocation, Box, Rc, Arena, leak tracking, and multithreaded allocation paths in [std/mem](std/mem). | Audit alignment, size overflow, allocation failure, thread teardown, and ownership of raw pointees. Add fallible allocation only with a real caller and a coherent error contract. |
| Collections | Present: Vec, HashMap, HashSet, Deque, Queue, Heap, BitSet, and interning. | Verify owned keys/values, replacement, removal, resize, and partially consumed iterators. Specify iteration order. No need for another collection family yet. |
| Algorithms and iterators | Present: sort/search/reverse and a broad [iterator library](std/iter.ag). | Document mutation and comparator contracts; test non-Copy items and borrowed adapters. Avoid callback workarounds caused by inference limitations. |
| Strings, bytes, text | Present: String, Bytes, Slice, text utilities, UTF-8 tests, slicing, comparison, and splitting. | Make byte-oriented and Unicode operations distinguishable; test embedded NULs, invalid encodings, and dangling subviews. |
| Formatting and logging | Present: Display, buffered output, format macros, logging, source reflection, and profiler support. | Check formatting of generic/owned values and failure paths. Keep output allocation and borrowing predictable. |
| Files and filesystem | Present: File, buffered I/O, scanner, streams, paths, metadata, directories, and structured read/open errors. | Error contracts remain mixed: [fs.ag](std/fs.ag) has Result APIs alongside bool-returning writes/removals. Preserve error context, partial I/O, and cleanup. |
| Processes and CLI | Present: argument parsing, environment, Command/Child, exit status, and workspace CLI use. | Exercise quoting, child failure, descriptor inheritance, environment ownership, and target limitations through real commands. |
| Synchronization | Present: Mutex/Guard, RawMutex, WaitGroup, Condvar, RwLock and guards, atomics. [Implementation](std/sync.ag). | Audit shared access and guard lifetimes together with Send. Stress contention and shutdown; do not assume a full Sync-like guarantee exists. |
| Channels | Present: bounded/unbounded queues, blocking and try operations, close-and-drain behavior. [Implementation](std/channel.ag). | Test multiple producers/consumers, close while blocked, rejected-send ownership, and queue cleanup. Endpoint ownership and returned unsent values are design candidates, not approved APIs. |
| TCP/UDP/DNS | Present: socket/address abstractions, transports, resolver, timeout fixtures. | Test malformed input, timeouts, cancellation strategy, descriptor cleanup, and supported address families. Source breadth does not certify every protocol case. |
| HTTP/TLS and streaming protocols | Present: HTTP/1.1, HTTP/2, HPACK, TLS via OpenSSL, WebSocket, SSE, cookies. | Harden size limits, partial frames, malformed peers, TLS verification failures, disconnects, and bounded memory. Do not expand into another major protocol this cycle. |
| OS event APIs | Present: syscall wrappers, epoll, eventfd, io_uring, vector I/O and sendfile. | Document Linux/kernel requirements and error handling. These APIs do not imply a general language async runtime. |
| JSON and serialization | Present: readers/writers, traits, generated methods, containers, malformed-input fixtures. | Bound recursion and allocations; test duplicate fields, unknown fields, numeric limits, and cleanup after partial decode. |
| Time, random, UUID, hashing, math | Present: utility modules, SHA-256, integer math and libm bindings. | State units, entropy sources, numeric limits, and external dependencies. Avoid presenting SHA-256 availability as a complete cryptography library. |
| Terminal support | Present: ANSI, keys, raw mode, event loop, PTY, size handling. | Exercise real terminal restoration on error and exit, resize behavior, and platform exclusions. Keep terminal work application-driven. |
| Dynamic runtime services | Present: explicit generational heap, runtime values, type registry, casts and method dispatch under [std/rt](std/rt). | Clarify the intended user and ownership boundary. Native RAII does not require using this heap. Remove obsolete comments claiming the language lacks payload enums. |
| Testing and observability | Present: std.test, leak diagnostics, backtraces, profiler, integration and performance tools. | Make failure messages, cleanup counts, and compiler identity part of reproducible reports. Backtraces and leak checks complement, but do not replace, semantic checking. |

The core runtime is freestanding on its supported native path. That does not make every program dependency-free: [TLS](std/net/tls.ag) uses OpenSSL shared libraries and [math](std/math.ag) binds libm. Publish dependencies per module.

## Compiler, tools, and distribution review

| Area | Current status | Next focus |
| --- | --- | --- |
| Rust stage0 | Broad bootstrap implementation with unit tests and target-specific ABI code. | Keep it as a bootstrap and comparison compiler during this plan. Fix shared semantics in both stages; do not delete it merely because a self-build succeeds. |
| Silver stage1 | Native frontend/backend and workspace driver exist; supported native commands no longer delegate to stage0. | Complete semantic contracts and native artifact behavior. Use stage0 comparisons as evidence, not as permission to preserve a shared bug. |
| Stage2/stage3 | Full local runs and one matching-binary observation are recorded. | Repeat against release candidates and run the expanded gate regularly. Distinguish behavioral agreement, same-mode reproducibility, and cross-generation hash equality. |
| Compiler architecture | Reusable library and driver packages exist; canonical types and lowered expression pools exist. | Reduce repeated CST/type reconstruction as each feature is repaired. `expr.ag` is over 6,000 lines and lowering over 4,000; extract cohesive responsibilities when touched, without a whole-compiler rewrite. |
| Ownership pipeline | Place/type machinery and many negative/runtime regressions exist; stage1 still has syntax-oriented tracking and separate metadata. | Establish shared typed facts and control-flow joins consumed consistently by checking and drop emission. |
| Diagnostics | Message catalogs, suggestions, source spans, and focused ordering/Send tests exist. | General mismatch reporting, imported locations, macro origins, notes, and consistent check/build/editor results need stronger gates. |
| Native build and cache | Parallel module work, cache integrity, cleanup, workspace builds, and reproducibility tests exist. | Dependency-local invalidation, accurate progress, explicit option support, and correct no-cache behavior are higher value than another backend. |
| Packages | Local/nested workspaces and a manifest specification exist; stage0 has Git-source support, stage1 rejects it. | Reach reproducible package reuse through pinned sources and an offline path. The [v0.1 package spec](docs/Silver%20Package%20Specification%20v0.1.md) explicitly lacks lockfiles, registry solving, archive/checksum contracts. |
| AGM and foreign bindings | Readers and supported publication/import paths exist; rich native metadata parity and foreign generation remain incomplete. | Version the contract, validate incompatible target/layout metadata, and round-trip owned generic exports. Keep foreign generation an explicit separate capability. |
| Silver language server | Hover, definitions, document symbols, completion, diagnostics, and formatting are wired in [main.ag](bin/aglsp/src/main.ag). | Reuse compiler facts and improve project/import resolution. The native server does not advertise the Rust server's ownership inlay hints or rename/reference support. |
| Formatting | Native formatter trims whitespace and blank lines. [Implementation](bin/aglsp/src/format.ag). | A syntax-aware formatter is useful, but must preserve literals/comments and be idempotent. Start with existing syntax and explicit unsupported handling. |
| Debugger support | Stage0 has DWARF machinery; stage1 has backtrace support, but full debugger metadata parity is not established. | Inspect emitted artifacts and use a debugger to prove locals, fields, imports and generic frames. Do not equate a crash backtrace with DWARF parity. |
| Linux | Main self-host and native integration path. | Declare the exact supported architecture/toolchain and prioritize x86_64 Linux correctness and installation. |
| WASI | Stage0 target/runtime and a dedicated CI job exist. | Preserve that support and report skips. Stage1 cross-target parity is a separate project; do not claim it from LLVM target registration. |
| Windows | Stage0 ABI/link/runtime work and historical cross-execution evidence exist; port notes contain conflicting historical approaches. | Reconcile documentation and run a real host gate before expanding the support promise. No dedicated Windows job appears in current CI. |
| Other targets | No supported end-to-end stage1 macOS/AArch64 path established by this audit. | Defer broad portability until target, layout, syscalls, assembly, allocator, linker and tests have an explicit port budget. |
| CI and releases | Linux native and WASI jobs, bundle/install scripts, Nix setup, and optional FFI checks exist. | Add periodic full generational checks, fresh-install smoke tests, explicit skip accounting, and release artifacts tied to tested commits. |

## What distinguishes Silver

Silver's strongest potential is the combination of C-style source, inferred generic convenience, deterministic cleanup, concise borrowing receivers, built-in task syntax, and a library broad enough to write real tools. Square-bracket tuples, Python-style slicing, and `value ? fallback` give it recognizable choices of its own.

These ingredients are not individually unique. The advantage must come from how well they work together. A user should be able to write a parser, CLI, or service with few casts, little ownership boilerplate, and predictable resource use. Today, uneven stage support and stale documentation weaken that advantage more than missing syntax does.

| Comparison | Similarity | Difference and lesson for Silver |
| --- | --- | --- |
| C and C++ | Familiar declarations, pointers, native control; C++ also uses RAII and generic abstractions. | Silver combines these with move/borrow checks and payload enums. Keep C interoperability clear and avoid accumulating competing ways to express the same contract. [C++ resource guidance](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#S-resource). |
| Rust | Ownership, borrowing, deterministic cleanup, traits, generics, enum-based errors, and Send-like checks. | Silver favors C-style declarations, concise receivers, and easy raw-pointer access. Its safety guarantees are less established and must be stated narrowly. Adopt Rust's precision about guarantees without copying its surface syntax. [Rust ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html). |
| Zig | Native systems work, explicit low-level access, defer, compile-time ambitions, and C interoperability. | Silver already emphasizes compiler-managed drops and trait-based APIs. Zig's explicit allocation/error contracts are useful references; Silver's general comptime support is much less established. [Zig reference](https://ziglang.org/documentation/master/). |
| Go | Task/channel ergonomics and a practical networking-library direction. | Silver uses owned values and 1:1 OS tasks with a bounded registry; Go has garbage collection and scheduled goroutines. Borrow library usability ideas without implying equivalent task scale or runtime behavior. [Go FAQ](https://go.dev/doc/faq). |

There is no benchmark evidence in this audit that Silver beats these languages. Self-hosting is valuable evidence that Silver can support a substantial application; it is not by itself a user-facing competitive advantage.

## How the plan fits the way we write

1. Prefer `&self` and `&mut self` for caller-owned state; keep raw pointers where the ownership or FFI contract requires them. Fix checker limitations that force pointer workarounds.
2. Keep type inference useful at constructors, generic calls, iterator adapters, and return boundaries. Remove repeated casts only when the compiler has enough information to do the right thing.
3. Use existing traits, stdlib helpers, and structured errors. Replace repeated high-level bool/sentinel failure handling where callers need context.
4. Preserve automatic field cleanup. Require a clear reason for explicit resource release, and prove exactly-once behavior when changing it.
5. Land one coherent change at a time with source, tests, and docs aligned. Avoid both speculative abstractions and permanent special cases for one fixture.
6. Design public syntax with before/after examples and tradeoffs. This plan recommends decision points; it does not approve new keywords or incompatible API changes.
7. Use real applications to choose ergonomics work. The compiler and AGLSP are existing workloads; add one CLI and one bounded service workload, not a collection of toy demos.

## Six-month delivery sequence

Each chunk is a planning envelope containing small, atomic changes. Reserve roughly one quarter of each chunk for regressions, review, and documentation. C13 is additional contingency. Do not compensate for an overrun by weakening a safety gate.

| Milestone | Chunks | Intended result |
| --- | --- | --- |
| Month 1 | C01–C02 | An accurate contract and no silent known option/evaluation mismatches. |
| Month 2 | C03–C04 | Stronger typed checking, ownership, and cleanup for the supported core. |
| Month 3 | C05–C06 | Predictable incremental builds and reliable artifact boundaries. |
| Month 4 | C07–C08 | Reproducible package reuse and consistent generic/library ergonomics. |
| Month 5 | C09–C10 | Useful editor diagnostics and safer everyday development. |
| Month 6 | C11–C13 | Application evidence, measured costs, and a validated Linux preview release. |

### C01: establish the supported contract

October 13–26. Dependency: none. Owner area: docs, test gates, compiler driver.

- Reconcile SYNTAX, README, checklist and completion records. Document existing tuples, slices, literals, aliases, Result/Optional, channels and receiver shorthand. Replace historical status claims with dated evidence.
- Capture fresh stage0/stage1 baselines on the same commit. Record compiler hashes, build flags, fixture counts and every skip. Add a scheduled or manually triggered full stage2/stage3 gate using the existing script.
- Enumerate supported native options and targets. Immediately reject unsupported target requests before cfg and code generation can disagree. Audit remaining accepted-but-ineffective options.

Done when every advertised language family has a source/test reference, published examples match the implementation, target rejection has a regression, and baseline failures are either repaired or assigned a concrete blocking issue. Historical test totals do not satisfy this gate.

### C02: repair evaluation, bounds and representation defects

October 27–November 9. Dependency: C01. Owner area: lowering, backend, collections.

- Reproduce and fix compound assignment so the target and index evaluate once. Cover calls, fields, pointer targets, and early failure in both compiler stages.
- Decide the safe indexing contract for Vec, Slice and arrays. Recommended default: checked public access in every profile, with any unchecked operation explicit. Measure costs and review compatibility before changing APIs.
- Test enum tag limits and indirect C callbacks. Fix incorrect representation where tractable; otherwise reject out-of-range/unsupported forms explicitly and retain them as release blockers if they are advertised.

Done when side-effect counters agree across stages and optimization levels, bounds tests have deterministic documented outcomes, and the supported discriminant/ABI matrix has no silent truncation. Broader ABI completion can continue in C06.

### C03: make checking use resolved type facts

November 10–23. Dependency: C02. Owner area: frontend types, lowering and diagnostics.

- Inventory unsupported-body exits, unknown-type propagation, generic probe filtering, and checks rediscovered in codegen. Select the core expression families used by the compiler and standard library first.
- Retain resolved bindings, argument/return types, field projections and selected callees for those families. Reuse them in `check`, build and LSP instead of adding another parallel type query.
- Require invalid core programs to fail during checking with a source diagnostic. Unsupported constructs must produce an explicit limitation rather than a successful but incomplete check.

Done when the agreed core matrix covers literals, calls, methods, generics, aggregates, assignments and returns; each has valid and invalid cases; and check/build agree on semantic acceptance. Link/environment errors remain a separate build responsibility. A whole-frontend rewrite is outside this chunk.

### C04: prove ownership at control-flow boundaries

November 24–December 7. Dependency: C03. Owner area: ownership, borrow analysis and drop emission.

- Use typed places for the supported core and implement explicit state merges at branches and loops. Cover partial initialization, reinitialization, field moves, indexing, aliases and temporaries.
- Audit references and views across returns, global stores, container mutation/reallocation, callbacks and generic wrappers. Unknown provenance must not silently become a safe borrow.
- Make cleanup consume the same ownership decisions. Exercise struct cascades, custom enum Drop, defer, return/break/continue, overwrite, partial iteration and failed construction/spawn.

Done when every boundary has an accepted control, a rejected misuse where applicable, and an execution case proving destructor counts or released resources. `memory_pentest`, leak-focused fixtures and self-builds must pass. If the core remains incomplete, move later discretionary work out of this schedule.

### C05: make incremental compilation local and measurable

December 8–21. Dependencies: C03–C04. Owner area: native build graph and cache.

- Replace whole-closure object keys with actual source/import/artifact dependency keys. Preserve compiler, flags, target, cfg, generated-code and imported-generic dependencies.
- Make cold, warm, leaf-edit, shared-dependency-edit and no-cache builds observable. Preserve corruption recovery, concurrent writes, symlink-safe cleaning and module runtime-state ownership.
- Correct progress accounting. Benchmark the compiler workspace and one independent multi-module application using at least ten comparable runs per case.

Done when a leaf edit rebuilds exactly the affected dependency closure, warm builds reuse valid objects, and both cached and uncached executables produce the expected changed result. Publish rebuilt-module sets and median timing/RSS results; do not substitute faster timing for cache correctness.

### C06: stabilize native artifacts and build options

December 22–January 4. Dependency: C05. Owner area: AGM, ABI and driver.

- Round-trip the supported exported types, generics, ownership metadata, link names and native objects between stages. Reject incompatible versions, targets and truncated artifacts usefully.
- Finish the supported direct/indirect C ABI cases from C02, including enums, aggregate arguments/returns and callback ownership. Use an independently compiled C consumer/provider.
- Implement or reject advertised tree-shake/output options. Keep required runtime shims and externally referenced symbols. Extend existing same-mode reproducibility checks to the compiler workspace.

Done when an external consumer builds without producer source, cache modes preserve behavior, invalid artifacts fail locally, and every supported flag has a behavioral test. Byte identity across deliberately different cache modes is not required. Foreign `.submodule.toml` generation remains separate unless it blocks the chosen consumer.

### C07: support reproducible package reuse

January 5–18. Dependency: C06. Owner area: workspace resolver and package specification.

- Complete a narrow native Git dependency path starting with immutable revisions, based on the existing stage0/package contract. Preserve local-workspace behavior, cycle reporting and unknown-key diagnostics.
- Define a minimal reproducibility record for resolved revisions and artifact identity. Prefer an extension of the manifest contract or a small lockfile over introducing a registry/version solver. Review the public format before implementation.
- Exercise clean checkout, cold fetch, offline rebuild, dependency update, missing revision, network failure and incompatible artifact cases through `agc build`.

Done when the same recorded dependency graph rebuilds offline after initial fetch and changing a pinned revision changes only the expected dependencies and runtime result. Registry publishing, semver solving and a package website are deferred. If this chunk overruns, retain local/pinned vendored reuse as the release scope and clearly defer Git support.

### C08: remove friction from existing language and library APIs

January 19–February 1. Dependencies: C04 and C06. Owner area: inference, traits, stdlib.

- Choose the three most repeated workarounds in compiler, CLI and service code. Prioritize contextual generic inference, method/associated-type selection, borrowed iteration and callback typing.
- Make selected imports and aliases consistent across source and artifacts. Document current non-restrictive semantics; any change to scope restriction requires a separate compatibility decision.
- Standardize errors in the I/O and collection operations used by these applications. Preserve operation/path/code context and owned-value cleanup. Improve Result helpers before proposing propagation syntax.

Done when each workaround has a smaller idiomatic replacement, equivalent behavior in both stages, and a negative test preventing ambiguous inference or ownership misuse. Do not add a cast, wrapper or hardcoded container exception merely to make an example compile.

### C09: make diagnostics and editor behavior dependable

February 2–15. Dependency: C03, with C08 fixes incorporated. Owner area: diagnostic catalog, compiler API and AGLSP.

- Compare check/build/editor diagnostics for type mismatch, unknown names, imports, generic failures, moves and borrows. Preserve primary/secondary spans and macro/import origins.
- Use shared resolved symbols for native LSP hover, definitions and completions. Verify edits to imported files and malformed/incomplete buffers.
- Port ownership and argument inlay hints where the compiler has trustworthy facts. Add rename/references only if symbol identity is sufficiently complete; do not build them from text matching.

Done when protocol tests cover open/change/close, cross-file updates, invalid buffers and correct source positions. Target a provisional p95 below 200 ms for warm diagnostics on the selected small project, measured on a recorded machine. Revise the performance target from the baseline rather than hiding slow cases.

### C10: improve formatting and debugging

February 16–March 1. Dependency: C09. Owner area: tooling.

- Build a syntax-aware formatter for supported syntax using retained source structure. Preserve comments and literal contents; make unsupported forms an explicit limitation.
- Establish native debug metadata support with a bounded first scope: lines, parameters, scalar/struct locals and imported source files. Publish limits for enums and generic frames.
- Package the compiler, stdlib and native LSP together and run an installation smoke test in a clean environment.

Done when formatting is idempotent, formatted fixtures preserve behavior, and a real debugger can stop at source lines and inspect the promised values. If both formatter and debugger work do not fit, ship debugger correctness first and retain the current formatter with an honest description. C13 is not a license to start two unfinished rewrites.

### C11: validate the language in two maintained applications

March 2–15. Dependencies: core correctness plus C07–C10 features actually delivered. Owner area: examples/applications and runtime.

- Maintain a CLI that reads files, parses structured input, uses collections, reports recoverable errors and has a real multi-package build. A log/JSON analysis tool is a suitable default.
- Extend the existing HTTP example into a bounded local service with request limits, timeout handling, graceful shutdown and a bounded worker/queue model using supported APIs.
- Stress task-registry exhaustion, close while blocked, malformed requests, partial reads/writes, failed TLS verification where TLS is enabled, and process shutdown. Turn discoveries into small compiler/runtime fixes.

Done when both applications have end-to-end tests and documented install/run commands, owned resources are released on error paths, and the service survives a repeatable one-hour bounded-load/connection-churn run without unexplained growth. This is workload evidence, not a general security or race-freedom certification.

### C12: stabilize and measure the release candidate

March 16–29. Dependency: C11. Owner area: compiler, runtime and release tooling.

- Freeze language/API additions. Run full stage0/stage1 corpora, frontend gates, stage2/stage3 corpora, artifact/cache/FFI checks and application suites on the same candidate commit.
- Measure cold/warm/edit compile time, peak compiler RSS, binary size, CLI throughput and service latency/memory. Use at least ten comparable runs and report medians; include latency percentiles for services.
- Preserve stage0 WASI coverage with explicit skips. Reconcile Windows support claims with fresh available evidence. Do not add a new target to rescue the release narrative.

Done when correctness gates pass, every skip has a reason, no material unexplained performance regression remains, and a fresh installation runs both applications. Report known limitations instead of counting only favorable measurements.

### C13: contingency, release and the next decision

March 30–April 12. Dependency: all release-blocking gates. Owner area: release and documentation.

- Spend this chunk on overruns, regressions, external-user installation feedback and documentation. Start no new language feature.
- Prepare a Linux preview release with tested binaries, compiler/stdlib versions, dependency requirements, hashes, support matrix, migration notes and known limitations. Publish when explicitly authorized.
- Reassess deferred features using application evidence and measured pain. Select the next quarter's top three objectives.

Done when another developer can install, build, debug and run the sample applications from documented steps, and the release evidence is tied to the exact artifact-producing commit. If correctness gates fail, produce a candidate and a blocker list instead of declaring a stable release.

## Additions ranked by value

These are recommendations, not approved syntax. Existing features such as tuples, import aliases, bounded channels and binary literals are deliberately excluded from the new-feature list.

| Rank | Addition or extension | Why it is useful | Decision and timing |
| --- | --- | --- | --- |
| 1 | Consistent contextual inference and borrowed collection APIs | Removes casts and repeated type arguments from code we already write. | Committed direction in C08, bounded by concrete examples and sound selection rules. |
| 2 | Reproducible package dependencies | Lets people reuse Silver libraries outside this monorepo. | C07, starting with immutable revisions and offline builds. |
| 3 | Error helpers, then possible propagation syntax | Reduces repetitive Result handling without discarding failure information. | Helpers in C08; syntax proposal only after examples prove the remaining need. Preserve existing fallback `?` behavior. |
| 4 | Macro hygiene | Prevents accidental capture and makes library macros safer to compose. | First stretch feature after correctness gates; retain limited macros if time runs short. |
| 5 | Capturing closures | Would improve iterator and callback ergonomics. | Next-cycle candidate. First specify capture-by-copy/move/borrow, escape rules, representation and destructor behavior. Noncapturing callbacks already cover many uses. |
| 6 | Scoped tasks and task groups | Could make join-on-exit and borrowed parallel work expressible. | Next-cycle candidate after C04/C11. Compare library task groups with language borrowing support; do not silently change detached Task semantics. |
| 7 | Channel endpoint ownership and recoverable sends | Can clarify sharing, shutdown and recovery of unsent owned values. | Driven by C11 failures or awkward code; settle Send/shared-access rules before adding Arc-like APIs. |
| 8 | Small, specified comptime/reflection extensions | Useful for tables, layout queries and repetitive declarations. | Add only an evidenced operation with deterministic limits. General execution/reflection is deferred. |
| 9 | Native stage1 WASI, then another native platform | Broadens deployment once the compiler contract is stable. | Next-cycle port budget. Preserve stage0 WASI now; choose Windows or AArch64 based on an actual user and test hardware. |
| 10 | Trait objects, richer patterns, const generics or specialization | Could support more library designs. | Require a blocking application example and a coherence/layout design first. They are not six-month commitments. |

Defer async/await, a green-thread scheduler, exceptions/unwinding, general-purpose GC as the default memory model, a package registry, additional major network protocols, a new backend, and broad complex-number/SIMD language work. Their implementation and verification costs compete directly with the current compiler's reliability.

## Decisions that need explicit design review

| Decision | Recommended starting point | Tradeoff to resolve |
| --- | --- | --- |
| Safe indexing | Check ordinary public indexing in all profiles; keep unchecked access explicit. | Predictability versus measured hot-loop cost and compatibility with existing release behavior. |
| Error propagation | Improve Result helpers before adding syntax. | Less boilerplate versus collision with existing fallback `?` and error conversion rules. |
| Import selection | Document and stabilize today's alias behavior first. | Restrictive imports improve namespace control but would change existing non-restrictive behavior. |
| Package reproducibility | Record immutable revisions with a minimal format. | A lockfile is familiar and extensible; exact manifest pins are simpler but need a transitive graph policy. |
| Task lifetime | Preserve explicit `wait`; design scoped joining separately. | Detached work is flexible; lexical joining simplifies lifetimes but can block at scope exit. |
| Failure policy | Distinguish recoverable Result errors, programmer-error traps and allocation failure. | Explicit contracts help callers; retrofitting every low-level operation at once would be disruptive. |

## Progress measures and release gates

At the end of each chunk record its ID, exact commit, completed behavior, tests run, skips, remaining blockers and next action. Keep day-to-day notes in the existing ignored trackers; keep durable contracts and release evidence in public docs. Update this plan when scope changes instead of leaving completed features in a missing list.

| Measure | Initial state | Six-month acceptance target |
| --- | --- | --- |
| Contract accuracy | Several demonstrated doc/source contradictions. | No known contradiction in the supported language and platform matrix; syntax examples checked by tooling. |
| Silent compiler differences | Compound-target evaluation and target/cfg divergence visible in source. | All reproduced miscompilations fixed; unsupported forms reject explicitly. No accepted option with silently absent promised behavior. |
| Semantic acceptance | Focused checking exists; general parity is incomplete. | Check/build/editor agree on the defined core matrix, with negative tests and useful source diagnostics. |
| Ownership | Broad regression coverage, incomplete general typed analysis. | Every published ownership rule has rejection and cleanup evidence; unresolved soundness defects block the corresponding guarantee. |
| Generational behavior | Historical full stage2/stage3 success. | Fresh full candidate runs with recorded compiler identity and justified skips; no stage0 execution hidden in native paths. |
| Incremental builds | Every loaded source contributes to object keys. | Warm reuse and dependency-local rebuild sets verified through changed runtime output. |
| Package reuse | Local workspaces supported; native Git resolution absent. | Reproducible offline reuse of the delivered package scope, with unsupported dependency kinds clearly rejected. |
| Developer tooling | Basic native LSP and whitespace formatting. | Accurate cross-file diagnostics/navigation; promised debug inspection works; formatter scope stated and tested. |
| Runtime resilience | Many focused runtime/network fixtures. | CLI failure-path coverage and repeatable bounded service stress pass with explained memory/resource behavior. |
| Performance | No fresh measurements in this audit. | Comparable ten-run baselines and results published. Investigate median regressions over 10%; accept only a documented correctness or product tradeoff. |
| Installation | Build and bundle scripts exist. | Fresh environment installs and runs compiler, stdlib, native LSP and sample applications from documented steps. |

These are acceptance targets, not claims already established by this review. In particular, "all tests pass" cannot replace the named safety, target, package or resource contract.

## Verification workflow for implementation chunks

Use the LLVM 22 development environment. Rebuild stage0 after Rust changes; self-host scripts may otherwise reuse an old executable. Typical commands, with final selection determined by the change:

```bash
cargo build -p agc
cargo test -p agc
python3 tests/run_tests.py --no-tui
python3 tests/run_tests.py --no-tui memory_pentest cascade_drop_test
bash tests/selfhost/run_stage.sh --include-std
bash tests/selfhost/run_native.sh --no-tui --jobs 4
./target/debug/agc build silver.toml --bin agc --no-cache -o /tmp/agc-plan-stage1
python3 tests/selfhost/check_stage2.py --stage1 /tmp/agc-plan-stage1 --full-corpus --stage3 --jobs 4
```

Run full corpora sequentially because some network helpers use fixed ports. Do not enable leak checking on stage builds; use it on the selected runtime fixtures. Exercise package changes through `agc build`, ABI changes through C consumers, editor changes through protocol interaction, and debug support through a debugger. Compiler and stdlib behavior changes require the repository's full pre-commit checks, not only a small new regression.

For performance work record machine, compiler generation/hash, optimization flags, worker count, cache state, dependency state and workload. Measure cold build, unchanged build, leaf edit and shared dependency edit separately. For service latency include p1, p5, p10, p50, p95 and p99 and state concurrency and error rates.

## Scope control

Correctness, unsupported-target rejection, cleanup and honest support claims are mandatory. If C03/C04 expose larger ownership or typing work, move package expansion, formatter breadth and new syntax into the following cycle. Do not attempt all of them in parallel to preserve dates.

The first cuts are new syntax and extra platforms, followed by rename/reference tooling, formatter breadth and native Git convenience beyond pinned reuse. Keep the CLI application; narrow the service to existing HTTP support if necessary. Keep a fresh generational gate, reproducible builds and install validation.

The April deliverable can be a deliberately scoped Linux preview. It should be useful, documented and repeatable. It should not claim complete language parity, Rust-equivalent safety, universal reproducibility or production protocol compliance without the evidence those claims require.

## Audit handoff

This task changes only `plan.md`. Compiler tests, integration suites, benchmarks, debugger sessions and remote CI were not run for this documentation-only review. All 37 local links resolve; chunk IDs, calendar coverage, command/script references and document formatting were checked. The next implementation action is C01, beginning with a fresh same-commit baseline and the native target guard, followed by the compound-assignment regression in C02.
