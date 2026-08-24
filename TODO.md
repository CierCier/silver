# Roadmap Todos

Working checklist derived from `handoff.md` §3. One atomic commit per item;
each commit lands green (`cargo test -p agc --lib`, `cargo test -p aglsp`,
relevant integration tests).

## Phase 1 — Compiler Frontend & Diagnostics

- [x] 1. Commit guard provenance (`db32b7c`)
- [x] 2. Inferred `let` bindings (`97b4813`)
- [x] 3. Monomorphization fixpoint cap (`e2008635`)
- [x] 4. ~~Monomorphization typed keys~~ — DESCOPED: instantiation keys are
      already canonical namespaced mangled names (`type::`/`fn::`/`impl::`);
      an enum-key rewrite is high-churn, zero-behavior-change refactoring of
      a 3000-line module with no known bug to justify it
- [x] 5. Harden build cache store — cache init/clean errors now surface as
      warnings instead of silent degradation (atomic temp+rename writes
      already existed). Parallel check-mode execution DESCOPED: `agc check`
      is contractually frontend-only; running artifact codegen there would
      break that contract (`driver.rs:1213`)

## Phase 2 — Standard Library Safety & Consistency

- [ ] 6. Deterministic `Vec` bounds — out-of-bounds access aborts in ALL
      build profiles (not just `@cfg(debug)`); add `try_get`
      (`std/mem/vec.ag`)
- [ ] 7. Normalize predicate & capacity APIs — `Result.is_ok()/is_err()`,
      `String.is_empty()`, `Bytes.is_empty()`, `.capacity()` normalization
      (`std/result.ag`, `std/string.ag`, `std/bytes.ag`, containers)
- [ ] 8. Fallible collection accessors — `try_pop`, `try_front`, `try_back`,
      `get_opt` (`vec`, `deque`, `queue`, `heap`, `map`)
- [ ] 9. POD safety disclaimers — document owned-non-POD limitations of
      Box/Rc/Vec headers (`std/mem/*.ag`)

## Phase 3 — Concurrency & Synchronization

> NOTE: `std/channel.ag` and `std/sync.ag` carry **uncommitted user work** —
> items 10–11 are BLOCKED until that work is committed by its author.

- [ ] 10. [BLOCKED: uncommitted user work] Channel bounded semantics &
      reset (`std/channel.ag`)
- [ ] 11. [BLOCKED: uncommitted user work] Condvar/RwLock naming aliases
      (`std/sync.ag`)
- [ ] 12. Futex-based thread joining — replace spin-loop in `thread_join`
      with futex wait/wake (`std/thread.ag`, `std/rt/thread_registry.ag`)
- [ ] 13. Static Send enforcement — reject moving locked sync structures
      across task boundaries (`agc/src/semantic/send_check.rs`)

## Phase 4 — Selective Imports (Major Feature)

- [ ] 14. Selective import grammar & AST — `Token::As`, parse
      `import std.io { print, println as pln };` into `ImportItem`
- [ ] 15. Import lowering — selective inlining/symbol filtering during
      import lowering; propagate through `.agm` artifacts
- [ ] 16. LSP exposure — completions/references aware of selective imports
