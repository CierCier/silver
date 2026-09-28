# Silver Frontend Completion Todos

This checklist tracks the remaining stage1 frontend work. A checked item means
its focused tests pass and `bash tests/selfhost/run_stage.sh` passes afterward.
The native bridge is a separate backend dependency and is not treated as
frontend completion.

## Completion condition

Stage1 owns the complete source frontend:

- source/module loading and package dependency visibility;
- full declaration and expression typing;
- imports, artifacts, generics, and monomorph requests;
- ownership, borrowing, escape, and drop decisions;
- stable diagnostics and frontend command behavior;
- deterministic stage1 output over the full corpus.

Every slice must preserve the stage0 baseline and run the self-host stage gate.

> Correction (2026-09-28): several `[x]` below overstate parity — see
> `todo.md` FE-001..FE-010. Overclaimed items are marked `[~]` (partial:
> collection/projection lands, fixpoint/enforcement deferred). Do not promote
> back to `[x]` without the gate named in the item's Done-when.

## 0. Baseline and infrastructure

- [x] Keep the stage0 Rust bootstrap as the seed compiler.
- [x] Split stage1 into `bin/agc` and reusable `libs/agc`.
- [x] Establish stage0 -> stage1 build and execution harness.
- [x] Establish token, parse-acceptance, semantic-status, artifact, bridge, cache, and native gates.
- [x] Add a stage1-owned deterministic diagnostic test corpus.
- [x] Add a stage1-owned unit-test execution surface.
- [x] Add repeat-build byte-identity checks for frontend artifacts.

## 1. Typed declaration model & Canonical Type Table

- [x] Add canonical `TypeKind` and `TypeId` (`u32`) storage in `libs/agc/src/frontend/types.ag`.
- [x] Pre-seed built-in primitive constants (void, bool, int8..128, uint8..128, float32..80, str, char).
- [x] Represent pointers, references, arrays, slices, named types, type parameters, and function signatures.
- [x] Provide O(1) type property queries (`is_primitive`, `is_numeric`, `is_pointer`, `is_copy`).
- [x] Wire `TypeTable` into `typecheck.ag` and `hir.ag` to eliminate manual `strcmp` checks.
- [x] Register imported declarations in the typed environment with `TypeId`.
- [x] Replace `TypeProjection` name-only checks with typed declaration lookup.
- [x] Verify stage0 -> stage1 build and `bash tests/selfhost/run_stage.sh`.

## 2. Expressions and inference

- [x] Build a typed expression tree for every expression kind (`ast_expr.ag`, `lower_expr.ag`).
- [x] Type literals with signedness, width, float, complex, char, bool, and string rules.
- [x] Infer local declarations and initializer expressions (`typecheck_expr.ag`).
- [x] Resolve identifiers through lexical scopes and module visibility.
- [x] Type calls, methods, constructors, and argument arity.
- [x] Implement numeric promotion and explicit/implicit casts.
- [x] Type unary, binary, comparison, logical, and compound operators.
- [x] Resolve built-in operators and `__add`/`__index_get`-style overloads.
- [x] Type field access, indexing, address-of, dereference, and `move` expressions.
- [x] Type `if`, `while`, `for`, `match`, `break`, `continue`, `return`, and `defer` paths.
- [x] Record inferred types for LSP-compatible source ranges.
- [x] Add differential expression fixtures and exact primary-diagnostic checks.

**Exit evidence:** stage1 and stage0 agree on valid/invalid expression status and
primary diagnostics over the full expression corpus.

## 3. Declarations, traits, and generics

- [x] Resolve trait declarations and generic bounds.
- [x] Resolve impl blocks and method ownership.
- [x] Type method receivers and associated functions.
- [x] Type generic parameters, defaults, and generic arguments.
- [x] Record monomorphization requests for functions, impls, and nested calls (`monomorph.ag`).
- [~] Run generic request fixpoint and enforce the generation limit (256 cap in `monomorph.ag` — currently stored at `monomorph.ag:83`, never read; see FE-001).
- [x] Type generic operators and methods deferred until concrete substitution.
- [x] Type aliases and imported generic templates.
- [x] Add generic/trait/method differential fixtures.

**Exit evidence:** generic and trait-heavy corpus files produce the same
acceptance and primary diagnostics as stage0.

## 4. Modules, packages, and artifacts

- [x] Model the complete package graph and dependency ownership.
- [x] Resolve selective imports, aliases, re-exports, and transitive visibility.
- [x] Load source-module declarations with their typed signatures.
- [x] Load `.agm` signatures, layouts, generic templates, and dependencies.
- [~] Implement stage1 `.agm` serialization with version compatibility (reader-only today, `artifacts.ag:1-4`; see FE-006).
- [~] Implement stage1 dependency cache keys and artifact publication (planning stage1-owned, publishing stage0-owned; see FE-006).
- [x] Implement package test target discovery and dependency-aware execution.
- [x] Implement submodule build selection and failure behavior.
- [x] Add package/import/artifact differential fixtures.

**Exit evidence:** stage1 can check a package graph using source and binary
dependencies without delegating planning to stage0.

## 5. Ownership and borrowing

- [~] Implement `TypeProperties {is_copy, needs_drop}` (only `is_copy` stub exists, `types.ag:274-283`, no `needs_drop`; see FE-005).
- [x] Implement `Place` and projection overlap for fields and indices (`place.ag`).
- [x] Implement $O(1)$ `BitSet` local variable state tracking (`bitset.ag`, `ownership.ag`).
- [~] Implement `move_out`, `copy_from`, `initialize`, and `read` operations (name-based transfer only, `ownership.ag:274-315`; see FE-003).
- [~] Implement move diagnostics with original move spans and notes (no note spans, `ownership.ag:46-56`; see FE-002).
- [~] Implement lexical scopes and control-flow-sensitive move state (flat source-order walk; see FE-003).
- [~] Implement shared/mutable borrow loans and NLL release points (root-place loans only, `borrow.ag:1-3`; see FE-004).
- [~] Implement field/index disjointness and call/receiver borrow propagation (`borrow.ag`) (partial; see FE-004).
- [~] Implement reference escape checks (`return &local` only, `borrow.ag:206-213`; see FE-004).
- [~] Implement Drop type propagation and per-field drop decisions (absent; see FE-005/LANG-006).
- [~] Implement enum payload ownership and active-variant cleanup (constructor `move` check only, `ownership.ag:153-197`; see LANG-006).
- [~] Implement loop, branch, defer, and early-return cleanup behavior (see LANG-006).
- [~] Add the full ownership/borrow/RAII fixture matrix (acceptance-status parity only; see GATE-003).

**Exit evidence:** stage0 and stage1 agree on ownership acceptance, move
origins, borrow conflicts, and escape diagnostics.

## 6. Diagnostics and frontend driver

- [~] Move every user-facing message into a shared catalog (`messages.ag` — 19 fns vs 100+ in stage0; see FE-002).
- [~] Match stage0 severity, span, note, and source-rendering behavior (`error:` hardcoded, `diagnostics.ag:131-146`; see FE-002).
- [ ] Add warning flags and linter diagnostics (absent; see FE-002).
- [x] Add fuzzy typo suggestions (`levenshtein`, `fuzzy_suggest` in `messages.ag`).
- [x] Implement the complete frontend CLI contract (`driver.ag`).
- [x] Own `lex`, `parse`, `ast`, and `check` argument planning in stage1.
- [x] Implement stage1 test discovery and execution.
- [x] Own cache, clean, init, and package command planning.
- [x] Stage0 backend bridge remains strictly separated for code generation.

**Exit evidence:** frontend commands and diagnostics match stage0 without the
bridge for all non-native operations.

## 7. Verification and fixpoint

- [~] Compare full rendered diagnostics, not only exit status (status + Send/artifact primaries only; see GATE-003).
- [~] Compare AST structure, spans, and recovery behavior (acceptance parity; see FE-008).
- [~] Compare normalized `.agm` contents and cache keys (version read + malformed rejection; no writer diff; see FE-006).
- [x] Add seeded property tests for lexer, types, ownership, and diagnostics.
- [x] Run native test matrix (200 passed, 0 failed, 1 skip).
- [x] Build stage1 with stage0 and run the 348-file self-host stage gate (348/348 passed, 0 skips).
- [x] Verify repeat-build reproducibility and parallel-build ordering.
- [x] Remove deferred-boundary skips from the self-host gate.

**Exit condition:** the self-host completion record is fully green, with no
frontend compatibility seam remaining except the explicitly separate native
backend migration.

## Backend work tracked separately (see `todo.md` P0-001..003, BE-001..006, GATE-001/002; experimental handoff fixes in `handoff.md` §§4,7: C-cast lowering, recursive if/else-if, Self receiver default, extern-C return stars, String==str extract, _start entry, __silver_argc/argv globals, Vec<T> mangling)

- [ ] Replace the native bridge with stage1 textual IR emission (P0-001..003 first for the FFI path).
- [ ] Implement stage1 linker/runtime startup and artifacts.
- [ ] Implement debug info, backtrace tables, and leak-check origins.
- [ ] Compare stage1 native output with stage0 before retiring the bridge.

### Uncommitted work inventory (MIG-007, 2026-09-28 — commit deferred to owner, do not start Phase 2 with a dirty tree)
- Untracked, part of experimental backend (tracked by P0/BE items): `libs/agc/src/backend/llvm/` (5 files), `vendor/llvm/llvm.ag`, `tests/selfhost/check_native_coverage.py` (informational, GATE-002).
- Untracked, needs a tracking home: `bin/agsm/` (wired in `silver.toml:37-38` + `run_stage.sh:23-25`, no plan row), `libs/agc/src/frontend/items.ag` (410 lines, type parser — no todos row).
- Modified, covered by this file + handoff §§4,7: `bin/agc/src/{driver,native_backend}.ag`, `libs/agc/src/frontend/{ast_expr,bindings,expressions,lower_expr,module_loader,symbols,typecheck_expr,types}.ag`.
- Modified, stage0-side: `bootstrap/stage0/agc/src/{driver.rs,link.rs}` (cache/bridge), `bootstrap/stage0/agsm/src/extract.rs`, `silver.toml`, `tests/selfhost/{run_stage.sh,check_native_backend.py,README.md}`.
