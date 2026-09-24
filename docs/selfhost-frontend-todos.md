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
- [x] Run generic request fixpoint and enforce the generation limit (256 cap in `monomorph.ag`).
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
- [x] Implement stage1 `.agm` serialization with version compatibility.
- [x] Implement stage1 dependency cache keys and artifact publication.
- [x] Implement package test target discovery and dependency-aware execution.
- [x] Implement submodule build selection and failure behavior.
- [x] Add package/import/artifact differential fixtures.

**Exit evidence:** stage1 can check a package graph using source and binary
dependencies without delegating planning to stage0.

## 5. Ownership and borrowing

- [x] Implement `TypeProperties {is_copy, needs_drop}`.
- [x] Implement `Place` and projection overlap for fields and indices (`place.ag`).
- [x] Implement $O(1)$ `BitSet` local variable state tracking (`bitset.ag`, `ownership.ag`).
- [x] Implement `move_out`, `copy_from`, `initialize`, and `read` operations.
- [x] Implement move diagnostics with original move spans and notes.
- [x] Implement lexical scopes and control-flow-sensitive move state.
- [x] Implement shared/mutable borrow loans and NLL release points.
- [x] Implement field/index disjointness and call/receiver borrow propagation (`borrow.ag`).
- [x] Implement reference escape checks.
- [x] Implement Drop type propagation and per-field drop decisions.
- [x] Implement enum payload ownership and active-variant cleanup.
- [x] Implement loop, branch, defer, and early-return cleanup behavior.
- [x] Add the full ownership/borrow/RAII fixture matrix.

**Exit evidence:** stage0 and stage1 agree on ownership acceptance, move
origins, borrow conflicts, and escape diagnostics.

## 6. Diagnostics and frontend driver

- [x] Move every user-facing message into a shared catalog (`messages.ag`).
- [x] Match stage0 severity, span, note, and source-rendering behavior.
- [x] Add warning flags and linter diagnostics.
- [x] Add fuzzy typo suggestions (`levenshtein`, `fuzzy_suggest` in `messages.ag`).
- [x] Implement the complete frontend CLI contract (`driver.ag`).
- [x] Own `lex`, `parse`, `ast`, and `check` argument planning in stage1.
- [x] Implement stage1 test discovery and execution.
- [x] Own cache, clean, init, and package command planning.
- [x] Stage0 backend bridge remains strictly separated for code generation.

**Exit evidence:** frontend commands and diagnostics match stage0 without the
bridge for all non-native operations.

## 7. Verification and fixpoint

- [x] Compare full rendered diagnostics, not only exit status.
- [x] Compare AST structure, spans, and recovery behavior.
- [x] Compare normalized `.agm` contents and cache keys.
- [x] Add seeded property tests for lexer, types, ownership, and diagnostics.
- [x] Run native test matrix (200 passed, 0 failed, 1 skip).
- [x] Build stage1 with stage0 and run the 348-file self-host stage gate (348/348 passed, 0 skips).
- [x] Verify repeat-build reproducibility and parallel-build ordering.
- [x] Remove deferred-boundary skips from the self-host gate.

**Exit condition:** the self-host completion record is fully green, with no
frontend compatibility seam remaining except the explicitly separate native
backend migration.

## Backend work tracked separately

- [ ] Replace the native bridge with stage1 textual IR emission.
- [ ] Implement stage1 linker/runtime startup and artifacts.
- [ ] Implement debug info, backtrace tables, and leak-check origins.
- [ ] Compare stage1 native output with stage0 before retiring the bridge.
