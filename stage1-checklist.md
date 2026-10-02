# Stage1 feature and migration checklist

Updated: 2026-10-02. This is a status inventory of the self-host migration, not
a promise that every feature described by the language syntax is implemented
end to end in stage1. A feature may parse or pass a semantic-status comparison
while still lacking type checking, native lowering, or runtime parity.

## Status key

- [x] Implemented and covered by the cited current gate.
- [~] Implemented for a limited projection or selected cases; remaining scope is stated.
- [ ] Missing, delegated, failing, or not yet established.
- [?] Current support is unclear; needs a focused probe before being called complete.

## Current migration snapshot

- [x] Stage1 is built by stage0 from the Silver workspace and has its own CLI.
- [x] Stage1-owned `lex`, `parse`, and `check` commands run locally.
- [x] Stage0 verification passes: 592 Cargo tests; complete fresh-cache integration run has 224 passed, 0 failed, and 2 skipped. The range-continue regression passes after repairing the bootstrap increment target. Stage0 now checks `else-if` chains iteratively instead of nesting one frame per arm, so long Silver dispatch chains no longer exhaust its 32-deep budget.
- [x] Expanded frontend checks compare 373 source files for token/span and AST/CST-span behavior, and 377 files for semantic acceptance status; current evidence reports zero mismatches and zero deferred-boundary skips.
- [x] Eleven focused semantic regression fixtures pass, including generic type and borrow-escape cases.
- [~] Selected direct-native stage1 gates pass with stage0 disabled: smoke builds/runs, formatting, linker behavior, assertions, raw argv, enum/layout and target ABI cases, generic free calls, cache behavior, and the selected stage2 gate.
- [~] Stage1 builds stage2 with stage0 unavailable; stage2 handles help/version, lex/parse/check on input over 128 tokens, and builds/runs six programs covering control flow, generic calls, function pointers, and aggregate layouts. This is selected stage2 evidence, not full stage2 or stage3 parity.
- [ ] The latest full integration corpus with direct native stage1 and stage0 unavailable has 111 passed, 113 failed, and 2 skipped out of 226. `condvar_test` and `rwlock_test` timed out. `unwrap_or_test`, `channel_test`, and `channel_bounded_test` now pass after unwrap-or lowering landed. Native parity is incomplete.
- [ ] Full stage0-independent package/build/run/test behavior, complete stage2 coverage, stage3 self-compilation, native ownership/leak parity, and full runtime parity are not established.

Primary evidence: [self-host gate definitions](tests/selfhost/README.md) and
the runtime fixtures linked below. Test totals are from the latest recorded
run and should be refreshed after material changes.

## CLI, workspace, and build lifecycle

| Feature | Current status | Remaining work |
| --- | --- | --- |
| CLI entry point and argument handling | [x] | Keep command behavior and diagnostics aligned with stage0. |
| `lex` token dump | [x] | Current parity gate compares kind, raw text, byte span, and line/column span. |
| `parse` lossless CST dump | [~] | Span boundaries and parser acceptance are compared; identical AST/CST trees or serialization are not expected or proven. |
| `check` command | [~] | Local frontend and selected overload diagnostics work; full typed checking and rendered diagnostic parity remain open. |
| `ast` command | [~] | Command exists in the driver; complete stage0-compatible behavior and parity need a focused gate. |
| `build`, `run`, `test`, package operations | [~] | Selected source builds/runs use the native backend; unsupported programs and normal package paths can delegate through the explicit stage0 bridge. Full native command coverage fails. |
| Manifest parsing and target selection | [x] | Stage1 plans supported workspace targets and target selection. Expand compatibility coverage against stage0. |
| Workspace dependencies | [~] | Local/workspace resolution is present. Git package dependencies and targets are explicitly unsupported by the stage1 resolver. |
| Package manifest and target keys | [~] | A supported subset is parsed; unknown keys fail with an error. Full manifest compatibility is unproven. |
| Clean/init commands | [~] | Recognized by workspace command planning; end-to-end parity is not established by the current no-stage0 corpus. |
| Native compiler cache | [~] | Cache behavior parity has a focused passing gate. Reproducible compiler artifact digests and the full cache contract remain open. |
| Stage0 bridge | [x] | Explicit compatibility bridge exists and is used for paths not handled by the native backend. |
| Bridge-free operation | [ ] | The complete command and integration surface must work with stage0 unavailable. |

## Lexing, parsing, and source representation

| Feature | Current status | Remaining work |
| --- | --- | --- |
| UTF-8 source tokenization, comments, identifiers, keywords, literals, and punctuation | [x] | Token parity is covered over the current corpus; uncommon and malformed-input edges still need systematic coverage. |
| Byte and line/column source locations | [x] | Token spans and AST/CST span-boundary projection are gated. |
| Parser acceptance over repository, example, and optional standard-library corpus | [x] | This proves acceptance parity, not identical trees or all downstream semantics. |
| Lossless CST and AST-span projection | [~] | Retained source structure is a CST projection; full stage0 AST equivalence is not a goal of the span gate and typed HIR coverage is only a selected slice. |
| Imports and module loading | [~] | Source/module projection and selected workspace/artifact cases pass; broad visibility, re-export, and package behavior remain open. |
| Selective imports and import aliases | [ ] | The syntax reference marks these as unimplemented. |
| Complex number type support | [ ] | Complex literal token support exists, while type-level support is incomplete. |
| Octal and binary integer literals | [ ] | Not supported by the current syntax/lexer contract. |
| Tuple types, tuple literals, and pattern destructuring | [ ] | Not supported by the current syntax contract. |

## Types, declarations, and semantic analysis

| Feature | Current status | Remaining work |
| --- | --- | --- |
| Primitive integer, float, bool, char, string, void, pointer, and reference syntax | [~] | Syntax and frontend acceptance are broad; native operations and ABI cases vary by type and need full parity coverage. |
| Structs, fields, arrays, globals, constants, and type aliases | [~] | Selected aggregate/global/array paths work natively. String globals now emit constant bytes, including escaped and empty strings. Globals take precedence over unqualified enum variants; [collision regression](tests/native_global_variant_collision_test.ag) covers these cases. Aggregate expressions and layout coverage remain incomplete. |
| Enums and payload variants | [~] | Selected unit, small-payload, String-payload, assignment, and generic multi-field layout cases pass; broad enum operations, ownership, and payload combinations remain open. |
| Function declarations, calls, parameters, and returns | [~] | Direct concrete calls and selected return/argument cases work. Many unresolved free functions, methods, and expression contexts remain in the native corpus. |
| Declared function pointers | [~] | [Native signature tests](tests/native_function_pointer_test.ag) cover callback parameters, zero-argument wide returns, numeric argument/result conversion, and void callbacks stored in struct fields. Function-valued returns and inferred callback declarations remain unsupported; global function-pointer initializers must fail closed. |
| Overload declaration identity and selection | [~] | Canonical free-function signature identity and selected ambiguity diagnostics are covered. Complete overload resolution and method overload parity are unproven. |
| Generic free functions | [~] | Explicit specialization and inferred nested free calls over named, array, pointer, and reference shapes pass selected tests. Numeric literals can match a numeric type already inferred from an earlier argument, covered by [literal inference](tests/generic_method_literal_inference_test.ag). Broader numeric coercion, multiple-argument, and nested aggregate inference remain open. |
| Generic methods and receiver inference | [~] | Selected Vec/String receiver specialization and MethodInferenceBox.set work. Method return parsing now stops at associated-type semicolons and substitutes T/E inside generic applications. Broad generic method-call inference remains open. |
| Traits, `impl`, associated types, and operator protocols | [~] | Syntax and portions of semantic resolution exist. Complete trait selection, associated types, operator lowering, and runtime behavior are not established. |
| Numeric casts and promotions | [~] | Selected signed/unsigned integer and `u32`/`u64` to/from `f64` cases pass, including some argument, store, return, and operation contexts. Wider types and broader method/operator boundaries remain open. |
| `comptime` | [~] | Literal cast/fold smoke cases pass. The full compile-time evaluator and supported-operation parity are not established. |
| Attributes and conditional compilation | [~] | Selected cfg selection and attributes are exercised by native gates; full attribute and target-matrix parity remains open. |
| Type checking through `check` | [~] | Ambiguous overload rejection and selected HIR type/call checks exist. General expression mismatch/type inference diagnostics remain incomplete. |
| Typed HIR retention and lowering | [~] | First typed declaration, call-argument, arity, and return-expression slices are tested. A complete retained typed representation and pass boundary are not established. |
| Visibility and re-exports | [~] | Focused module/artifact cases pass; full visibility and re-export compatibility remains open. |

## Expressions, control flow, and code generation

| Feature | Current status | Remaining work |
| --- | --- | --- |
| Arithmetic, comparison, logical, bitwise, unary, and cast expressions | [~] | Selected native paths pass. Full operator/type combinations and error behavior remain incomplete. |
| Unwrap-or expressions (`value ? fallback`) | [~] | Native `UnwrapOr` AST/lowering covers Optional, Result, and pointer handling with lazy fallback evaluation; [unwrap-or tests](tests/unwrap_or_test.ag) pass natively, including struct payloads, chained fallbacks, single-evaluation of calls, and fallback laziness, and both channel fixtures pass. `check` promotes the unwrap-or diagnostics and rejects dangling-`?` operands; broader inference promotion stays partial with FE-010 and full rendered diagnostics with GATE-003. |
| Direct calls, method calls, static methods, and chained receivers | [~] | Concrete calls plus selected `Vec<String>`/`String` receiver chains pass. Many methods and generic receiver cases remain unsupported or fail. |
| Field access, pointer/reference auto-dereference, and indexing | [~] | Selected pointer/string/indexed reads work and a `String.data[index]` ownership regression is fixed. Aggregate/index-set coverage and indexed ownership typing remain open. |
| Struct/array initialization and aggregate values | [~] | Local array initialization, selected structs, arrays, globals, and nested/generic layouts have coverage. General aggregate expressions and argument/return contexts remain incomplete. |
| `if`, `while`, C-style `for`, `for-in`, `break`, `continue`, and `return` | [~] | Range loops and consuming Vec iteration now execute their bodies and preserve following statements. [Loop regression](tests/native_for_in_continue_test.ag) covers continue, cached endpoints, and source evaluation order. Pointer/reference iterator protocols and cleanup remain open; one generic fixture has duplicate stdlib setup symbols. |
| `match` and enum patterns | [~] | Selected enum, void-valued, and payload paths work. Exhaustiveness, arm typing, moves, and full pattern coverage remain open. |
| `defer` | [~] | Native backend fails closed for unsupported defer cases; full defer lowering and cleanup semantics are not established. |
| Inline assembly | [?] | Present in the language syntax. Stage1 native behavior has not been established by the current gates. |
| Builtin macros (`@print`, `@format`, `@size`, memory operations, assertions) | [~] | Formatting, assertions, and adversarial memory intrinsic cases have selected gates; complete builtin and formatting-trait parity remains open. |
| LLVM generation, target ABI, and linking | [~] | Stage1 emits and links selected native programs. LLVM target data is used for tested layouts and the dynamic ELF interpreter is discovered; full target/ABI/codegen parity remains incomplete. |
| Native library linking and external declarations | [~] | Selected linker contracts pass. Full `extern`, link attribute, platform, and package combinations remain unproven. |

## Ownership, borrowing, and runtime behavior

| Feature | Current status | Remaining work |
| --- | --- | --- |
| Borrow origins and reference escape checking | [~] | Selected accepted/rejected escape regressions and Send boundaries pass. Analysis is not yet complete typed-CFG parity. |
| Move analysis and use-after-move rejection | [~] | Native compilation invokes ownership analysis and selected negative fixtures pass. Branches, partial initialization, indexed places, temporaries, and reinitialization need broader typed analysis. |
| Copy/type properties and partial initialization | [ ] | Full property propagation and partially initialized aggregate behavior are not established. |
| Destructors, field cascades, overwrite cleanup, temporaries, and scope-exit drops | [ ] | Native statement emission has no drop elaboration or live/drop flags. Drop-counter fixtures fail; add move-aware cleanup on overwrite, scope exit, return, break, and continue before claiming resource cleanup. |
| `defer` and cleanup order on early exits | [ ] | Full native LIFO cleanup and return/break/continue coverage is not established. |
| Allocation and reallocation | [~] | Selected generic allocation/reallocation and layout paths pass; complete allocator and resource cleanup parity remains open. |
| Runtime panic/assertion behavior | [~] | Selected `@assert` failure reporting is covered; panic/unwind behavior and cleanup interactions remain open. |
| Threads, `launch`/`wait`, channels, condition variables, and locks | [~] | [Futex wake](tests/native_futex_wake_test.ag) passes after globals take precedence over colliding unqualified enum variants. Channel payload assertions now pass with unwrap-or lowering; condition-variable and RwLock guards still time out and depend on missing native cleanup. Full concurrency behavior remains open. |
| Standard library behavior | [~] | Frontend status includes 121 stdlib files in the expanded history and current stdlib-inclusive semantic comparison; many stdlib-heavy programs still fail in the native backend. This does not certify every library API. |

## Diagnostics, artifacts, and compatibility

| Feature | Current status | Remaining work |
| --- | --- | --- |
| Lexer/parser diagnostics and source locations | [~] | Selected exact location/order checks pass; full message content and formatting parity are open. |
| Semantic diagnostics | [~] | Semantic acceptance status matches the current comparison corpus; this is not complete type checking or full diagnostic parity. |
| Diagnostic ordering | [x] | Focused two-error source-order gate passes. |
| Typed Send diagnostic | [x] | Focused launch/Send primary-diagnostic boundary passes. |
| Complete diagnostic catalog/rendering | [ ] | Compare all codes, labels, notes, spans, ordering, and output formatting against stage0. |
| AGM artifact reader | [x] | Reader supports v2 and v6-v11 layouts with malformed-input and cross-stage tests. |
| Stage1 artifact publication | [~] | Frontend metadata projection is published and cross-read; rich layout, ABI, and native-library metadata remains stage0-produced. |
| Artifact imports and cache dependency keys | [~] | Selected round trips and imported-source dependency cache behavior pass; full artifact/native cache parity is open. |
| AGLSP/AGSM package checks | [x] | Wired into the frontend gate; this is not complete package-build or native migration coverage. |

## Remaining completion gates

- [ ] Make the full 226-case integration corpus pass through direct native stage1 with stage0 unavailable; explain intentional skips and eliminate the condition-variable/RwLock timeouts.
- [ ] Close native failure clusters in unresolved calls/methods/arguments, aggregate and stdlib-heavy expressions, ownership/drop behavior, and runtime state mutation.
- [ ] Complete generic inference across multiple arguments, nested aggregates, methods, and receiver contexts.
- [ ] Complete typed CFG-based borrow, move, type-property, partial-init, and drop analysis, then verify cleanup with ownership/leak-focused tests.
- [ ] Expand `check` to general expression typing and compare full rendered diagnostics.
- [ ] Complete native artifact publication, ABI/layout metadata, module visibility/re-export behavior, and cache/reproducibility contracts.
- [ ] Run the broader native corpus with stage2 and stage0 disabled, then attempt stage3 self-compilation. Six compiled runtime programs do not establish full stage2 parity.
- [ ] Re-run compiler tests, the full integration suite, `memory_pentest`/`cascade_drop_test`, frontend parity, and the relevant self-host gates after compiler changes. Report pass, fail, skip, and unrun results separately.

## Scope notes

This checklist covers the stage1 compiler and migration boundaries visible in
the repository. It inventories language/compiler features, not every function
in `std/`; each standard-library module needs its own API-level inventory if
that is the intended scope. The authoritative syntax contract is
[SYNTAX.md](SYNTAX.md). Compiler architecture and stage0 contracts are in
[docs/compiler-guide.md](docs/compiler-guide.md). The migration tracker and
handoff contain detailed work IDs and run logs; update this checklist when
those statuses change.
