# Stage1 feature and migration checklist

Updated: 2026-10-09. This is a status inventory of the self-host migration, not
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
- [x] Current PR CI on `ab73fef` passes all Cargo test groups and the stage0 integration suite: 275 passed, 0 failed, and 3 skipped across 278 fixtures. Two skips are stage1-only regressions; `mem_growth_watch` is the default benchmark skip.
- [x] Expanded frontend checks pass: token and AST-span parity on 425 files, semantic status parity on 429 files, GATE-003 projection, and AGM reader/cross-stage checks.
- [x] Current PR CI runs the full default integration corpus directly through stage1: 277 passed, 0 failed, and 1 skipped (`mem_growth_watch`) across 278 fixtures. The previous nine-failure report is historical and superseded.
- [~] The maintained `run_native_no_stage0.sh` currently lists 69 focused integration fixtures, along with smoke builds/runs, formatting, linker behavior, assertions, raw argv, enum/layout and target ABI cases, generic free calls, macro borrow checks, and the selected stage2 gate. The full direct-stage1 corpus passes in CI.
- [~] Stage1 builds stage2 with stage0 unavailable; stage2 handles help/version, lex/parse/check on input over 128 tokens, and builds/runs selected programs covering control flow, generic calls, function pointers, aggregate layouts, and ownership/drop behavior. This is selected stage2 evidence, not full stage2 or stage3 parity.
- [x] Full default Linux integration corpus through direct native stage1 passes on the current PR head. Earlier reports of `json_containers_test`, `json_robust_test`, `json_tcp_test`, `json_test`, `macro_test`, `module_import_test`, `serialize_auto_test`, `serialize_containers_test`, and `vec_macro_test` failing came from an older binary; current CI reports zero failures. One benchmark fixture is skipped by default.
- [ ] Full stage0-independent package/build/run/test behavior, complete stage2 coverage, stage3 self-compilation, native ownership/leak parity, and full runtime parity are not established.

Primary evidence: [self-host gate definitions](tests/selfhost/README.md) and
the runtime fixtures linked below. Test totals are from the latest recorded
run and should be refreshed after material changes.

## Completion work plan

1. Broaden macro/derive coverage beyond the current passing `macro_test`,
   `vec_macro_test`, JSON, and serialization fixtures. Keep generated string
   borrowing checks covered as macro forms expand.
2. Close the AGM gap by aligning module publication, import metadata, native
   object generation, and the module integration fixture around stage1-owned
   contracts; retain local errors for unsupported artifact forms.
3. The driver audit and direct-stage1 full integration gate now pass on
   `ab73fef`. Expand stage2 coverage and check for a stage1-to-stage2 fixed
   point. Stage0 remains limited to bootstrapping and comparison.

## CLI, workspace, and build lifecycle

| Feature | Current status | Remaining work |
| --- | --- | --- |
| CLI entry point and argument handling | [x] | Keep command behavior and diagnostics aligned with stage0. |
| `lex` token dump | [x] | Current parity gate compares kind, raw text, byte span, and line/column span. |
| `parse` lossless CST dump | [~] | Span boundaries and parser acceptance are compared; identical AST/CST trees or serialization are not expected or proven. |
| `check` command | [~] | Local frontend and selected overload diagnostics work; full typed checking and rendered diagnostic parity remain open. |
| `ast` command | [~] | Command exists in the driver; complete stage0-compatible behavior and parity need a focused gate. |
| `build`, `run`, `test`, package operations | [~] | Supported source and workspace build/run requests use stage1; unsupported requests fail locally. Several integration fixtures still expose native-backend gaps. |
| Manifest parsing and target selection | [x] | Stage1 plans supported workspace targets and target selection. Expand compatibility coverage against stage0. |
| Workspace dependencies | [~] | Local/workspace resolution is present. Git package dependencies and targets are explicitly unsupported by the stage1 resolver. |
| Package manifest and target keys | [~] | A supported subset is parsed; unknown keys fail with an error. Full manifest compatibility is unproven. |
| Clean/init commands | [~] | Recognized by workspace command planning; end-to-end parity is not established by the current no-stage0 corpus. |
| Native compiler cache | [~] | Cache behavior parity has a focused passing gate. Reproducible compiler artifact digests and the full cache contract remain open. |
| Stage0 runtime delegation | [x] | Stage1 has no compiler or module-generator dispatch path; the workspace gate traps both old cwd-relative fallback locations. |
| Native integration parity | [x] | The default Linux integration corpus passes using stage1 alone: 277 passed, 0 failed, and 1 benchmark skip in CI. Unsupported behavior outside the corpus and full semantic/codegen parity remain open. |

## Lexing, parsing, and source representation

| Feature | Current status | Remaining work |
| --- | --- | --- |
| UTF-8 source tokenization, comments, identifiers, keywords, literals, and punctuation | [x] | Token parity is covered over the current corpus; uncommon and malformed-input edges still need systematic coverage. |
| Byte and line/column source locations | [x] | Token spans and AST/CST span-boundary projection are gated. |
| Parser acceptance over repository, example, and optional standard-library corpus | [x] | This proves acceptance parity, not identical trees or all downstream semantics. |
| Lossless CST and AST-span projection | [~] | Retained source structure is a CST projection; full stage0 AST equivalence is not a goal of the span gate and typed HIR coverage is only a selected slice. |
| Imports and module loading | [~] | Source imports and prebuilt `.agm` artifacts work on selected paths. Stage1 does not build `.submodule.toml` inputs; broad visibility, re-export, and package behavior remain open. |
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
| Function declarations, calls, parameters, and returns | [~] | All current integration fixtures pass. Broader free-function, method, coercion, and expression contexts outside that corpus remain unestablished. |
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
| Borrow origins and reference escape checking | [~] | Selected accepted/rejected escape regressions and Send boundaries pass. Native lowering rejects borrowed block results traced to local storage, including method-derived `str` results in `tests/selfhost/fixtures/macro_str_borrow_error_test.ag`. Analysis is not yet complete typed-CFG parity. |
| Move analysis and use-after-move rejection | [~] | Native compilation invokes ownership analysis and selected negative fixtures pass. Branches, partial initialization, indexed places, temporaries, and reinitialization need broader typed analysis. |
| Copy/type properties and partial initialization | [ ] | Full property propagation and partially initialized aggregate behavior are not established. |
| Destructors, field cascades, overwrite cleanup, temporaries, and scope-exit drops | [~] | Slice 1 native drop elaboration runs guarded drops on scope/return/loop exits with move-aware clearing, overwrite predrop, and zero-initialized locals; guard destruction, drop counters, cascades, and field predrops pass focused gates. Nested field paths, index drops, and temporaries remain open. |
| `defer` and cleanup order on early exits | [ ] | Full native LIFO cleanup and return/break/continue coverage is not established. |
| Allocation and reallocation | [~] | Selected generic allocation/reallocation and layout paths pass; complete allocator and resource cleanup parity remains open. |
| Runtime panic/assertion behavior | [~] | Selected `@assert` failure reporting is covered; panic/unwind behavior and cleanup interactions remain open. |
| Threads, `launch`/`wait`, channels, condition variables, and locks | [~] | The full integration corpus covers launch/wait and failed-spawn cleanup; futex, channel, condition-variable, and RwLock fixtures pass. Broader concurrent workloads and race/ownership analysis remain open. |
| Standard library behavior | [~] | The full default integration corpus passes, including its stdlib-heavy fixtures. This does not certify every library API or every call context. |

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
| AGLSP/AGSM package checks | [x] | Stage1 builds and protocol-tests AGLSP; AGSM CLI is integrated into `agc`. Foreign `.submodule.toml` generation remains unsupported and missing artifacts fail locally. |

## Remaining completion gates

- [x] Make the full default integration corpus pass through direct native stage1 with stage0 unavailable: current CI reports 277 passed, 0 failed, and 1 default benchmark skip across 278 fixtures.
- [ ] Expand coverage beyond the passing integration corpus for unsupported calls/methods/arguments, aggregate expressions, stdlib contexts, ownership/drop behavior, and runtime state mutation.
- [ ] Complete generic inference across multiple arguments, nested aggregates, methods, and receiver contexts.
- [ ] Complete typed CFG-based borrow, move, type-property, partial-init, and drop analysis, then verify cleanup with ownership/leak-focused tests.
- [ ] Expand `check` to general expression typing and compare full rendered diagnostics.
- [ ] Complete native artifact publication, ABI/layout metadata, module visibility/re-export behavior, and cache/reproducibility contracts.
- [ ] Run the full integration corpus through stage2 with stage0 disabled, then attempt stage3 self-compilation. Selected stage2 runtime programs do not establish full stage2 parity.
- [ ] Re-run compiler tests, the full integration suite, `memory_pentest`/`cascade_drop_test`, frontend parity, and relevant self-host gates after future compiler changes. Report pass, fail, skip, and unrun results separately.

## Scope notes

This checklist covers the stage1 compiler and migration boundaries visible in
the repository. It inventories language/compiler features, not every function
in `std/`; each standard-library module needs its own API-level inventory if
that is the intended scope. The authoritative syntax contract is
[SYNTAX.md](SYNTAX.md). Compiler architecture and stage0 contracts are in
[docs/compiler-guide.md](docs/compiler-guide.md). The migration tracker and
handoff contain detailed work IDs and run logs; update this checklist when
those statuses change.
