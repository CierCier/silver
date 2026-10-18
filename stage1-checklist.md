# Stage1 feature and migration checklist

Reviewed: 2026-10-18. This is a status inventory of the self-host migration, not
a promise that every feature described by the language syntax is implemented
end to end in stage1. A feature may parse or pass a semantic-status comparison
while still lacking type checking, native lowering, or runtime parity.

## Status key

- [x] Implemented for the stated scope, with a linked gate or dated result.
- [~] Implemented for a limited projection or selected cases; remaining scope is stated.
- [ ] Missing, delegated, failing, or not yet established.
- [?] Current support is unclear; needs a focused probe before being called complete.

## Current migration snapshot

- [x] Stage1 is built by stage0 from the Silver workspace and has its own CLI.
- [x] Stage1-owned `lex`, `parse`, and `check` commands run locally.
- [~] The [completion record](docs/selfhost-completion-record.md) separates dated local results, historical CI, and checks that have not run. Old PR counts are not current certification.
- [~] [Frontend gates](tests/selfhost/run_stage.sh) compare tokens, parser acceptance, span projections, semantic status, selected diagnostics and artifacts. These do not prove complete checking or identical diagnostics.
- [~] [Native gates](tests/selfhost/run_native.sh) bootstrap stage1 and run its own backend and integration corpus. [The focused gate](tests/selfhost/run_native_no_stage0.sh) covers fewer fixtures and does not replace the full corpus.
- [~] [Generation checks](tests/selfhost/check_stage2.py) self-build stage2 and optionally stage3, with focused runtimes and optional complete corpora. The 2026-10-12 record reports full stage2/stage3 passes; this is dated evidence, not a current fixed-point guarantee.
- [~] [Native target checks](tests/selfhost/check_native_target.py) require the exact LLVM host triple and preserve frontend target cfg and forwarded program arguments.
- [ ] Full native option parity, complete typed checking/ownership, allocator leak-check instrumentation, artifact parity, and full runtime parity remain open. Accepted native `--leak-check` currently leaves instrumentation disabled.

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
3. Capture dated, reproducible stage0/stage1 results and run the optional full
   stage2/stage3 gates. Audit accepted native options separately from corpus
   results. Stage0 remains a bootstrap and comparison compiler.

## CLI, workspace, and build lifecycle

| Feature | Current status | Remaining work |
| --- | --- | --- |
| CLI entry point and argument handling | [x] | Keep command behavior and diagnostics aligned with stage0. |
| `lex` token dump | [x] | Current parity gate compares kind, raw text, byte span, and line/column span. |
| `parse` lossless CST dump | [~] | Span boundaries and parser acceptance are compared; identical AST/CST trees or serialization are not expected or proven. |
| `check` command | [~] | Local frontend and selected overload diagnostics work; full typed checking and rendered diagnostic parity remain open. |
| `ast` command | [~] | Command exists in the driver; complete stage0-compatible behavior and parity need a focused gate. |
| `build`, `run`, `test`, package operations | [~] | Source and workspace build/run requests use stage1. Workspace `test` is explicitly unsupported; accepted options can be ineffective. See the [native option audit](docs/compiler-guide.md#native-options). |
| Manifest parsing and target selection | [x] | Stage1 plans supported workspace targets and target selection. Expand compatibility coverage against stage0. |
| Workspace dependencies | [~] | Local/workspace resolution is present. Git package dependencies and targets are explicitly unsupported by the stage1 resolver. |
| Package manifest and target keys | [~] | A supported subset is parsed; unknown keys fail with an error. Full manifest compatibility is unproven. |
| Clean/init commands | [~] | Native cache cleaning is implemented and gated. Package initialization is recognized by the parser but has no working stage1 initialization path. |
| Native compiler cache | [~] | Cache behavior parity has a focused passing gate. Reproducible compiler artifact digests and the full cache contract remain open. |
| Stage0 runtime delegation | [x] | Stage1 has no compiler or module-generator dispatch path; the workspace gate traps both old cwd-relative fallback locations. |
| Native integration parity | [~] | Dated full-corpus results are in the [verification record](docs/selfhost-completion-record.md). A passing corpus does not certify untested behavior, complete semantic checking, or native leak instrumentation. |

## Lexing, parsing, and source representation

| Feature | Current status | Remaining work |
| --- | --- | --- |
| UTF-8 source tokenization, comments, identifiers, keywords, literals, and punctuation | [x] | Token parity is covered over the current corpus; uncommon and malformed-input edges still need systematic coverage. |
| Byte and line/column source locations | [x] | Token spans and AST/CST span-boundary projection are gated. |
| Parser acceptance over repository, example, and optional standard-library corpus | [x] | This proves acceptance parity, not identical trees or all downstream semantics. |
| Lossless CST and AST-span projection | [~] | Retained source structure is a CST projection; full stage0 AST equivalence is not a goal of the span gate and typed HIR coverage is only a selected slice. |
| Imports and module loading | [~] | Source imports and prebuilt `.agm` artifacts work on selected paths. Stage1 does not build `.submodule.toml` inputs; broad visibility, re-export, and package behavior remain open. |
| Selective imports and import aliases | [~] | Source selection and aliases are exercised by [selective imports](tests/selective_import_test.ag) and [source aliases](tests/source_import_alias_test.ag). Complete re-export and visibility behavior remains open. |
| Complex number type support | [ ] | Complex literal token support exists, while type-level support is incomplete. |
| Octal and binary integer literals | [x] | Both lexers accept prefixed literals and separators; [literal/reflection fixtures](tests/literals_and_reflection_test.ag) exercise their values. |
| Tuple types, tuple literals, and pattern destructuring | [~] | Square-bracket forms are implemented, including [destructuring](tests/destructure_let_test.ag), [generic returns](tests/generic_tuple_return_test.ag), and [partial cleanup](tests/tuple_partial_index_drop_test.ag). Parentheses do not define tuples. |
| Slice types and slice expressions | [~] | [Slice syntax](tests/slice_syntax_test.ag) and [first-class slices](tests/test_first_class_slices.ag) cover borrowed views. Complete bounds, lifetime, and cross-target behavior remains open. |

## Types, declarations, and semantic analysis

| Feature | Current status | Remaining work |
| --- | --- | --- |
| Primitive integer, float, bool, char, string, void, pointer, and reference syntax | [~] | Syntax and frontend acceptance are broad; native operations and ABI cases vary by type and need full parity coverage. |
| Structs, fields, arrays, globals, constants, and type aliases | [~] | Selected aggregate/global/array paths work natively. String globals now emit constant bytes, including escaped and empty strings. Globals take precedence over unqualified enum variants; [collision regression](tests/native_global_variant_collision_test.ag) covers these cases. Aggregate expressions and layout coverage remain incomplete. |
| Enums and payload variants | [~] | Selected unit, small-payload, String-payload, assignment, and generic multi-field layout cases pass; broad enum operations, ownership, and payload combinations remain open. |
| Function declarations, calls, parameters, and returns | [~] | The [dated corpus records](docs/selfhost-completion-record.md) cover selected call and return contexts. Broader free-function, method, coercion, and expression contexts remain unestablished. |
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
| Direct calls, method calls, static methods, and chained receivers | [~] | Concrete calls and selected generic receiver chains have runtime coverage. [Receiver shorthand](std/optional.ag) is used in the stdlib; call resolution beyond the fixtures remains unestablished. |
| Field access, pointer/reference auto-dereference, and indexing | [~] | Selected pointer/string/indexed reads work and a `String.data[index]` ownership regression is fixed. Aggregate/index-set coverage and indexed ownership typing remain open. |
| Struct/array initialization and aggregate values | [~] | Local array initialization, selected structs, arrays, globals, and nested/generic layouts have coverage. General aggregate expressions and argument/return contexts remain incomplete. |
| `if`, `while`, C-style `for`, `for-in`, `break`, `continue`, and `return` | [~] | [Loop regression](tests/native_for_in_continue_test.ag) covers continue, cached endpoints, and source evaluation order. [Iterator ergonomics](tests/iterator_ergonomics_test.ag) covers borrowed and consuming Vec iteration and pointer reuse. Control-flow and cleanup behavior beyond these fixtures remains open. |
| `match` and enum patterns | [~] | Selected enum, void-valued, and payload paths work. Exhaustiveness, arm typing, moves, and full pattern coverage remain open. |
| `defer` | [~] | [Defer fixtures](tests/defer_test.ag) cover LIFO scope/return behavior. Broader cleanup interactions with moves and loop exits remain a separate verification task. |
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
| `defer` and cleanup order on early exits | [~] | [Defer](tests/defer_test.ag) and [ownership pentest](tests/memory_pentest.ag) exercise scope and early exits; a complete control-flow proof is not established. |
| Allocation and reallocation | [~] | Selected generic allocation/reallocation and layout paths pass; complete allocator and resource cleanup parity remains open. |
| Runtime panic/assertion behavior | [~] | Selected `@assert` failure reporting is covered; panic/unwind behavior and cleanup interactions remain open. |
| Threads, `launch`/`wait`, channels, condition variables, and locks | [~] | The full integration corpus covers launch/wait and failed-spawn cleanup; futex, channel, condition-variable, and RwLock fixtures pass. Broader concurrent workloads and race/ownership analysis remain open. |
| Standard library behavior | [~] | The [dated corpus records](docs/selfhost-completion-record.md) include stdlib-heavy fixtures. These results do not certify every library API or call context. |

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

- [~] Keep the full direct-native integration corpus passing, with dated compiler/source identities, explicit skips, and checking/instrumentation limits in the completion record.
- [ ] Expand coverage beyond the passing integration corpus for unsupported calls/methods/arguments, aggregate expressions, stdlib contexts, ownership/drop behavior, and runtime state mutation.
- [ ] Complete generic inference across multiple arguments, nested aggregates, methods, and receiver contexts.
- [ ] Complete typed CFG-based borrow, move, type-property, partial-init, and drop analysis, then verify cleanup with ownership/leak-focused tests.
- [ ] Expand `check` to general expression typing and compare full rendered diagnostics.
- [ ] Complete native artifact publication, ABI/layout metadata, module visibility/re-export behavior, and cache/reproducibility contracts.
- [~] Repeat the optional full stage2/stage3 corpora from the dated 2026-10-12 run on each release candidate. Selected runtime programs and a self-build do not establish complete language parity.
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
