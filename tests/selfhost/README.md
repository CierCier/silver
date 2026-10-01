# Self-host gates

`run_stage.sh` is the frontend stage0/stage1 boundary:

1. build the Rust stage0 compiler;
2. compile `silver.toml` with stage0 to produce the Silver stage1 binary;
3. exercise stage1-owned manifest planning, target selection, argument
   boundaries, and the transitional native request;
4. run focused HIR declaration, call, arity, and return-type checks;
5. require stage0 and stage1 diagnostics for a real two-error fixture to be
   emitted in source-span order;
6. compare stage0 `--emit=tokens` with stage1 `lex` over the deterministic
   top-level `tests/*.ag` and `examples/*.ag` corpus (`--include-std` adds
   recursively discovered `std/**/*.ag` files);
7. compare stage0 AST span boundaries with stage1 CST span boundaries over
   that same corpus, preserving source locations without requiring identical
   tree shapes;
8. compare parser acceptance for that corpus;
9. compare semantic check status over that corpus with the integration
   CPU cfg defaults and fixture-specific additions;
10. compare the focused typed Send boundary and exercise the AGM reader.

Stage1's executable dump interfaces are `agc lex <file>` (token records) and
`agc parse <file>` (a lossless CST tree with byte spans). Stage0
`--emit=tokens` and `--emit=ast` use different serializations and tree models.
The token gate normalizes token kind, raw text, and byte span. The AST-span gate
excludes parser-specific root `Program` spans and requires each semantic AST
span's start and end boundaries to appear among stage1 CST node/token boundaries.
It does not claim byte-identical AST/CST serialization or equality of tree
shapes. Both gates report the first mismatch per file.

`check_diagnostic_order.py` validates the exact pair of primary error locations
from `diagnostic_order_fixture.ag`, then fails if either compiler changes the
diagnostic sequence away from ascending line/column order.

`check_semantic_regressions.py` compares focused nested generic type and borrow
escape cases against stage0, including accepted scalar returns and rejected
local reference escapes.

`run_native.sh` is the Linux command-surface gate. It builds the same stage1
binary, exports `SILVER_STAGE0`, checks the bridge boundary, runs the opt-in
stage1 native smoke and linker-contract gates, checks native cache behavior,
and runs the integration runner through `--compiler "$stage1"`. During the
backend migration, stage1 keeps `lex`, `parse`, and `check` local and delegates
native build/run/package/cache operations to the verified stage0 backend
through an explicit compatibility seam. The `SILVER_STAGE1_NATIVE=1` path
tests direct, package, and run invocations plus cfg selection, raw argc/argv,
literal comptime casts, generic `Vec<String>` methods and chained receivers,
formatting, concrete generic enum payload round trips, and fail-closed fallback
for `defer`; it is a smoke
backend, not the complete native compiler. Inputs outside the backend's current
scope fail closed to the explicit stage0 bridge instead of producing
unverified output. Stage0 root-object caching is enabled, and source imports
inlined during lowering are included in dependency keys.

`check_native_enum_layout.py` disables the stage0 bridge and checks small and
String payloads in `Optional` and both `Result` variants. It also checks empty
variants, assignment, and aligned fields in a generic two-field enum. It does
not prove native ownership cleanup or a working stage2 compiler.

`check_stage2.py` builds stage2 with the native stage1 backend while stage0 is
unavailable. It then runs stage2's help, version, lexer, parser, and checker on
an input exceeding 128 tokens, and builds and executes a program with stage2.
This proves those stage2 paths. It does not cover the full native corpus or
stage3 self-compilation.

`check_native_layout.py` checks target ABI sizes for i128, nested and generic
structs, and arrays. `check_generic_function_native.py` exercises concrete free
function calls, nested generic arguments, and allocation and reallocation.
Both gates disable stage0 fallback.

The frontend gate compares token kind, raw text, byte span, and line/column
span. `check_hir.py` covers the retained HIR's first typed declaration,
call-argument, arity, and return-expression slice. The semantic gate compares
acceptance status rather than the complete
stage0 diagnostic catalog because the typed checker is still being built;
this remains a conservative projection. It prepares temporary AGM artifacts
(including a deterministic RayGUI namespace
fixture), checks malformed-artifact rejection, and reports zero
deferred-boundary skips. `check_send.py` adds an exact primary-diagnostic
boundary for the typed launch projection. `check_artifacts.py` covers all
supported reader versions, empty source strings, the legacy v9 empty trailer
in synthetic and vendored artifacts, malformed-input rejection, and three
cross-stage roundtrips: both stage1 publication paths are read by stage0, and
a stage0-produced artifact is imported by stage1. See `docs/agm-format.md`
for the byte layout and version-bump contract. Stage1 publication currently
covers only its frontend metadata projection; rich layout/ABI/native-library
metadata remains stage0-produced.

The frontend gates are intentionally independent
of LLVM and of the linker. A green frontend gate is not a claim that native
backend or stage2 parity is complete.
