# Self-host gates

`run_stage.sh` is the frontend stage0/stage1 boundary:

1. build the Rust stage0 compiler;
2. compile `silver.toml` with stage0 to produce the Silver stage1 binary;
3. exercise stage1-owned manifest planning, target selection, argument
   boundaries, and the transitional native request;
4. run focused HIR declaration, call, arity, and return-type checks;
5. compare stage0 `--emit=tokens` with stage1 `lex` over `tests/` and
   `examples/` (add `--include-std` for the full standard-library corpus);
6. compare parser acceptance for the same corpus;
7. compare semantic check status over the same corpus with the integration
   CPU cfg defaults and fixture-specific additions;
8. compare the focused typed Send boundary and exercise the AGM reader.

`run_native.sh` is the Linux command-surface gate. It builds the same stage1
binary, exports `SILVER_STAGE0`, checks the bridge boundary, exercises the
opt-in stage1 native smoke path, checks cache reproducibility, and runs the
complete native integration runner with
`--compiler "$stage1"`. During the backend migration, stage1 keeps `lex`,
`parse`, and `check` local and delegates native build/run/package/cache
operations to the verified stage0 backend through an explicit compatibility
seam. The opt-in `SILVER_STAGE1_NATIVE=1` path also builds and runs a small
`i32 main()` fixture through stage1-owned textual LLVM IR and `llc`/`cc`; it is
a smoke backend, not the complete native compiler. This makes the boundary
executable without presenting the transitional backend as a completed
self-host implementation. Stage0 dependency-module
cache entries remain enabled; root-object reuse is currently disabled because
implicit imports are injected after dependency-graph discovery.

The frontend gate compares token kind, raw text, byte span, and line/column
span. `check_hir.py` covers the retained HIR's first typed declaration,
call-argument, arity, and return-expression slice. The semantic gate compares
acceptance status rather than the complete
stage0 diagnostic catalog because the typed checker is still being built. It
now prepares temporary AGM artifacts (including a deterministic RayGUI
namespace fixture), checks malformed-artifact rejection, and reports zero
deferred-boundary skips. `check_send.py` adds an exact primary-diagnostic
boundary for the typed launch projection, while `check_artifacts.py` covers
all supported AGM versions and malformed inputs. The name pass consumes a
typed local-binding projection, while ownership and borrow checks remain
conservative projections. The frontend gates are intentionally independent
of LLVM and of the linker. A green frontend gate is not a claim that native
backend or stage2 parity is complete.
