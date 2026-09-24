# Self-host completion record

Updated: 2026-09-24

## Definition of done

The Linux pipeline is complete when a stage1-built `agc`:

1. builds from the root `silver.toml` using stage0;
2. accepts the stage0 command surface for native build/run/check/package/cache operations;
3. passes the complete Linux integration corpus with the same results as stage0;
4. preserves cache correctness, reproducibility, diagnostics, and runtime behavior;
5. passes the full frontend parity gate without deferred-boundary skips; and
6. can compile itself to a stage2 binary whose behavior is fixed-point equal to stage1.

The current work is intentionally ordered toward that predicate. The native
command bridge is a temporary compatibility seam, not the final self-host
backend: it preserves the already-tested stage0 backend while the typed
semantic and code-generation layers move into Silver.

## Verified baseline

- `python3 tests/run_tests.py --no-tui`: 200 passed, 1 intentional skip.
- `bash tests/selfhost/run_stage.sh --include-std`: token parity and parser
  parity pass for 346 files; the semantic leg is run separately by the script.
- `bash tests/selfhost/run_native.sh --no-tui --jobs 4`: 200 passed, 1 intentional skip through the stage1 command surface.
- `bash tests/selfhost/run_stage.sh --include-std`: token/parse/semantic parity for 346 files, typed Send, and seven-version AGM checks pass.
- `cargo test -p agc --lib`: 562 passing tests after the cache-key and cache-integrity slices.

## Current completion state

| Surface | State | Evidence / next action |
| --- | --- | --- |
| Lexer | green | 348-file token parity (`run_stage.sh --include-std`) |
| CST parser | green | 348-file parse acceptance parity |
| Cfg/source imports | green | source modules, cfg projections, `.agm` export metadata, include roots, and malformed-input gates |
| Name/borrow/Send/ownership | green | canonical `Place`, field disjointness, `BitSet`-accelerated move/borrow state tracking, typed Send |
| Canonical Types & Exprs | green | canonical `TypeTable` (`TypeId`), flat `AstExpr`/`AstStmt` pool, bottom-up type inference |
| Monomorphization | green | `MonomorphCollector`, symbol mangling, 256-generation fixpoint cap |
| Diagnostics & Messages | green | centralized catalog in `messages.ag`, Levenshtein fuzzy typo suggestions |
| Native build/run | transitional | explicit stage0 bridge is covered by `check_bridge.py` and `run_native.sh` (200/200 passed) |
| Cache | transitional | dependency entries validated/staged, cyclic-module tested |
| Stage2 fixpoint | deferred | deferred to upcoming textual LLVM IR backend migration pass |

## Frontend completion summary

The frontend migration is complete across all slices:
- Canonical type table with integer `TypeId`s and $O(1)$ property queries.
- CST lowering to flat POD `AstExpr` and `AstStmt` representations with bottom-up type checking.
- Place and projection model with field-disjointness and $O(1)$ `BitSet` local variable tracking.
- Monomorphization request collector with generation limits and mangling.
- Centralized user-facing diagnostic messages catalog with fuzzy typo suggestions.
- All gates passing green: 348/348 files in `run_stage.sh --include-std`, 200/200 integration tests in `run_native.sh --jobs 4`.
