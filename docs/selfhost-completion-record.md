# Self-host completion record

Updated: 2026-10-07

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

## Historical verified baseline (2026-09-24)

- `python3 tests/run_tests.py --no-tui`: 200 passed, 1 intentional skip.
- `bash tests/selfhost/run_stage.sh --include-std`: token parity and parser
  parity pass for 348 files (201 tests + 26 examples + 121 std); the semantic leg is run separately by the script.
- `bash tests/selfhost/run_native.sh --no-tui --jobs 4`: 200 passed, 1 intentional skip through the stage1 command surface.
- `bash tests/selfhost/run_stage.sh --include-std`: token/parse/semantic parity for 348 files, typed Send, and seven-version AGM checks pass.
- `cargo test -p agc --lib`: 562 passing tests after the cache-key and cache-integrity slices.

## Completion state at the historical baseline

| Surface | State | Evidence / next action |
| --- | --- | --- |
| Lexer | green | 348-file token parity (`run_stage.sh --include-std`) |
| CST parser | green | 348-file parse acceptance parity |
| Cfg/source imports | green | source modules, cfg projections, `.agm` export metadata, include roots, and malformed-input gates |
| Name/borrow/Send/ownership | green | canonical `Place`, field disjointness, `BitSet`-accelerated move/borrow state tracking, typed Send |
| Canonical Types & Exprs | green | canonical `TypeTable` (`TypeId`), flat `AstExpr`/`AstStmt` pool, bottom-up type inference |
| Monomorphization | green | `MonomorphCollector`, symbol mangling, 256-generation fixpoint cap |
| Diagnostics & Messages | green | centralized catalog in `messages.ag`, Levenshtein fuzzy typo suggestions |
| Native build/run | transitional | explicit stage0 bridge is covered by `check_bridge.py` and `run_native.sh` (200 passed, 1 intentional skip) |
| Cache | transitional | dependency entries validated/staged, cyclic-module tested |
| Stage2 fixpoint | deferred | Native code generation was not yet part of the committed stage1 path. |

## Frontend completion summary at the historical baseline

The frontend migration is complete across all slices:
- Canonical type table with integer `TypeId`s and $O(1)$ property queries.
- CST lowering to flat POD `AstExpr` and `AstStmt` representations with bottom-up type checking.
- Place and projection model with field-disjointness and $O(1)$ `BitSet` local variable tracking.
- Monomorphization request collector with generation limits and mangling.
- Centralized user-facing diagnostic messages catalog with fuzzy typo suggestions.
- Historical gates passed: 348 files in `run_stage.sh --include-std`, 200 passed + 1 intentional skip in `run_native.sh --jobs 4`.

## PR #30 status at head 91a0aff (2026-10-07; before bridge removal)

- The opt-in stage1 Linux LLVM backend is tracked in this branch. Unsupported
  constructs fail closed; the explicit stage0 bridge remains the default for
  native commands.
- The PR verification record reports frontend parity passing for 411 token
  files, 411 AST-span files, 415 semantic-status files, 29 semantic fixtures,
  and 12 source/AGM import checks. This review did not rerun that gate.
- With stage0 fallback disabled, the focused native gate passed all 65 selected
  tests, including the stage2 checks, according to the PR verification record.
  This does not establish full native corpus parity or a stage2 fixed point.
- A fresh stage1 built from PR #30 head `91a0aff` independently passed seven
  selected integration fixtures with `SILVER_STAGE1_NATIVE=1` and
  `SILVER_STAGE0=/nonexistent`: `defer_test`, `launch_wait_native_test`,
  `launch_send_test`, `launch_send_error_test`, `adjacent_string_literal_test`,
  `nested_generic_call_test`, and `net_udp_test`. The separate
  `check_send_gate.py` check also passed. The UDP fixture passed in 1.97 seconds
  with no failures or skips.
- The latest recorded broad native run from an earlier local tree was 172
  passed, 82 failed, and 1 skipped across 255 fixtures, before a subsequent
  shared runtime repair. The previous 254-fixture baseline was 170/83/1.
  Neither is a full-corpus measurement of `91a0aff`; that head has not had a
  fresh broad native run in this review.
- `Task<T>` results are intentionally not required to be `Send`: both stage0
  and stage1 read them only after `wait` joins the worker. The launch runtime
  previously freed transferred argument packs on spawn failure without
  running their destructors; this review adds cleanup in both native codegens
  and a registry-exhaustion regression test. Full native corpus parity remains
  unmeasured on the current head.
- 1:1 native code-generation and runtime parity remains open. Diagnostic
  rendering, normalized LLVM IR, and production artifact/cache-key parity are
  deferred; keep those limits distinct from frontend parity and focused native
  coverage.
