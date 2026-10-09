# Self-host completion record

Updated: 2026-10-09

## Definition of done

The Linux pipeline is complete when a stage1-built `agc`:

1. builds from the root `silver.toml` using stage0;
2. accepts the stage0 command surface for native build/run/check/package/cache operations;
3. passes the complete Linux integration corpus with the same results as stage0;
4. preserves cache correctness, reproducibility, diagnostics, and runtime behavior;
5. passes the full frontend parity gate without deferred-boundary skips; and
6. can compile itself to a stage2 binary whose behavior is fixed-point equal to stage1.

Stage1 now owns the native backend for supported commands and returns local
errors for unsupported requests. Stage0 builds stage1 and remains available for
comparison; stage1 runtime commands do not delegate to it.

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

## Current status at PR #30 head ab73fef (2026-10-09)

- Evidence: [PR #30 workflow run](https://github.com/CierCier/silver/actions/runs/37908849775).
- Latest PR CI checks pass: Greptile, the native test job, and the WASM job.
- Cargo test groups pass. The stage0 integration run reports 275 passed, 0
  failed, and 3 skipped across 278 fixtures. The skips are `mem_growth_watch`
  and two stage1-only regression fixtures.
- `run_native.sh` bootstraps stage1, then invokes stage1 directly for runtime
  checks. Its integration run reports 277 passed, 0 failed, and 1 skipped
  across 278 fixtures. The only skip is `mem_growth_watch`.
- Frontend parity passes for 425 token files, 425 AST-span files, and 429
  semantic-status files. GATE-003 projection and AGM reader/cross-stage checks
  pass. Native backend, Send, linker, argv, enum/layout, generic-call, and
  selected stage2 checks also pass.
- The focused `run_native_no_stage0.sh` currently lists 69 integration
  fixtures. The current CI's full stage1 integration run is broader than that
  focused fixture list. Full stage2 corpus and stage3 self-compilation remain
  unverified.
- The former nine stage1 integration failures (258/9/3 on an older binary) are
  resolved on this head; the latest run has no test failures.
- Failed launch cleanup is implemented in both codegens and covered by
  `launch_failure_drop_test`. Non-Send task results are intentionally allowed:
  `wait` joins before exposing the result, covered by `Task<Rc<i64>>` in
  `launch_send_test`.
- Broader typed ownership analysis, complete diagnostic rendering, native
  artifact/cache parity, complete stage2 coverage, and stage3 remain open.

### Uncommitted runtime-boundary cleanup

The working-tree follow-up removes the stage1 compiler and AGSM runtime
lookups. Stage1 still imports prebuilt `.agm` files; it now rejects
`.submodule.toml` generation locally. A fresh stage0 bootstrap and stage1 build
passed, along with the workspace, Send, native-backend, and four module-import
regression checks. The full integration suite and CI were not rerun for this
follow-up.

### Stage1 AGLSP and AGSM command integration — 2026-10-09

The Silver workspace removes its separate AGSM executable target and exposes
the AGSM CLI under `agc agsm`. `run_stage.sh` now uses the freshly built stage1
compiler to build AGLSP, then exercises the resulting language server. AGSM's
unsupported foreign-module build path returns a local error; no executable or
environment-based fallback is used. The semantic gate explicitly expects
missing raylib/curl `.agm` artifacts to fail locally.

Verification: `bash tests/selfhost/run_stage.sh --include-std` passed, including
AGLSP protocol checks, workspace checks, 425 token and AST-span files, 429
semantic-status files, HIR/type checks, diagnostics, cfg, Send, and artifact
checks. Two absent foreign AGM cases are expected local failures. Native
`.submodule.toml` generation remains unsupported; the full integration suite
and CI were not run in this continuation.

## Historical PR #30 status at head 91a0aff (2026-10-07; before bridge removal)

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
