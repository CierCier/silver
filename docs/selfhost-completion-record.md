# Self-host completion record

Reviewed: 2026-10-18. Historical results below apply only to their named source
and compiler snapshots. They do not certify the current tree.

## Current supported contract

Stage0 bootstraps stage1 from the root workspace. Stage1 owns supported native
compilation and does not dispatch to stage0. The [syntax reference](../SYNTAX.md)
links language families to implementations and runtime fixtures; this
record separates verified compiler behavior from remaining checking,
ownership, ABI, and artifact work.

The native driver accepts only the exact LLVM host target triple for code
generation. Source and workspace `check` commands retain target-dependent cfg
selection. `lex`, `parse`, and `ast` accept `--target` but dump unfiltered
source. The [target gate](../tests/selfhost/check_native_target.py) exercises
rejection before output creation and preservation of program arguments.

The [native option audit](compiler-guide.md#native-options) is part of this
contract. Parser acceptance is not evidence of option support. In particular,
native `--leak-check` leaves allocator instrumentation disabled, debug-info
flags do not enable DWARF, and several output/build flags have no effect.
Destruction-counter fixtures still prove their observed cleanup behavior, but
passing them with an ignored leak flag does not prove absence of allocator leaks.

The reproducible baseline command and manually triggered generation workflow
record source/compiler identities, commands, failures, and skips as described
in the [gate documentation](../tests/selfhost/README.md). A dirty worktree must
be identified as a source snapshot, not reported as a clean commit baseline.
CI results require a published workflow run; local execution does not certify
that GitHub Actions ran.

## Clean C01 baseline (2026-10-18)

The recorder completed against clean commit
`4ce949f942301e84a13e8af8bea9ccf717f95872` with `status: ok` and
`snapshot_stable: true`. It rebuilt debug stage0 with Cargo, bootstrapped
stage1 with `--no-cache`, and ran both complete corpora sequentially with
eight workers. Stage0 reported `v0.2.6-247-g4ce949f`; stage1 reported
`agc 0.2.6`.

| Compiler | SHA-256 | Passed / failed / skipped |
| --- | --- | --- |
| Stage0 | `c9a95492f81adc249bed6ae8f852dc41fc9cd2003a106aad67b4d03fb86c1f46` | 289 / 0 / 7 |
| Stage1 | `b4e6835e8eca74773e4cf076c460cd3a36567239407e49c02d55356f2fa906aa` | 294 / 0 / 2 |

Skips are identical to the dirty-snapshot run below. Evidence is retained at
`/tmp/silver-c01-clean-4ce949f/baseline.json` with per-command logs. This
record and the subsequent recorder edge-case fixes were added after capture;
the compiler implementation remains unchanged. Stage1's hash matches the
compiler used for the earlier full stage2/stage3 run.

`run_stage.sh --include-std` also passed using Cargo stage0 and a fresh native
stage1: token and AST-span parity on 445 files, semantic-status parity on
449 files, typed Send, AGLSP protocol, workspace, and artifact checks. Two
foreign AGM cases lacked artifacts. GATE-003 still defers rendered diagnostic,
normalized LLVM IR, and production AGM/cache-key comparisons. The script
started before the documentation commits and ran unchanged compiler sources;
this is a separate frontend run, not part of the recorder's source snapshot.

Implementation PR: [#45](https://github.com/CierCier/silver/pull/45).
Hosted generations remain unrun; the local results do not certify hosted CI.

## Dirty-snapshot C01 baseline (2026-10-18)

The recorder completed with `status: ok` and unchanged source during the run.
The base commit was `962ed91c884f409957cb810b61390c68f6dcc5d4`, with the
uncommitted C01 changes identified by source fingerprint
`9e0851fe58b19c90b9e698a9125a36e9492da18fbc35d931865025e848ad14bf`.
This is a dirty source snapshot, not a clean committed or hosted CI baseline.
The final documentation annotations, including this section, were added after
capture; compiler and test-runner code stayed unchanged.

```bash
python3 tests/selfhost/capture_baseline.py \
    --out /tmp/silver-c01-baseline-20261018 --profile debug --jobs 8 --generations
```

Stage0 was rebuilt with Cargo and stage1 freshly bootstrapped with
`--no-cache`. The native generation gate built stage2 and stage3 with
`--no-cache`; compiler builds did not enable leak checking. All four compilers
report version 0.2.6. Their complete integration corpora ran sequentially, each
covering 296 fixtures:

| Compiler | SHA-256 | Passed / failed / skipped |
| --- | --- | --- |
| Stage0 | `1e62960bf26050fc9bb25a89279280569515e4947b2d47a840927cdd6f7da03e` | 289 / 0 / 7 |
| Stage1 | `b4e6835e8eca74773e4cf076c460cd3a36567239407e49c02d55356f2fa906aa` | 294 / 0 / 2 |
| Stage2 | `6d84d5b4d90aec3663d5cbc43f8033adedfcb5fc8d3bc58d7db5398cee7c9f94` | 294 / 0 / 2 |
| Stage3 | `6d84d5b4d90aec3663d5cbc43f8033adedfcb5fc8d3bc58d7db5398cee7c9f94` | 294 / 0 / 2 |

Every stage skipped `mem_growth_watch`, the manual benchmark, and
`rust_ffi_test`, whose optional Rust FFI library was not built. Stage0 also
skipped five stage1-only regressions: `json_synthesis_scope_test`,
`match_multi_payload_move_test`, `match_payload_remainder_test`,
`method_slice_coercion_test`, and `unwrap_or_array_element_error_test`.

The generation gate passed its native self-build, frontend/workspace, C enum
ABI, focused runtime, and Drop-rejection checks. Matching stage2/stage3 hashes
are one observation in this build, not a complete language or reproducibility
guarantee. Other checks passed: 609 Cargo tests, 15 recorder tests, 17 focused
syntax/API fixtures in each of stage0 and stage1, the README and combined
syntax examples in both stages, native target/workspace/link gates, relative
documentation links, and actionlint for the new workflow.

Local evidence is in `/tmp/silver-c01-baseline-20261018/baseline.json` and its
`logs/` and `generations/` directories. The JSON preserves tool/compiler
versions, commands, statuses, counts, and every skip reason. Its fingerprint
covers tracked and non-ignored source, excluding ignored artifacts and other
ambient build inputs. Native allocator leak instrumentation remains disabled,
as documented above. At that capture, full frontend parity, publication, and a
clean-commit baseline remained outstanding. The newer results above supersede
those local gaps; the manual hosted workflow has not run.

## Definition of done

The Linux pipeline is complete when a stage1-built `agc`:

1. builds from the root `silver.toml` using stage0;
2. implements the documented native build/run/check/package/cache paths, with
   explicit errors for unsupported requests and behavioral coverage for every
   advertised option;
3. passes the complete Linux integration corpus with the same results as stage0;
4. preserves cache correctness, reproducibility, diagnostics, and runtime behavior;
5. passes the full frontend parity gate without deferred-boundary skips; and
6. can compile itself to a stage2 binary whose behavior is fixed-point equal to stage1.

Stage1 owns the native backend for supported commands and explicitly rejects
some unsupported requests, including non-host native targets. The option audit
lists other requests that are still accepted without their promised behavior;
the completion criterion above is not yet met. Stage0 builds stage1 and remains
available for comparison; stage1 runtime commands do not delegate to it.

## Local generational verification (2026-10-12)

The follow-up from `aa8ec54` repairs native ownership
field metadata, generic pointer implementation selection, and preservation of
borrowed pointer bindings after for-in loops. Ownership snapshots the canonical
type table so its private Drop marks cannot change codegen's Copy policy.
Existing fixtures cover imported and nested generic Copy fields, borrowed
iteration, consuming iteration, pointer reuse, and destructor counts.

- Cargo tests: 609 passed. Stage0 integration: 283 passed, 0 failed, 4 skipped.
- Fresh stage1 passes the complete default corpus at 285 passed, 0 failed,
  2 skipped, seven focused ownership/iterator fixtures, native backend smoke,
  29 semantic regressions, 12 source/AGM import checks, and 13 protocol checks
  on the AGLSP executable built by stage1.
- Semantic status parity: 440 files; two absent foreign AGM cases fail locally
  as expected. This compares acceptance status, not complete diagnostic output.
- Stage1 builds stage2, and stage2 builds stage3, using the root workspace with
  object caching disabled. Stage2 and stage3 each pass the complete default
  integration corpus: 285 passed, 0 failed, 2 skipped across 287 fixtures.
- Stage2 retains the focused frontend, runtime, C enum ABI, and Drop-ancestor
  rejection checks. Stage3 passes frontend commands, the compiler workspace
  check, C enum ABI probes, and aggregate/drop, field Copy, iterator, and
  structured I/O runtimes.
- Stage2 and stage3 binaries match SHA-256 in this Linux build:
  `6425b6d55417fd457a4f7325f95848053e4af04d5d767ccdb5da9d2c38380874`.
  This is one observed build, not a general reproducibility guarantee.

The native corpus skips `mem_growth_watch` by default and `rust_ffi_test`
because its Rust library is absent. Stage0 also skips two stage1-only fixtures.
CI was not run. The modified Rust file passes rustfmt; package-wide formatting
still reports pre-existing differences in other files.

Reproduce the expanded local gate with an up-to-date stage1 binary:

```bash
python3 tests/selfhost/check_stage2.py --stage1 /tmp/agc-stage1 \
    --full-corpus --stage3 --jobs 8
```

The expanded options are opt-in; the default CI workload is unchanged. Complete
typed ownership/borrow analysis, diagnostic rendering, artifact/cache parity,
and language behavior beyond this corpus remain open.

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

These are the implementation claims recorded at that historical baseline.
They do not establish complete frontend behavior:
- Canonical type table with integer `TypeId`s and $O(1)$ property queries.
- CST lowering to flat POD `AstExpr` and `AstStmt` representations with bottom-up type checking.
- Place and projection model with field-disjointness and $O(1)$ `BitSet` local variable tracking.
- Monomorphization request collector with generation limits and mangling.
- Centralized user-facing diagnostic messages catalog with fuzzy typo suggestions.
- Historical gates passed: 348 files in `run_stage.sh --include-std`, 200 passed + 1 intentional skip in `run_native.sh --jobs 4`.

## Historical status at PR #30 head ab73fef (2026-10-09)

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
