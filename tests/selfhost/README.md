# Self-host gates

`run_stage.sh` builds stage1 with the Rust stage0 bootstrap, then uses stage1 to
build and exercise AGLSP and its integrated `agc agsm` command before checking
frontend parity. It compares tokenization, parser acceptance, AST/CST span
boundaries, semantic check status, typed Send behavior, diagnostics, and AGM
artifacts over the configured source corpus. These checks do not claim native
backend parity.

`run_native.sh` is the Linux command-surface and integration gate. Stage0 is
used to produce stage1; all subsequent stage1 check/build/run operations use
stage1's own backend. The gate covers workspace commands, native backend and
linker contracts, cache behavior, stage2 smoke checks, and the integration
runner invoked with `--compiler "$stage1"`. The workspace check places a
failing compiler at the old cwd-relative fallback path and verifies stage1
does not launch it. Unsupported requests fail locally. The full integration
corpus remains the measure of native parity; focused smoke gates do not replace it.

`run_native_no_stage0.sh /path/to/agc-stage1` runs focused native fixtures,
the workspace launch-trap check, and stage2 checks against stage1 directly. It
does not replace the full integration corpus exercised by `run_native.sh`.

`check_native_backend.py` covers direct-file, workspace build, and run paths.
Other focused checks exercise linker selection, format/assert/args support,
enum and aggregate layout, generic functions and methods, stage2, and cache behavior.
`check_native_coverage.py` reports a broader per-fixture build/run inventory;
failures identify unsupported backend work and are not silently skipped.

`check_stage2.py` builds stage2 with the native stage1 backend, then exercises
stage2 help/version, lexer/parser/checker, and representative compiled
arithmetic, control-flow, generics, function pointers, aggregate ownership,
references, pointer outputs, and matches. Its default run is a focused gate.
Use the optional local checks to run the complete corpus with stage2, build
stage3 through stage2, and run the corpus with stage3:

```bash
python3 tests/selfhost/check_stage2.py --stage1 /path/to/agc-stage1 \
    --full-corpus --stage3 --jobs 4
```

`--stage3` also checks frontend commands, the compiler workspace, the C enum
ABI, and aggregate/drop, field Copy, iterator, and structured I/O runtimes.
Both self-builds bypass the object cache. The optional corpus runs use the
existing integration harness and its normal fixture skips. They establish
behavior on the tested corpus, not binary identity or complete language parity.
These options are opt-in so the default CI gate keeps its existing workload.

The semantic regression checks compare focused generic and borrow-escape cases
against stage0. The diagnostic gate checks primary error ordering. The AGM
gates cover supported reader versions, malformed input, and cross-stage
roundtrips; stage1 artifact publication remains a frontend metadata projection.
Stage1 imports prebuilt `.agm` files and reports the expected artifact path when
a matching `.submodule.toml` exists without one; it does not build those inputs.

All reported stage0 use above is bootstrap or comparison-only. Stage1 runtime
commands have no stage0 delegation path.
