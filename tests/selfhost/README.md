# Self-host gates

`run_stage.sh` builds stage1 with the Rust stage0 bootstrap, then checks
frontend parity. It compares tokenization, parser acceptance, AST/CST span
boundaries, semantic check status, typed Send behavior, diagnostics, and AGM
artifacts over the configured source corpus. These checks do not claim native
backend parity.

`run_native.sh` is the Linux command-surface and integration gate. Stage0 is
used to produce stage1; all subsequent stage1 check/build/run operations use
stage1's own backend. The gate covers workspace commands, native backend and
linker contracts, cache behavior, stage2 smoke checks, and the integration
runner invoked with `--compiler "$stage1"`. The workspace check supplies a
failing fake `SILVER_STAGE0` executable and verifies stage1 never launches it.
Unsupported requests fail locally. The full integration corpus remains the
measure of native parity; focused smoke gates do not replace it.

`run_native_no_stage0.sh /path/to/agc-stage1` runs the focused native fixtures,
the workspace launch-trap check, and stage2 checks with the legacy bridge
variables unset. It is useful for a direct stage1 run but does not replace the
full integration corpus exercised by `run_native.sh`.

`check_native_backend.py` covers direct-file, workspace build, and run paths.
Other focused checks exercise linker selection, format/assert/args support,
enum and aggregate layout, generic functions, stage2, and cache behavior.
`check_native_coverage.py` reports a broader per-fixture build/run inventory;
failures identify unsupported backend work and are not silently skipped.

`check_stage2.py` builds stage2 with the native stage1 backend, then exercises
stage2 help/version, lexer/parser/checker, and representative compiled
arithmetic, control-flow, generics, function pointers, aggregate ownership,
references, pointer outputs, and matches. It does not establish full stage2 or
stage3 parity.

The semantic regression checks compare focused generic and borrow-escape cases
against stage0. The diagnostic gate checks primary error ordering. The AGM
gates cover supported reader versions, malformed input, and cross-stage
roundtrips; stage1 artifact publication remains a frontend metadata projection.

All reported stage0 use above is bootstrap or comparison-only. Stage1 runtime
commands have no stage0 delegation path.
