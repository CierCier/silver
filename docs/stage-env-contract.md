# Silver stage env contract (BE-005/BE-006)

All `SILVER_*` environment variables, who reads them, and what they mean.
Authoritative 2026-10-07 (branch `bootstrap-migration`).

| Variable | Read by | Values | Meaning |
| --- | --- | --- | --- |
| `SILVER_STAGE0` | `bin/agc/src/backend_bridge.ag:17-27` | path to stage0 `agc`, or nonexistent | Explicit compatibility seam: stage1 delegates native build/run/package/cache to this binary. Point at a missing path + `SILVER_STAGE1_NATIVE=1` to force the stage1-owned backend. |
| `SILVER_STAGE1_NATIVE` | `bin/agc/src/native_backend.ag:21-28` | `1`/`true`/`yes` | Opt in to the tracked stage1-owned Linux LLVM backend for supported programs. Unsupported constructs fail closed; this does not enable complete stage0 parity. |
| `SILVER_STAGE1_CC` | `bin/agc/src/native_backend.ag` | path to `cc` | C driver fallback if no direct stage1 linker is available. |
| `SILVER_STAGE1_LLD` | `bin/agc/src/native_backend.ag` | path to `ld.lld` or `lld` | Optional direct GNU linker override used after `SILVER_USE_MOLD` selection. |
| `SILVER_STAGE1_MOLD` | `bin/agc/src/native_backend.ag` | path to `mold` | Optional mold executable override; used only when `SILVER_USE_MOLD=1`. |
| `SILVER_DUMP_IR` | `bin/agc/src/native_backend.ag:690-697` | any value (presence) | Dump the stage1-emitted LLVM IR to stderr (also used to diagnose P0-001). |
| `SILVER_DYNAMIC_LINKER` | `bin/agc/src/native_backend.ag` | absolute ld path | Forwarded only for non-static links that include `-l` flags or `#[link]` attributes. |
| `SILVER_LINKER` | `bin/agc/src/native_backend.ag`; stage0 `bootstrap/stage0/agc/src/link.rs:57-77` | path to GNU linker | Stage1 Linux direct-linker override. Stage0 reads this only in `find_lld_link()` for COFF; its Linux `link_exe_with_ld_lld()` instead chooses mold/ld.lld/lld and falls back to `cc`. |
| `SILVER_USE_MOLD` | `bin/agc/src/native_backend.ag`; stage0 `bootstrap/stage0/agc/src/link.rs:584-603` | `1` | Prefer mold if executable; otherwise stage1 falls through to ld.lld/lld, then cc. |
| `SILVER_TEST_RUNNER` | `bootstrap/stage0/agc/src/driver.rs:1009,1036` | path | Stage0-only test runner hook. |
| `SILVER_AGSM` | `libs/agc/src/frontend/module_loader.ag:180` | path | Override for the AGSM tool used in module loading. |
| `SILVER_FAKE_ARGS` | `tests/selfhost/check_workspace.py:69-198` | path | Test-only: fake backend script records argv here to assert argv forwarding. |
| `SILVER_BRIDGE_SENTINEL` | `tests/selfhost/check_bridge.py:40-47` | `present` | Test-only: proves bridge argv forwarding is exact. |

Notes:
- The bridge contract (`SILVER_STAGE0`, `SILVER_STAGE1_NATIVE`, `SILVER_STAGE1_CC`,
  `SILVER_DUMP_IR`, `SILVER_DYNAMIC_LINKER`) is implemented solely in Silver;
  stage0 `driver.rs`/`link.rs` know nothing about it (BE-006).
- `check_native_link_contract.py --stage1 <agc>` gates the BE-005 stage1 Linux linker choice, emitted link arguments/output, static-link handling, and runtime `--run-arg` forwarding; it is opt-in and is not part of `run_stage.sh`.
- The stage1-owned backend is an opt-in Linux backend with a tested supported subset, not a single-`i32 main()` path. `tests/selfhost/run_native_no_stage0.sh` exercises 65 focused native fixtures with stage0 unavailable; it does not establish full native parity. Unsupported constructs fail closed, with the explicit stage0 bridge available when configured. Stage0 routes targets through GNU/COFF/Wasm/unsupported-MinGW flavors at `bootstrap/stage0/agc/src/link.rs:396-412`; its COFF and Wasm flags, CRT discovery, and MinGW error behavior do not apply to this Linux-only stage1 linker.
- Stage0's Linux cc path also derives compiler library directories and dependency-object directories and forwards target, sysroot, debug-info, and dependency paths (`bootstrap/stage0/agc/src/link.rs:655-713`). Stage1 has no target/sysroot/dependency-graph inputs; those stage0 details are intentionally inapplicable. Stage1 forwards `-L`/`-l`, AST `#[link]` names, `NIX_LDFLAGS` `-L` dirs, static selection, rpaths, and `--run-arg`.
- Stage0 Linux does not consult `SILVER_LINKER`: `link_exe_with_ld_lld()` selects mold/ld.lld/lld and falls back to cc (`bootstrap/stage0/agc/src/link.rs:578-653`, with dispatch fallback at `:387-413`); `SILVER_LINKER` is read only by the COFF `find_lld_link()` implementation (`:57-77`). Stage1 supports the explicit direct-linker override on its Linux backend as a stage1-specific extension.
- `ROOT_OBJECT_CACHE_ENABLED=false` (`driver.rs:35`) forces root recompilation
  until implicit imports enter the dependency key — second half of BE-006,
  still open.
