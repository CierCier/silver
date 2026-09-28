# Silver stage env contract (BE-006, S2-7 companion)

All `SILVER_*` environment variables, who reads them, and what they mean.
Authoritative 2026-09-28 (branch `bootstrap-migration`).

| Variable | Read by | Values | Meaning |
| --- | --- | --- | --- |
| `SILVER_STAGE0` | `bin/agc/src/backend_bridge.ag:17-27` | path to stage0 `agc`, or nonexistent | Explicit compatibility seam: stage1 delegates native build/run/package/cache to this binary. Point at a missing path + `SILVER_STAGE1_NATIVE=1` to force the stage1-owned backend. |
| `SILVER_STAGE1_NATIVE` | `bin/agc/src/native_backend.ag:21-28` | `1`/`true`/`yes` | Opt in to the stage1-owned smoke backend (single `i32 main()` only; everything else fails closed to the bridge). |
| `SILVER_STAGE1_CC` | `bin/agc/src/native_backend.ag:34-70` | path to `cc` | C driver for the smoke link (default: `cc` from `PATH`). Pinned by `check_native_backend.py --cc`. |
| `SILVER_DUMP_IR` | `bin/agc/src/native_backend.ag:690-697` | any value (presence) | Dump the stage1-emitted LLVM IR to stderr (also used to diagnose P0-001). |
| `SILVER_DYNAMIC_LINKER` | `bin/agc/src/native_backend.ag:773-781` | absolute ld path | Forwarded as `-Wl,-dynamic-linker,<path>` on the smoke link. |
| `SILVER_LINKER` | `bootstrap/stage0/agc/src/link.rs` | path to linker | Stage0-only link driver override (not honored by stage1; see BE-005). |
| `SILVER_USE_MOLD` | `bootstrap/stage0/agc/src/link.rs` | `1` | Stage0-only: prefer `mold` (not honored by stage1; see BE-005). |
| `SILVER_TEST_RUNNER` | `bootstrap/stage0/agc/src/driver.rs:1009,1036` | path | Stage0-only test runner hook. |
| `SILVER_AGSM` | `libs/agc/src/frontend/module_loader.ag:180` | path | Override for the AGSM tool used in module loading. |
| `SILVER_FAKE_ARGS` | `tests/selfhost/check_workspace.py:69-198` | path | Test-only: fake backend script records argv here to assert argv forwarding. |
| `SILVER_BRIDGE_SENTINEL` | `tests/selfhost/check_bridge.py:40-47` | `present` | Test-only: proves bridge argv forwarding is exact. |

Notes:
- The bridge contract (`SILVER_STAGE0`, `SILVER_STAGE1_NATIVE`, `SILVER_STAGE1_CC`,
  `SILVER_DUMP_IR`, `SILVER_DYNAMIC_LINKER`) is implemented solely in Silver;
  stage0 `driver.rs`/`link.rs` know nothing about it (BE-006).
- `ROOT_OBJECT_CACHE_ENABLED=false` (`driver.rs:35`) forces root recompilation
  until implicit imports enter the dependency key — second half of BE-006,
  still open.
