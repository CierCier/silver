# Silver stage environment contract

Stage0 may build the stage1 compiler, but stage1 never launches stage0 at
runtime. `SILVER_STAGE0` and `SILVER_STAGE1_NATIVE` are not read by stage1;
native compilation is the default for supported programs, and unsupported
requests fail locally.

| Variable | Read by | Values | Meaning |
| --- | --- | --- | --- |
| `SILVER_STAGE1_CC` | `bin/agc/src/native_backend.ag` | compiler path | C compiler used by stage1 when linking through a compiler driver. |
| `SILVER_STAGE1_LLD` | `bin/agc/src/native_backend.ag` | linker path | Override for the stage1 Linux direct linker. |
| `SILVER_STAGE1_MOLD` | `bin/agc/src/native_backend.ag` | linker path | Override for mold when `SILVER_USE_MOLD=1`. |
| `SILVER_DUMP_IR` | `bin/agc/src/native_backend.ag` | presence | Dump stage1-emitted LLVM IR to stderr. |
| `SILVER_DYNAMIC_LINKER` | `bin/agc/src/native_backend.ag` | absolute path | Dynamic linker used for non-static links that include native libraries. |
| `SILVER_LINKER` | `bin/agc/src/native_backend.ag` | linker path | Stage1 Linux direct-linker override. |
| `SILVER_USE_MOLD` | `bin/agc/src/native_backend.ag` | `1` | Prefer mold before other stage1 linkers. |
| `SILVER_TEST_RUNNER` | stage0 `bootstrap/stage0/agc/src/driver.rs` | path | Stage0-only test runner hook. |
| `SILVER_AGSM` | `libs/agc/src/frontend/module_loader.ag` | path | Override for the AGSM tool used in module loading. |

`tests/selfhost/check_workspace.py` sets `SILVER_STAGE0` to a failing marker
executable while exercising stage1 check/build/run and unsupported test
commands. This guards against reintroducing a runtime launch path.

The stage1 backend is Linux-only. Unsupported language constructs and workspace
operations return stage1 diagnostics rather than falling back to stage0. Full
integration parity and non-Linux linker behavior remain separate work.
