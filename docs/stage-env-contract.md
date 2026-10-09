# Silver stage environment contract

Stage1 uses its own native backend at runtime. Supported programs compile
natively, and unsupported requests fail locally.

| Variable | Read by | Values | Meaning |
| --- | --- | --- | --- |
| `SILVER_STAGE1_CC` | `bin/agc/src/native_backend.ag` | compiler path | C compiler used by stage1 when linking through a compiler driver. |
| `SILVER_STAGE1_LLD` | `bin/agc/src/native_backend.ag` | linker path | Override for the stage1 Linux direct linker. |
| `SILVER_STAGE1_MOLD` | `bin/agc/src/native_backend.ag` | linker path | Override for mold when `SILVER_USE_MOLD=1`. |
| `SILVER_DUMP_IR` | `bin/agc/src/native_backend.ag` | presence | Dump stage1-emitted LLVM IR to stderr. |
| `SILVER_DYNAMIC_LINKER` | `bin/agc/src/native_backend.ag` | absolute path | Dynamic linker used for non-static links that include native libraries. |
| `SILVER_LINKER` | `bin/agc/src/native_backend.ag` | linker path | Stage1 Linux direct-linker override. |
| `SILVER_USE_MOLD` | `bin/agc/src/native_backend.ag` | `1` | Prefer mold before other stage1 linkers. |

The stage1 backend is Linux-only. Unsupported language constructs and workspace
operations return stage1 diagnostics rather than falling back to stage0. Full
integration parity and non-Linux linker behavior remain separate work.
