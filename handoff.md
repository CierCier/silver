# Silver Self-Host Migration Handoff
>
> Status (2026-09-28): the committed build path is frontend-only
> (`lex`/`parse`/`ast`/`check` local; native via the explicit `SILVER_STAGE0`
> bridge; `SILVER_STAGE1_NATIVE=1` is an `i32 main()`-only smoke backend —
> see `bin/agc/src/native_backend.ag:1-6`, `tests/selfhost/check_native_backend.py`,
> `tests/selfhost/README.md:22-29`, `silver.toml`). Sections 1–4 below describe
> EXPERIMENTAL UNCOMMITTED work (untracked `libs/agc/src/backend/llvm/`,
> `vendor/llvm/llvm.ag`, modified `bin/agc/src/native_backend.ag`): a full-tree
> stage2 attempt that links a ~300KB ELF but segfaults on startup. Do not read
> it as the committed stage1 capability.

## 1. Executive Summary & Objective

- **Goal**: Complete the self-hosting migration of the Silver programming language compiler (`agc`) so that it compiles itself natively without any dependency on the Rust stage0 bootstrap (`bootstrap/stage0/`).
- **Success Criteria**:
  1. `agc-stage1` natively compiles `silver.toml --bin agc` into `agc-stage2` with `SILVER_STAGE1_NATIVE=1` and `SILVER_STAGE0=/tmp/missing` (zero Rust stage0 fallback).
  2. `agc-stage2` natively compiles `silver.toml --bin agc` into `agc-stage3`.
  3. Fixpoint/parity is verified between `agc-stage2` and `agc-stage3`, and the Rust bootstrap dependency is completely eliminated.
- **Current Milestone** (experimental, uncommitted — see banner above):
  - `agc-stage1` now natively parses, typechecks, lowers, and generates native LLVM IR for the complete Silver self-host codebase (1,554 top-level symbols — a runtime `symbols.len()` count from `bin/agc/src/driver.ag:380-382`, not a file count; cf. 348-file corpus in `docs/selfhost-completion-record.md` — across `libs/agc`, `bin/agc`, `std`, and `vendor/llvm`).
  - `agc-stage1` successfully linked and generated a 300KB native x86_64 ELF binary at `/tmp/silver-selfhost/agc-stage2`.
  - When running `/tmp/silver-selfhost/agc-stage2 --help`, the binary segfaults during CLI argument handling. The exact root causes have been traced, proven with GDB and assembly inspection, and documented below.

---

## 2. Toolchain State

| Component | Status | Location / Artifact | Notes |
| :--- | :--- | :--- | :--- |
| **Rust Stage0** | Operational | `target/debug/agc` | Used solely to build `agc-stage1` during migration bootstrap. |
| **Silver Stage1** | Operational | `/tmp/silver-selfhost/agc-stage1` | Built from `bin/agc` + `libs/agc`. Compiles `silver.toml` natively under `SILVER_STAGE1_NATIVE=1`. |
| **Silver Stage2** | Generated (debugging) | `/tmp/silver-selfhost/agc-stage2` | 300KB ELF 64-bit executable. Segfaults on startup due to missing argc/argv globals and generic method resolution. |
| **Silver Stage3** | Pending Stage2 fix | `/tmp/silver-selfhost/agc-stage3` | Will establish fixpoint once Stage2 runs. |

---

## 3. Root Cause Analysis: The Stage2 Crash

Running `/tmp/silver-selfhost/agc-stage2 --help` resulted in:
```
Program received signal SIGSEGV, Segmentation fault.
0x000000000041550b in silver_rt_memcpy ()
#0  0x000000000041550b in silver_rt_memcpy ()
#1  0x000000000040a51d in String__push_bytes ()
#2  0x000000000040c25f in String__from_file ()
#3  0x0000000000402f04 in emit_tokens ()
#4  0x0000000000403e76 in run_frontend ()
#5  0x00000000004023a6 in main ()
```

Investigation via GDB and disassembly revealed three distinct issues:

### Issue A: `silver_argc` and `silver_argv` Return 0 / NULL
- **Mechanism**:
  - In `std/sys/entry.ag`, `silver_argc()` returns `__silver_argc` and `silver_argv()` returns `__silver_argv`.
  - In `libs/agc/src/backend/llvm/generator.ag`, the auto-generated `_start` entry point defines `__silver_argc` and `__silver_argv` inside an inline assembly string (`.section .bss ...`).
  - However, `__silver_argc` and `__silver_argv` were **not added as LLVM module globals** via `LLVMAddGlobal`.
  - When `silver_argc()` was compiled, `llvm_emit_expr` looked for `__silver_argc` in locals (not found), then in LLVM functions (not found), and then via `LLVMGetNamedGlobal(ctx.module, "__silver_argc")` (returned NULL).
  - Consequently, `silver_argc()` was compiled into:
    ```assembly
    0x00000000004023b0 <+0>: xor %eax, %eax
    0x00000000004023b2 <+2>: ret
    ```
  - Because `silver_argc()` returned 0, `std/args.ag`'s `args()` returned an empty vector (`arguments.len() == 0`).

### Issue B: Generic Method Mangling Mismatch (`Vec<T>` vs `Vec<String>`)
- **Mechanism**:
  - In `driver.ag:514`: `String* command = arguments.get_ptr(1);`.
  - `arguments` has type `Vec<String>`.
  - In `libs/agc/src/backend/llvm/expr.ag:1051`, `struct_name = ctx.types.type_name(rx_type)`. For `Vec<String>`, `type_name` produces `"Vec<String>"`.
  - Method mangling constructs `"Vec<String>__get_ptr"`.
  - But any methods declared on `impl<T> Vec<T>` or `impl Vec<T>` in `generator.ag` are either mangled as `"Vec__get_ptr"` or skipped because generic types (`T*`) fail concrete type lowering in Pass 1.
  - As a result, `LLVMGetNamedFunction("Vec<String>__get_ptr")` returned NULL.
  - The call to `arguments.get_ptr(1)` evaluated to NULL, leaving the alloca for `command` uninitialized on the stack.

### Issue C: Uninitialized Stack Pointer Fallthrough
- **Mechanism**:
  - In `run_frontend()`:
    1. `arguments` was empty (due to Issue A).
    2. `command = arguments.get_ptr(1)` failed to emit a store (due to Issue B), leaving a garbage pointer in the stack slot.
    3. `is_frontend_command(command)` was called with that garbage pointer, reading random memory.
    4. By chance or corruption, execution fell through to `emit_tokens(input.to_str())`, where `input` was also uninitialized garbage.
    5. `String.from_file(input)` attempted `memcpy` on an invalid pointer, triggering SIGSEGV in `silver_rt_memcpy`.

---

## 4. Fixes Completed in this Session

1. **C-Style Cast Lowering & Codegen**:
   - `AstPool.add_cast(expr_id, target_type_id, span)` in `libs/agc/src/frontend/ast_expr.ag`.
   - `cannot_be_cast_operand` and `try_parse_cast_type` in `libs/agc/src/frontend/lower_expr.ag` recognizing `(str)0`, `(u8*)0`, `(const char*)ptr`, `(void*)ptr`, `(SyntaxNode*)0`.
   - `ExprKind.Cast` in `infer_expr` (`libs/agc/src/frontend/typecheck_expr.ag`).
   - `type_id_from_text` updated in `libs/agc/src/frontend/items.ag` to strip `const` qualifiers before/after `&`.
   - Bitcasts, `ptrtoint`, `inttoptr`, `intcast`, `fpext`, `fptrunc` using `LLVMGetTypeKind` in `libs/agc/src/backend/llvm/expr.ag`.
2. **Recursive Nested `if` / `else if` / `else` Lowering**:
   - Replaced flat parsing in `lower_block` with recursive `lower_if_stmt` in `libs/agc/src/frontend/lower_expr.ag`.
3. **Method Receiver Parameter Typing**:
   - `collect_parameter_bindings` in `libs/agc/src/frontend/bindings.ag` defaults `self`'s type to `"Self"`.
   - `llvm_param_types` and method parameter alloca in `libs/agc/src/backend/llvm/generator.ag` resolve `"Self"` (or `self` with empty name) to the implementing `struct_name`.
4. **Extern C Function Return Types**:
   - `generator.ag` now scans all tokens between return start and function name, correctly preserving pointer stars (`LLVMOpaqueValue*`, etc.) for LLVM C API declarations.
5. **String Struct Comparisons (`String == str`)**:
   - In `libs/agc/src/backend/llvm/expr.ag`, comparing `String` to `str` extracts the underlying `u8* data` pointer via `LLVMBuildExtractValue(..., 0)` before calling `strcmp` / `icmp`.
   - Declared `LLVMBuildExtractValue` in `vendor/llvm/llvm.ag`.
6. **Automatic Linux `_start` Entry Point**:
   - In `libs/agc/src/backend/llvm/generator.ag`, `llvm_emit_entry_point` generates the entry point, argc/argv extraction, stack alignment, call to `main`, and exit syscall (231) when `_start` is not already defined in the source.

---

## 5. Active Changes & Inventory

### Modified Files:
- `libs/agc/src/agc.ag` — Exposes native backend modules.
- `libs/agc/src/frontend/ast_expr.ag` — Added `Cast` AST variant and pool helper.
- `libs/agc/src/frontend/bindings.ag` — Default `Self` type for receiver parameters.
- `libs/agc/src/frontend/expressions.ag` — Cast expression type inference.
- `libs/agc/src/frontend/items.ag` — Type parser handles `const` and pointer projections.
- `libs/agc/src/frontend/lower_expr.ag` — Cast parsing and recursive `if-else-if` lowering.
- `libs/agc/src/frontend/module_loader.ag` — Import path handling.
- `libs/agc/src/frontend/symbols.ag` — Symbol indexing support.
- `libs/agc/src/frontend/typecheck_expr.ag` — Cast expression checking.
- `bin/agc/src/driver.ag` — Driver integration for native compilation.
- `bin/agc/src/native_backend.ag` — Backend orchestrator using native LLVM generator.
- `silver.toml` — Workspace configuration.
- `tests/selfhost/run_stage.sh` — Test driver for stage bootstrap validation.

### Untracked Files:
- `libs/agc/src/backend/llvm/` — The pure Silver LLVM IR code generator (`generator.ag`, `expr.ag`, `stmt.ag`, `types.ag`, `context.ag`; ABI helpers live in `types.ag`, there is no `abi.ag`).
- `vendor/llvm/llvm.ag` — Direct Silver bindings to the LLVM-C API.
- `bin/agsm/` — Self-hosted Silver Source Map tool.

---

## 6. How to Reproduce & Verify

All commands run from repository root (`/home/cier/Projects/silver`):

### Step 1: Build Stage 1 (using Stage 0 Rust compiler)
```bash
cargo build -p agc
timeout 60s cargo run -p agc -- build silver.toml --bin agc -o /tmp/silver-selfhost/agc-stage1
```

### Step 2: Build Stage 2 (using Stage 1 Native Silver compiler)
```bash
timeout 60s env SILVER_STAGE1_NATIVE=1 SILVER_STAGE0=/tmp/missing \
  /tmp/silver-selfhost/agc-stage1 build silver.toml --bin agc -o /tmp/silver-selfhost/agc-stage2
```
*Expected result*: Prints `check ok: 1554 top-level symbols` and creates `/tmp/silver-selfhost/agc-stage2`.

### Step 3: Run Stage 2 with GDB to Inspect Startup State
```bash
gdb -batch \
  -ex "break main" \
  -ex "run" \
  -ex "print (int)silver_argc()" \
  -ex "print (char**)silver_argv()" \
  --args /tmp/silver-selfhost/agc-stage2 --help
```

---

## 7. Immediate Next Steps for Next Session

1. **Fix `__silver_argc` / `__silver_argv` Globals in `generator.ag`**:
   - In `libs/agc/src/backend/llvm/generator.ag`, add `__silver_argc`, `__silver_argv`, and `__silver_envp` to the LLVM module via `LLVMAddGlobal` with external linkage (or common linkage).
   - This ensures `silver_argc()` in `std/sys/entry.ag` successfully loads `__silver_argc`, allowing `args()` to populate CLI arguments.
2. **Fix Method Resolution for Generic Structs (`Vec<T>` / `Vec<String>`)**:
   - In `libs/agc/src/backend/llvm/expr.ag` (`ExprKind.MethodCall`), strip generic type parameters when searching for struct methods: e.g., if `struct_name` is `"Vec<String>"`, the base struct name is `"Vec"`.
   - Ensure the mangled name resolves to `"Vec__get_ptr"` (or match the monomorphized name).
   - In `generator.ag`, ensure `impl<T>` method definitions are either monomorphized or lowered with pointer-sized element slots (`u8*` / opaque pointer).
3. **Verify `agc-stage2 --help`**:
   - Run `/tmp/silver-selfhost/agc-stage2 --help` and verify it exits cleanly with code 0 and displays the usage message.
4. **Compile `agc-stage3`**:
   - Run:
     ```bash
     timeout 60s env SILVER_STAGE1_NATIVE=1 SILVER_STAGE0=/tmp/missing \
       /tmp/silver-selfhost/agc-stage2 build silver.toml --bin agc -o /tmp/silver-selfhost/agc-stage3
     ```
   - Verify parity and execute `tests/selfhost/run_stage.sh`.
