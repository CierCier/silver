# WebAssembly Target — Design & Implementation Status

Target: `wasm32-wasip1` · Status: P0–P3 implemented (commit 9e9ad6e) · Author: implementation survey, 2026-09-23

This document records the WebAssembly target as implemented: every
Linux/x86_64 assumption the port had to remove, the WASI mapping chosen for
each, and what was verified by execution. The plan it supersedes called for
`wasm32-wasip1` over freestanding `unknown-unknown`, a hard error on threads,
and node as the runner — all three held.

Toolchain (verified present): LLVM 22 with the WebAssembly backend
(`wasm32`/`wasm64` registered), `wasm-ld` 22.1.8, node 24 (built-in WASI).

---

## 0. TL;DR

```bash
./target/debug/agc --target wasm32-wasip1 -o hello.wasm hello.ag --no-progress
node /tmp/silver-wasm-run.mjs hello.wasm; echo $?
# hello wasm 42 / 42
```

A Silver program that prints, allocates, formats strings, and returns an
exit code builds to a 6.8 KiB module and runs correctly under node: allocator
(`memory.grow`), `fd_write`, string formatting, and exit-code propagation
all verified against a wat control. `agc run --target wasm32-wasip1` works
through the same node shim the driver writes to the temp dir.

---

## 1. Target plumbing (compiler)

All in `bin/agc/src`, all following the Windows-port precedent of failing
loudly instead of miscompiling silently:

- **Cfg derivation** (`cfg.rs`): `triple_os` normalizes `wasi`/`wasip*`
  tokens to `os.wasi` regardless of position, so `wasm32-wasip1`,
  `wasm32-wasi`, and `wasm32-unknown-wasip1` all gate consistently.
  `arch.wasm32` derives from the triple automatically. No `cpu.*` keys on
  cross targets, so `@cfg(cpu.x)` folds `false` everywhere on wasm.
- **ABI** (`codegen/abi.rs`): new `WasmAbi` implementing the tool-conventions
  Basic C ABI — single-member aggregates pass by value as the member's own
  type (`{i64}`→`i64`), all other aggregates pass by pointer (`byval`),
  returns mirror with `sret`. The trait gained type-aware
  `struct_needs_byval`/`struct_needs_sret` because size alone cannot express
  the rule (`{i64}` vs `{i32,i32}` are both 8 bytes). The old silent
  AMD64-fallback for unknown triples never fires on wasm.
- **Inline asm** (`codegen/llvm_ir/expr.rs`): any `asm()` is a hard compile
  error on wasm triples — the x86 register model cannot be expressed.
- **`launch`** (`tasks.rs`): hard compile error — WASI preview1 is
  single-threaded, no thread runtime to reach.
- **Linker** (`link.rs`): `LinkFlavor::WasmLd` shells to `wasm-ld` (or
  `ld -flavor wasm`) with `--no-entry --export=_start --allow-undefined`;
  WASI imports resolve in the host at instantiation. Output defaults to
  `a.wasm` (`driver.rs`); object files use `.o.wasm`.
- **Imports** (`#[link_module("wasi_snapshot_preview1")]` + `#[link_name]`):
  new attribute pair emitting LLVM `wasm-import-module`/`wasm-import-name`.
  Without it, WASI calls are inexpressible — this was the first hard blocker.
- **Memory builtins** (`builtin_macros`): `@wasm_memory_size()` /
  `@wasm_memory_grow(pages)` → `llvm.wasm.memory.size/grow`; compile errors
  on native targets.
- **Debug**: DWARF off by default on wasm (the `dwarf_bt` post-pass parses
  ELF objects, which wasm objects are not); the host unwinds via the module
  `name` section. Explicit `-g` still forces it on.

## 2. Runtime seam (`std/sys/os_wasi.ag`, 771 lines)

Unconditional file, imported by the `os.ag` dispatcher only on `os.wasi`.
Two facts shape everything in it: WASI returns `errno` (0 = success), never
`-errno`, so every wrapper negates into the seam's `-1..-4095` encoding; and
there is no kernel address space, so pages come from `memory.grow` over a
grow-only bump region rooted at `__heap_base` (`munmap` is a best-effort
no-op — the page allocator recycles small blocks itself, same model as the
Windows path).

| Seam function | WASI mapping |
|---|---|
| `sys_write/read/close` | `fd_write` / `fd_read` (single iovec) / `fd_close` |
| `sys_open/openat` | `path_open` through a preopened dir fd found by scanning fd 3.. with `fd_prestat_get` |
| `sys_lseek/fcntl` | `fd_seek`; nonblock flag set accepted |
| `sys_stat` | `path_filestat_get` synthesized into the Linux `struct stat` layout (size@48, mtim@88/96, mode@24) — same contract the Windows seam honors |
| `sys_mkdir/rmdir/unlink/rename` | `path_create_directory` / `path_remove_directory` / `path_unlink_file` / `path_rename` |
| `sys_list_dir` | `fd_readdir` loop into the shared NUL-separated buffer layout |
| `sys_access` | `path_filestat_get` probe (F_OK semantics) |
| `sys_clock_gettime` | `clock_time_get` (monotonic/realtime) |
| `sys_getrandom` | `random_get` |
| `sys_nanosleep` | `poll_oneoff` clock subscription |
| `sys_exit/exit_group` | `proc_exit` |
| `sys_getpid/kill` | stubs (1 / ESRCH-style); `thread_*`, `sys_futex` | single-threaded no-ops |
| `sys_console_size/is_tty/raw_enable/raw_restore` | console absent under WASI stdio redirect: size/tty/raw report "not a console" errors |
| `sys_wait_readable` | `poll_oneoff` fd subscription (0 = timeout, 1 = ready) |

Entry (`std/sys/entry.ag`): the `_start` blob is x86 asm, so wasm gets a
portable `_start` built from `args_sizes_get`/`args_get` (+ `environ_*`) and
`proc_exit`, reaching `main` through the new `@call_main()` builtin, which
normalizes any declared return type (`void`→0, `i64`→trunc) to the i32 exit
code. `__silver_cpu_init` is an empty stub on wasm (no cpuid; probe globals
stay zero-initialized, which is exactly what portable fallbacks want).
Backtrace (`std/rt/backtrace.ag`): rbp reader is Linux-only; wasm gets
plain-load/store lock helpers plus a stub printer (the host unwinds via the
names section), and the leak-check origin walker skips the rbp chain.

`std/env.ag` settles the one open design question from the plan: WASI
preview1 exposes no `getcwd`, the host preopens a directory as `.`, and
relative paths resolve inside it — so `current_dir()` reports `"."` on wasm
via an in-function `@cfg(os.wasi)`.

## 3. Stdlib surface

`std/fs.ag`, `term/size.ag`, and `term/raw.ag` needed no wasm changes: the
earlier seam refactor already routes them through `sys_list_dir`,
`sys_console_size`, and `sys_raw_enable`, all of which now exist on wasi.
Linux-only modules (`net`, `process`, `epoll`, `pty`, raw syscalls) are
empty on wasm by construction — consumers, not the modules, carry any
gating. `std/thread.ag` inherits the `launch` hard error.

## 4. Test harness (`tests/run_tests.py`)

`WASM_SKIP` covers threads/launch, sockets/process/pty/kernel interfaces,
raw-asm tests, SIMD probes, and tests asserting native object properties
(DWARF sections, ELF linkage, `.agm` workflow) — everything else is expected
to work. Wasm builds force `--no-cache` (same artifact-divergence hazard as
the COFF path). The node shim lives at `<tmpdir>/silver-wasm-run.mjs`,
shared by `agc run` and the harness: `wasi.start(instance)` with preopened
cwd, exit via `exit(wasi.start(instance) ?? 0)`.

**Harness gotcha (verified, do not "fix"):** exit-code propagation was
confirmed end-to-end against a wat control (`proc_exit(42)` → shell sees
42, repeatedly, plus Silver's own 42). During verification, hand-rolled node
one-liners with bare `wasi.start()` and empty args/env intermittently
reported exit 0 for the identical module — a Node WASI edge in ad-hoc
scripts, not in the shipped shim. If exit codes ever look stuck at 0, suspect
the invocation wrapper first (args/env/preopens/returnOnExit), not the
module: re-check against the wat control before touching the compiler.

## 5. Open items

1. **CI leg** — added (`.github/workflows/ci.yml`, `wasm` job: LLVM 22 +
   node 24 + `run_tests.py --release --target wasm32-wasip1`), not yet
   observed on a remote runner.
2. **Full wasm-leg suite run** — done: **143 passed, 0 failed, 57 skipped**
   (see §6 for what the failures taught). Re-run after any seam change.
3. **Non-goals holding**: WASI-threads, sockets, DWARF, `mprotect`
   semantics, atelier-on-wasm (explicitly out of scope).
4. **Scratch leftovers** — `.agents/`, `amt`, `silver_dbg.txt`,
   `ball.ag`, `plan.md` are untracked and unrelated to the port.
   (`tests/adversarial_mem_intrinsics_test.ag` shipped with the port and
   passes both legs.)

## 6. Verification log (2026-09-23)

- `agc --target wasm32-wasip1` hello (return 42): builds, prints via
  `@println`/`fd_write`, exit 42 under node — all green.
- `cargo test -p agc --lib`: 553/553. Full native integration:
  199/199, 0 failed. Full wasm leg: 143/0/57.
- `check` clean on `wasm32-wasip1` for std files touched by the port and
  for `read_dir`-calling programs (the seam carries them).
- Pre-existing failure that motivated the port's shape: before `os.wasi`
  existed, any wasm build died at `unknown identifier
  'silver_print_backtrace'` (`os.unknown` pruned both backtrace branches).

### What the first failing leg taught (all fixed, all with regression cover)

- **Latent call-arg bug, exposed by wasm validation**: lazily-instantiated
  generic methods never registered their signature, so call sites skipped
  argument conversion (and drop-flag clearing, user casts, ABI coercion).
  Native llc tolerates `i64` constants for `i32` params; wasm rejects them.
  Fixed in `codegen/llvm_ir/symbols.rs` (register on lazy instantiate) +
  `call.rs` (resolve the signature against the materialized name, which is
  computed after selection runs). Native suite still 199/199 — the fix only
  makes previously-lucky code exact.
- **`f80`/`c80` gate** (`semantic/typeck.rs`, `types/mod.rs`): recursive
  predicate + hook at params/returns/fields/globals/lets/casts; unit-tested
  both directions (wasm errors, native clean).
- **WASI rights must be per-kind**: a blanket ALL mask makes directory
  opens fail on real hosts; files get the file-rights subset, directories
  get `PATH_OPEN|FD_READDIR|PATH_FILESTAT_GET|FD_FILESTAT_GET`.
- **WASI errno table** (`wasi_err`): only host-observed codes are mapped
  (44→2, 8→9, 20→17, 54→20, 31→21, 37→36, 55→39, 76→13, 28→9 with the
  node/wasmtime evidence recorded); everything else passes through.
- **mmap/munmap/mremap validation**: bump base is now page-aligned;
  null/unaligned munmap/mremap inputs return EINVAL like the kernel;
  `kill` returns ESRCH except for self.
- **Non-bugs that looked like bugs**: WASI whence IS Linux-ordered
  (a mistranslation was written, then reverted after the
  whence=2/off=5→29 matrix proved END semantics); `read_dir(".")` never
  worked because directory opens needed the rights fix, not a path fix;
  `fs`/`io` tests now use relative paths (hermetic on native too).
