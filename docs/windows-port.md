# Windows Port — Design & Gap Analysis

Branch: `feat/windows` · Status: P1a + P1b landed (see §7) · Author: porting survey, 2026-09-11

This document catalogs every Linux-only assumption in Silver and proposes the Windows
equivalent for each, phased so the compiler builds first, then compiles correct COFF,
then runs real Silver programs against a Win32-backed runtime.

---

## 0. TL;DR

Silver is currently ** ELF-only in three independent places**, any one of which breaks Windows:

1. **The link driver** (`bin/agc/src/link.rs`) is a GNU/ELF machine: `cc`, `ld.lld -flavor gnu`,
   PT_INTERP, `-rpath`, `.so`, and a `-nostdlib` "no CRT" design that relies on Linux's
   custom-`_start` ET_EXEC model.
2. **The ABI layer** (`bin/agc/src/codegen/abi.rs`) implements System V AMD64 classification
   only, and its dispatcher (`abi.rs:316-333`) selects on *architecture*, ignoring the OS in the
   triple — an `x86_64-pc-windows-msvc` triple today compiles **silently wrong** calls.
3. **The stdlib runtime** (`std/sys/**`) is a freestanding, libc-less Linux runtime: every OS
   service is a raw `syscall` instruction via inline asm, `_start` reads argc/argv off the Linux
   process stack, threads are raw `clone(2)`, futexes, mmap. None of that exists on Windows.

Plus a tail of hard-coded artifacts: ELF/DWARF object post-pass, `.so`/`a.out`/`.o` naming,
XDG/HOME paths, `prefer-dynamic` LLVM linkage, a Linux-expecting test harness, and a
Linux-only CI leg.

The machine this was written on now has: VS 2022 Community (C++ workload) + Windows SDK
10.0.26100, Rust 1.98 `x86_64-pc-windows-msvc`, and LLVM 22.1.8 (winget, `C:\Program Files\LLVM`).
The only toolchain gap is `llvm-config.exe` — see §2.3.

**Phasing**: P0 toolchain → P1 compiler core (ABI + codegen + link driver, MSVC CRT entry)
→ P2 inline-asm policy → P3 `std.sys.win` runtime → P4 debug/backtrace → P5 harness/CI.
P1+P3 get "hello world" running; everything else follows.

---

## 1. Target architecture

```
                   ┌────────────────────────────────────────────────┐
                   │                 TargetConfig                   │
                   │  from triple: {linux-gnu, windows-msvc, ...}   │
                   ├────────────────────────────────────────────────┤
   codegen ───────►│  AbiHandler   LinkFlavor   ObjectFormat        │
                   │  SysV | Win64  GnuLd|LldLink  Elf | Coff       │
                   └───────┬──────────────────┬─────────────────────┘
                           │                  │
                 std/sys/linux.ag            std/sys/win.ag   (new)
                 _start + syscall asm        mainCRTStartup + Win32 externs
```

One `TargetConfig` derived from the triple, consumed by every OS-sensitive pass. Today the
"target config" is implicit and Linux-shaped; the port's central refactor is making it explicit.
`should_force_non_pie` (`link.rs:145-150`) is the only triple-OS check in the tree today.

Triple default: `entry.rs:634` falls back to `x86_64-unknown-linux-gnu`; on Windows hosts the
default must become `TargetMachine::get_default_triple()` (already portable — `driver.rs:832-845`
uses it for artifact compatibility checks).

### Non-negotiable semantic shifts (Linux assumptions that don't survive)

| Linux assumption | Windows reality |
|---|---|
| Non-PIE codegen, fixed load addresses | Everything is position-independent; ASLR always on. `RelocMode::Default` + COFF handles this; drop the non-PIE forcing for windows triples. |
| No CRT (`-nostdlib`, custom `_start`) | Win64 requires valid unwind info from frame 0 and has no user-mode `syscall` contract. Recommended: link the UCRT + `mainCRTStartup` (§3.2). |
| `long` = 8 bytes, `long double` = 80-bit | Win64 `long` = 4 bytes; no 80-bit float. Any C-layout interop must be re-derived. |
| rbp = frame pointer only by our convention | Same convention works (we emit `frame-pointer=all`, `stmt.rs:39`), but Win64 additionally **requires** `.pdata/.xdata` unwind info for every non-leaf function — LLVM emits it automatically for windows triples. |
| fds are ints; 0/1/2 always open | Win32 uses `HANDLE`s; UCRT exposes CRT fds if we go that route. See §4.4. |
| `rbx, rdi, rsi, r12-r15, xmm6-15` are caller-saved | **Callee-saved (nonvolatile) on Win64.** Every inline-asm blob that scratches them without declaring a clobber is broken (§3). |
| `-rpath` for shared modules | No rpath; DLLs are found next to the exe or on `PATH`. |

---

## 2. P0 — Toolchain & build environment

### 2.1 What a Windows dev box needs

| Component | Why | Install |
|---|---|---|
| VS 2022 Build Tools (C++ workload) | Rust MSVC linker (`link.exe`), `MSVC\Tools\MSVC\<ver>\lib\x64` CRT libs (`libcmt.lib`, `vcruntime.lib`) | `winget install Microsoft.VisualStudio.2022.BuildTools` (or Community) |
| Windows SDK 10+ | `kernel32.lib`, `ucrt.lib`, `uuid.lib` — the link driver's only native deps | Included with the above |
| LLVM 22.x (official installer) | `llvm-sys 221` / `inkwell llvm22-1` build, `clang`, `lld-link`, `llvm-lib`, `llvm-readobj` | `winget install LLVM.LLVM` |
| Rust `x86_64-pc-windows-msvc` | Building the compiler itself | `rustup` default |

`scripts/setup-windows.ps1` (added on this branch) verifies/installs all of the above, sets
`LLVM_SYS_221_PREFIX=C:\Program Files\LLVM`, prepends `C:\Program Files\LLVM\bin` to the user
`PATH` (LLVM-C.dll must be resolvable at agc runtime), and smoke-tests the set
(`clang --version`, `lld-link --version`, `llvm-config` shim status, `cargo check -p agc`).

### 2.2 `prefer-dynamic` on Windows

`bin/agc/Cargo.toml:9` sets `llvm-sys` feature `prefer-dynamic`. The official Windows
installer ships **no static LLVM archives** and **no `LLVM.lib`** — only `LLVM-C.lib`
(import lib for `LLVM-C.dll`). Consequences:

- Static linking of LLVM on Windows is impossible with the official package; the practical
  model is dynamic (`LLVM-C.lib` + `LLVM-C.dll` on PATH) — i.e. `prefer-dynamic` is actually
  the *correct* mode on Windows, opposite of the Linux instinct.
- If `llvm-sys 221`'s Windows probing can't be satisfied by the shared-only install, gate the
  feature per-target in `Cargo.toml` (`[target.'cfg(unix)'.dependencies]` table) rather than
  globally. **Verify at P0**: `cargo build -p agc` with `LLVM_SYS_221_PREFIX` set.

### 2.3 The `llvm-config.exe` gap

The official Windows installer does **not** ship `llvm-config.exe` (confirmed: LLVM 22.1.8
install lacks it), and `llvm-sys` probes with it. Options, in order of preference:

1. **Shim** (recommended): a ~100-line Rust helper installed as `llvm-config.exe` on PATH that
   maps the queries `llvm-sys` makes (`--version`, `--prefix`, `--libdir`, `--libs`, `--ldflags`,
   `--system-libs`, `--link-mode`) onto the official install layout (`LLVM-C.lib`, `LLVM-C.dll`).
   Precedent exists in several inkwell-on-Windows setups. Live in `scripts/llvm-config-shim/`,
   built by `setup-windows.ps1` with the system toolchain.
2. **llvm-sys native Windows path**: if llvm-sys 221's build script honors
   `LLVM_SYS_221_PREFIX` without `llvm-config` on MSVC, prefer that (zero extra tooling).
   Verify first — cheapest outcome.
3. Self-built LLVM (`LLVM_BUILD_LLVM_DYLIB` is unsupported on Windows targets; static build
   with `LLVMTarget*` archives works but is a multi-hour dependency we don't want).

---

## 3. P1 — Compiler core

### 3.1 Win64 ABI (`codegen/abi.rs`)

Implement `Win64Abi` alongside `Amd64Abi` and dispatch on the triple's **OS** component
(fix point: `abi.rs:316-333`, currently arch-only — doc at `abi.rs:313-315` already lists
`Win64Abi` as future work):

- Integer args: `rcx, rdx, r8, r9`; float args: `xmm0-3`, **by ordinal position** (no SysV
  INTEGER/FLOAT eightbyte classification; a `double` in arg 2 occupies `xmm1` even if arg 1
  is a float too).
- Aggregates pass by value **only at exactly 1, 2, 4, or 8 bytes** — always in the integer
  class, including single-`float`/`double` member aggregates (XMM is reserved for scalar
  float/double arguments; clang/MSVC interop requires aggregates to stay in the integer
  class). All other sizes (3, 5, 6, 7, and everything > 8) pass **byval-by-pointer**
  (caller makes a temporary copy). No 9-16-byte two-eightbyte case exists.
- Returns: 1/2/4/8-byte aggregates in `rax`; everything else via a hidden `sret`
  pointer (caller-allocated, returned in `rax`).
- 32-byte shadow space at every call site; stack always 16-byte aligned before `call`.
- Varargs: caller-cleanup; float args passed in *both* `xmmN` and the integer slot.

Known follow-up (shared with SysV, not Win64-specific): **call-site sret materialization**.
`lower_function_type` shapes extern-C declarations with a hidden return pointer, but
`call.rs` does not yet allocate the destination temporary and pass it — a call to an
extern-C function returning a large struct fails loudly (LLVM argument-count mismatch)
rather than miscompiling. Needed before P3 relies on C interop with large struct returns.

Data layout differences (i128 align 8 vs 16, etc.) flow from the triple automatically via
`create_target_machine` (`codegen/llvm_ir/entry.rs:643-669`); no manual work beyond not
overriding it.

### 3.2 Entry & CRT — the big design decision

Linux today: `std/sys/entry.ag:40-66` defines `_start` as one inline-asm blob reading
argc/argv off the process stack and exiting via `exit_group` — only possible because Linux
runs a CRT-less static ET_EXEC. On Windows we recommend **linking the UCRT and using the
standard `mainCRTStartup` entry**:

- `link.exe`/`lld-link` pull `mainCRTStartup` from the MSVC CRT libs
  (`libcmt.lib`/`msvcrt.lib` + `vcruntime.lib` in `VC\Tools\MSVC\<ver>\lib\x64`), which
  initializes the CRT heap, stdio fds, and environ — and is the de-facto Win64 ABI anchor
  that every native library (OpenSSL, Rust FFI dylib) already assumes.
- Silver keeps a *reduced* `_start` equivalent: a Silver-defined `main` shim
  (`std/sys/entry_win.ag`) that fills `__silver_argc`/`__silver_argv` from
  `GetCommandLineW` + `CommandLineToArgvW` (UTF-16 → UTF-8), runs `__silver_cpu_init`,
  calls the user `main`, then runs the existing shutdown sequence
  (`__silver_thread_registry_join_all`, `__silver_flush_all`, `__silver_leak_check_report`)
  and returns into the CRT (or `ExitProcess(code)` to match the current exit-path semantics).
- The CRT-free `/ENTRY:` model (custom entry, no CRT, own `memcpy/memset` to satisfy
  compiler-rt calls) is *achievable* — Silver already defines `memcpy/memset/memmove/strlen`
  in `std/mem/memory.ag:707-721` — but fragile (every CRT-touching library pulls `ucrt` in
  anyway) and deferred as a philosophy goal, not the port path.

Consequence for `link.rs`: the `-nostdlib` fallback (`link.rs:293-307`) and
`DYNAMIC_LINKER`/PT_INTERP logic (`link.rs:104-117, 232, 305`) become `GnuLd`-flavor-only.
The Windows flavor adds the CRT + SDK libs and *drops* `--dynamic-linker`.

### 3.3 Link driver (`bin/agc/src/link.rs`)

Restructure around a `LinkFlavor { GnuLd, LldLink, MsvcLink }` (MinGW later, not required):

| Concern | Linux today | Windows flavor |
|---|---|---|
| Tool discovery | `cc` (`link.rs:90-102`), `ld.lld`/`mold`/`lld -flavor gnu` (`195-266`) | `lld-link` (ships with LLVM; no VS dependency for the *driver*, but VS libs needed for CRT) or VS `link.exe` located via `vswhere.exe`; honor `SILVER_LINKER` / `CC` env overrides |
| Library flags | `-l<name>` (`34-40`), `.so` detection (`156-179`) | `<name>.lib`; native deps limited to what std declares (`#[link(name)]`) |
| Search dirs | `cc -print-search-dirs` (`44-59`), `LIBRARY_PATH`, `NIX_LDFLAGS` (`61-86`) | Windows SDK + MSVC lib dirs discovered via `vswhere` (or `lld-link /winsysroot:`), then `%LIB%` (`;`-separated) |
| Shared modules | `-shared` → `.so` (`322-351`); `driver.rs:827-829` names artifacts | `/DLL` → `.dll` + import `.lib`; consumers link the import lib; DLL found next to exe (no rpath — `254-258, 310-314, 345` become Gnu-only) |
| PIE | forced non-PIE on linux (`145-150, 284-286`) | n/a (COFF is position-independent); guard the flag to Gnu flavors |
| `-Wl` passthrough | multiple sites | `/FOO` passthrough; no `-Wl,` on MSVC |
| CRT | deliberately omitted (`293-307`) | `libcmt.lib` (static) or `msvcrt.lib` + `vcruntime.lib`, `kernel32.lib`, `ucrt.lib`, `/SUBSYSTEM:CONSOLE` |

### 3.4 Driver/output plumbing (`bin/agc/src/driver.rs`)

- `default_output_for` (`471-491`): `a.out` → `a.exe` on windows triples; `.o` → `.obj`; `.s` → `.asm`.
- Run-mode temp binary (`665-670`, `2182-2186`): append `.exe` and delete the actual produced file
  (`CreateProcess` will otherwise fail on `a.out`-style names; `remove_file` misses the `+ .exe`).
- `module_binary_output_path` (`827-829`): `.so` → `.dll`.
- Object re-emission pass (`entry.rs:747-786`) and `module.tmp.o` (`1805`) follow the extension map.

---

## 4. P2/P3 — Runtime: inline asm & `std.sys.win`

### 4.1 Inline-asm policy (`codegen/llvm_ir/expr.rs:1787-1881`)

- Today `asm("...")` hardcodes the Linux syscall constraint set
  (`={rax},{rdi},{rsi},{rdx},{r10},{r8},{r9}`, clobbers `~{rcx},~{r11}`) — an error message
  already says "x86_64 syscall ABI".
- Windows: Win64 constraint set (`{rcx},{rdx},{r8},{r9}` for the first four, values returned in
  `rax`), plus **nonvolatile-aware clobber modeling**: `rbx, rdi, rsi, rbp, r12-r15, xmm6-15`
  are callee-saved on Win64 — if a blob writes them they must be listed, or codegen must add
  implicit clobbers. SysV blobs that scratch `rdi/rsi/rdx` relying on caller-saved-ness are the
  audit target (e.g. `std/map.ag:38-78` probe asm writes `%rdx`; check every site listed in §4.5).
- **Language-level rule**: raw `syscall` is a Linux/userland-contract instruction (Win64 has no
  stable user `syscall`; WoW64 breaks it). On Windows triples, `asm()` still emits the blob but
  `std` must not use `syscall` — enforce by convention + review, not by parse rejection.

### 4.2 `std/sys/win` — new sibling of `std/sys/linux.ag`

Mirror the existing raw-layer layout (`std/sys/raw/*.ag` generated by
`scripts/gen-syscall-wrappers.py` — that generator stays Linux-only; the win layer is
hand-written `extern "C"` declarations):

| `std/sys` layer | Linux mechanism | Win32 replacement |
|---|---|---|
| `raw/memory.ag` | `sys_mmap/munmap/mprotect` (`std/mem/alloc.ag:52-66`) | `VirtualAlloc(MEM_RESERVE\|MEM_COMMIT, PAGE_READWRITE)` / `VirtualFree(MEM_RELEASE)` / `VirtualProtect` — kernel32. The existing huge-block header records base→offset (`alloc.ag:218-222`), which is exactly what `MEM_RELEASE` needs (exact base). `MEM_MIN_VALID_ADDR = 65536` (`alloc.ag:44`) coincidentally matches Windows' 0x10000 user floor — keep the constant, fix the comment. |
| `raw/process.ag` (fork/exec/clone/exit) | `sys_fork/execve/exit_group/kill` | No fork. `CreateProcessW`; exit via `ExitProcess`; `TerminateProcess` for kill. |
| `raw/thread.ag` + `std/thread.ag` | raw `clone` shim with hand-mapped 2 MiB stack (`thread.ag:32-44, 87-97`) | `CreateThread(dwStackSize=2MiB)` — the OS maps the stack; the clone shim and its asm disappear on Windows. `sys_gettid` → `GetCurrentThreadId`. |
| `raw/sync.ag` (futex) | `sys_futex` (`std/sync.ag:11-41`, `std/channel.ag`, `rt/thread_registry.ag`) | `WaitOnAddress` / `WakeByAddressSingle` / `WakeByAddressAll` (kernel32, Win8+) — direct semantic match for compare-and-wait on a 4-byte word. |
| `raw/fs.ag` (77 calls) + `std/io/file.ag` | `sys_read/write/open/close/stat/unlink/rename/lseek/ioctl/getdents64` | `CreateFileW/ReadFile/WriteFile/CloseHandle/SetFilePointerEx/GetFileInformationByHandleEx/DeleteFileW/MoveFileExW(MOVEFILE_REPLACE_EXISTING)/CreateDirectoryW/RemoveDirectoryW/FindFirstFileW` |
| `raw/time.ag` | `sys_clock_gettime/nanosleep` | `QueryPerformanceCounter`, `GetSystemTimePreciseAsFileTime`, `Sleep(ms)` |
| `raw/misc.ag` (`sys_getrandom`) | `getrandom` (`net/websocket.ag:134-138`) | `ProcessPrng` (bcryptprimitives, Win10+) or `BCryptGenRandom` |
| `errno.ag` | Linux errno numbers | Win32/WSA error-code → Silver errno mapping table (keep the existing `Result` shape) |
| `result.ag` constants | `STDIN_FD=0/1/2`, `O_*`, `PROT_*`, `MAP_*` | HANDLE-based std handles (below); no `O_*` — mode mapping lives in `io/file.ag` |
| `entry.ag` | `_start` asm blob | `main` shim per §3.2 (arg capture + shutdown sequence) |

`#[link(name)]` mapping: `kernel32` → `kernel32.lib`, `m` → the UCRT lib set on MSVC. The link
driver's SDK/MSVC lib paths (§3.3) make these resolvable.

### 4.3 What does *not* port (document + cfg-gate)

| Module | Reason | Windows answer |
|---|---|---|
| `std/sys/epoll.ag`, `std/sys/io_uring.ag`, `std/sys/eventfd.ag` | Linux-only kernel interfaces (io_uring mmaps its SQ/CQ rings — no analogue) | IOCP wrapper later (`CreateIoCompletionPort`); near-term: cfg-gate behind `os.linux`, tests skip |
| `std/sys/raw/socket.ag` / `std/net/*` | raw socket syscalls, AF_UNIX (`tcp.ag:26, 283, 327`) | WinSock2 layer (`WSAStartup`, `SOCKET` ≠ fd, `closesocket`); AF_UNIX exists Win10 1803+ with caveats — phase 2 |
| `std/net/tls.ag` (OpenSSL) | `SSL_set_fd(ssl, i32 fd)` takes a Unix fd (`tls.ag:44`) | Windows OpenSSL builds exist (`libssl.lib`/`libcrypto.lib`); declare `SSL_set_fd` taking a `SOCKET` (u64) under a cfg; or Schannel later |
| `std/sys/io_vec.ag` | `readv/writev/sendfile` | loops or `WSASend/WSARecv` with `WSABUF` |

### 4.4 I/O shape

Recommendation: **HANDLE-based** `File { h: u64 }` (replacing `i32 fd`, `io/file.ag:304-316`)
with `STDIN/STDOUT/STDERR` resolved at startup via `GetStdHandle`. Rationale: CRT fd semantics
(`_open` mode bits, text mode translation) create quiet corruptions; HANDLEs are the native
shape and the `BufWriter`/`Scanner` code above them is fd-shape-agnostic. Console writes that
must render UTF-8 correctly when attached to a console use `WriteConsoleW` (UTF-16) with a
`WriteFile` fallback for pipes/files. `fmt.ag`'s `__fmt_write_fd` (`fmt.ag:28`) and the
backtrace writer (`rt/backtrace.ag:66-69, 333`) get a shared `write_stderr(bytes)` helper so
fd-vs-HANDLE knowledge lives in one place.

UTF-16 boundary: all `*W` calls need UTF-8→UTF-16 conversion helpers in std (`String` is UTF-8);
add `utf8_to_utf16` / `utf16_to_utf8` in a `std/sys/win/unicode.ag`.

### 4.5 Inline-asm site audit (portable vs audit-list)

Already portable on Win64 (fixed registers, no calls, SSE2 baseline): `std/cpu.ag` (cpuid/xgetbv),
`rt/heap.ag:452-463` (SIMD sweep), `mem/memory.ag` SIMD (memcpy/memset/strlen), `rt/backtrace.ag`
locks, `map.ag`, `string.ag:477`, `json.ag`, `io/scanner.ag`, `algo/search.ag`.
**Audit for Win64 nonvolatiles** (`rdi/rsi/rbx/r12-r15` written but not clobber-listed):
`mem/memory.ag` SIMD loops, `map.ag:38-78`, `io/scanner.ag:77,139`. The asm emitter should
grow a debug mode that checks declared clobbers against the Win64 nonvolatile set.

---

## 5. P4 — Debug info, backtraces, object post-pass

- **Debug format**: MSVC triples normally get CodeView, but CodeView selection is driven by
  the IR `CodeView` module flag (set by clang), not by LLVM core — raw `llc`/our emitter can
  emit **DWARF inside COFF** by simply not setting that flag. Verify empirically at P4; if it
  holds, `debug_info.rs:104-114` needs no change and all existing DWARF parsers still work.
- **`dwarf_bt.rs` ELF parser** (`148-303`): add a COFF reader (machine `AMD64` sections,
  `IMAGE_SYMBOL` symtab, in-place relocations `DIR64`/`REL32`/`SECREL` — no RELA addends)
  feeding the *same* DWARF line/info/abbrev/str parsers (`305-757`). The re-emit pipeline
  (`entry.rs:747-786`) is format-agnostic in shape.
- **Backtrace runtime** (`std/rt/backtrace.ag`): the rbp-chain walker works unchanged on Win64
  *because* we emit `frame-pointer=all` (`stmt.rs:39`) — and the symbol tables
  (`__silver_bt_*`) are COFF-COMDAT-compatible (`generate.rs:416-519`). Gaps: stderr writes via
  `sys_write` fd 2 → shared `write_stderr` helper (§4.4); foreign frames (UCRT/OpenSSL) break
  the chain exactly as on Linux — optional `CaptureStackBackTrace` fallback later.
  CFA=`rbp+16` arg model (`dwarf_bt.rs:36-38`) holds for standard Win64 prologues.
- **`abort()`** (`mem/memory.ag:733-738`, `fmt.ag:836-838`): print trace, then
  `ExitProcess(134)` — deliberately keeps the test suite's 134 expectation instead of the
  native 3/0xC0000409.

---

## 6. P5 — Test harness, scripts, CI

`tests/run_tests.py` (all portable-except items):

1. Binary names: `target/<mode>/agc` → `agc.exe` (`L493, 505`); `-o bin_<name>` → `+ ".exe"` for
   both `-o` and execution (`274, 292, 334`).
2. Exit codes: accept `134` (our `ExitProcess(134)` design makes this a no-op) — `216-221, 377-379`.
3. `rust_ffi_test`: `LD_LIBRARY_PATH` → prepend to `PATH` (`314-317`); detect `silver_ffi.dll` +
   `silver_ffi.lib` instead of `.so/.a` (`120-128`).
4. OpenSSL discovery: Windows paths / `shutil.which("openssl")` instead of `ldconfig` (`102-118`);
   TLS tests otherwise SKIP (acceptable initially).
5. ELF post-checks: `ldd` (`346-352`) and `readelf -S .debug_info` (`354-360`) →
   `llvm-readobj --coff-imports` / `llvm-readobj --sections` (ships with LLVM) or skip.
6. TUI ANSI: enable VT processing (`os.system("")` trick) or default `--no-tui` on nt (`24-37, 448-468`).
7. `static_link_test`, `syscall_test`, `syscall_wrapper_test`, `allocator_threads_test`
   (raw `clone`), `cfg_derived_test` (`os.linux` assertions — add `os.windows` branch): **skip-gate
   on Windows** until the corresponding runtime pieces land; `backtrace_test` becomes
   a primary Windows acceptance test.
8. Leak-check tests: gate until the Win32 allocator + leak rbp-walk is proven.

Scripts/CI: `scripts/create-windows-bundle.ps1` (zip with `*.exe`, triple
`x86_64-pc-windows-msvc`); CI matrix gains a `windows-latest` leg installing LLVM via winget +
`LLVM_SYS_221_PREFIX`, running `cargo test -p agc` and the ported harness subset.

Path/sysroot conventions (small but real): `module_loader.rs:619-634` (`HOME`/XDG, `/usr/local/share/silver`)
→ `%LOCALAPPDATA%\silver` + exe-relative; `package.rs:1527-1541` hard-errors without `HOME` →
`%LOCALAPPDATA%\silver\cache` (the pattern already exists: `cache_store.rs:331-336`).

---

## 7. Implementation order & acceptance gates

| Phase | Work | Gate |
|---|---|---|
| P0 | `setup-windows.ps1` + llvm-config shim; `cargo build -p agc` green on Windows | `agc --version` runs |
| P1a | Triple default + `Win64Abi` + codegen COFF object emission | `agc --emit obj` produces a valid COFF (`llvm-readobj` verifies) |
| P1b | `LldLink` flavor + CRT linking + `.exe`/`.obj` driver plumbing | link a hello-world with `extern "C" { puts }` |
| P2 | Win64 asm constraints + nonvolatile clobber modeling | asm audit list clean |
| P3 | `std.sys.win` (allocator, threads, sync, io, entry) | `tests/memory_pentest.ag` passes on Windows; "hello world" + threads + `Vec`/`String`/`Rc` programs run |
| P4 | COFF reader in `dwarf_bt.rs`; DWARF-in-COFF verified | `backtrace_test.ag` prints resolved frames + args on Windows |
| P5 | harness port + CI leg | `run_tests.py` green (minus skip-gated set) on a Windows runner |

### Landed (2026-09-11, this branch)

**P1a — done, verified cross-compiling on Linux.**
- `codegen/abi.rs`: `Win64Abi` implemented (aggregates by value only at
  exactly 1/2/4/8 bytes, always in the integer class — no single-float XMM
  coercion; 3/5/6/7-byte and >8-byte aggregates pass byval-by-pointer, sret
  for all non-{1,2,4,8} returns); `get_abi_handler` now dispatches on the
  triple's **OS** component (was arch-only — the silent-miscompile bug from
  §3.1); `target_is_windows(triple)` helper shared with link/driver.
- `types.rs`: `lower_abi_type` now delegates all struct sizes to the ABI
  handler (the hardcoded SysV small-struct match is gone); the SysV
  `{f32,f32} → <2 x float>` SSE coercion moved into `Amd64Abi`.
- `entry.rs`: the ABI handler is constructed from the resolved triple; the
  no-target default is the **host** triple (`TargetMachine::get_default_triple`)
  instead of hardcoded `x86_64-unknown-linux-gnu`. The `.agm`/IR-string codegen
  path takes the target too.
- Verified: `windows_triple_emits_coff_object` (COFF AMD64 header) and
  `windows_triple_uses_win64_abi_for_extern_structs` (16-byte struct → ptr
  param on windows, register struct on linux) in `codegen/llvm_ir/tests.rs`.

**P1b — driver plumbing done; CRT link needs the P0 Windows box.**
- `link.rs`: `LinkFlavor { GnuLd, LldLink }`; windows flavor resolves the tool
  via `SILVER_LINKER` → `lld-link` → VS `link.exe` (vswhere), builds `/OUT:`,
  `/MACHINE:X64`, `/SUBSYSTEM:CONSOLE`, `/LIBPATH:` from `%LIB%` + vswhere-
  discovered MSVC/SDK dirs, and the CRT set (`/MD` default, `--static` → `/MT`).
  Shared modules emit `/DLL`. Command shape verified against real `lld-link`
  on Linux (COFF object → PE).
- `driver.rs`: `a.exe`/`.obj`/`.asm` defaults, run-mode temp binary gets
  `.exe` (CreateProcess contract), module binaries `.dll`/`.obj`,
  test-harness temp binaries get `.exe` on windows hosts.
- `module_artifact.rs`: consumer lookups accept both posix and windows
  extensions (producer/consumer symmetric across toolchain versions).

**P2 — interim policy**: `asm()` on windows triples is a hard codegen error
(pointing at this doc) until Win64 constraint modeling lands. Rationale: the
emitter hardcodes the SysV syscall register binding, and Win64 would silently
corrupt nonvolatile registers. Note `std/cpu.ag`'s cpuid/xgetbv blobs are
*textually* SysV (%rdi/%rsi) — porting them is part of P3's std gating, not
just emitter constraints.

**P1b remaining**: first real link on a Windows host (CRT + SDK libs) — the
only part of P1 that cannot be verified on Linux.

`memory_pentest.ag` remains the definitive regression gate at every phase that touches
ownership/ABI/codegen (per AGENTS.md §8).

## 8. Risks

1. **ABI miscompilation is silent** — until `Win64Abi` lands, any windows-triple test is
   misleading. Land P1a before anything else codegen-related.
2. **CRT choice locks in ecosystem compat** — UCRT-first is reversible (`/ENTRY:` later) but
   library assumptions (OpenSSL, FFI) harden around it quickly.
3. **llvm-sys on Windows is the least-controlled dependency** — if neither the shim nor the
   native path works, the fallback is a self-built LLVM static tree (hours, not days).
4. **Win32 runtime breadth** — `std.sys.win` is ~10 files with fiddly Unicode/error-mapping
   corners; pentest-grade memory tests are the main defense, and the allocator's
   base/offset header scheme maps cleanly onto `VirtualAlloc`.
5. **Test-suite exit-code drift** — `ExitProcess(134)` is a deliberate contract; document it
   next to `get_expected_exit` so nobody "fixes" it to 3.
