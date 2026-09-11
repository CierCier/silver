#!/usr/bin/env python3
"""
Integration test harness for the Silver compiler (agc).
Provides a modern parallel TUI test runner for integration tests.
"""

import argparse
import atexit
import dataclasses
import glob
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

# Terminal styling
C_RESET = "\033[0m"
C_BOLD = "\033[1m"
C_DIM = "\033[2m"
C_RED = "\033[31m"
C_GREEN = "\033[32m"
C_YELLOW = "\033[33m"
C_BLUE = "\033[34m"
C_MAGENTA = "\033[35m"
C_CYAN = "\033[36m"
C_WHITE = "\033[37m"
C_BG_RED = "\033[41m"
C_BG_GREEN = "\033[42m"
C_CLEAR_LINE = "\033[2K"
C_CURSOR_UP = "\033[A"

LEAK_CHECK_TESTS = {
    "memory_pentest",
    "alloc_validity_test",
    "string_test",
    "vec_test",
    "mem_test",
    "memmove_scalar_test",
    "channel_test",
    "memory_stress",
    "http_test",
    "cookie_test",
    "assignment_drop_test",
    "field_predrop_test",
    "temp_operator_test",
    "enum_move_test",
    "enum_cascade_test",
}

EXPECTED_COMPILE_FAILURES = {
    "enum_arity_error_test",
    "inherent_drop_error_test",
    "static_volatile_negative_test",
    "target_feature_error_test",
    "borrow_origin_escape_error_test",
    "enum_move_in_error_test",
    "launch_wait_error_test",
    "launch_send_error_test",
    "borrow_conflict_error_test",
    "struct_borrow_error_test",
    "call_borrow_conflict_error_test",
    "match_guard_move_error_test",
}

DEFAULT_SKIP = {
    "mem_growth_watch",  # manual 30-sec memory growth benchmark
}

IS_WINDOWS = os.name == "nt"


def target_is_windows_name(target: Optional[str]) -> bool:
    return bool(target) and any(
        t in target.lower() for t in ("windows", "win32", "mingw")
    )

# Tests that exercise Linux-only mechanisms (raw syscall/clone asm, epoll-
# adjacent kernel interfaces). These are platform tests by design, not
# portable suite members; on Windows the equivalent coverage comes from the
# std.sys.win seam and the thread/allocator tests that run on both.
WINDOWS_SKIP = {
    "syscall_test": "raw Linux syscall asm (x86_64 syscall ABI)",
    "syscall_wrapper_test": "raw Linux syscall wrappers (std.sys.syscall)",
    "allocator_threads_test": "raw clone(2) thread creation",
    "http_test": "std.net sockets over Linux syscalls (winsock layer pending)",
    "cookie_test": "std.net sockets over Linux syscalls (winsock layer pending)",
}


@dataclasses.dataclass
class TestResult:
    name: str
    status: str  # "PASS", "FAIL", "SKIP"
    reason: str = ""
    compile_ms: int = 0
    run_ms: int = 0
    peak_mem_kb: int = 0
    compile_output: str = ""
    run_output: str = ""
    exit_code: Optional[int] = None
    expected_exit: int = 0


class BackgroundServices:
    def __init__(self, root: Path, workdir: Path):
        self.root = root
        self.workdir = workdir
        self.procs: List[subprocess.Popen] = []
        self.openssl_lib = self._find_openssl()
        self.has_node = shutil.which("node") is not None
        self.has_go = shutil.which("go") is not None
        self.ffi_dir = self._find_ffi()
        self.modlib_dir = workdir / "modlib"

    def _find_openssl(self) -> str:
        if not shutil.which("openssl"):
            return ""
        for pattern in ["/nix/store/*openssl-*/lib", "/usr/lib", "/usr/local/lib"]:
            for d in glob.glob(pattern):
                p = Path(d)
                if (p / "libssl.so").is_file() or (p / "libssl.so.3").is_file():
                    return str(p)
        try:
            out = subprocess.check_output(["ldconfig", "-p"], stderr=subprocess.DEVNULL).decode(errors="replace")
            for line in out.splitlines():
                if "libssl.so" in line and "=>" in line:
                    path = line.split("=>")[-1].strip()
                    return str(Path(path).parent)
        except Exception:
            pass
        return ""

    def _find_ffi(self) -> str:
        env_val = os.environ.get("SILVER_FFI_LIBRARY_DIR", "")
        if env_val:
            return env_val
        for build_mode in ["debug", "release"]:
            candidate = self.root / "target" / build_mode
            if IS_WINDOWS:
                if (candidate / "silver_ffi.dll").is_file() or (candidate / "silver_ffi.lib").is_file():
                    return str(candidate)
            elif (candidate / "libsilver_ffi.a").is_file() or (candidate / "libsilver_ffi.so").is_file():
                return str(candidate)
        return ""

    def start_service_if_needed(self, test_names: Set[str], agc_bin: Path):
        # Module import precompilation
        if "module_import_test" in test_names:
            self.modlib_dir.mkdir(parents=True, exist_ok=True)
            mod_src = self.root / "tests/modules/module_lib.ag"
            if mod_src.is_file():
                res = subprocess.run(
                    [str(agc_bin), "--emit=module", str(mod_src)],
                    cwd=str(self.modlib_dir),
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
                if res.returncode != 0:
                    print(f"{C_YELLOW}warning: failed to emit module_lib for module_import_test{C_RESET}")

        # Node TLS server
        if ("tls_test" in test_names or "https_server_test" in test_names) and self.has_node and self.openssl_lib:
            self._spawn_daemon(["node", "tests/tls_server.js"], "TLS_NODE_READY", "tls_node.log")

        # Node HTTP2 server
        if "http2_tls_test" in test_names and self.has_node:
            self._spawn_daemon(["node", "tests/h2_server.js"], "H2C_NODE_READY", "h2_node.log")

        # Node WebSocket / SSE server
        if ("websocket_test" in test_names or "sse_test" in test_names) and self.has_node:
            self._spawn_daemon(["node", "tests/ws_server.js"], "WS_NODE_READY", "ws_node.log")

        # Go HTTP perf server
        if "http_perf_test" in test_names and self.has_go:
            perf_bin = self.workdir / "perf_server"
            src = self.root / "tests/perf/http_server.go"
            if src.is_file():
                env = os.environ.copy()
                env["CGO_ENABLED"] = "0"
                subprocess.run(["go", "build", "-o", str(perf_bin), str(src)], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                if perf_bin.is_file():
                    self._spawn_daemon([str(perf_bin)], "PERF_SERVER_READY", "perf_server.log")

    def _spawn_daemon(self, cmd: List[str], ready_token: str, log_name: str):
        log_path = self.workdir / log_name
        try:
            log_file = open(log_path, "w")
            p = subprocess.Popen(cmd, cwd=str(self.root), stdout=log_file, stderr=subprocess.STDOUT)
            self.procs.append(p)
            for _ in range(50):
                time.sleep(0.1)
                if log_path.exists() and ready_token in log_path.read_text(errors="replace"):
                    break
        except Exception as e:
            print(f"{C_YELLOW}warning: failed to spawn {cmd[0]}: {e}{C_RESET}")

    def cleanup(self):
        for p in self.procs:
            try:
                p.terminate()
                p.wait(timeout=1)
            except Exception:
                try:
                    p.kill()
                except Exception:
                    pass


def get_test_stdin(name: str) -> Optional[bytes]:
    if name == "scanner_test":
        return b"\xef\xbb\xbf3\n10 -20 caf\xc3\xa9\nlast line\n"
    elif name == "scanner_wide_test":
        return (
            b"-5 -300 70000 -9000000000 170141183460469231731687303715884105727 255 65000 4000000000 "
            b"18446744073709551615 340282366920938463463374607431768211455 300 -40000 256 -1 3.5 -2.25 1e3 true false hello\n"
        )
    return None


def get_expected_exit(name: str, content: str) -> int:
    for line in content.splitlines():
        line = line.strip()
        if line.startswith("//"):
            rest = line[2:].strip()
            for prefix in ("expected_exit:", "expected_exit =", "exit:"):
                if rest.startswith(prefix):
                    val = rest[len(prefix):].strip()
                    try:
                        return int(val)
                    except ValueError:
                        pass
    if name in ("syscall_test", "syscall_wrapper_test"):
        return 42
    if name == "static_volatile_test":
        return 7
    if name in ("assert_fail_test", "backtrace_test"):
        return 134
    return 0


def get_extra_flags(name: str, content: str, services: BackgroundServices) -> List[str]:
    flags = ["--cfg", "cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1"]
    for line in content.splitlines():
        line = line.strip()
        if line.startswith("//"):
            rest = line[2:].strip()
            for prefix in ("compile_flags:", "flags:"):
                if rest.startswith(prefix):
                    flags.extend(rest[len(prefix):].strip().split())

    if name in (
        "static_link_test",
        "thread_test",
        "launch_wait_test",
        "channel_test",
        "guard_test",
        "launch_send_test",
    ):
        flags.append("--static-runtime")
    if name in ("tls_test", "http2_tls_test", "https_server_test") and services.openssl_lib:
        flags.extend(["-L", services.openssl_lib])
    if name == "rust_ffi_test" and services.ffi_dir:
        flags.extend(["-L", services.ffi_dir])
    if name == "module_import_test":
        flags.extend(["-I", str(services.modlib_dir)])
    if name == "cfg_test":
        flags.extend(["--cfg", "cfg_test_flag=1,cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1"])
    if name == "ternary_test":
        flags.extend(["--cfg", "cpu.sse41=1"])
    if name == "target_feature_test":
        flags.extend(["--cfg", "cpu.avx2=1"])
    if name in ("cfg_derived_test", "volatile_attr_test"):
        flags.append("-O2")
    if name in LEAK_CHECK_TESTS and "--leak-check" not in flags:
        flags.append("--leak-check")

    return flags


def run_single_test(
    test_path: Path,
    agc_bin: Path,
    workdir: Path,
    services: BackgroundServices,
    timeout_secs: int = 120,
    update_worker=None,
    target: Optional[str] = None,
    runner: Optional[List[str]] = None,
    libdirs: Optional[List[List[str]]] = None,
) -> TestResult:
    name = test_path.stem
    content = test_path.read_text(errors="replace")
    # A windows target produces a PE: the image name must end in .exe (and
    # Wine/CreateProcess refuse to execute extensionless images).
    target_is_windows = bool(target) and any(
        t in target.lower() for t in ("windows", "win32", "mingw")
    )
    needs_exe = target_is_windows or IS_WINDOWS
    bin_path = workdir / (f"bin_{name}.exe" if needs_exe else f"bin_{name}")
    run_dir = workdir / f"run_{name}"
    run_dir.mkdir(parents=True, exist_ok=True)
    if name in ("tls_test", "http2_tls_test", "https_server_test"):
        run_dir = services.root

    is_expected_compile_err = name in EXPECTED_COMPILE_FAILURES
    for line in content.splitlines():
        if "compile_fail" in line or "expected_compile_failure" in line:
            is_expected_compile_err = True

    expected_code = get_expected_exit(name, content)
    extra_flags = get_extra_flags(name, content, services)
    if target:
        extra_flags = ["--target", target] + extra_flags
    if target_is_windows_name(target):
        # COFF has no linkonce dedup: cached .agm artifacts and the app unit
        # would define the same std symbols twice. Single-unit linking until
        # artifact dedup/import-libs land (docs/windows-port.md §4.3).
        extra_flags += ["--no-cache"]
    for libdir in libdirs or []:
        extra_flags += ["-L", *libdir]

    if update_worker:
        update_worker(name, "compiling")

    # Compile phase
    compile_cmd = [str(agc_bin), str(test_path), "-o", str(bin_path), "--no-progress"] + extra_flags
    t0 = time.perf_counter()
    try:
        cp = subprocess.run(compile_cmd, cwd=str(services.root), stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    except Exception as e:
        return TestResult(name, "FAIL", f"failed to spawn compiler: {e}")

    compile_ms = int((time.perf_counter() - t0) * 1000)
    compile_output = cp.stdout + ("\n" + cp.stderr if cp.stderr else "")

    if cp.returncode != 0:
        if is_expected_compile_err:
            return TestResult(name, "PASS", "expected compile error", compile_ms=compile_ms, compile_output=compile_output)
        return TestResult(name, "FAIL", "compile error", compile_ms=compile_ms, compile_output=compile_output)

    if is_expected_compile_err:
        return TestResult(name, "FAIL", "expected compile failure but succeeded", compile_ms=compile_ms, compile_output=compile_output)

    if update_worker:
        update_worker(name, "running")

    # Run phase
    env = os.environ.copy()
    if name == "rust_ffi_test" and services.ffi_dir:
        if IS_WINDOWS:
            # DLL resolution: prepend the ffi dir to PATH.
            env["PATH"] = f"{services.ffi_dir};{env.get('PATH', '')}"
        else:
            ld_path = env.get("LD_LIBRARY_PATH", "")
            env["LD_LIBRARY_PATH"] = f"{services.ffi_dir}:{ld_path}" if ld_path else services.ffi_dir

    stdin_data = get_test_stdin(name)
    t0 = time.perf_counter()
    try:
        run_kwargs = {
            "cwd": str(run_dir),
            "stdout": subprocess.PIPE,
            "stderr": subprocess.PIPE,
            "timeout": timeout_secs,
            "env": env,
        }
        if stdin_data is not None:
            run_kwargs["input"] = stdin_data
        else:
            run_kwargs["stdin"] = subprocess.DEVNULL

        run_cmd = ([*runner, str(bin_path)]) if runner else [str(bin_path)]
        rp = subprocess.run(run_cmd, **run_kwargs)
    except subprocess.TimeoutExpired:
        return TestResult(name, "FAIL", f"timed out after {timeout_secs}s", compile_ms=compile_ms)
    except Exception as e:
        return TestResult(name, "FAIL", f"failed to execute test: {e}", compile_ms=compile_ms)

    run_ms = int((time.perf_counter() - t0) * 1000)
    stdout_str = rp.stdout.decode(errors="replace")
    stderr_str = rp.stderr.decode(errors="replace")
    run_output = stdout_str + ("\n" + stderr_str if stderr_str else "")

    # Post-run special assertions
    if name == "static_link_test":
        if IS_WINDOWS:
            # Static CRT (/MT): the import table must not reference the
            # dynamic UCRT/vcruntime DLLs. System DLLs (kernel32 etc.) are
            # always imported and fine.
            readobj = shutil.which("llvm-readobj")
            if readobj:
                try:
                    imp_out = subprocess.check_output(
                        [readobj, "--coff-imports", str(bin_path)], stderr=subprocess.DEVNULL
                    ).decode(errors="replace")
                    bad = [dll for dll in ("ucrtbase.dll", "vcruntime140.dll", "msvcp140.dll") if dll in imp_out]
                    if bad:
                        return TestResult(name, "FAIL", f"binary imports dynamic CRT: {', '.join(bad)}", compile_ms, run_ms, run_output=run_output)
                except Exception:
                    pass
        else:
            try:
                ldd_out = subprocess.check_output(["ldd", str(bin_path)], stderr=subprocess.STDOUT).decode(errors="replace")
                if "not a dynamic executable" not in ldd_out:
                    return TestResult(name, "FAIL", "binary is not static", compile_ms, run_ms, run_output=run_output)
            except Exception:
                pass

    if name == "cfg_derived_test":
        readobj = shutil.which("llvm-readobj") if IS_WINDOWS else None
        if readobj:
            try:
                re_out = subprocess.check_output(
                    [readobj, "--sections", str(bin_path)], stderr=subprocess.DEVNULL
                ).decode(errors="replace")
                if ".debug_info" in re_out:
                    return TestResult(name, "FAIL", "release build still contains DWARF", compile_ms, run_ms, run_output=run_output)
            except Exception:
                pass
        else:
            try:
                re_out = subprocess.check_output(["readelf", "-S", str(bin_path)], stderr=subprocess.DEVNULL).decode(errors="replace")
                if ".debug_info" in re_out:
                    return TestResult(name, "FAIL", "release build still contains DWARF", compile_ms, run_ms, run_output=run_output)
            except Exception:
                pass

    if name == "backtrace_test":
        needed_frames = [
            "level3 at backtrace_test.ag:",
            "level2 at backtrace_test.ag:",
            "level1 at backtrace_test.ag:",
            "main at backtrace_test.ag:",
            "__silver_assert_failed",
            "args: x=",
        ]
        missing = [f for f in needed_frames if f not in run_output]
        if missing:
            return TestResult(name, "FAIL", f"backtrace missing frames: {', '.join(missing)}", compile_ms, run_ms, run_output=run_output)

    # Check exit code
    exit_code = rp.returncode
    # On Linux, negative code or signal
    if exit_code < 0:
        exit_code = 128 + (-exit_code)

    if exit_code != expected_code:
        return TestResult(
            name,
            "FAIL",
            f"exit code {exit_code}, expected {expected_code}",
            compile_ms=compile_ms,
            run_ms=run_ms,
            exit_code=exit_code,
            expected_exit=expected_code,
            run_output=run_output,
        )

    return TestResult(name, "PASS", compile_ms=compile_ms, run_ms=run_ms, exit_code=exit_code, expected_exit=expected_code, run_output=run_output)


class TestDashboard:
    def __init__(self, total_tests: int, jobs: int, is_tty: bool, verbose: bool):
        self.total = total_tests
        self.jobs = jobs
        self.is_tty = is_tty
        self.verbose = verbose
        self.passed = 0
        self.failed = 0
        self.skipped = 0
        self.done = 0
        self.workers: Dict[int, str] = {i: "idle" for i in range(jobs)}
        self.start_time = time.perf_counter()
        self.lock = threading.Lock()

    def update_worker(self, worker_id: int, text: str):
        with self.lock:
            self.workers[worker_id] = text
            if self.is_tty:
                self._render_tui()

    def on_result(self, res: TestResult):
        with self.lock:
            self.done += 1
            if res.status == "PASS":
                self.passed += 1
            elif res.status == "FAIL":
                self.failed += 1
            elif res.status == "SKIP":
                self.skipped += 1

            if not self.is_tty:
                # Streaming plain log for non-tty
                badge = f"[{res.status}]"
                timing = f"({res.compile_ms + res.run_ms}ms)" if res.status == "PASS" else f"({res.reason})"
                print(f"  {badge:6} {res.name:<28} {timing}", flush=True)
                if res.status == "FAIL":
                    if res.compile_output:
                        print("    [Compiler Output]:\n" + "\n".join("      " + l for l in res.compile_output.splitlines()), flush=True)
                    if res.run_output:
                        print("    [Runtime Output]:\n" + "\n".join("      " + l for l in res.run_output.splitlines()), flush=True)
            else:
                self._render_tui(finished_res=res)

    def _render_tui(self, finished_res: Optional[TestResult] = None):
        elapsed = time.perf_counter() - self.start_time
        pct = (self.done / self.total) * 100 if self.total > 0 else 0
        bar_len = 24
        filled = int((pct / 100) * bar_len)
        bar = "█" * filled + "░" * (bar_len - filled)

        # If a test just finished, print a persistent line for it
        if finished_res:
            sys.stdout.write("\r" + C_CLEAR_LINE)
            if finished_res.status == "PASS":
                status_str = f"{C_GREEN}{C_BOLD}PASS{C_RESET}"
                extra = f"{C_DIM}({finished_res.compile_ms + finished_res.run_ms}ms){C_RESET}"
            elif finished_res.status == "SKIP":
                status_str = f"{C_YELLOW}{C_BOLD}SKIP{C_RESET}"
                extra = f"{C_DIM}({finished_res.reason}){C_RESET}" if finished_res.reason else ""
            else:
                status_str = f"{C_RED}{C_BOLD}FAIL{C_RESET}"
                extra = f"{C_RED}({finished_res.reason}){C_RESET}"
            sys.stdout.write(f"  {status_str}  {finished_res.name:<28} {extra}\n")

        # Live status header line
        sys.stdout.write("\r" + C_CLEAR_LINE)
        sys.stdout.write(
            f"[{bar}] {pct:5.1f}% ({self.done}/{self.total}) | "
            f"{C_GREEN}Passed: {self.passed}{C_RESET}  "
            f"{C_RED}Failed: {self.failed}{C_RESET}  "
            f"{C_YELLOW}Skipped: {self.skipped}{C_RESET} | "
            f"{elapsed:.1f}s"
        )
        sys.stdout.flush()

    def finish(self):
        if self.is_tty:
            sys.stdout.write("\n\n")
            sys.stdout.flush()


def main():
    parser = argparse.ArgumentParser(description="Silver Compiler Integration Test Runner")
    parser.add_argument("filters", nargs="*", default=[], help="Run only tests matching these filters/names")
    parser.add_argument("-j", "--jobs", type=int, default=os.cpu_count() or 4, help="Number of concurrent workers")
    parser.add_argument("--release", action="store_true", help="Use release compiler build")
    parser.add_argument("--debug", action="store_true", help="Use debug compiler build (default)")
    parser.add_argument("--no-tui", action="store_true", help="Disable live interactive TUI")
    parser.add_argument("-v", "--verbose", action="store_true", help="Verbose test execution output")
    parser.add_argument("--timeout", type=int, default=120, help="Per-test timeout in seconds")
    parser.add_argument("--target", type=str, default="", help="Cross-compile for this target triple (e.g. x86_64-pc-windows-msvc)")
    parser.add_argument("--runner", type=str, default="", help="Prefix command used to execute each test binary (e.g. 'wine' on a posix host)")
    parser.add_argument("--libdir", action="append", default=[], help="Library search dir passed as -L to every compile (repeatable; e.g. generated Windows import libs)")
    parser.add_argument("--compare", type=str, default="", help="Compare run time metrics with a baseline file")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent.parent
    os.chdir(root)

    mode = "release" if args.release else "debug"
    agc_bin = root / "target" / mode / ("agc.exe" if IS_WINDOWS else "agc")

    # Ensure agc is built
    print(f"{C_BOLD}== Building agc ({mode}) =={C_RESET}")
    build_cmd = ["cargo", "build", "-p", "agc"]
    if args.release:
        build_cmd.append("--release")
    res = subprocess.run(build_cmd)
    if res.returncode != 0:
        print(f"{C_RED}error: failed to build agc{C_RESET}")
        sys.exit(1)

    if not agc_bin.is_file():
        print(f"{C_RED}error: agc binary not found at {agc_bin}{C_RESET}")
        sys.exit(1)

    # Collect test files
    tests_dir = root / "tests"
    all_test_files = sorted(tests_dir.glob("*.ag"))
    selected_tests: List[Path] = []

    filters = [f.strip().removesuffix(".ag").lower() for f in args.filters if f.strip()]
    is_explicit = bool(filters)

    for p in all_test_files:
        stem = p.stem
        if is_explicit:
            if any(f in stem.lower() for f in filters):
                selected_tests.append(p)
        else:
            selected_tests.append(p)

    if not selected_tests:
        print(f"{C_RED}error: no tests matched {args.filters}{C_RESET}")
        sys.exit(1)

    # Initialize workdir and services
    workdir = Path(tempfile.mkdtemp(prefix="silver-test-run-"))
    services = BackgroundServices(root, workdir)
    atexit.register(services.cleanup)
    atexit.register(lambda: shutil.rmtree(workdir, ignore_errors=True))

    target = args.target or None
    runner = shlex.split(args.runner) if args.runner else None
    libdirs = [shlex.split(d) for d in args.libdir] if args.libdir else []

    selected_stems = {p.stem for p in selected_tests}
    services.start_service_if_needed(selected_stems, agc_bin)

    if IS_WINDOWS:
        os.system("")  # enable ANSI escape processing in the legacy console
    is_tty = sys.stdout.isatty() and not args.no_tui
    dashboard = TestDashboard(len(selected_tests), args.jobs, is_tty, args.verbose)

    print(f"\n{C_BOLD}Running {len(selected_tests)} integration tests with {args.jobs} workers...{C_RESET}\n")

    results: List[TestResult] = []
    results_lock = threading.Lock()
    test_queue = list(selected_tests)
    queue_lock = threading.Lock()

    def worker_thread(worker_id: int):
        while True:
            with queue_lock:
                if not test_queue:
                    dashboard.update_worker(worker_id, "idle")
                    break
                test_path = test_queue.pop(0)

            name = test_path.stem

            # Check skip rules
            skip_reason = ""
            if not is_explicit and name in DEFAULT_SKIP:
                skip_reason = "in default skip list"
            elif name in ("tls_test", "http2_tls_test", "https_server_test") and (not services.openssl_lib or not services.has_node):
                skip_reason = "requires OpenSSL and Node.js"
            elif name in ("http2_test", "websocket_test", "sse_test") and not services.has_node:
                skip_reason = "requires Node.js"
            elif name == "http_perf_test" and not services.has_go:
                skip_reason = "requires Go compiler"
            elif name == "rust_ffi_test" and not services.ffi_dir:
                skip_reason = "requires built Rust FFI library (build ffi/rust)"
            elif IS_WINDOWS and name in WINDOWS_SKIP:
                skip_reason = WINDOWS_SKIP[name]
            elif target and target_is_windows_name(target) and name in WINDOWS_SKIP:
                # Cross-target runs: same skip set as a windows host.
                skip_reason = WINDOWS_SKIP[name]

            if skip_reason:
                res = TestResult(name, "SKIP", skip_reason)
            else:
                res = run_single_test(
                    test_path,
                    agc_bin,
                    workdir,
                    services,
                    timeout_secs=args.timeout,
                    update_worker=lambda n, st: dashboard.update_worker(worker_id, f"{st} {n}"),
                    target=target,
                    runner=runner,
                    libdirs=libdirs,
                )

            with results_lock:
                results.append(res)
            dashboard.on_result(res)

    threads = []
    for wid in range(args.jobs):
        t = threading.Thread(target=worker_thread, args=(wid,), daemon=True)
        t.start()
        threads.append(t)

    for t in threads:
        t.join()

    dashboard.finish()

    # Save metrics
    bench_dir = root / "bench"
    bench_dir.mkdir(exist_ok=True)
    metrics_file = bench_dir / "metrics.tsv"
    current_file = bench_dir / "current.tsv"
    try:
        with open(current_file, "w") as f:
            f.write(f"# run {time.strftime('%F %T')}\n")
            for r in results:
                if r.status == "PASS":
                    f.write(f"{r.name}\t{r.compile_ms}\t{r.run_ms}\t{r.peak_mem_kb}\n")
        with open(metrics_file, "a") as f:
            for r in results:
                if r.status == "PASS":
                    f.write(f"{r.name}\t{r.compile_ms}\t{r.run_ms}\t{r.peak_mem_kb}\n")
    except Exception:
        pass

    # Print summary & slowest tests
    passed_tests = [r for r in results if r.status == "PASS"]
    failed_tests = [r for r in results if r.status == "FAIL"]
    skipped_tests = [r for r in results if r.status == "SKIP"]

    passed_tests.sort(key=lambda r: r.run_ms, reverse=True)
    if passed_tests and any(r.run_ms > 0 for r in passed_tests):
        print(f"{C_BOLD}== Slowest Tests (Execution) =={C_RESET}")
        for r in passed_tests[:5]:
            if r.run_ms > 0:
                print(f"  {r.name:<26} {r.run_ms:6} ms (compile: {r.compile_ms} ms)")
        print()

    passed_tests.sort(key=lambda r: r.compile_ms, reverse=True)
    if passed_tests:
        print(f"{C_BOLD}== Slowest Tests (Compilation) =={C_RESET}")
        for r in passed_tests[:5]:
            print(f"  {r.name:<26} {r.compile_ms:6} ms")
        print()

    # If failures occurred, print details
    if failed_tests:
        print(f"{C_RED}{C_BOLD}== Failures ({len(failed_tests)}) =={C_RESET}")
        for r in failed_tests:
            print(f"\n{C_RED}{C_BOLD}---- {r.name} ({r.reason}) ----{C_RESET}")
            if r.compile_output:
                print(f"{C_YELLOW}Compiler Output:{C_RESET}")
                for line in r.compile_output.splitlines():
                    print("  " + line)
            if r.run_output:
                print(f"{C_YELLOW}Runtime Output:{C_RESET}")
                for line in r.run_output.splitlines():
                    print("  " + line)
        print()

    # Final summary banner
    total_compile = sum(r.compile_ms for r in passed_tests)
    total_run = sum(r.run_ms for r in passed_tests)
    total_wall = int((time.perf_counter() - dashboard.start_time) * 1000)

    print(f"{C_BOLD}== Summary =={C_RESET}")
    print(f"  {C_GREEN}Passed:   {len(passed_tests)}{C_RESET}")
    print(f"  {C_RED}Failed:   {len(failed_tests)}{C_RESET}")
    print(f"  {C_YELLOW}Skipped:  {len(skipped_tests)}{C_RESET}")
    print(f"  Total Wall Time:    {total_wall / 1000:.2f}s")
    print(f"  Total Compile Time: {total_compile / 1000:.2f}s")
    print(f"  Total Run Time:     {total_run / 1000:.2f}s\n")

    if failed_tests:
        failing_names = ", ".join(r.name for r in failed_tests)
        print(f"{C_RED}{C_BOLD}Failing tests: {failing_names}{C_RESET}")
        sys.exit(1)

    print(f"{C_GREEN}{C_BOLD}All tests passed successfully!{C_RESET}")
    sys.exit(0)


if __name__ == "__main__":
    main()
