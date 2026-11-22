#!/usr/bin/env python3
"""
Generate Silver syscall wrappers from the Linux syscall tables of every
supported architecture.

Usage:
    python3 scripts/gen-syscall-wrappers.py <arg_counts.json> <output.ag> <raw-directory> \
        x86_64=<unistd_64.h|compiler> aarch64=<unistd_64.h|compiler>

The output file becomes a small aggregate importing generated raw syscall
families, written below <raw-directory>. Each family module holds one
`@cfg(os.linux) && @cfg(arch.<arch>)` block per architecture with that
architecture's numbers and wrappers. A syscall the architecture lacks (aarch64
has no `open`, `stat`, `fork`, ...) gets no wrapper there, so a caller that
needs it fails to compile instead of issuing the wrong number.

A compiler argument resolves `asm/unistd_64.h` through its include paths. The
aarch64 header is in nixpkgs' cross kernel headers:
    nix build --print-out-paths nixpkgs#pkgsCross.aarch64-multiplatform.linuxHeaders
    # -> <out>/include/asm/unistd_64.h
"""

import json
import re
import subprocess
import sys
from datetime import date
from pathlib import Path


SYSCALL_GROUPS: dict[str, set[str]] = {
    "fs": {
        "access", "faccessat", "faccessat2", "chdir", "chmod", "chown",
        "close", "copy_file_range", "creat", "fchdir", "fchmod", "fchmodat",
        "fchown", "fchownat", "fcntl", "fdatasync", "fgetxattr", "flock",
        "fstat", "fstatfs", "fsync", "ftruncate", "futimesat", "getcwd",
        "getdents", "getdents64", "getxattr", "ioctl", "link", "linkat",
        "listxattr", "lgetxattr", "llistxattr", "lremovexattr",
        "lsetxattr", "lstat", "lseek", "mkdir", "mkdirat", "newfstatat",
        "open", "open_by_handle_at", "open_tree", "openat", "openat2",
        "pread64", "pwrite64", "read", "readahead", "readlink",
        "readlinkat", "readv", "removexattr", "rename", "renameat",
        "renameat2", "rmdir", "sendfile", "setxattr", "stat", "statfs",
        "statx", "symlink", "symlinkat", "sync", "sync_file_range",
        "syncfs", "tee", "truncate", "unlink", "unlinkat", "utime",
        "utimensat", "utimes", "vmsplice", "write", "writev",
    },
    "memory": {
        "brk", "madvise", "mbind", "migrate_pages", "mincore", "mlock",
        "mlock2", "mlockall", "mmap", "mprotect", "mremap", "munlock",
        "munlockall", "munmap", "move_pages", "process_madvise",
        "process_mrelease", "remap_file_pages", "set_mempolicy",
    },
    "socket": {
        "accept", "accept4", "bind", "connect", "getpeername", "getsockname",
        "getsockopt", "listen", "recvmmsg", "recvfrom", "recvmsg",
        "sendmmsg", "sendmsg", "sendto", "setsockopt", "shutdown", "socket",
        "socketpair",
    },
    "process": {
        "arch_prctl", "clone", "clone3", "execve", "execveat", "exit",
        "exit_group", "fork", "getegid", "geteuid", "getgid", "getgroups",
        "getpgid", "getpgrp", "getpid", "getppid", "getresgid", "getresuid",
        "gettid", "getuid", "kill", "prctl", "setgid", "setgroups",
        "setpgid", "setresgid", "setresuid", "setsid", "setuid", "tgkill",
        "tkill", "wait4", "waitid",
    },
    "sync": {
        "futex", "get_robust_list", "restart_syscall", "set_robust_list",
    },
    "thread": {
        "getcpu", "getrusage", "rseq", "sched_getaffinity",
        "sched_getparam", "sched_getscheduler", "sched_rr_get_interval",
        "sched_setaffinity", "sched_setparam", "sched_setscheduler",
        "sched_yield", "set_tid_address",
    },
    "time": {
        "adjtimex", "clock_adjtime", "clock_getres", "clock_gettime",
        "clock_nanosleep", "getitimer", "nanosleep", "setitimer",
        "timer_create", "timer_delete", "timer_getoverrun", "timer_gettime",
        "timer_settime", "timerfd_create", "timerfd_gettime",
        "timerfd_settime",
    },
    "poll": {
        "epoll_create", "epoll_create1", "epoll_ctl", "epoll_pwait",
        "epoll_pwait2", "epoll_wait", "eventfd", "eventfd2", "fanotify_init",
        "fanotify_mark", "inotify_add_watch", "inotify_init",
        "inotify_init1", "inotify_rm_watch", "io_uring_enter",
        "io_uring_register", "io_uring_setup", "pselect6", "poll", "ppoll",
        "select", "signalfd", "signalfd4",
    },
}


# std/sys/os_linux.ag defines typed `sys_<name>` seam functions with these
# names, so their generated wrappers keep a `raw_` prefix to stay unambiguous.
# Several are x86_64-only syscalls (`dup2`, `epoll_wait`, `fork`, `lstat`,
# `pipe`, `poll`, `readlink`) whose seams use calls that aarch64 also has.
RAW_PREFIXED: set[str] = {
    "access", "clock_gettime", "close", "dup2", "epoll_wait", "exit",
    "exit_group", "fork", "futex", "getpid", "getrandom", "kill", "lseek",
    "lstat", "mkdir", "mmap", "mprotect", "munmap", "nanosleep", "open",
    "pipe", "poll", "read", "readlink", "rename", "rmdir", "stat", "unlink",
    "write",
}


# Extra full-arity wrappers named `sys_<name><n>`, for callers that need the
# arguments the arg-count table omits (io_uring_enter's sigset and size).
EXTRA_ARITY: dict[str, int] = {"io_uring_enter": 6}


def parse_kernel_header(path: str) -> dict[str, int]:
    """Parse unistd_64.h and return syscall_name -> syscall_number."""
    syscalls: dict[str, int] = {}
    with open(path) as f:
        for line in f:
            m = re.match(r"#define\s+__NR_(\w+)\s+(\d+)", line)
            if m:
                syscalls[m.group(1)] = int(m.group(2))
    return syscalls

def resolve_header(source: str) -> str:
    """Resolve a header path directly or through a compiler's include paths."""
    path = Path(source)
    if path.is_file():
        return str(path)

    probe = subprocess.run(
        [source, "-E", "-Wp,-v", "-x", "c", "/dev/null"],
        capture_output=True,
        text=True,
        check=False,
    )
    if probe.returncode != 0:
        raise FileNotFoundError(
            f"{source!r} is not a header path or a usable C compiler"
        )

    in_search_list = False
    for line in probe.stderr.splitlines():
        directory = line.strip()
        if directory == "#include <...> search starts here:":
            in_search_list = True
            continue
        if directory == "End of search list.":
            break
        if in_search_list and directory and not directory.startswith("ignoring "):
            candidate = Path(directory) / "asm" / "unistd_64.h"
            if candidate.is_file():
                return str(candidate)
    raise FileNotFoundError(
        f"compiler {source!r} exposes no asm/unistd_64.h include"
    )




ARCHES: tuple[str, ...] = ("x86_64", "aarch64")


def generate_header(tables: dict[str, dict[str, int]]) -> list[str]:
    counts = ", ".join(f"{arch}: {len(tables[arch])}" for arch in ARCHES)
    return [
        "// Auto-generated by scripts/gen-syscall-wrappers.py",
        f"// Source: Linux syscall tables ({counts} syscalls)",
        f"// Generated: {date.today().isoformat()}",
        "// DO NOT EDIT -- regenerate with:",
        "//   python3 scripts/gen-syscall-wrappers.py std/sys/syscall_args.json \\",
        "//       std/sys/linux.ag std/sys/raw \\",
        "//       x86_64=<unistd_64.h|compiler> aarch64=<unistd_64.h|compiler>",
        "",
    ]


def arch_block(arch: str, body: list[str]) -> list[str]:
    """Wraps `body` so it compiles only for Linux on `arch`."""
    return [f"if (@cfg(os.linux) && @cfg(arch.{arch})) {{", *body, "}", ""]


def generate_enum(syscalls: dict[str, int], enum_name: str, arch: str) -> list[str]:
    lines = [
        f"/// Every Linux {arch} syscall number."
        if enum_name == "SYSCALL"
        else f"/// Linux {arch} syscall numbers for the `{enum_name}` family.",
        "/// Each variant's value is the kernel's `__NR_<name>` number.",
        f"enum {enum_name} {{",
    ]
    for name in sorted(syscalls, key=syscalls.get):
        lines.append(f"    {name.upper()} = {syscalls[name]};")
    lines.append("}")
    return lines


def generate_wrappers(
    syscalls: dict[str, int],
    arg_counts: dict[str, int | None],
    enum_name: str,
    arch: str,
) -> list[str]:
    """The enum and wrappers of one syscall family on `arch`."""
    lines = generate_enum(syscalls, enum_name, arch)
    lines.append("")

    for name in sorted(syscalls, key=syscalls.get):
        arg_count = arg_counts.get(name)
        if arg_count is None:
            n = 6
            args = [f"a{i}" for i in range(1, n + 1)]
            params = ", ".join(f"i64 a{i}" for i in range(1, n + 1))
            call_args = ", ".join([f"(i64){enum_name}.{name.upper()}"] + args)
            comment = "  // arg count guessed"
        else:
            n = arg_count
            params = ", ".join(f"i64 a{i}" for i in range(1, n + 1))
            call_args = ", ".join(
                [f"(i64){enum_name}.{name.upper()}"]
                + [f"a{i}" for i in range(1, n + 1)]
            )
            comment = ""
        prefix = "raw_sys_" if name in RAW_PREFIXED else "sys_"
        lines += wrapper_doc(name, syscalls[name], arg_count)
        lines.append(f"i64 {prefix}{name}({params}) {{")
        lines.append(f"    return syscall{n}({call_args});{comment}")
        lines.append("}")
        lines.append("")
        extra = EXTRA_ARITY.get(name)
        if extra is not None:
            params = ", ".join(f"i64 a{i}" for i in range(1, extra + 1))
            call_args = ", ".join(
                [f"(i64){enum_name}.{name.upper()}"]
                + [f"a{i}" for i in range(1, extra + 1)]
            )
            lines += wrapper_doc(name, syscalls[name], extra)
            lines.append(f"/// Passes all {extra} kernel arguments.")
            lines.append(f"i64 sys_{name}{extra}({params}) {{")
            lines.append(f"    return syscall{extra}({call_args});")
            lines.append("}")
            lines.append("")
    return lines[:-1]


def wrapper_doc(name: str, number: int, arg_count: int | None) -> list[str]:
    lines = [
        f"/// Raw `{name}` syscall (number {number}). Arguments pass through",
        "/// unchanged in the syscall registers; the caller owns any pointed-to memory.",
        "/// Returns the kernel result: non-negative on success, `-errno` on failure.",
        "/// Does not set errno and does not retry on EINTR.",
    ]
    if arg_count is None:
        lines.append("/// The argument count is unknown, so all six registers are passed.")
    if name in RAW_PREFIXED:
        lines.append(f"/// Prefer the typed `sys_{name}` seam in std/sys/os_linux.ag.")
    return lines


def grouped_syscalls(syscalls: dict[str, int]) -> dict[str, dict[str, int]]:
    groups = {name: {} for name in SYSCALL_GROUPS}
    groups["misc"] = {}
    owners: dict[str, str] = {}
    for group, names in SYSCALL_GROUPS.items():
        for name in names:
            if name not in syscalls:
                continue
            if name in owners:
                raise ValueError(f"syscall assigned to multiple groups: {name}")
            owners[name] = group
            groups[group][name] = syscalls[name]
    for name, number in syscalls.items():
        groups[owners.get(name, "misc")][name] = number
    return {group: values for group, values in groups.items() if values}


def generate_aggregate(groups: list[str]) -> str:
    lines = [
        "// Auto-generated aggregate for all raw Linux syscall wrappers.",
        "import std.sys.raw.numbers;",
    ]
    lines.extend(f"import std.sys.raw.{group};" for group in groups)
    lines.append("")
    return "\n".join(lines)


def generate_split(
    tables: dict[str, dict[str, int]],
    arg_counts: dict[str, int | None],
    output_path: str,
    split_dir: str,
) -> None:
    grouped = {arch: grouped_syscalls(tables[arch]) for arch in ARCHES}
    header = generate_header(tables)
    raw_dir = Path(split_dir)
    raw_dir.mkdir(parents=True, exist_ok=True)

    numbers = list(header)
    for arch in ARCHES:
        numbers += arch_block(arch, generate_enum(tables[arch], "SYSCALL", arch))
    (raw_dir / "numbers.ag").write_text("\n".join(numbers))

    groups = sorted(set().union(*(grouped[arch] for arch in ARCHES)))
    for group in groups:
        lines = header + ["import std.sys.syscall;", ""]
        for arch in ARCHES:
            family = grouped[arch].get(group)
            if family:
                lines += arch_block(arch, generate_wrappers(family, arg_counts, f"RAW_{group.upper()}", arch))
        (raw_dir / f"{group}.ag").write_text("\n".join(lines))
    Path(output_path).write_text(generate_aggregate(groups))


def main():
    args = sys.argv[1:]
    if len(args) != 3 + len(ARCHES):
        print(__doc__, file=sys.stderr)
        sys.exit(1)
    arg_count_path, output_path, split_dir = args[:3]
    sources = dict(arg.split("=", 1) for arg in args[3:])
    if set(sources) != set(ARCHES):
        print(f"Error: give exactly one table per architecture: {', '.join(ARCHES)}", file=sys.stderr)
        sys.exit(1)

    tables: dict[str, dict[str, int]] = {}
    for arch in ARCHES:
        try:
            header_path = resolve_header(sources[arch])
        except FileNotFoundError as error:
            print(f"Error: {error}", file=sys.stderr)
            sys.exit(1)
        tables[arch] = parse_kernel_header(header_path)
        if not tables[arch]:
            print(f"Error: no syscalls found in {header_path}", file=sys.stderr)
            sys.exit(1)

    with open(arg_count_path) as f:
        arg_counts = json.load(f)
    missing = set().union(*tables.values()) - set(arg_counts)
    if missing:
        print(
            f"Warning: {len(missing)} syscalls missing from arg count JSON: "
            f"{', '.join(sorted(missing)[:10])}...",
            file=sys.stderr,
        )

    generate_split(tables, arg_counts, output_path, split_dir)
    print(f"Generated split syscall modules -> {output_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
