#!/usr/bin/env python3
"""
Generate Silver syscall wrappers from the Linux x86_64 syscall table.

Usage:
    python3 scripts/gen-syscall-wrappers.py <unistd_64.h|compiler> <arg_counts.json> [output.ag]
    python3 scripts/gen-syscall-wrappers.py <unistd_64.h|compiler> <arg_counts.json> \
        <output.ag> --split-dir <raw-directory>

Without `--split-dir`, the script preserves the original single-file output.
With it, the output file becomes a small aggregate importing generated raw
syscall families, and the raw modules are written below `--split-dir`.
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




def generate_header(syscall_count: int) -> list[str]:
    return [
        "// Auto-generated by scripts/gen-syscall-wrappers.py",
        f"// Source: Linux x86_64 syscall table ({syscall_count} syscalls)",
        f"// Generated: {date.today().isoformat()}",
        "// DO NOT EDIT -- regenerate with:",
        "//   python3 scripts/gen-syscall-wrappers.py \\",
        "//       <unistd_64.h|compiler> \\",
        "//       std/sys/syscall_args.json \\",
        "//       std/sys/linux.ag --split-dir std/sys/raw",
        "",
    ]


def generate_enum(syscalls: dict[str, int], enum_name: str) -> list[str]:
    lines = [f"// Syscall number constants", f"enum {enum_name} {{"]
    for name in sorted(syscalls, key=syscalls.get):
        lines.append(f"    {name.upper()} = {syscalls[name]};")
    lines.extend(["}", ""])
    return lines


def generate_wrappers(
    syscalls: dict[str, int],
    arg_counts: dict[str, int | None],
    enum_name: str,
) -> str:
    lines = generate_header(len(syscalls))
    lines.append("import std.sys.syscall;")
    lines.append("")
    lines += generate_enum(syscalls, enum_name)

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
        lines.append(f"i64 sys_{name}({params}) {{")
        lines.append(f"    return syscall{n}({call_args});{comment}")
        lines.append("}")
        lines.append("")
    return "\n".join(lines)


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
    syscalls: dict[str, int],
    arg_counts: dict[str, int | None],
    output_path: str,
    split_dir: str,
) -> None:
    grouped = grouped_syscalls(syscalls)
    raw_dir = Path(split_dir)
    raw_dir.mkdir(parents=True, exist_ok=True)
    (raw_dir / "numbers.ag").write_text(
        "\n".join(generate_header(len(syscalls)) + generate_enum(syscalls, "SYSCALL"))
    )
    groups = sorted(grouped)
    for group in groups:
        (raw_dir / f"{group}.ag").write_text(
            generate_wrappers(grouped[group], arg_counts, f"RAW_{group.upper()}")
        )
    Path(output_path).write_text(generate_aggregate(groups))


def main():
    args = sys.argv[1:]
    split_dir = None
    if "--split-dir" in args:
        index = args.index("--split-dir")
        if index + 1 >= len(args):
            print("--split-dir requires a directory", file=sys.stderr)
            sys.exit(1)
        split_dir = args[index + 1]
        args = args[:index] + args[index + 2:]
    if len(args) < 2:
        print(__doc__, file=sys.stderr)
        sys.exit(1)

    header_source, arg_count_path = args[0], args[1]
    output_path = args[2] if len(args) > 2 else None
    try:
        header_path = resolve_header(header_source)
    except FileNotFoundError as error:
        print(f"Error: {error}", file=sys.stderr)
        sys.exit(1)
    syscalls = parse_kernel_header(header_path)
    if not syscalls:
        print(f"Error: no syscalls found in {header_path}", file=sys.stderr)
        sys.exit(1)
    with open(arg_count_path) as f:
        arg_counts = json.load(f)
    missing = set(syscalls) - set(arg_counts)
    if missing:
        print(
            f"Warning: {len(missing)} syscalls missing from arg count JSON: "
            f"{', '.join(sorted(missing)[:10])}...",
            file=sys.stderr,
        )

    if split_dir:
        if output_path is None:
            print("--split-dir requires an output.ag path", file=sys.stderr)
            sys.exit(1)
        generate_split(syscalls, arg_counts, output_path, split_dir)
        print(f"Generated split syscall modules -> {output_path}", file=sys.stderr)
    else:
        source = generate_wrappers(syscalls, arg_counts, "SYSCALL")
        if output_path:
            Path(output_path).write_text(source)
            print(f"Generated {len(syscalls)} wrappers -> {output_path}", file=sys.stderr)
        else:
            sys.stdout.write(source)


if __name__ == "__main__":
    main()
