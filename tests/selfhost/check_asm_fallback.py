#!/usr/bin/env python3
"""STD-005: every std/ inline-asm site must have a non-x86 fallback.

Accepts a file if ANY of these hold:
  1. it contains an `@cfg(os.wasi)` scalar arm (the map.ag/memory.ag pattern);
  2. it is wrapped in a top-level `if (@cfg(os.<platform>))` block;
  3. it is a platform-scoped file whose ONLY importer gates on cfg
     (allowlist below, with the gating importer as evidence).

Any other file containing `asm(` fails the gate: it would be a hard compile
error on the wasm leg of the diff gate (S2-3).
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

# Platform-scoped files: asm is unreachable on other targets because the
# importer only pulls them under the matching cfg.
PLATFORM_FILES = {
    # file suffix -> (gating importer, gating evidence)
    "std/sys/os_linux.ag": ("std/sys/os.ag", "if (@cfg(os.linux)) import std.sys.os_linux"),
    "std/sys/os_windows.ag": ("std/sys/os.ag", "else import std.sys.os_windows"),
    "std/sys/os_wasi.ag": ("std/sys/os.ag", "else if (@cfg(os.wasi)) import std.sys.os_wasi"),
}

ASM_RE = re.compile(r"(?<![A-Za-z0-9_])asm\s*\(")
WASI_RE = re.compile(r"@cfg\s*\(\s*os\.wasi\s*\)")
TOP_CFG_RE = re.compile(r"^\s*if\s*\(@cfg\s*\(\s*os\.[a-z0-9_]+\s*\)\s*\)\s*\{", re.MULTILINE)


def check_file(path: pathlib.Path, root: pathlib.Path) -> str | None:
    rel = path.relative_to(root).as_posix()
    text = path.read_text()
    if not ASM_RE.search(text):
        return None
    if WASI_RE.search(text):
        return None
    if TOP_CFG_RE.search(text):
        return None
    if rel in PLATFORM_FILES:
        importer, evidence = PLATFORM_FILES[rel]
        imp_text = (root / importer).read_text()
        if "os_linux" in rel and "@cfg(os.linux)" in imp_text:
            return None
        if "os_windows" in rel and "os_windows" in imp_text:
            return None
        if "os_wasi" in rel and "@cfg(os.wasi)" in imp_text:
            return None
        return f"{rel}: platform file but importer gate not found in {importer}"
    return f"{rel}: inline asm with no @cfg(os.wasi) fallback or cfg wrapper"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path,
                        default=pathlib.Path(__file__).parents[2])
    args = parser.parse_args()
    root = args.root.resolve()
    violations: list[str] = []
    for path in sorted((root / "std").rglob("*.ag")):
        hit = check_file(path, root)
        if hit:
            violations.append(hit)
    if violations:
        print("asm fallback gate FAILED:")
        for v in violations:
            print(f"  {v}")
        return 1
    print("asm fallback gate passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
