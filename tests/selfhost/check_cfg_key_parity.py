#!/usr/bin/env python3
"""Compare stage0/stage1 target-derived and explicit cfg behavior."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import sys


FEATURES = ("sse41", "sse42", "popcnt", "fma", "avx", "avx2", "avx512f")
LINUX = ["--target", "x86_64-unknown-linux-gnu"]
CASES: dict[str, tuple[list[str], set[str]]] = {
    "default debug and linux target": (LINUX, {"debug", "linux", "x86_64"}),
    "release excludes default debug": (LINUX + ["-O2"], {"release", "linux", "x86_64"}),
    "explicit debug overrides optimization": (
        LINUX + ["-O2", "--cfg", "debug=1"], {"debug", "release", "linux", "x86_64"}
    ),
    "explicit debug before optimization": (
        LINUX + ["--cfg", "debug=1", "-O2"], {"debug", "release", "linux", "x86_64"}
    ),
    "explicit release retains default debug": (
        LINUX + ["--cfg", "release=1"], {"debug", "release", "linux", "x86_64"}
    ),
    "windows target": (
        ["--target", "x86_64-pc-windows-msvc"], {"debug", "windows", "x86_64"}
    ),
    "explicit linux survives windows target": (
        ["--cfg", "os.linux=1", "--target", "x86_64-pc-windows-msvc"],
        {"debug", "linux", "windows", "x86_64"},
    ),
    "wasi target": (
        ["--target", "wasm32-wasip1"], {"debug", "wasi", "wasm32"}
    ),
    "aarch64 target": (
        ["--target", "aarch64-unknown-linux-gnu"], {"debug", "linux", "aarch64"}
    ),
    "explicit x86_64 survives aarch64 target": (
        ["--cfg", "arch.x86_64=1", "--target", "aarch64-unknown-linux-gnu"],
        {"debug", "linux", "aarch64", "x86_64"},
    ),
}
for feature in FEATURES:
    CASES[f"explicit cpu.{feature}"] = (
        LINUX + ["--cfg", f"cpu.{feature}=1"], {"debug", "linux", "x86_64", f"cpu_{feature}"}
    )


def run(compiler: pathlib.Path, fixture: pathlib.Path, flags: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(compiler), "--no-cache", "check", *flags, str(fixture)],
        cwd=fixture.parents[2],
        capture_output=True,
        text=True,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", required=True, type=pathlib.Path)
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = args.root.resolve()
    fixture = root / "tests/selfhost/cfg_key_parity.ag"
    compilers = {"stage0": args.stage0.resolve(), "stage1": args.stage1.resolve()}
    failures: list[str] = []
    for case, (flags, expected) in CASES.items():
        for stage, compiler in compilers.items():
            result = run(compiler, fixture, flags)
            output = result.stdout + result.stderr
            actual = {marker for marker in (
                "debug", "release", "windows", "wasi", "linux", "aarch64", "wasm32", "x86_64",
                *(f"cpu_{feature}" for feature in FEATURES),
            ) if f"{marker}_missing" in output}
            if result.returncode == 0 or actual != expected:
                failures.append(
                    f"{stage} {case}: expected failure with active cfg markers {sorted(expected)}, "
                    f"got status {result.returncode} and markers {sorted(actual)} "
                    f"(flags: {flags!r})\n{output.strip()}"
                )
    if failures:
        print("cfg key parity failed:\n" + "\n".join(failures), file=sys.stderr)
        return 1
    print(f"cfg key parity passed ({len(CASES)} cases, stage0 and stage1)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
