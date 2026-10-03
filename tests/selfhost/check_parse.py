#!/usr/bin/env python3
"""Check that stage1 accepts every source file accepted by stage0's parser."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import sys


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    parser.add_argument("--include-std", action="store_true")
    args = parser.parse_args()
    root = args.root.resolve()
    paths = []
    for directory in ("tests", "examples"):
        paths.extend(sorted((root / directory).glob("*.ag")))
    if args.include_std:
        paths.extend(sorted((root / "std").rglob("*.ag")))

    failures = []
    for path in paths:
        old = subprocess.run(
            [str(args.stage0), "--no-cache", "--emit=ast", str(path)],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
        new = subprocess.run(
            [str(args.stage1), "parse", str(path)],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
        if (old.returncode == 0) != (new.returncode == 0):
            failures.append((path, old.returncode, new.returncode, new.stderr[:300]))
    if failures:
        for path, old_code, new_code, error in failures[:20]:
            print(f"{path.relative_to(root)}: stage0={old_code}, stage1={new_code}: {error}", file=sys.stderr)
        print(f"parse parity failed: {len(failures)} file(s)", file=sys.stderr)
        return 1
    print(f"parse acceptance parity passed: {len(paths)} file(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
