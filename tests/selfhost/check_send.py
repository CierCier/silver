#!/usr/bin/env python3
"""Focused typed-Send parity gate for the first expression projection slice."""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess

ANSI = re.compile(r"\x1b\[[0-9;]*m")


def run(binary: pathlib.Path, path: pathlib.Path, root: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(binary), "check", str(path), "--no-cache"],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )


def primary_lines(result: subprocess.CompletedProcess[str]) -> list[str]:
    text = ANSI.sub("", result.stderr)
    return [line for line in text.splitlines() if line.startswith("error:")]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    args = parser.parse_args()

    root = args.root.resolve()
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()
    error_case = root / "tests/launch_send_error_test.ag"
    positive_case = root / "tests/launch_send_test.ag"

    old_error = run(stage0, error_case, root)
    new_error = run(stage1, error_case, root)
    old_positive = run(stage0, positive_case, root)
    new_positive = run(stage1, positive_case, root)

    failures: list[str] = []
    if old_error.returncode == 0 or new_error.returncode == 0:
        failures.append("negative Send fixture was accepted")
    if old_error.returncode != 0 and primary_lines(old_error) != primary_lines(new_error):
        failures.append("negative Send primary diagnostics differ")
    if old_positive.returncode != 0 or new_positive.returncode != 0:
        failures.append("positive Send fixture was rejected")

    if failures:
        for failure in failures:
            print(f"typed Send parity failed: {failure}")
        print("stage0:", primary_lines(old_error))
        print("stage1:", primary_lines(new_error))
        return 1

    print("typed Send parity passed: launch error and positive boundary")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
