#!/usr/bin/env python3
"""Require stage0 and stage1 diagnostics to follow source-span order."""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys


ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
HEADER = re.compile(r"^error:\s+.+:(\d+):(\d+):\s+(.+)$")
EXPECTED = {
    "unknown identifier 'absent_earlier'": 2,
    "unknown identifier 'absent_later'": 6,
}


def diagnostics(output: str, label: str) -> list[tuple[int, int, str]]:
    parsed = []
    for line in ANSI.sub("", output).splitlines():
        match = HEADER.match(line)
        if match is not None:
            parsed.append((int(match.group(1)), int(match.group(2)), match.group(3)))
    if len(parsed) != len(EXPECTED):
        raise AssertionError(
            f"{label}: expected {len(EXPECTED)} primary diagnostics, found {len(parsed)}\n{output}"
        )
    for message, line_number in EXPECTED.items():
        if not any(line == line_number and message in text for line, _column, text in parsed):
            raise AssertionError(f"{label}: missing {message!r} at line {line_number}\n{output}")
    spans = [(line, column) for line, column, _message in parsed]
    if spans != sorted(spans):
        raise AssertionError(f"{label}: primary diagnostics are not span-sorted: {spans}")
    return parsed


def run(binary: pathlib.Path, args: list[str], cwd: pathlib.Path, label: str) -> None:
    result = subprocess.run(
        [str(binary), *args],
        cwd=cwd,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode == 0:
        raise AssertionError(f"{label}: invalid fixture was accepted")
    diagnostics(result.stderr, label)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()

    root = pathlib.Path(__file__).parents[2].resolve()
    fixture = root / "tests/selfhost/diagnostic_order_fixture.ag"
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()
    for label, binary in (("stage0", stage0), ("stage1", stage1)):
        if not binary.is_file():
            parser.error(f"{label} compiler does not exist: {binary}")
    run(stage0, ["--no-cache", "check", str(fixture)], root, "stage0")
    run(stage1, ["check", str(fixture)], root, "stage1")
    print("diagnostic emission order passed: stage0 and stage1 are span-sorted")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except AssertionError as error:
        print(f"diagnostic emission order failed: {error}", file=sys.stderr)
        raise SystemExit(1)
