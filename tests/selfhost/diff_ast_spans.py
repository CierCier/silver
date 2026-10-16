#!/usr/bin/env python3
"""Compare stage0 semantic AST span boundaries with the concrete stage1 syntax tree.

The root Program nodes describe each parser's input boundary, not a semantic
item span, so they are excluded. Other CST node and token boundaries preserve
the source locations used by semantic AST nodes.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys

SPAN_RE = re.compile(r"\[(\d+)\.\.(\d+)\]")
STAGE1_NODE_RE = re.compile(r"(?:`-- |\|-- )([A-Za-z][A-Za-z0-9]*) \[(\d+)\.\.(\d+)\]")


def corpus_paths(root: pathlib.Path, include_std: bool) -> list[pathlib.Path]:
    paths: list[pathlib.Path] = []
    for directory in ("tests", "examples"):
        paths.extend(sorted((root / directory).glob("*.ag")))
    if include_std:
        paths.extend(sorted((root / "std").rglob("*.ag")))
    return paths


def run(command: list[str], cwd: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)


def stage0_spans(text: str) -> list[tuple[int, int]]:
    return [
        (int(match.group(1)), int(match.group(2)))
        for line in text.splitlines()
        if not line.startswith("Program [")
        and (match := SPAN_RE.search(line)) is not None
    ]


def stage1_spans(text: str) -> list[tuple[int, int]]:
    spans = []
    for line in text.splitlines():
        match = STAGE1_NODE_RE.search(line)
        if match is None:
            continue
        kind, start, end = match.groups()
        if kind != "Program":
            spans.append((int(start), int(end)))
    return spans


def compare_file(root: pathlib.Path, stage0: pathlib.Path, stage1: pathlib.Path, path: pathlib.Path) -> str | None:
    old = run([str(stage0), "--no-cache", "--emit=ast", str(path)], root)
    new = run([str(stage1), "parse", str(path)], root)
    if old.returncode != new.returncode:
        return f"parse exit mismatch: stage0={old.returncode}, stage1={new.returncode}; stage1 stderr: {new.stderr[:300]}"
    if old.returncode != 0:
        return None
    try:
        expected = stage0_spans(old.stdout)
        actual = stage1_spans(new.stdout)
    except ValueError as error:
        return f"unrecognised dump: {error}"

    starts = {start for start, _ in actual}
    ends = {end for _, end in actual}
    for index, (start, end) in enumerate(expected):
        if start not in starts or end not in ends:
            return (
                f"AST span {index} {start}..{end} has a boundary absent from "
                f"stage1 CST spans (stage0={len(expected)}, stage1={len(actual)})"
            )
    return None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    parser.add_argument("--include-std", action="store_true")
    args = parser.parse_args()
    root = args.root.resolve()
    paths = corpus_paths(root, args.include_std)
    failures = []
    for path in paths:
        error = compare_file(root, args.stage0.resolve(), args.stage1.resolve(), path)
        if error:
            failures.append((path, error))
    if failures:
        for path, error in failures[:20]:
            print(f"{path.relative_to(root)}: {error}", file=sys.stderr)
        print(f"AST span parity failed: {len(failures)} file(s)", file=sys.stderr)
        return 1
    print(f"AST span parity passed: {len(paths)} file(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
