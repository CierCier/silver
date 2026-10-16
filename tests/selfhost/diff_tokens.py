#!/usr/bin/env python3
"""Compare the stage0 and stage1 lexer contracts on a Silver source corpus."""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys

SPAN_RE = re.compile(r" \[(\d+)\.\.(\d+)\] ")


def stage0_records(text: str, source: bytes) -> list[tuple[str, int, int, str]]:
    records = []
    last_end = 0
    for line in text.splitlines():
        if line.startswith("Comment {"):
            match = re.search(r"\} \[(\d+)\.\.(\d+)\] ", line)
            if match is None:
                raise ValueError(f"unrecognised stage0 comment line: {line!r}")
            start = int(match.group(1))
            end = int(match.group(2))
            raw = source[start:end]
            if raw.startswith(b"//"):
                raw = raw[2:]
                if raw.startswith(b"/"):
                    raw = raw[1:]
            elif raw.startswith(b"/*"):
                raw = raw[2:]
                if raw.startswith(b"*"):
                    raw = raw[1:]
                if raw.endswith(b"*/"):
                    raw = raw[:-2]
            records.append(("Comment", start, end, raw.strip().decode("utf-8", "surrogateescape")))
            last_end = end
            continue

        selected = None
        for match in SPAN_RE.finditer(line):
            prefix = line[: match.start()]
            if not prefix:
                continue
            if not re.fullmatch(r"[A-Za-z][A-Za-z0-9]*(?:\(.*\))?", prefix):
                continue
            start = int(match.group(1))
            if start < last_end:
                continue
            selected = (match, prefix, start, int(match.group(2)))
        if selected is None:
            continue
        match, kind, start, end = selected
        if kind.endswith(")") and "(" in kind:
            kind = kind[: kind.index("(")]
        records.append((kind, start, end, source[start:end].decode("utf-8", "surrogateescape")))
        last_end = end
    return records


def stage1_records(text: str) -> list[tuple[str, int, int, str]]:
    records = []
    for line in text.splitlines():
        fields = line.split("|", 5)
        if len(fields) != 6:
            raise ValueError(f"unrecognised stage1 token line: {line!r}")
        kind, start, end, _start_pos, _end_pos, text_hex = fields
        records.append((kind, int(start), int(end), bytes.fromhex(text_hex).decode("utf-8", "surrogateescape")))
    return records


def corpus_paths(root: pathlib.Path, include_std: bool) -> list[pathlib.Path]:
    paths: list[pathlib.Path] = []
    for directory in ("tests", "examples"):
        paths.extend(sorted((root / directory).glob("*.ag")))
    if include_std:
        paths.extend(sorted((root / "std").rglob("*.ag")))
    return paths


def run(command: list[str], cwd: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=cwd, text=True, capture_output=True, check=False)


def compare_file(root: pathlib.Path, stage0: pathlib.Path, stage1: pathlib.Path, path: pathlib.Path) -> str | None:
    old = run([str(stage0), "--no-cache", "--emit=tokens", str(path)], root)
    new = run([str(stage1), "lex", str(path)], root)
    if old.returncode != new.returncode:
        return f"exit mismatch: stage0={old.returncode}, stage1={new.returncode}\n{new.stderr[:500]}"
    if old.returncode != 0:
        return None
    try:
        old_records = stage0_records(old.stdout, path.read_bytes())
        new_records = stage1_records(new.stdout)
    except ValueError as error:
        return str(error)
    if old_records != new_records:
        for index, (left, right) in enumerate(zip(old_records, new_records)):
            if left != right:
                return f"token {index} mismatch: stage0={left!r}, stage1={right!r}"
        return f"token count mismatch: stage0={len(old_records)}, stage1={len(new_records)}"
    return None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    parser.add_argument("--include-std", action="store_true")
    args = parser.parse_args()
    root = args.root.resolve()
    failures = []
    paths = corpus_paths(root, args.include_std)
    for path in paths:
        error = compare_file(root, args.stage0.resolve(), args.stage1.resolve(), path)
        if error:
            failures.append((path, error))
    if failures:
        for path, error in failures[:20]:
            print(f"{path.relative_to(root)}: {error}", file=sys.stderr)
        print(f"token parity failed: {len(failures)} file(s)", file=sys.stderr)
        return 1
    print(f"token parity passed: {len(paths)} file(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
