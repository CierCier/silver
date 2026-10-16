#!/usr/bin/env python3
"""GATE-003 parity checks for stage1 frontend surfaces that are implemented.

LLVM IR, production AGM publication, and observable cache keys are not exposed
by stage1's current driver; those comparisons are intentionally reported as
deferred until stage1 provides those interfaces.
"""

from __future__ import annotations

import argparse
import pathlib
import re
import subprocess
import sys
import tempfile

from diff_ast_spans import stage0_spans, stage1_spans
from diff_tokens import stage0_records, stage1_records

ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
DIAGNOSTIC = re.compile(r"^error:\s+.+:(\d+):(\d+):\s+(.+)$")
RUST_UNTERMINATED = re.compile(
    r'InvalidString \{ span: \((\d+),\s*\d+\), message: "Unterminated string literal" \}'
)


def run(binary: pathlib.Path, args: list[str], cwd: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(binary), *args], cwd=cwd, capture_output=True, text=True,
        timeout=30, check=False,
    )


def unterminated_signature(stderr: str, source: pathlib.Path) -> tuple[str, int, int] | None:
    clean = ANSI.sub("", stderr)
    rust_match = RUST_UNTERMINATED.search(clean)
    if rust_match is not None:
        offset = int(rust_match.group(1))
        before = source.read_bytes()[:offset].decode("utf-8")
        line = before.count("\n") + 1
        column = len(before.rsplit("\n", 1)[-1]) + 1
        return ("unterminated string literal", line, column)
    for line_text in clean.splitlines():
        match = DIAGNOSTIC.match(line_text)
        if match is not None and "Unterminated string literal" in match.group(3):
            return (
                "unterminated string literal",
                int(match.group(1)),
                int(match.group(2)),
            )
    return None


def compare_lexical_error(stage0: pathlib.Path, stage1: pathlib.Path, root: pathlib.Path,
                          source: pathlib.Path) -> list[str]:
    old = run(stage0, ["--no-cache", "--emit=tokens", str(source)], root)
    new = run(stage1, ["lex", str(source)], root)
    failures: list[str] = []
    if old.returncode != 2 or new.returncode != 2:
        failures.append(f"lex error status: stage0={old.returncode}, stage1={new.returncode}")
    old_signature = unterminated_signature(old.stderr, source)
    new_signature = unterminated_signature(new.stderr, source)
    if old_signature is None or old_signature != new_signature:
        failures.append(
            f"lex error identity differs: stage0={old_signature}, stage1={new_signature}"
        )
    return failures


def compare_tokens(stage0: pathlib.Path, stage1: pathlib.Path, root: pathlib.Path,
                   source: pathlib.Path) -> list[str]:
    old = run(stage0, ["--no-cache", "--emit=tokens", str(source)], root)
    new = run(stage1, ["lex", str(source)], root)
    failures: list[str] = []
    if old.returncode != new.returncode:
        failures.append(f"lex exit: stage0={old.returncode}, stage1={new.returncode}")
    if old.stderr != new.stderr:
        failures.append("lex stderr differs")
    if old.returncode == 0 and new.returncode == 0:
        try:
            if stage0_records(old.stdout, source.read_bytes()) != stage1_records(new.stdout):
                failures.append("lex token records differ (including byte spans)")
        except (ValueError, UnicodeError) as error:
            failures.append(f"cannot decode token records: {error}")
    elif old.stdout != new.stdout:
        failures.append("lex stdout differs on failure")
    return failures


def compare_ast_spans(stage0: pathlib.Path, stage1: pathlib.Path, root: pathlib.Path,
                      source: pathlib.Path, command: str) -> list[str]:
    old = run(stage0, ["--no-cache", "--emit=ast", str(source)], root)
    new = run(stage1, [command, str(source)], root)
    failures: list[str] = []
    if old.returncode != new.returncode:
        failures.append(f"{command} exit: stage0={old.returncode}, stage1={new.returncode}")
    if old.returncode == 0 and new.returncode == 0:
        old_spans = stage0_spans(old.stdout)
        new_spans = stage1_spans(new.stdout)
        if not old_spans or not new_spans:
            failures.append(f"{command} did not expose parse-tree spans")
        else:
            boundaries = {(start, "start") for start, _ in new_spans}
            boundaries.update((end, "end") for _, end in new_spans)
            for index, (start, end) in enumerate(old_spans):
                if (start, "start") not in boundaries or (end, "end") not in boundaries:
                    failures.append(
                        f"{command} AST span {index} {start}..{end} "
                        "has a boundary absent from the stage1 CST"
                    )
                    break
    return failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = args.root.resolve()
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()
    for label, binary in (("stage0", stage0), ("stage1", stage1)):
        if not binary.is_file():
            parser.error(f"{label} compiler does not exist: {binary}")

    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="silver-gate003-") as temporary:
        fixture_dir = pathlib.Path(temporary)
        valid = fixture_dir / "same_input.ag"
        valid.write_text("i32 main() { return 42; }\n", encoding="utf-8")
        failures.extend(f"valid fixture: {error}" for error in compare_tokens(stage0, stage1, root, valid))
        for command in ("parse", "ast"):
            failures.extend(
                f"valid fixture: {error}"
                for error in compare_ast_spans(stage0, stage1, root, valid, command)
            )

        malformed = fixture_dir / "malformed.ag"
        malformed.write_text('i32 main() { return "unterminated; }\n', encoding="utf-8")
        failures.extend(
            f"malformed fixture: {error}"
            for error in compare_lexical_error(stage0, stage1, root, malformed)
        )
        failures.extend(
            f"malformed fixture: {error}"
            for error in compare_ast_spans(stage0, stage1, root, malformed, "parse")
        )

    if failures:
        for failure in failures:
            print(f"GATE-003 failed: {failure}", file=sys.stderr)
        return 1

    print("GATE-003 frontend boundary passed: lex records, lexical error location, parse status, and AST spans")
    print("GATE-003 deferred: rendered diagnostic parity requires the unfinished frontend catalog and renderer work")
    print("GATE-003 deferred: normalized .ll requires the unfinished LLVM backend; production .agm/cache-key comparison requires the unfinished artifact-writer/cache-key interfaces")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
