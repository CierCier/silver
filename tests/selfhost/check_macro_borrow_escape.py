#!/usr/bin/env python3
"""Verify stage1 rejects borrowed block results tied to local storage."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import sys
import tempfile


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = pathlib.Path(__file__).resolve().parents[2]
    error_fixture = root / "tests/selfhost/fixtures/macro_str_borrow_error_test.ag"
    reference_fixture = root / "tests/selfhost/fixtures/macro_str_literal_ref_error_test.ag"
    literal_fixture = root / "tests/selfhost/fixtures/macro_str_literal_result_test.ag"

    with tempfile.TemporaryDirectory(prefix="silver-macro-borrow-") as temp:
        for index, (label, fixture) in enumerate((
            ("borrowed block result", error_fixture),
            ("reference to a literal-backed local", reference_fixture),
        )):
            error_output = pathlib.Path(temp) / f"borrow-error-{index}"
            error_result = subprocess.run(
                [
                    str(args.stage1.resolve()),
                    str(fixture),
                    "-o",
                    str(error_output),
                    "--no-progress",
                ],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            error_text = error_result.stdout + error_result.stderr
            if error_result.returncode == 0 or "block-result-borrow" not in error_text:
                print(f"stage1 did not reject the {label}", file=sys.stderr)
                print(error_text, file=sys.stderr)
                return 1

        literal_output = pathlib.Path(temp) / "literal-result"
        literal_result = subprocess.run(
            [
                str(args.stage1.resolve()),
                str(literal_fixture),
                "-o",
                str(literal_output),
                "--no-progress",
            ],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
        if literal_result.returncode != 0:
            print("stage1 rejected the literal-backed block result", file=sys.stderr)
            print(literal_result.stdout + literal_result.stderr, file=sys.stderr)
            return 1

        run_result = subprocess.run(
            [str(literal_output)],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )
        if run_result.returncode != 0:
            print("literal-backed block result did not run successfully", file=sys.stderr)
            print(run_result.stdout + run_result.stderr, file=sys.stderr)
            return 1

    print("stage1 rejects local borrows and accepts literal-backed block results")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
