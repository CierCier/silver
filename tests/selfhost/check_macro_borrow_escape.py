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
    alias_fixture = root / "tests/selfhost/fixtures/macro_str_alias_write_error_test.ag"
    fake_writer_fixture = root / "tests/selfhost/fixtures/macro_str_fake_json_writer_error_test.ag"
    reference_fixture = root / "tests/selfhost/fixtures/macro_str_literal_ref_error_test.ag"
    literal_fixture = root / "tests/selfhost/fixtures/macro_str_literal_result_test.ag"
    json_writer_fixture = root / "tests/selfhost/fixtures/macro_str_json_writer_result_test.ag"

    with tempfile.TemporaryDirectory(prefix="silver-macro-borrow-") as temp:
        for index, (label, fixture) in enumerate((
            ("borrowed block result", error_fixture),
            ("string reassigned through a pointer", alias_fixture),
            ("user-defined JsonWriter.finish result", fake_writer_fixture),
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

        for index, (label, fixture) in enumerate((
            ("literal-backed block result", literal_fixture),
            ("detached JsonWriter result", json_writer_fixture),
        )):
            output = pathlib.Path(temp) / f"accepted-result-{index}"
            result = subprocess.run(
                [
                    str(args.stage1.resolve()),
                    str(fixture),
                    "-o",
                    str(output),
                    "--no-progress",
                ],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            if result.returncode != 0:
                print(f"stage1 rejected the {label}", file=sys.stderr)
                print(result.stdout + result.stderr, file=sys.stderr)
                return 1

            run_result = subprocess.run(
                [str(output)],
                cwd=root,
                capture_output=True,
                text=True,
                check=False,
            )
            if run_result.returncode != 0:
                print(f"{label} did not run successfully", file=sys.stderr)
                print(run_result.stdout + run_result.stderr, file=sys.stderr)
                return 1

    print("stage1 rejects local and aliased borrows and accepts safe block results")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
