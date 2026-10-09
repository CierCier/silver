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
    fixture = root / "tests/selfhost/fixtures/macro_str_borrow_error_test.ag"

    with tempfile.TemporaryDirectory(prefix="silver-macro-borrow-") as temp:
        output_path = pathlib.Path(temp) / "app"
        result = subprocess.run(
            [
                str(args.stage1.resolve()),
                str(fixture),
                "-o",
                str(output_path),
                "--no-progress",
            ],
            cwd=root,
            capture_output=True,
            text=True,
            check=False,
        )

    output = result.stdout + result.stderr
    if result.returncode == 0 or "block-result-borrow" not in output:
        print("stage1 did not reject the borrowed block result", file=sys.stderr)
        print(output, file=sys.stderr)
        return 1
    print("stage1 rejects borrowed block results that reference block-local storage")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
