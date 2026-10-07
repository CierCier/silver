#!/usr/bin/env python3
"""Exercise native LLVM lowering of @format through the stage1 compiler."""

from __future__ import annotations

import argparse
import os
import pathlib
import shutil
import subprocess
import tempfile


def run(stage1: pathlib.Path, args: list[str], env: dict[str, str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), *args],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    parser.add_argument("--cc", type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    cc = args.cc or shutil.which("cc")
    if cc is None or not pathlib.Path(cc).is_file():
        raise SystemExit("cc is required for the native @format checker")

    fixture = pathlib.Path(__file__).with_name("format_native_fixture.ag")
    with tempfile.TemporaryDirectory(prefix="silver-native-format-") as temporary:
        output = pathlib.Path(temporary) / "format-native"
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(pathlib.Path(temporary) / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        built = run(stage1, ["build", str(fixture), "--no-cache", "-o", str(output)], env)
        if built.returncode != 0:
            raise AssertionError(f"native @format build failed: {built.returncode}\n{built.stderr}")

        executed = subprocess.run(
            [str(output)], text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, check=False
        )
        expected = (
            "escaped {}: -12 | 34 | 1.25 | true | x\n"
            "label: 73\nstatus: Ready\nprint:7|println:8\n"
        )
        expected_stderr = "eprint:9|eprintln:10\n"
        if (
            executed.returncode != 0
            or executed.stdout != expected
            or executed.stderr != expected_stderr
        ):
            raise AssertionError(
                f"native @format output mismatch: rc={executed.returncode}, "
                f"stdout={executed.stdout!r}, expected_stdout={expected!r}, "
                f"stderr={executed.stderr!r}, expected_stderr={expected_stderr!r}"
            )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
