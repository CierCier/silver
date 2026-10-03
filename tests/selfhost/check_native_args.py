#!/usr/bin/env python3
"""Exercise native argv passthrough plus generic Vec<String> methods.

Builds args_native_fixture.ag with the stage1 native backend (no stage0
fallback), runs it with two arguments, and checks stdout plus exit code.
This covers std.args:args(), Vec<String>::len/get monomorphization, and
chained .to_str() calls on rvalue receivers.
"""

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
        raise SystemExit("cc is required for the native argv checker")

    fixture = pathlib.Path(__file__).with_name("args_native_fixture.ag")
    with tempfile.TemporaryDirectory(prefix="silver-native-args-") as temporary:
        output = pathlib.Path(temporary) / "args-native"
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(pathlib.Path(temporary) / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        built = run(stage1, ["build", str(fixture), "--no-cache", "-o", str(output)], env)
        if built.returncode != 0:
            raise AssertionError(f"native argv build failed: {built.returncode}\n{built.stderr}")

        executed = subprocess.run(
            [str(output), "alpha", "beta"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        expected = "alpha\nbeta\n"
        if executed.returncode != 42 or executed.stdout != expected:
            raise AssertionError(
                f"native argv output mismatch: rc={executed.returncode}, "
                f"stdout={executed.stdout!r}, stderr={executed.stderr!r}, expected={expected!r}"
            )
    print("stage1 native argv passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
