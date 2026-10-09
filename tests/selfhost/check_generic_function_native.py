#!/usr/bin/env python3
"""Run structural free-function specializations without a stage0 fallback."""

from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import tempfile


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    fixture = pathlib.Path(__file__).with_name("generic_function_native_fixture.ag")
    with tempfile.TemporaryDirectory(prefix="silver-native-generic-fn-") as temporary:
        output = pathlib.Path(temporary) / "generic-function"
        env = os.environ.copy()
        built = subprocess.run(
            [str(stage1), "build", str(fixture), "--no-cache", "-o", str(output)],
            env=env, capture_output=True, text=True, timeout=120, check=False,
        )
        if built.returncode != 0:
            raise AssertionError(f"generic free-function build failed: {built.returncode}\n{built.stderr}")
        executed = subprocess.run(
            [str(output)], capture_output=True, text=True, timeout=30, check=False,
        )
        if executed.returncode != 42 or executed.stdout or executed.stderr:
            raise AssertionError(
                f"generic free-function runtime failed: rc={executed.returncode}, "
                f"stdout={executed.stdout!r}, stderr={executed.stderr!r}"
            )
    print("stage1 native structural generic free functions passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
