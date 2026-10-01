#!/usr/bin/env python3
"""Check LLVM target ABI sizes in a native stage1 build."""

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
    root = pathlib.Path(__file__).parents[2]
    fixture = pathlib.Path(__file__).with_name("layout_native_fixture.ag")
    with tempfile.TemporaryDirectory(prefix="silver-native-layout-") as temporary:
        output = pathlib.Path(temporary) / "layout"
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(pathlib.Path(temporary) / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        built = subprocess.run(
            [str(args.stage1.resolve()), "build", str(fixture), "--no-cache", "-o", str(output)],
            cwd=root, env=env, capture_output=True, text=True, timeout=180, check=False,
        )
        if built.returncode != 0:
            raise AssertionError(f"native layout build failed: {built.returncode}\n{built.stderr}")
        executed = subprocess.run([str(output)], capture_output=True, text=True, timeout=30, check=False)
        if executed.returncode != 0 or executed.stdout or executed.stderr:
            raise AssertionError(
                f"target layout assertions failed: rc={executed.returncode}, "
                f"stdout={executed.stdout!r}, stderr={executed.stderr!r}"
            )
    print("stage1 native target ABI sizes passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
