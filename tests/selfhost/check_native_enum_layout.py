#!/usr/bin/env python3
"""Check concrete generic enum layouts with the stage0 bridge unavailable."""

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
    fixture = pathlib.Path(__file__).with_name("enum_layout_native_fixture.ag")
    with tempfile.TemporaryDirectory(prefix="silver-native-enum-") as temporary:
        output = pathlib.Path(temporary) / "enum-layout"
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(pathlib.Path(temporary) / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        built = subprocess.run(
            [str(args.stage1.resolve()), "build", str(fixture), "--no-cache", "-o", str(output)],
            env=env, capture_output=True, text=True, timeout=120, check=False,
        )
        if built.returncode != 0:
            raise AssertionError(f"native enum build failed: {built.returncode}\n{built.stderr}")
        executed = subprocess.run(
            [str(output)], capture_output=True, text=True, timeout=30, check=False,
        )
        expected = "enum payload\nresult payload\nerror payload\n"
        if executed.returncode != 42 or executed.stdout != expected or executed.stderr:
            raise AssertionError(
                f"native enum round trip failed: rc={executed.returncode}, "
                f"stdout={executed.stdout!r}, stderr={executed.stderr!r}"
            )
        unsupported = pathlib.Path(temporary) / "unsupported.ag"
        unsupported.write_text(
            'import std.string;\n'
            'i32 main() {\n'
            '    String text = String.from_str("a:b");\n'
            '    text.split_once(":");\n'
            '    return 0;\n'
            '}\n', encoding="utf-8",
        )
        rejected = subprocess.run(
            [str(args.stage1.resolve()), "build", str(unsupported), "--no-cache", "-o", str(output)],
            env=env, capture_output=True, text=True, timeout=120, check=False,
        )
        if rejected.returncode != 2 or "stage0 backend unavailable" not in rejected.stderr:
            raise AssertionError(
                f"unsupported tuple method did not fail closed: rc={rejected.returncode}\n"
                f"{rejected.stderr}"
            )
    print("stage1 native generic enum layouts passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
