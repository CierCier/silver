#!/usr/bin/env python3
"""Check concrete generic enum layouts through stage1's native backend."""

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
        tuple_method = pathlib.Path(temporary) / "tuple-method.ag"
        tuple_method.write_text(
            'import std.string;\n'
            'i32 main() {\n'
            '    String text = String.from_str("left:right");\n'
            '    Optional<[String, String]> parts = text.split_once(":");\n'
            '    if (!parts.is_some()) { return 1; }\n'
            '    let [left, right] = parts.unwrap();\n'
            '    bool ok = left == "left" && right == "right";\n'
            '    left.drop(); right.drop(); text.drop();\n'
            '    if (ok) { return 42; }\n'
            '    return 2;\n'
            '}\n', encoding="utf-8",
        )
        built_tuple_method = subprocess.run(
            [str(args.stage1.resolve()), "build", str(tuple_method), "--no-cache", "-o", str(output)],
            env=env, capture_output=True, text=True, timeout=120, check=False,
        )
        if built_tuple_method.returncode != 0:
            raise AssertionError(
                "native tuple-returning method build failed: "
                f"rc={built_tuple_method.returncode}\n{built_tuple_method.stderr}"
            )
        executed_tuple_method = subprocess.run(
            [str(output)], capture_output=True, text=True, timeout=30, check=False,
        )
        if executed_tuple_method.returncode != 42 or executed_tuple_method.stdout or executed_tuple_method.stderr:
            raise AssertionError(
                "native tuple-returning method result was incorrect: "
                f"rc={executed_tuple_method.returncode}, "
                f"stdout={executed_tuple_method.stdout!r}, "
                f"stderr={executed_tuple_method.stderr!r}"
            )
    print("stage1 native generic enum layouts passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
