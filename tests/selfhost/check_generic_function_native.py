#!/usr/bin/env python3
"""Run structural free-function specializations with the native backend."""

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
        for name, source, diagnostic in (
            (
                "macro_arity",
                "macro i32 first(i32 value) { return value; }\n"
                "i32 invoke<T>(T value) { return @first(0, 1); }\n"
                "i32 main() { return invoke((i32)0); }\n",
                "macro '@first' expects 1 argument(s), got 2",
            ),
            (
                "char_pointer",
                "void put(char* target) { target[0] = (char)0x1f642; }\n"
                "void forward<T>(T value) { put(value); }\n"
                "i32 main() { u8 bytes[1] = {0}; forward(&bytes[0]); return 0; }\n",
                "pointer argument type mismatch: expected char*, found &u8",
            ),
        ):
            rejected_source = pathlib.Path(temporary) / f"{name}.ag"
            rejected_source.write_text(source, encoding="utf-8")
            checked = subprocess.run(
                [str(stage1), "check", str(rejected_source), "--no-cache"],
                env=env, capture_output=True, text=True, timeout=30, check=False,
            )
            if checked.returncode != 2 or diagnostic not in checked.stdout + checked.stderr:
                raise AssertionError(
                    f"{name} generic-call diagnostic failed: rc={checked.returncode}\n"
                    f"{checked.stdout}{checked.stderr}"
                )
    print("stage1 native structural generic free functions passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
