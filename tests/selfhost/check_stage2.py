#!/usr/bin/env python3
"""Build stage2 natively, then exercise its frontend and compiler without stage0."""

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
    with tempfile.TemporaryDirectory(prefix="silver-stage2-") as temporary:
        work = pathlib.Path(temporary)
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(work / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        stage2 = work / "agc-stage2"

        def invoke(binary: pathlib.Path, arguments: list[str]) -> subprocess.CompletedProcess[str]:
            result = subprocess.run(
                [str(binary), *arguments], cwd=root, env=env,
                capture_output=True, text=True, timeout=300, check=False,
            )
            if result.returncode != 0:
                raise AssertionError(
                    f"{binary.name} {arguments!r} failed: {result.returncode}\n"
                    f"{result.stdout}\n{result.stderr}"
                )
            return result

        invoke(args.stage1.resolve(), ["build", str(root / "silver.toml"),
                                      "--bin", "agc", "--no-cache", "-o", str(stage2)])
        invoke(stage2, ["--version"])
        invoke(stage2, ["--help"])
        source = work / "main.ag"
        # Exceed the 128-token capacity that previously corrupted Vec<Token>.
        declarations = "".join(f"    i64 value_{index} = {index};\n" for index in range(64))
        source.write_text("i32 main() {\n" + declarations + "    return 42;\n}\n", encoding="utf-8")
        for command in ["lex", "parse", "check"]:
            invoke(stage2, [command, str(source)])
        app = work / "app"
        invoke(stage2, ["build", str(source), "--no-cache", "-o", str(app)])
        executed = subprocess.run([str(app)], timeout=30, check=False)
        if executed.returncode != 42:
            raise AssertionError(f"stage2-compiled program returned {executed.returncode}, expected 42")
    print("native stage2 build, frontend commands, and program execution passed without stage0")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
