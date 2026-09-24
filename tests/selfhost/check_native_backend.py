#!/usr/bin/env python3
"""Verify the opt-in stage1 Linux native smoke backend without stage0."""

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
    parser.add_argument("--llc", type=pathlib.Path)
    parser.add_argument("--cc", type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    llc = (args.llc or shutil.which("llc"))
    cc = (args.cc or shutil.which("cc"))
    if llc is None or cc is None or not pathlib.Path(llc).is_file() or not pathlib.Path(cc).is_file():
        raise SystemExit("llc and cc are required for the stage1 native smoke gate")

    with tempfile.TemporaryDirectory(prefix="silver-native-backend-") as temporary:
        root = pathlib.Path(temporary)
        source = root / "main.ag"
        source.write_text("i32 main() {\n    return 42;\n}\n")
        package = root / "package"
        package.mkdir()
        (package / "silver.toml").write_text(
            'name = "native-smoke"\nversion = "0.1.0"\n\n[bin."app"]\nentry = "main.ag"\n'
        )
        (package / "main.ag").write_text(source.read_text())

        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(root / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        env["SILVER_STAGE1_LLC"] = str(pathlib.Path(llc).resolve())
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        direct_output = root / "direct"
        direct = run(stage1, ["build", str(source), "--no-cache", "-o", str(direct_output)], env)
        if direct.returncode != 0:
            raise AssertionError(f"direct native build failed: {direct.returncode}\n{direct.stderr}")
        executed = subprocess.run([str(direct_output)], check=False)
        if executed.returncode != 42:
            raise AssertionError(f"direct native program returned {executed.returncode}, expected 42")

        package_output = root / "package-output"
        package_result = run(
            stage1,
            ["build", str(package / "silver.toml"), "--no-cache", "-o", str(package_output)],
            env,
        )
        if package_result.returncode != 0:
            raise AssertionError(
                f"package native build failed: {package_result.returncode}\n{package_result.stderr}"
            )
        executed = subprocess.run([str(package_output)], check=False)
        if executed.returncode != 42:
            raise AssertionError(f"package native program returned {executed.returncode}, expected 42")

        run_output = root / "run-output"
        run_result = run(
            stage1,
            ["run", str(package / "silver.toml"), "--no-cache", "-o", str(run_output)],
            env,
        )
        if run_result.returncode != 42:
            raise AssertionError(
                f"native run returned {run_result.returncode}, expected 42\n{run_result.stderr}"
            )

        unsupported = root / "unsupported.ag"
        unsupported.write_text("i32 main() {\n    i32 value = 1;\n    return value;\n}\n")
        failed = run(stage1, ["build", str(unsupported), "-o", str(root / "unsupported")], env)
        if failed.returncode == 0 or "stage0 backend unavailable" not in failed.stderr:
            raise AssertionError("unsupported input did not fall back to the explicit bridge")

    print("stage1 native smoke backend passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
