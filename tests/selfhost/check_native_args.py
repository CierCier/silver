#!/usr/bin/env python3
"""Exercise native argv passthrough plus generic Vec<String> methods.

Builds args_native_fixture.ag with the stage1 native backend, runs it with two
arguments, and checks stdout plus exit code.
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


def run(
    stage1: pathlib.Path,
    args: list[str],
    env: dict[str, str],
    cwd: pathlib.Path | None = None,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), *args],
        cwd=cwd,
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

        forwarded = run(
            stage1,
            [
                "run",
                str(fixture),
                "--run-arg",
                "--no-cache",
                "--run-arg",
                "beta",
            ],
            env,
        )
        expected_forwarded = "--no-cache\nbeta\n"
        if forwarded.returncode != 42 or forwarded.stdout != expected_forwarded:
            raise AssertionError(
                f"program arguments after -- changed: rc={forwarded.returncode}, "
                f"stdout={forwarded.stdout!r}, stderr={forwarded.stderr!r}"
            )

        separated = run(stage1, ["run", str(fixture), "--", "alpha", "--no-cache"], env)
        expected_separated = "alpha\n--no-cache\n"
        if separated.returncode != 42 or separated.stdout != expected_separated:
            raise AssertionError(
                f"arguments after -- were not forwarded: rc={separated.returncode}, "
                f"stdout={separated.stdout!r}, stderr={separated.stderr!r}"
            )

        output_directory = pathlib.Path(temporary) / "output-option"
        output_directory.mkdir()
        output_value = run(
            stage1,
            ["build", str(fixture), "-o", "--no-cache"],
            env,
            cwd=output_directory,
        )
        if output_value.returncode != 0 or not (output_directory / "--no-cache").is_file():
            raise AssertionError(
                f"--no-cache was removed as an -o value: rc={output_value.returncode}, "
                f"stderr={output_value.stderr!r}"
            )
    print("stage1 native argv passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
