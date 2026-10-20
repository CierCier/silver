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
        temporary_path = pathlib.Path(temporary)
        output = temporary_path / "args-native"
        env = os.environ.copy()
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        built = run(stage1, ["--no-cache", "build", str(fixture), "-o", str(output)], env)
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

        command_named_source = temporary_path / "build.ag"
        shutil.copyfile(fixture, command_named_source)
        command_named_output = temporary_path / "command-named-source"
        named_source_build = run(
            stage1,
            ["build", str(command_named_source), "--no-cache", "-o", str(command_named_output)],
            env,
        )
        if named_source_build.returncode != 0 or not command_named_output.is_file():
            raise AssertionError(
                f"source path named like a command was misread: rc={named_source_build.returncode}, "
                f"stderr={named_source_build.stderr!r}"
            )

        command_named_output_dir = temporary_path / "output-option"
        command_named_output_dir.mkdir()
        output_value = run(
            stage1,
            ["--output", "build", "build", str(fixture), "--no-cache"],
            env,
            cwd=command_named_output_dir,
        )
        if output_value.returncode != 0 or not (command_named_output_dir / "build").is_file():
            raise AssertionError(
                f"option value equal to a command name changed parsing: rc={output_value.returncode}, "
                f"stderr={output_value.stderr!r}"
            )

        workspace = temporary_path / "workspace"
        workspace.mkdir()
        (workspace / "main.ag").write_text("i32 main() { return 0; }\n")
        (workspace / "build").write_text(
            'name = "fixture"\nversion = "0.1.0"\n\n[bin.app]\nentry = "main.ag"\n'
        )
        workspace_output = workspace / "app"
        workspace_build = run(
            stage1,
            ["--manifest", "build", "build", "--no-cache", "-o", str(workspace_output)],
            env,
            cwd=workspace,
        )
        if workspace_build.returncode != 0 or not workspace_output.is_file():
            raise AssertionError(
                f"workspace manifest value equal to a command name changed parsing: "
                f"rc={workspace_build.returncode}, stderr={workspace_build.stderr!r}"
            )

        checked = run(stage1, ["--no-cache", "check", str(fixture)], env)
        if checked.returncode != 0:
            raise AssertionError(f"leading-option check failed: {checked.returncode}\n{checked.stderr}")

        include_value = temporary_path / "check"
        include_value.mkdir()
        checked_with_value = run(
            stage1,
            ["--include-path", "check", "check", str(fixture)],
            env,
            cwd=temporary_path,
        )
        if checked_with_value.returncode != 0:
            raise AssertionError(
                f"check after an option operand named like a command failed: "
                f"{checked_with_value.returncode}\n{checked_with_value.stderr}"
            )

        forwarded = run(
            stage1,
            [
                "--no-cache",
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

        run_output_value = run(
            stage1,
            ["--output", "run", "run", str(fixture), "--", "alpha", "beta"],
            env,
            cwd=temporary_path,
        )
        if run_output_value.returncode != 42 or run_output_value.stdout != "alpha\nbeta\n":
            raise AssertionError(
                f"run after an option operand named like a command failed: "
                f"rc={run_output_value.returncode}, stdout={run_output_value.stdout!r}, "
                f"stderr={run_output_value.stderr!r}"
            )

        separated = run(stage1, ["run", str(fixture), "--", "alpha", "--no-cache"], env)
        expected_separated = "alpha\n--no-cache\n"
        if separated.returncode != 42 or separated.stdout != expected_separated:
            raise AssertionError(
                f"arguments after -- were not forwarded: rc={separated.returncode}, "
                f"stdout={separated.stdout!r}, stderr={separated.stderr!r}"
            )

        output_directory = temporary_path / "flag-output-option"
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
