#!/usr/bin/env python3
"""Exercise stage1 native @assert lowering, source locations, and release pruning."""

from __future__ import annotations

import argparse
import os
import pathlib
import shutil
import signal
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


def build_and_run(
    stage1: pathlib.Path,
    fixture: pathlib.Path,
    output: pathlib.Path,
    env: dict[str, str],
    *flags: str,
) -> subprocess.CompletedProcess[str]:
    built = run(stage1, ["build", str(fixture), "--no-cache", *flags, "-o", str(output)], env)
    if built.returncode != 0:
        raise AssertionError(f"native @assert build failed: {built.returncode}\n{built.stderr}")
    return subprocess.run([str(output)], text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, check=False)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    parser.add_argument("--cc", type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    cc = args.cc or shutil.which("cc")
    if cc is None or not pathlib.Path(cc).is_file():
        raise SystemExit("cc is required for the native @assert checker")

    here = pathlib.Path(__file__).parent
    env = os.environ.copy()
    with tempfile.TemporaryDirectory(prefix="silver-native-assert-") as temporary:
        temp = pathlib.Path(temporary)
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        root_fixture = here / "assert_native_root_fixture.ag"
        root_run = build_and_run(stage1, root_fixture, temp / "root-assert", env)
        expected_root = f"assertion failed at {root_fixture.resolve()}:4: root assert marker"
        if root_run.returncode != -signal.SIGABRT or expected_root not in root_run.stderr:
            raise AssertionError(
                f"root @assert mismatch: rc={root_run.returncode}, stderr={root_run.stderr!r}, "
                f"expected={expected_root!r}"
            )

        imported_fixture = here / "assert_native_import_fixture.ag"
        imported_run = build_and_run(stage1, imported_fixture, temp / "imported-assert", env)
        helper = here / "assert_native_helper.ag"
        expected_imported = f"assertion failed at {helper.resolve()}:2: imported assert marker"
        if imported_run.returncode != -signal.SIGABRT or expected_imported not in imported_run.stderr:
            raise AssertionError(
                f"imported @assert mismatch: rc={imported_run.returncode}, "
                f"stderr={imported_run.stderr!r}, expected={expected_imported!r}"
            )

        release_fixture = here / "assert_native_release_fixture.ag"
        release_run = build_and_run(stage1, release_fixture, temp / "release-assert", env, "-O2")
        if release_run.returncode != 0:
            raise AssertionError(
                f"release @assert was not pruned: rc={release_run.returncode}, "
                f"stdout={release_run.stdout!r}, stderr={release_run.stderr!r}"
            )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
