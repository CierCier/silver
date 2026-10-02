#!/usr/bin/env python3
"""Check focused stage1 semantic regressions against stage0 behavior."""

from __future__ import annotations

import argparse
import pathlib
import os
import subprocess


CASES = {
    "nested_generic.ag": (True, True, ""),
    "nested_unknown.ag": (False, False, "unknown type 'Missing'"),
    "scalar_alias_return.ag": (True, True, ""),
    "indexed_holder.ag": (True, True, ""),
    "imported_global_return.ag": (True, True, ""),
    "branch_move_return.ag": (True, True, ""),
    "branch_move_reuse.ag": (
        False, False, "use of moved value 'incoming'"
    ),
    "branch_move_fallthrough.ag": (
        False, False, "use of moved value 'incoming'"
    ),
    "local_reference_return.ag": (
        False, False, "returned reference does not outlive the function"
    ),
    "ambiguous_overload.ag": (
        False, False, "call to overloaded function 'g' is ambiguous"
    ),
    "unsigned_float_cast.ag": (True, True, ""),
}


def check(
    compiler: pathlib.Path, source: pathlib.Path, *, stage1: bool
) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    if stage1:
        env["SILVER_STAGE0"] = "/nonexistent"
    return subprocess.run(
        ([str(compiler), "check", str(source), "--no-cache"] if stage1 else
         [str(compiler), "--no-cache", "check", str(source)]),
        cwd=source.parents[3],
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = pathlib.Path(__file__).parents[2]
    fixtures = root / "tests/selfhost/semantic_regressions"

    for name, (stage0_passes, stage1_passes, diagnostic) in CASES.items():
        source = fixtures / name
        old = check(args.stage0.resolve(), source, stage1=False)
        new = check(args.stage1.resolve(), source, stage1=True)
        if (old.returncode == 0) != stage0_passes:
            raise AssertionError(
                f"stage0 status mismatch for {name}: {old.returncode}\n{old.stderr}"
            )
        if (new.returncode == 0) != stage1_passes:
            raise AssertionError(
                f"stage1 status mismatch for {name}: {new.returncode}\n{new.stderr}"
            )
        if diagnostic and diagnostic not in new.stderr:
            raise AssertionError(
                f"stage1 diagnostic mismatch for {name}: expected {diagnostic!r}\n{new.stderr}"
            )
    print(f"semantic regressions passed: {len(CASES)} fixtures")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
