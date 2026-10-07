#!/usr/bin/env python3
"""Exercise stage1's launch Send classification without invoking stage0."""

import argparse
import os
import pathlib
import subprocess
import tempfile


def check(stage1: pathlib.Path, fixture: pathlib.Path, expected: bool,
          expected_types: tuple[str, ...] = ()) -> None:
    env = os.environ.copy()
    env["SILVER_STAGE0"] = "/nonexistent/silver-stage0"
    env["SILVER_STAGE1_NATIVE"] = "1"
    result = subprocess.run(
        [str(stage1), "check", str(fixture)],
        cwd=fixture.parents[1],
        env=env,
        capture_output=True,
        text=True,
        timeout=120,
        check=False,
    )
    output = result.stdout + result.stderr
    if (result.returncode == 0) != expected:
        raise AssertionError(
            f"stage1 Send check for {fixture.name} returned {result.returncode}:\n{output}"
        )
    for type_name in expected_types:
        diagnostic = f"launch argument of type {type_name} is not Send"
        if diagnostic not in output:
            raise AssertionError(
                f"stage1 Send diagnostic missing {diagnostic!r}:\n{output}"
            )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = pathlib.Path(__file__).resolve().parents[2]
    fixtures = root / "tests"
    check(args.stage1.resolve(), fixtures / "launch_send_test.ag", True)
    check(
        args.stage1.resolve(),
        fixtures / "launch_send_error_test.ag",
        False,
        (
            "Vec<Rc<i64>>",
            "Box<Rc<i64>>",
            "HashMap<i64, Rc<i64>>",
        ),
    )
    stage1 = args.stage1.resolve()
    fixture = fixtures / "launch_send_test.ag"
    env = os.environ.copy()
    env["SILVER_STAGE0"] = "/nonexistent/silver-stage0"
    env["SILVER_STAGE1_NATIVE"] = "1"
    with tempfile.TemporaryDirectory(prefix="silver-send-build-") as temporary:
        output = pathlib.Path(temporary) / "send-test"
        result = subprocess.run(
            [str(stage1), "build", str(fixture), "--no-cache", "-o", str(output)],
            cwd=root,
            env=env,
            capture_output=True,
            text=True,
            timeout=180,
            check=False,
        )
        diagnostics = result.stdout + result.stderr
        if "launch argument of type" in diagnostics:
            raise AssertionError(
                "native import-closure Send check rejected the positive fixture:\n"
                f"{diagnostics}"
            )
        if result.returncode != 0 and "stage0 backend unavailable" not in diagnostics:
            raise AssertionError(
                f"native Send build failed before the Launch fallback:\n{diagnostics}"
            )
        if result.returncode == 0:
            executed = subprocess.run([str(output)], timeout=60, check=False)
            if executed.returncode != 0:
                raise AssertionError(
                    f"native launch fixture returned {executed.returncode}, expected 0"
                )
    print("stage1 native Send gate passed")


if __name__ == "__main__":
    main()
