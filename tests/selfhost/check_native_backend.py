#!/usr/bin/env python3
"""Verify the opt-in stage1 Linux native smoke backend without stage0."""

from __future__ import annotations

import argparse
import os
import pathlib
import shutil
import subprocess
import tempfile


def run(
    stage1: pathlib.Path, args: list[str], env: dict[str, str], cwd: pathlib.Path
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), *args],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        cwd=cwd,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    parser.add_argument("--cc", type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    cc = (args.cc or shutil.which("cc"))
    if cc is None or not pathlib.Path(cc).is_file():
        raise SystemExit("cc is required for the stage1 native smoke gate")

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
        env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

        lint_source = root / "lint-shadow.ag"
        lint_source.write_text(
            "i32 main() {\n"
            "    let value = 1;\n"
            "    {\n"
            "        let value = 2;\n"
            "        if (value == 2) { return 1; }\n"
            "    }\n"
            "    return 0;\n"
            "}\n"
        )
        lint_result = run(stage1, ["check", str(lint_source)], env, root)
        if (
            lint_result.returncode != 0
            or "unused variable 'value'" not in lint_result.stderr
            or ":2:9:" not in lint_result.stderr
        ):
            raise AssertionError(
                "stage1 linter did not report the unused outer shadowed binding\n"
                f"stdout:\n{lint_result.stdout}\nstderr:\n{lint_result.stderr}"
            )
        lint_error = run(stage1, ["check", str(lint_source), "-Werror"], env, root)
        if lint_error.returncode == 0 or "error:" not in lint_error.stderr:
            raise AssertionError(
                "stage1 -Werror did not turn the unused-binding warning into an error\n"
                f"stdout:\n{lint_error.stdout}\nstderr:\n{lint_error.stderr}"
            )

        direct_output = root / "direct"
        direct = run(stage1, ["build", str(source), "--no-cache", "-o", str(direct_output)], env, root)
        if direct.returncode != 0:
            raise AssertionError(f"direct native build failed: {direct.returncode}\n{direct.stderr}")
        executed = subprocess.run([str(direct_output)], check=False)
        if executed.returncode != 42:
            raise AssertionError(f"direct native program returned {executed.returncode}, expected 42")
        cfg_source = root / "cfg.ag"
        cfg_source.write_text(
            "i32 main() {\n"
            "    if (@cfg(os.linux)) {\n"
            "        return 42;\n"
            "    }\n"
            "    return 0;\n"
            "}\n"
        )
        cfg_output = root / "cfg"
        cfg_build = run(
            stage1, ["build", str(cfg_source), "--no-cache", "-o", str(cfg_output)], env, root
        )
        if cfg_build.returncode != 0:
            raise AssertionError(f"native cfg build failed: {cfg_build.returncode}\n{cfg_build.stderr}")
        executed = subprocess.run([str(cfg_output)], check=False)
        if executed.returncode != 42:
            raise AssertionError(f"native cfg program returned {executed.returncode}, expected 42")

        argv_source = root / "argv.ag"
        argv_source.write_text(
            'extern "C" {\n'
            "    i32 __silver_argc;\n"
            "    i8** __silver_argv;\n"
            "}\n"
            "i32 main() {\n"
            "    if (__silver_argc == 2 && __silver_argv[1] != (i8*)0) {\n"
            "        return 42;\n"
            "    }\n"
            "    return 1;\n"
            "}\n"
        )
        argv_output = root / "argv"
        argv_build = run(
            stage1, ["build", str(argv_source), "--no-cache", "-o", str(argv_output)], env, root
        )
        if argv_build.returncode != 0:
            raise AssertionError(f"native argv build failed: {argv_build.returncode}\n{argv_build.stderr}")
        executed = subprocess.run([str(argv_output), "one"], check=False)
        if executed.returncode != 42:
            raise AssertionError(f"native argv program returned {executed.returncode}, expected 42")

        package_output = root / "package-output"
        package_result = run(
            stage1,
            ["build", str(package / "silver.toml"), "--no-cache", "-o", str(package_output)],
            env,
            root,
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
            root,
        )
        if run_result.returncode != 42:
            raise AssertionError(
                f"native run returned {run_result.returncode}, expected 42\n{run_result.stderr}"
            )

        comptime_cast = root / "comptime-cast.ag"
        comptime_cast.write_text(
            "i32 main() {\n"
            "    return (i32)comptime (i8) 255;\n"
            "}\n"
        )
        comptime_output = root / "comptime-cast"
        comptime_build = run(
            stage1,
            ["build", str(comptime_cast), "--no-cache", "-o", str(comptime_output)],
            env,
            root,
        )
        if comptime_build.returncode != 0:
            raise AssertionError(
                f"native comptime cast build failed: {comptime_build.returncode}\n"
                f"{comptime_build.stderr}"
            )
        executed = subprocess.run([str(comptime_output)], check=False)
        if executed.returncode != 255:
            raise AssertionError(
                f"native comptime narrowing returned {executed.returncode}, expected 255 (-1)"
            )
        comptime_bool = root / "comptime-bool.ag"
        comptime_bool.write_text(
            "i32 main() {\n"
            "    return comptime (i32) true;\n"
            "}\n"
        )
        bool_output = root / "comptime-bool"
        bool_build = run(
            stage1,
            ["build", str(comptime_bool), "--no-cache", "-o", str(bool_output)],
            env,
            root,
        )
        if bool_build.returncode != 0:
            raise AssertionError(
                f"native comptime bool cast build failed: {bool_build.returncode}\n"
                f"{bool_build.stderr}"
            )
        executed = subprocess.run([str(bool_output)], check=False)
        if executed.returncode != 1:
            raise AssertionError(
                f"native comptime bool cast returned {executed.returncode}, expected 1"
            )

        function_pointer_global = root / "function-pointer-global.ag"
        function_pointer_global.write_text(
            "i32 plus_one(i32 value) { return value + 1; }\n"
            "i32(i32) callback = &plus_one;\n"
            "i32 main() { return callback(41); }\n"
        )
        rejected_global = run(
            stage1,
            [
                "build",
                str(function_pointer_global),
                "--no-cache",
                "-o",
                str(root / "function-pointer-global"),
            ],
            env,
            root,
        )
        if (
            rejected_global.returncode == 0
            or "global-init 'callback'" not in rejected_global.stderr
        ):
            raise AssertionError(
                "unsupported global function-pointer initializer did not fail closed\n"
                f"return code: {rejected_global.returncode}\n"
                f"stdout:\n{rejected_global.stdout}\nstderr:\n{rejected_global.stderr}"
            )

        malformed_parameter = root / "malformed-function-pointer.ag"
        malformed_parameter.write_text(
            "i32 apply(i32(i32(,)) callback) { return 0; }\n"
            "i32 main() { return 0; }\n"
        )
        rejected_parameter = run(
            stage1,
            [
                "build",
                str(malformed_parameter),
                "--no-cache",
                "-o",
                str(root / "malformed-function-pointer"),
            ],
            env,
            root,
        )
        if (
            rejected_parameter.returncode == 0
            or "parameter-type 'callback'" not in rejected_parameter.stderr
        ):
            raise AssertionError(
                "malformed nested function-pointer parameter did not fail closed\n"
                f"return code: {rejected_parameter.returncode}\n"
                f"stdout:\n{rejected_parameter.stdout}\nstderr:\n{rejected_parameter.stderr}"
            )

        unsupported = root / "unsupported.ag"
        unsupported.write_text(
            "i32 main() {\n"
            "    defer { }\n"
            "    return 0;\n"
            "}\n"
        )
        failed = run(stage1, ["build", str(unsupported), "-o", str(root / "unsupported")], env, root)
        if failed.returncode == 0 or "stage0 backend unavailable" not in failed.stderr:
            raise AssertionError("unsupported input did not fall back to the explicit bridge")

    print("stage1 native smoke backend passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
