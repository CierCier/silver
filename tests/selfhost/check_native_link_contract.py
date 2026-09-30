#!/usr/bin/env python3
"""Gate stage1 Linux native linker selection, link arguments, and run argv."""

from __future__ import annotations

import json
import os
import pathlib
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
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()

    with tempfile.TemporaryDirectory(prefix="silver-native-link-contract-") as temporary:
        root = pathlib.Path(temporary)
        source = root / "main.ag"
        source.write_text("i32 main() { return 0; }\n")
        capture = root / "link.json"
        runtime = root / "runtime.json"
        linker = root / "fake-linker"
        linker.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, pathlib, sys\n"
            "args = sys.argv[1:]\n"
            "pathlib.Path(os.environ['LINK_CAPTURE']).write_text(json.dumps({'executable': sys.argv[0], 'args': args}))\n"
            "out = pathlib.Path(args[args.index('-o') + 1])\n"
            "out.write_text('#!/usr/bin/env python3\\nimport json, os, pathlib, sys\\n'"
            "+ \"pathlib.Path(os.environ['RUNTIME_CAPTURE']).write_text(json.dumps(sys.argv[1:]))\\n\")\n"
            "out.chmod(0o755)\n"
        )
        linker.chmod(0o755)
        env = os.environ.copy()
        env.update({
            "SILVER_STAGE0": str(root / "missing-stage0"),
            "SILVER_STAGE1_NATIVE": "1",
            "SILVER_LINKER": str(linker),
            "SILVER_USE_MOLD": "1",
            "SILVER_DYNAMIC_LINKER": "/test/ld-linux.so",
            "LINK_CAPTURE": str(capture),
            "RUNTIME_CAPTURE": str(runtime),
        })
        output = root / "app"
        result = run(
            stage1,
            ["run", str(source), "--no-cache", "-o", str(output), "-L", str(root),
             "-l", "sample", "--run-arg", "one", "--run-arg", "two words"],
            env,
            root,
        )
        if result.returncode != 0:
            raise AssertionError(f"stage1 native link/run failed: {result.returncode}\n{result.stderr}")
        if not output.is_file():
            raise AssertionError("selected linker did not receive/create requested output")
        invocation = json.loads(capture.read_text())
        if invocation["executable"] != str(linker):
            raise AssertionError(f"SILVER_LINKER executable was not selected: {invocation!r}")
        argv = invocation["args"]
        required = ["-o", str(output), "-L", str(root), "-rpath", str(root), "-l", "sample",
                    "--allow-shlib-undefined", "--dynamic-linker", "/test/ld-linux.so"]
        for item in required:
            if item not in argv:
                raise AssertionError(f"linker argv omitted {item!r}: {argv!r}")
        if "-static" in argv or not any(item.endswith(".o") for item in argv):
            raise AssertionError(f"unexpected non-Linux-linker arguments: {argv!r}")
        if json.loads(runtime.read_text()) != ["one", "two words"]:
            raise AssertionError(f"run arguments were not forwarded exactly: {runtime.read_text()}")

        static_output = root / "static-app"
        static_result = run(
            stage1,
            ["build", str(source), "--no-cache", "--static", "-o", str(static_output)],
            env,
            root,
        )
        if static_result.returncode != 0:
            raise AssertionError(f"static stage1 link failed: {static_result.returncode}\n{static_result.stderr}")
        static_invocation = json.loads(capture.read_text())
        static_argv = static_invocation["args"]
        if "-static" not in static_argv or "--dynamic-linker" in static_argv:
            raise AssertionError(f"static link did not suppress dynamic-linker setup: {static_argv!r}")
        env.pop("SILVER_LINKER")
        env["SILVER_STAGE1_MOLD"] = str(linker)
        mold_output = root / "mold-app"
        mold_result = run(
            stage1,
            ["build", str(source), "--no-cache", "-o", str(mold_output)],
            env,
            root,
        )
        if mold_result.returncode != 0:
            raise AssertionError(f"mold-selected stage1 link failed: {mold_result.returncode}\n{mold_result.stderr}")
        mold_invocation = json.loads(capture.read_text())
        if mold_invocation["executable"] != str(linker) or not mold_output.is_file():
            raise AssertionError(f"SILVER_USE_MOLD did not select the configured mold executable: {mold_invocation!r}")

    print("stage1 native linker contract passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
