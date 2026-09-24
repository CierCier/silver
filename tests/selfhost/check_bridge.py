#!/usr/bin/env python3
"""Check the transitional stage1 command bridge's process boundary."""

from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import tempfile


def run(binary: pathlib.Path, args: list[str], cwd: pathlib.Path, env: dict[str, str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(binary), *args],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    if not stage1.is_file():
        parser.error(f"stage1 compiler does not exist: {stage1}")

    root = pathlib.Path(__file__).parents[2]
    source = root / "tests/args_test.ag"
    with tempfile.TemporaryDirectory(prefix="silver-bridge-") as temp_name:
        temp = pathlib.Path(temp_name)
        marker = temp / "argv"
        backend = temp / "fake-agc"
        backend.write_text(
            "#!/usr/bin/env bash\n"
            "test \"${SILVER_BRIDGE_SENTINEL:-}\" = present || exit 91\n"
            f"printf '%s\\n' \"$@\" > {marker}\n"
            "exit 0\n",
            encoding="utf-8",
        )
        backend.chmod(0o755)
        env = os.environ.copy()
        env["SILVER_BRIDGE_SENTINEL"] = "present"
        env["SILVER_STAGE0"] = str(backend)

        local = run(stage1, ["lex", str(source)], root, env)
        if local.returncode != 0 or marker.exists():
            raise AssertionError("local lex unexpectedly invoked the backend")

        no_args = run(stage1, [], root, env)
        if no_args.returncode != 0:
            raise AssertionError("empty command did not reach the backend")

        native = run(stage1, [str(source), "-o", str(temp / "app")], root, env)
        if native.returncode != 0 or not marker.is_file():
            raise AssertionError("native command did not reach the backend")
        forwarded = marker.read_text(encoding="utf-8").splitlines()
        if forwarded != [str(source), "-o", str(temp / "app")]:
            raise AssertionError(f"argv changed while forwarding: {forwarded!r}")

        signal_backend = temp / "signal-agc"
        signal_backend.write_text("#!/usr/bin/env bash\nkill -TERM $$\n", encoding="utf-8")
        signal_backend.chmod(0o755)
        env["SILVER_STAGE0"] = str(signal_backend)
        signaled = run(stage1, [str(source)], root, env)
        if signaled.returncode != 143:
            raise AssertionError(f"signal status was not preserved: {signaled.returncode}")

        env.pop("SILVER_STAGE0", None)
        missing = run(stage1, [str(source)], temp, env)
        if missing.returncode == 0 or "stage0 backend unavailable" not in missing.stderr:
            raise AssertionError("missing backend did not fail closed")

    print("native bridge boundary passed: local commands, argv, signals, and fail-closed lookup")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
