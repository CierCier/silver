#!/usr/bin/env python3
"""Exercise stage1-owned workspace planning without a native backend."""

from __future__ import annotations

import argparse
import os
import subprocess
import tempfile
from pathlib import Path


def run(stage1: Path, args: list[str], env: dict[str, str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), *args],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        check=False,
    )


def require(result: subprocess.CompletedProcess[str], code: int, text: str = "") -> None:
    actual = result.returncode
    if actual != code:
        raise AssertionError(
            f"expected exit {code}, got {actual}\n"
            f"command: {result.args}\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        )
    if text and text not in result.stderr:
        raise AssertionError(
            f"expected {text!r} in stderr\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        )


def write_package(root: Path, *, bins: int = 1, libs: int = 0) -> Path:
    package = root / "package"
    package.mkdir(parents=True)
    (package / "main.ag").write_text("i32 main() { return 0; }\n")
    (package / "lib.ag").write_text("i32 answer() { return 42; }\n")
    lines = ['name = "fixture"', 'version = "0.1.0"']
    for index in range(bins):
        lines += ["", f"[bin.app{index}]", 'entry = "main.ag"']
    for index in range(libs):
        lines += ["", f"[lib.lib{index}]", 'entry = "lib.ag"']
    manifest = package / "silver.toml"
    manifest.write_text("\n".join(lines) + "\n")
    return manifest


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    if not stage1.is_file() or not os.access(stage1, os.X_OK):
        raise SystemExit(f"stage1 compiler is not executable: {stage1}")

    with tempfile.TemporaryDirectory(prefix="silver-workspace-") as temporary:
        root = Path(temporary)
        manifest = write_package(root, bins=1, libs=1)
        source = manifest.parent / "main.ag"
        backend_marker = root / "backend-called"
        fake_backend = root / "fake-backend"
        fake_backend.write_text(
            "#!/usr/bin/env bash\n"
            f"touch {backend_marker!s}\n"
            "printf '%s\\n' \"$@\" > \"$SILVER_FAKE_ARGS\"\n"
            "exit 0\n"
        )
        fake_backend.chmod(0o755)
        base_env = os.environ.copy()
        base_env["SILVER_STAGE0"] = str(fake_backend)
        base_env["SILVER_FAKE_ARGS"] = str(root / "backend-args")

        # Direct source and manifest checks stay entirely in stage1.
        require(run(stage1, ["check", str(source)], base_env), 0, "")
        require(run(stage1, ["check", str(manifest)], base_env), 0, "")
        if backend_marker.exists():
            raise AssertionError("check unexpectedly invoked the native backend")

        # Optional selector values and command placement follow stage0 syntax.
        require(run(stage1, ["check", "--bin", "app0", str(manifest)], base_env), 0)
        require(run(stage1, ["--bin", "app0", "check", str(manifest)], base_env), 0)
        require(run(stage1, ["check", "--bin=app0", str(manifest)], base_env), 0)
        require(run(stage1, ["check", "--lib", str(manifest)], base_env), 0)
        require(run(stage1, ["check", "--lib=lib0", str(manifest)], base_env), 0)
        require(
            run(stage1, ["check", "--lib", "missing", str(manifest)], base_env),
            2,
            "package has no `missing` target",
        )
        require(
            run(stage1, ["check", "--bin", "app0", "--lib", str(manifest)], base_env),
            2,
            "only one of `--bin` or `--lib`",
        )
        require(run(stage1, ["check", str(manifest.parent)], base_env), 0)

        dependency_root = root / "dependency-root"
        child = dependency_root / "child"
        child.mkdir(parents=True)
        (dependency_root / "main.ag").write_text("i32 main() { return 0; }\n")
        (child / "lib.ag").write_text("i32 child() { return 1; }\n")
        (child / "silver.toml").write_text(
            'name = "child"\nversion = "0.1.0"\n\n[lib."child"]\nentry = "lib.ag"\n'
        )
        (dependency_root / "silver.toml").write_text(
            'name = "root"\nversion = "0.1.0"\n\n[bin."root"]\nentry = "main.ag"\n\n[dependencies.child]\nmanifest = "child"\n'
        )
        require(run(stage1, ["check", str(dependency_root / "silver.toml")], base_env), 0)

        nested_root = root / "nested-manifest"
        nested_bin = nested_root / "bin"
        nested_bin.mkdir(parents=True)
        (nested_bin / "main.ag").write_text("i32 main() { return 0; }\n")
        (nested_bin / "lib.ag").write_text("i32 answer() { return 42; }\n")
        nested_manifest = nested_bin / "agc"
        nested_manifest.write_text(
            'name = "nested"\nversion = "0.1.0"\n\n'
            '[bin.agc]\nentry = "main.ag"\n\n'
            '[lib.agc]\nentry = "lib.ag"\n'
        )
        nested_root_manifest = nested_root / "silver.toml"
        nested_root_manifest.write_text(
            'name = "root"\nversion = "0.1.0"\n\n'
            '[bin.agc]\nmanifest = "bin/agc"\n\n'
            '[lib.agc]\nmanifest = "bin/agc"\n'
        )
        require(run(stage1, ["check", str(nested_root_manifest)], base_env), 0)
        require(run(stage1, ["check", "--bin", "agc", str(nested_root_manifest)], base_env), 0)
        require(run(stage1, ["check", "--lib", "agc", str(nested_root_manifest)], base_env), 0)
        require(run(stage1, ["check", str(nested_manifest)], base_env), 0)

        ambiguous = write_package(root / "ambiguous", bins=2)
        require(
            run(stage1, ["check", str(ambiguous)], base_env),
            2,
            "package target is ambiguous",
        )

        invalid_manifest = root / "invalid-manifest"
        invalid_manifest.mkdir()
        (invalid_manifest / "silver.toml").write_text(
            'name = fixture\nversion = "0.1.0"\n\n[bin.app]\nentry = "main.ag"\n'
        )
        (invalid_manifest / "main.ag").write_text("i32 main() { return 0; }\n")
        require(
            run(stage1, ["check", str(invalid_manifest / "silver.toml")], base_env),
            2,
            "manifest `name` must be a string",
        )

        missing = root / "missing"
        missing.mkdir()
        (missing / "silver.toml").write_text(
            'name = "missing"\nversion = "0.1.0"\n\n[bin.app]\nentry = "absent.ag"\n'
        )
        require(
            run(stage1, ["check", str(missing / "silver.toml")], base_env),
            2,
            "selected package target has no source entry",
        )

        # Native commands retain the manifest for the transitional bridge.
        require(run(stage1, ["build", str(manifest), "--no-cache"], base_env), 0)
        if not backend_marker.exists():
            raise AssertionError("build did not invoke the native backend")
        forwarded = (root / "backend-args").read_text().splitlines()
        if str(manifest) not in forwarded:
            raise AssertionError(f"manifest was not forwarded to backend: {forwarded}")

        nested_backend_args = root / "nested-backend-args"
        nested_env = base_env.copy()
        nested_env["SILVER_FAKE_ARGS"] = str(nested_backend_args)
        require(run(stage1, ["build", str(nested_root_manifest), "--no-cache"], nested_env), 0)
        nested_forwarded = nested_backend_args.read_text().splitlines()
        if str(nested_root_manifest) not in nested_forwarded:
            raise AssertionError(f"nested manifest was not forwarded to backend: {nested_forwarded}")

        # The native test command is still delegated as a whole until stage1
        # owns test discovery and execution; it must not be reduced to one
        # guessed target by the package planner.
        test_marker = root / "test-args"
        test_env = base_env.copy()
        test_env["SILVER_FAKE_ARGS"] = str(test_marker)
        (manifest.parent / "tests").mkdir()
        (manifest.parent / "tests" / "basic.ag").write_text("i32 main() { return 0; }\n")
        require(run(stage1, ["test", str(manifest)], test_env), 0)
        forwarded_test = test_marker.read_text().splitlines()
        if str(manifest) not in forwarded_test:
            raise AssertionError(f"test manifest was not forwarded: {forwarded_test}")

        # Arguments after -- belong to the target program, not the package input.
        run_marker = root / "run-args"
        run_env = base_env.copy()
        run_env["SILVER_FAKE_ARGS"] = str(run_marker)
        require(run(stage1, ["run", str(manifest), "--", "--flag"], run_env), 0)
        forwarded_run = run_marker.read_text().splitlines()
        if "--flag" not in forwarded_run or str(manifest) not in forwarded_run:
            raise AssertionError(f"run arguments were not preserved: {forwarded_run}")

    print("workspace checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
