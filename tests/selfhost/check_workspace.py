#!/usr/bin/env python3
"""Exercise stage1 workspace planning and trap any attempted stage0 launch."""

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
        cwd=Path(env["SILVER_STAGE0"]).parent,
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
            "#!/usr/bin/env python3\n"
            "from pathlib import Path\n"
            f"Path({str(backend_marker)!r}).touch()\n"
            "raise SystemExit(97)\n"
        )
        fake_backend.chmod(0o755)
        base_env = os.environ.copy()
        base_env["SILVER_STAGE0"] = str(fake_backend)

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

        output_path = root / "application"
        require(
            run(
                stage1,
                ["build", "--bin", "app0", str(manifest), "--no-cache", "-o", str(output_path)],
                base_env,
            ),
            0,
        )
        if not output_path.is_file() or not os.access(output_path, os.X_OK):
            raise AssertionError("stage1 build did not produce an executable")

        nested_output = root / "nested-application"
        require(
            run(stage1, ["build", "--bin", "agc", str(nested_root_manifest), "-o", str(nested_output)], base_env),
            0,
        )
        if not nested_output.is_file() or not os.access(nested_output, os.X_OK):
            raise AssertionError("stage1 did not build the selected nested package target")

        require(
            run(stage1, ["test", str(manifest)], base_env),
            2,
            "stage1 does not support the workspace test command yet",
        )
        require(
            run(stage1, ["t", str(manifest)], base_env),
            2,
            "stage1 does not support the workspace test command yet",
        )

        require(run(stage1, ["run", str(manifest), "--", "--flag"], base_env), 0)
        if backend_marker.exists():
            raise AssertionError("legacy SILVER_STAGE0 executable was launched")

    print("workspace checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
