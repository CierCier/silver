"""Run the enum C ABI probe against a compiler and its native executable."""

from __future__ import annotations

import pathlib
import subprocess


def run_enum_ffi_probe(
    binary: pathlib.Path,
    cc: pathlib.Path,
    root: pathlib.Path,
    work: pathlib.Path,
    label: str,
    env: dict[str, str],
) -> None:
    library = work / "libenum_ffi_probe.so"
    if not library.is_file():
        compiled = subprocess.run(
            [
                str(cc), "-shared", "-fPIC",
                str(pathlib.Path(__file__).with_name("enum_ffi_stub.c")),
                "-o", str(library),
            ],
            cwd=root,
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        if compiled.returncode != 0:
            raise AssertionError(
                f"{label}: C enum ABI stub compilation failed\n"
                f"{compiled.stdout}\n{compiled.stderr}"
            )

    run_env = env.copy()
    old_library_path = run_env.get("LD_LIBRARY_PATH")
    run_env["LD_LIBRARY_PATH"] = (
        str(work) if not old_library_path else f"{work}:{old_library_path}"
    )
    for fixture in ("enum_ffi_probe.ag", "enum_ffi_return_probe.ag"):
        source = pathlib.Path(__file__).with_name(fixture)
        output = work / f"{label}-{source.stem}"
        build = subprocess.run(
            [
                str(binary), "build", str(source), "--no-cache", "-L", str(work),
                "-o", str(output),
            ],
            cwd=root,
            env=env,
            capture_output=True,
            text=True,
            timeout=300,
            check=False,
        )
        if build.returncode != 0:
            raise AssertionError(
                f"{label}: {fixture} build failed ({build.returncode})\n"
                f"{build.stdout}\n{build.stderr}"
            )
        executed = subprocess.run(
            [str(output)],
            cwd=root,
            env=run_env,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        if (executed.returncode, executed.stdout, executed.stderr) != (42, "", ""):
            raise AssertionError(
                f"{label}: {fixture} returned {executed.returncode}, expected 42\n"
                f"stdout:\n{executed.stdout}\nstderr:\n{executed.stderr}"
            )
