#!/usr/bin/env python3
"""Gate the stage1 native target contract.

The supported stage1 native target is the exact LLVM default host triple on an
x86_64 Linux host. `build`, `run`, and `test` reject any other `--target` before
cfg derivation and code generation can disagree, while the frontend-only
`check` command still derives cfg from the requested triple.

The gate derives the accepted triple from the default build's own LLVM IR, then
observes behavior through the CLI: the accepted target must build, run, and emit
a relocatable object, and drive a package build and run; rejected requests must
report a target diagnostic, exit nonzero, and leave no artifact behind. Program
arguments after `--` or `--run-arg` are never treated as compiler options.
"""

from __future__ import annotations

import argparse
import os
import pathlib
import re
import shutil
import struct
import subprocess
import tempfile

# `value()` exists only under os.linux, so a successful check proves cfg was
# derived for the requested target, and a Windows target must fail the check.
CFG_SOURCE = (
    "#[cfg(os.linux)]\n"
    "i32 value() {\n"
    "    return 42;\n"
    "}\n"
    "\n"
    "i32 main() {\n"
    "    return value();\n"
    "}\n"
)

# Mirrors the source used to detect an unsupported target reaching the runtime.
RUN_MARKER = "stage1-native-target-ran"
RUN_MARKER_SOURCE = (
    "import std.io;\n"
    "\n"
    "i32 main() {\n"
    f'    println("{RUN_MARKER}");\n'
    "    return 42;\n"
    "}\n"
)

# Echoes its two program arguments only when cfg still reports the host. A
# leftover raw-argument scan would apply a forwarded `--target` to cfg, drop
# arch.x86_64, and make main return 1 without printing.
FORWARD_SOURCE = (
    "import std.args;\n"
    "import std.io;\n"
    "import std.mem.vec;\n"
    "import std.string;\n"
    "\n"
    "i32 main() {\n"
    "    if (@cfg(arch.x86_64) && @cfg(os.linux)) {\n"
    "        Vec<String> a = args();\n"
    "        println(a.get(1).to_str());\n"
    "        println(a.get(2).to_str());\n"
    "        return 42;\n"
    "    }\n"
    "    return 1;\n"
    "}\n"
)

# One representative per rejected category. Each entry is checked in
# `--target=<value>` form; a subset is also checked in split form.
REJECTED_TARGETS = (
    ("aarch64-unknown-linux-gnu", True),   # other architecture on Linux
    ("wasm32-wasi", True),                 # WASI, supported only by stage0
    ("x86_64-pc-windows-msvc", False),     # Windows
    ("x86_64-unknown-linux-musl", False),  # x86_64 Linux, incompatible environment
    ("not-a-real-triple", False),          # malformed
    ("", False),                           # empty value
)

TARGET_TRIPLE_IR = re.compile(r'target triple = "([^"]+)"')


def run(
    stage1: pathlib.Path,
    args: list[str],
    env: dict[str, str],
    cwd: pathlib.Path,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), *args],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=env,
        cwd=cwd,
        timeout=120,
        check=False,
    )


def require(condition: bool, message: str, result: subprocess.CompletedProcess[str]) -> None:
    if not condition:
        raise AssertionError(
            f"{message}\ncommand: {result.args}\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        )


def require_rejected(
    result: subprocess.CompletedProcess[str],
    outputs: tuple[pathlib.Path, ...],
    label: str,
    host_triple: str,
) -> None:
    require(result.returncode != 0, f"{label}: expected a nonzero exit", result)
    stderr = result.stderr.lower()
    require(
        "stage1 native backend" in stderr and "host target triple" in stderr,
        f"{label}: missing the native target diagnostic",
        result,
    )
    require(
        host_triple in result.stderr,
        f"{label}: diagnostic did not name the supported host triple",
        result,
    )
    for output in outputs:
        require(
            not output.exists(),
            f"{label}: rejected request created {output}",
            result,
        )


def require_runs(
    executable: pathlib.Path,
    cwd: pathlib.Path,
    label: str,
    expected: int,
) -> None:
    executed = subprocess.run(
        [str(executable)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=cwd,
        timeout=30,
        check=False,
    )
    if executed.returncode != expected:
        raise AssertionError(
            f"{label}: program returned {executed.returncode}, expected {expected}\n"
            f"stdout:\n{executed.stdout}\nstderr:\n{executed.stderr}"
        )


def discover_host_triple(
    stage1: pathlib.Path,
    source: pathlib.Path,
    env: dict[str, str],
    root: pathlib.Path,
) -> tuple[str, pathlib.Path]:
    """Build with the default target and read the triple LLVM actually used."""
    output = root / "host-probe"
    probe_env = dict(env)
    probe_env["SILVER_DUMP_IR"] = "1"
    result = run(
        stage1,
        ["build", str(source), "--no-cache", "-o", str(output)],
        probe_env,
        root,
    )
    require(result.returncode == 0, "default build failed while deriving the host triple", result)
    match = TARGET_TRIPLE_IR.search(result.stderr)
    require(match is not None, "default build did not report an LLVM target triple", result)
    host_triple = match.group(1)
    require(
        host_triple.startswith("x86_64-") and "-linux-" in host_triple,
        f"unexpected host triple {host_triple!r}",
        result,
    )
    return host_triple, output


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    parser.add_argument("--cc", type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    cc = args.cc or shutil.which("cc")
    if cc is None or not pathlib.Path(cc).is_file():
        raise SystemExit("cc is required for the stage1 native target gate")
    repository = pathlib.Path(__file__).resolve().parents[2]

    env = os.environ.copy()
    env["SILVER_STAGE1_CC"] = str(pathlib.Path(cc).resolve())

    with tempfile.TemporaryDirectory(prefix="silver-native-target-") as temporary:
        root = pathlib.Path(temporary)
        cfg_source = root / "cfg.ag"
        cfg_source.write_text(CFG_SOURCE)
        marker_source = root / "marker.ag"
        marker_source.write_text(RUN_MARKER_SOURCE)
        forward_source = root / "forward.ag"
        forward_source.write_text(FORWARD_SOURCE)

        host_triple, host_probe = discover_host_triple(stage1, cfg_source, env, root)
        require_runs(host_probe, root, "default host build", 42)

        # The exact host triple must build and run a cfg-dependent program in
        # both `--target=` and split forms.
        accepted_cases = (
            ("host triple --target=", [f"--target={host_triple}"]),
            ("host triple split", ["--target", host_triple]),
        )
        for index, (label, target_args) in enumerate(accepted_cases):
            output = root / f"accepted-{index}"
            built = run(
                stage1,
                ["build", str(cfg_source), "--no-cache", *target_args, "-o", str(output)],
                env,
                root,
            )
            require(built.returncode == 0, f"{label}: accepted target failed to build", built)
            require_runs(output, root, label, 42)

        run_output = root / "accepted-run"
        ran = run(
            stage1,
            ["run", str(cfg_source), "--no-cache",
             f"--target={host_triple}", "-o", str(run_output)],
            env,
            root,
        )
        require(ran.returncode == 42, "run with the host triple returned the wrong code", ran)

        # Object emit is the supported non-executable output mode. It must be a
        # relocatable object, not an executable with the same magic.
        object_output = root / "accepted.o"
        emitted = run(
            stage1,
            ["build", str(cfg_source), "--emit=object", "--no-cache",
             f"--target={host_triple}", "-o", str(object_output)],
            env,
            root,
        )
        require(
            emitted.returncode == 0 and object_output.is_file(),
            "object emit with the host triple failed",
            emitted,
        )
        header = object_output.read_bytes()[:20]
        require(header[:4] == b"\x7fELF", "object emit did not produce an ELF file", emitted)
        byte_order = "<" if header[5] == 1 else ">"
        e_type = struct.unpack_from(f"{byte_order}H", header, 16)[0]
        require(e_type == 1, "object emit did not produce a relocatable ET_REL object", emitted)

        # A package build and run must accept the host triple as well.
        package = root / "package"
        package.mkdir()
        (package / "silver.toml").write_text(
            'name = "native-target-contract"\nversion = "0.1.0"\n\n'
            '[bin."app"]\nentry = "main.ag"\n'
        )
        (package / "main.ag").write_text(CFG_SOURCE)
        package_output = root / "package-output"
        package_build = run(
            stage1,
            ["build", "--manifest", str(package / "silver.toml"), "--no-cache",
             f"--target={host_triple}", "-o", str(package_output)],
            env,
            root,
        )
        require(package_build.returncode == 0, "package build with the host triple failed", package_build)
        require_runs(package_output, root, "package build", 42)

        package_run = root / "package-run"
        package_ran = run(
            stage1,
            ["run", str(package / "silver.toml"), "--no-cache",
             f"--target={host_triple}", "-o", str(package_run)],
            env,
            root,
        )
        require(
            package_ran.returncode == 42,
            "package run with the host triple returned the wrong code",
            package_ran,
        )

        # Rejected targets must fail before producing output.
        for index, (target, split) in enumerate(REJECTED_TARGETS):
            joined_output = root / f"rejected-joined-{index}"
            joined = run(
                stage1,
                ["build", str(cfg_source), "--no-cache",
                 f"--target={target}", "-o", str(joined_output)],
                env,
                root,
            )
            require_rejected(joined, (joined_output,), f"build --target={target!r}", host_triple)
            if split:
                split_output = root / f"rejected-split-{index}"
                split_result = run(
                    stage1,
                    ["build", str(cfg_source), "--no-cache",
                     "--target", target, "-o", str(split_output)],
                    env,
                    root,
                )
                require_rejected(
                    split_result, (split_output,), f"build --target {target!r}", host_triple
                )

        # A rejected run must not execute the program.
        run_rejected_output = root / "rejected-run"
        run_rejected = run(
            stage1,
            ["run", str(marker_source), "-I", str(repository), "--no-cache",
             "--target=aarch64-unknown-linux-gnu", "-o", str(run_rejected_output)],
            env,
            root,
        )
        require_rejected(
            run_rejected, (run_rejected_output,), "run with a rejected target", host_triple
        )
        require(
            RUN_MARKER not in run_rejected.stdout,
            "a rejected run still executed the program",
            run_rejected,
        )

        # Object emit must reject a target too.
        rejected_object = root / "rejected-emit.o"
        object_rejected = run(
            stage1,
            ["build", str(cfg_source), "--emit=object", "--no-cache",
             "--target=wasm32-wasi", "-o", str(rejected_object)],
            env,
            root,
        )
        require_rejected(
            object_rejected, (rejected_object,), "object emit with a rejected target", host_triple
        )

        # Module emission must reject before publishing the artifact or its
        # object, leaving both `.agm` and `.o` paths untouched.
        module_artifact = root / "rejected-module.agm"
        module_object = root / "rejected-module.o"
        module_rejected = run(
            stage1,
            ["build", str(cfg_source), "--emit=module", "--no-cache",
             "--target=x86_64-pc-windows-msvc", "-o", str(module_artifact)],
            env,
            root,
        )
        require_rejected(
            module_rejected,
            (module_artifact, module_object),
            "module emit with a rejected target",
            host_triple,
        )

        # Package builds and commandless invocations reject unsupported targets
        # before workspace discovery or output creation.
        package_rejected_output = root / "package-rejected"
        package_rejected = run(
            stage1,
            ["build", "--manifest", str(package / "silver.toml"), "--no-cache",
             "--target=aarch64-unknown-linux-gnu", "-o", str(package_rejected_output)],
            env,
            root,
        )
        require_rejected(
            package_rejected,
            (package_rejected_output,),
            "package build with a rejected target",
            host_triple,
        )
        implicit_output = root / "implicit-package-rejected"
        implicit_rejected = run(
            stage1,
            ["--manifest", str(package / "silver.toml"), "--no-cache",
             "--target=aarch64-unknown-linux-gnu", "-o", str(implicit_output)],
            env,
            root,
        )
        require_rejected(
            implicit_rejected,
            (implicit_output,),
            "commandless invocation with a rejected target",
            host_triple,
        )

        # A rejected build must not clobber an existing output file.
        sentinel = root / "sentinel-output"
        sentinel.write_text("keep-me")
        clobbered = run(
            stage1,
            ["build", str(cfg_source), "--no-cache",
             "--target=x86_64-pc-windows-msvc", "-o", str(sentinel)],
            env,
            root,
        )
        require_rejected(clobbered, (), "build over an existing output", host_triple)
        require(
            sentinel.read_text() == "keep-me",
            "a rejected build clobbered the existing output file",
            clobbered,
        )

        # Rejection happens before the source is read, so a missing source still
        # reports the target diagnostic rather than a read error.
        for command in ("build", "test"):
            missing = run(
                stage1,
                [command, str(root / "does-not-exist.ag"), "--no-cache",
                 "--target=wasm32-wasi", "-o", str(root / "missing-output")],
                env,
                root,
            )
            require_rejected(
                missing, (root / "missing-output",), f"{command} missing source", host_triple
            )
            require(
                "failed to read" not in missing.stderr,
                "target rejection did not happen before the source was read",
                missing,
            )

        # A repeated `--target` is a parser error, not the native diagnostic.
        duplicate = run(
            stage1,
            ["build", str(cfg_source), "--no-cache",
             f"--target={host_triple}", "--target=aarch64-unknown-linux-gnu",
             "-o", str(root / "duplicate-output")],
            env,
            root,
        )
        require(duplicate.returncode != 0, "a repeated --target was accepted", duplicate)
        require(
            "may only be specified once" in duplicate.stderr,
            "a repeated --target did not report a parser error",
            duplicate,
        )
        require(
            "host target triple" not in duplicate.stderr,
            "a repeated --target reached the native target check",
            duplicate,
        )

        # `check` stays frontend-only: a nonnative triple is accepted and still
        # derives cfg, while the same triple cannot be built natively.
        nonnative_check = run(
            stage1,
            ["check", str(cfg_source), "--no-cache",
             "--target=aarch64-unknown-linux-gnu"],
            env,
            root,
        )
        require(nonnative_check.returncode == 0, "check rejected a nonnative target", nonnative_check)

        windows_check = run(
            stage1,
            ["check", str(cfg_source), "--no-cache",
             "--target=x86_64-pc-windows-msvc"],
            env,
            root,
        )
        require(
            windows_check.returncode != 0,
            "check did not apply the requested target cfg",
            windows_check,
        )
        require(
            "host target triple" not in windows_check.stderr,
            "check reported the native target diagnostic instead of checking",
            windows_check,
        )
        require(
            "unknown" in windows_check.stderr and "value" in windows_check.stderr,
            "check did not report the cfg-dependent missing symbol",
            windows_check,
        )

        # A `--target` after `--` or `--run-arg` is a program argument, not a
        # compiler option. The cfg-dependent source fails if a stale raw scan
        # applies the forwarded triple to cfg.
        forwarded = (
            ("-- separator", ["--", "--target", "aarch64-unknown-linux-gnu"]),
            (
                "--run-arg",
                ["--run-arg", "--target", "--run-arg", "aarch64-unknown-linux-gnu"],
            ),
        )
        for index, (label, trailing) in enumerate(forwarded):
            result = run(
                stage1,
                ["run", str(forward_source), "-I", str(repository), "--no-cache",
                 "-o", str(root / f"forwarded-{index}"), *trailing],
                env,
                root,
            )
            require(
                result.returncode == 42
                and result.stdout == "--target\naarch64-unknown-linux-gnu\n",
                f"{label}: forwarded --target affected the compiler",
                result,
            )

    print(f"stage1 native target contract passed (host {host_triple})")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
