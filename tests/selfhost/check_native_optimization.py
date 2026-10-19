#!/usr/bin/env python3
"""Prove native optimization, cfg, cache isolation, and package behavior."""

from __future__ import annotations

import argparse
import os
import pathlib
import re
import signal
import subprocess
import tempfile


PROBE = """i64 optimization_probe(i64 value) {
    if (value < 0) { return -1; }
    i64 temporary = value;
    i64 rounds = 0;
    while (rounds < 2) {
        temporary = temporary + value;
        rounds = rounds + 1;
    }
    return temporary / 3;
}
"""
MAIN = """i32 main() {
    if (optimization_probe(-1) != -1) { return 1; }
    if (@cfg(debug)) { return 41; }
    return (i32)optimization_probe(42);
}
"""
SOURCE = PROBE + MAIN


def invoke(compiler: pathlib.Path, args: list[str], work: pathlib.Path,
           dump: bool = False) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    env.pop("SILVER_DUMP_IR", None)
    if dump:
        env["SILVER_DUMP_IR"] = "1"
    return subprocess.run([str(compiler), *args], cwd=work, env=env,
                          capture_output=True, text=True, timeout=120)


def require_result(result: subprocess.CompletedProcess[str], expected: int) -> None:
    if result.returncode != expected:
        raise AssertionError(f"{result.args}: expected {expected}, got {result.returncode}"
                             f"\n{result.stdout}\n{result.stderr}")


def require_runs(binary: pathlib.Path, expected: int) -> None:
    result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=30)
    require_result(result, expected)


def require_ir(result: subprocess.CompletedProcess[str], optimized: bool) -> None:
    function = re.search(r"define[^\n]*@optimization_probe[^\n]*\{(.*?)^\}",
                         result.stderr, re.MULTILINE | re.DOTALL)
    if function is None:
        raise AssertionError(f"missing optimization_probe definition\n{result.stderr}")
    body = function.group(1)
    if optimized:
        if re.search(r"%temporary\b[^\n]*alloca", body):
            raise AssertionError(f"optimized loop retains a scalar stack slot:\n{body}")
        if "ret i64" not in body:
            raise AssertionError(f"optimized probe has no return:\n{body}")
    elif not re.search(r"%temporary\b[^\n]*alloca", body):
        raise AssertionError(f"O0 probe was unexpectedly optimized:\n{body}")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    compiler = args.stage1.resolve()
    with tempfile.TemporaryDirectory(prefix="silver-native-optimization-") as temporary:
        work = pathlib.Path(temporary)
        source = work / "main.ag"
        source.write_text(SOURCE)
        cases = [([], False), (["-O0"], False), (["--opt-level=0"], False),
                 (["-O"], True), (["-O", "2"], True),
                 (["--opt-level", "2"], True)]
        cases += [([f"-O{level}"], True) for level in ("1", "2", "3", "s", "z", "fast")]
        for index, (flags, optimized) in enumerate(cases):
            binary = work / f"case-{index}"
            result = invoke(compiler, ["build", str(source), "--no-cache", *flags,
                                       "-o", str(binary)], work, dump=True)
            require_result(result, 0)
            require_ir(result, optimized)
            require_runs(binary, 42 if optimized else 41)

        for flags in (["-O9"], ["--opt-level=garbage"], ["--opt-level="]):
            binary = work / "invalid"
            result = invoke(compiler, ["build", str(work / "missing.ag"), *flags,
                                       "-o", str(binary)], work)
            if result.returncode == 0 or "invalid optimization level" not in result.stderr:
                raise AssertionError(f"invalid level did not fail before source access: {result}")
            if binary.exists():
                raise AssertionError("invalid optimization level created output")

        # Optimized correctness fixtures explicitly enable debug assertions.
        # Prove the flag keeps assertions active while IR passes are selected.
        assertion = work / "assertion.ag"
        assertion.write_text('import std.sys.entry;\n'
                             'i32 main() { @assert(false, "optimized assertion marker"); return 42; }\n')
        binary = work / "assertion"
        result = invoke(compiler, ["build", str(assertion), "-O2", "--cfg", "debug=1",
                                   "--no-cache", "-I", str(pathlib.Path(__file__).resolve().parents[2]),
                                   "-o", str(binary)], work)
        require_result(result, 0)
        result = subprocess.run([str(binary)], capture_output=True, text=True, timeout=30)
        require_result(result, -signal.SIGABRT)
        if "optimized assertion marker" not in result.stderr:
            raise AssertionError(f"optimized assertion lost its diagnostic: {result.stderr}")

        # A cached debug object must not satisfy a subsequent optimized build.
        for index, flags in enumerate((["-O0"], ["-O2"], ["-O0"], ["--opt-level=2"])):
            binary = work / f"cached-{index}"
            result = invoke(compiler, ["build", str(source), *flags, "--cache-dir",
                                       str(work / "cache"), "-o", str(binary)], work)
            require_result(result, 0)
            require_runs(binary, 41 if flags == ["-O0"] else 42)

        manifest = work / "silver.toml"
        manifest.write_text('name = "optimization-probe"\nversion = "0.1.0"\n'
                            '[bin."app"]\nentry = "main.ag"\n')
        (work / "operations.ag").write_text(PROBE)
        source.write_text("import operations;\n" + MAIN)
        binary = work / "package"
        result = invoke(compiler, ["build", "--manifest", str(manifest),
                                   "--opt-level=2", "--no-cache", "-o", str(binary)],
                        work, dump=True)
        require_result(result, 0)
        require_ir(result, True)
        require_runs(binary, 42)

        object_file = work / "probe.o"
        result = invoke(compiler, ["build", str(source), "--emit=object", "-O2",
                                   "--no-cache", "-o", str(object_file)], work, dump=True)
        require_result(result, 0)
        require_ir(result, True)
        if not object_file.is_file():
            raise AssertionError("optimized object emission produced no file")

        module_file = work / "probe.agm"
        result = invoke(compiler, ["build", str(source), "--emit=module", "-O2",
                                   "--no-cache", "-o", str(module_file)], work, dump=True)
        require_result(result, 0)
        require_ir(result, True)
        if not module_file.is_file() or not module_file.with_suffix(".o").is_file():
            raise AssertionError("optimized module emission did not publish metadata and object")

        # Forwarded optimization options must not change compiler cfg or passes.
        for trailing in (["--", "-O2"], ["--run-arg", "-O2"]):
            result = invoke(compiler, ["run", str(source), "--no-cache",
                                       "-o", str(work / "forwarded"), *trailing], work)
            require_result(result, 41)
    print("stage1 native optimization contract passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
