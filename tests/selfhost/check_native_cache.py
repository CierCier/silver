#!/usr/bin/env python3
"""Prove native stage0/stage1 cache and artifact parity on Linux.

This gate builds the same source through both command surfaces, repeats each
build, checks an uncached build, and compares executable bytes plus the
content-addressed cache tree. During the backend migration stage1 delegates
native work to stage0, so this is a regression gate for that compatibility
boundary, not a claim that the Silver cache implementation is complete.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import asdict, dataclass

REPO_ROOT = pathlib.Path(__file__).parents[2]


@dataclass
class RunResult:
    returncode: int
    stdout: str
    stderr: str
    seconds: float


def sha256(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def snapshot(root: pathlib.Path) -> dict[str, str]:
    if not root.exists():
        return {}
    return {
        path.relative_to(root).as_posix(): sha256(path)
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def normalized_text(text: str, replacements: dict[str, str]) -> str:
    for old, new in replacements.items():
        text = text.replace(old, new)
    return text


def run(
    binary: pathlib.Path,
    args: list[str],
    cwd: pathlib.Path,
    env: dict[str, str],
) -> RunResult:
    started = time.perf_counter()
    result = subprocess.run(
        [str(binary), *args],
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    return RunResult(
        returncode=result.returncode,
        stdout=result.stdout,
        stderr=result.stderr,
        seconds=time.perf_counter() - started,
    )


def exercise(binary: pathlib.Path, root: pathlib.Path, stage0: pathlib.Path) -> dict[str, object]:
    work = root / "work"
    cache = work / "cache"
    source = work / "main.ag"
    output = work / "app"
    uncached_output = work / "app-no-cache"
    work.mkdir(parents=True)
    source.write_text("i32 main() {\n    return 0;\n}\n", encoding="utf-8")

    env = os.environ.copy()
    env["SILVER_STAGE0"] = str(stage0)
    base = [str(source), "--cache-dir", str(cache), "--no-progress"]
    first = run(binary, [*base, "-o", str(output)], root, env)
    first_cache = snapshot(cache)
    second = run(binary, [*base, "-o", str(output)], root, env)
    second_cache = snapshot(cache)
    uncached = run(
        binary,
        [str(source), "--cache-dir", str(cache), "--no-cache", "--no-progress", "-o", str(uncached_output)],
        root,
        env,
    )
    info = run(binary, ["--cache-dir", str(cache), "--cache-info"], root, env)
    cycle_cache = work / "cycle-cache"
    cycle_output = work / "cycle-app"
    cycle = run(
        binary,
        [
            str(REPO_ROOT / "tests/cyclic_test.ag"),
            "--cache-dir",
            str(cycle_cache),
            "--no-progress",
            "-o",
            str(cycle_output),
        ],
        root,
        env,
    )

    replacements = {str(work): "<WORK>", str(cache): "<CACHE>"}
    return {
        "first": first,
        "second": second,
        "uncached": uncached,
        "info": info,
        "cycle": cycle,
        "first_cache": first_cache,
        "second_cache": second_cache,
        "output": sha256(output),
        "uncached_output": sha256(uncached_output),
        "stdout": normalized_text(first.stdout, replacements),
        "stderr": normalized_text(first.stderr, replacements),
    }


def as_json(value: object) -> str:
    if isinstance(value, RunResult):
        return json.dumps(asdict(value), sort_keys=True)
    if isinstance(value, dict):
        normalized = {
            key: asdict(item) if isinstance(item, RunResult) else item
            for key, item in value.items()
        }
        return json.dumps(normalized, sort_keys=True)
    return json.dumps(value, sort_keys=True)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    args = parser.parse_args()

    root = args.root.resolve()
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()
    for binary in (stage0, stage1):
        if not binary.is_file():
            parser.error(f"compiler does not exist: {binary}")

    with tempfile.TemporaryDirectory(prefix="silver-cache-parity-") as temp:
        temp_root = pathlib.Path(temp)
        old = exercise(stage0, temp_root, stage0)
        shutil.rmtree(temp_root / "work")
        new = exercise(stage1, temp_root, stage0)

        failures: list[str] = []
        for label, result in (("stage0", old), ("stage1", new)):
            if result["first_cache"] != result["second_cache"]:
                failures.append(f"{label}: repeated cache tree differs")
            if result["output"] != result["uncached_output"]:
                failures.append(f"{label}: cached and uncached executables differ")
            cycle: RunResult = result["cycle"]  # type: ignore[assignment]
            if cycle.returncode != 0:
                failures.append(f"{label}: cyclic cache build failed ({cycle.returncode})")
        for key in ("first", "second", "uncached", "info", "cycle"):
            old_run: RunResult = old[key]  # type: ignore[assignment]
            new_run: RunResult = new[key]  # type: ignore[assignment]
            if old_run.returncode != new_run.returncode:
                failures.append(f"{key}: return code {old_run.returncode} != {new_run.returncode}")
            if old_run.stdout != new_run.stdout:
                failures.append(f"{key}: stdout differs")
            if old_run.stderr != new_run.stderr:
                failures.append(f"{key}: stderr differs")
        for key in ("first_cache", "second_cache", "output", "uncached_output", "stdout", "stderr"):
            if old[key] != new[key]:
                failures.append(f"{key}: content differs")

        if failures:
            for failure in failures:
                print(f"cache parity failed: {failure}", file=sys.stderr)
            print(f"stage0: {as_json(old)}", file=sys.stderr)
            print(f"stage1: {as_json(new)}", file=sys.stderr)
            return 1

        old_first: RunResult = old["first"]  # type: ignore[assignment]
        old_second: RunResult = old["second"]  # type: ignore[assignment]
        new_first: RunResult = new["first"]  # type: ignore[assignment]
        new_second: RunResult = new["second"]  # type: ignore[assignment]
        print(
            "native cache parity passed: "
            f"stage0={old_first.seconds:.3f}s/{old_second.seconds:.3f}s, "
            f"stage1={new_first.seconds:.3f}s/{new_second.seconds:.3f}s, "
            f"dependency_cache_files={len(old['first_cache'])}"  # type: ignore[arg-type]
        )
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
