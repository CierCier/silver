#!/usr/bin/env python3
"""Check repeated and uncached native builds for stage0 and stage1 on Linux."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pathlib
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


def exercise(
    binary: pathlib.Path, temp_root: pathlib.Path, root: pathlib.Path
) -> dict[str, object]:
    work = temp_root / "work"
    cache = work / "cache"
    source = work / "main.ag"
    output = work / "app"
    uncached_output = work / "app-no-cache"
    work.mkdir(parents=True)
    source.write_text("i32 main() {\n    return 0;\n}\n", encoding="utf-8")

    env = os.environ.copy()
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
    cycle_cache = work / "cycle-cache"
    cycle_output = work / "cycle-app"
    cycle = run(
        binary,
        [
            str(root / "tests/cyclic_test.ag"),
            "--cache-dir",
            str(cycle_cache),
            "--no-progress",
            "-o",
            str(cycle_output),
        ],
        root,
        env,
    )

    return {
        "first": first,
        "second": second,
        "uncached": uncached,
        "cycle": cycle,
        "first_cache": first_cache,
        "second_cache": second_cache,
        "output": sha256(output),
        "uncached_output": sha256(uncached_output),
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

    with tempfile.TemporaryDirectory(prefix="silver-native-cache-") as temp:
        temp_root = pathlib.Path(temp)
        results = (
            ("stage0", exercise(stage0, temp_root / "stage0", root)),
            ("stage1", exercise(stage1, temp_root / "stage1", root)),
        )

        failures: list[str] = []
        for label, result in results:
            for key in ("first", "second", "uncached", "cycle"):
                command: RunResult = result[key]  # type: ignore[assignment]
                if command.returncode != 0:
                    failures.append(
                        f"{label}: {key} build failed ({command.returncode})"
                    )
            if result["first_cache"] != result["second_cache"]:
                failures.append(f"{label}: repeated cache tree differs")
            if result["output"] != result["uncached_output"]:
                failures.append(f"{label}: cached and uncached executables differ")

        if failures:
            for failure in failures:
                print(f"native cache check failed: {failure}", file=sys.stderr)
            for label, result in results:
                print(f"{label}: {as_json(result)}", file=sys.stderr)
            return 1

        old_first: RunResult = results[0][1]["first"]  # type: ignore[assignment]
        old_second: RunResult = results[0][1]["second"]  # type: ignore[assignment]
        new_first: RunResult = results[1][1]["first"]  # type: ignore[assignment]
        new_second: RunResult = results[1][1]["second"]  # type: ignore[assignment]
        print(
            "native cache checks passed: "
            f"stage0={old_first.seconds:.3f}s/{old_second.seconds:.3f}s, "
            f"stage1={new_first.seconds:.3f}s/{new_second.seconds:.3f}s, "
            f"cache_files={len(results[1][1]['first_cache'])}"  # type: ignore[arg-type]
        )
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
