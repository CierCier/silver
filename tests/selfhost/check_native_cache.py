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
    binary: pathlib.Path, temp_root: pathlib.Path, root: pathlib.Path,
    check_corrupt_entry: bool = False,
) -> dict[str, object]:
    work = temp_root / "work"
    cache = work / "cache"
    source = work / "main.ag"
    output = work / "app"
    uncached_output = work / "app-no-cache"
    work.mkdir(parents=True)
    source.write_text("i32 main() {\n    return 0;\n}\n", encoding="utf-8")

    env = os.environ.copy()
    base = [str(source), "--cache-dir", str(cache), "--no-progress", "--verbose"]
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
    uncached_cache = snapshot(cache)
    corrupt_rebuild = None
    corrupt_rebuild_cache = uncached_cache
    if check_corrupt_entry:
        cached_objects = sorted(cache.rglob("*.o"))
        if cached_objects:
            cached_objects[0].write_bytes(b"not an object")
            corrupt_rebuild = run(binary, [*base, "-o", str(output)], root, env)
            corrupt_rebuild_cache = snapshot(cache)
    source.write_text("i32 main() {\n    return 7;\n}\n", encoding="utf-8")
    changed_output = work / "app-changed"
    changed = run(binary, [*base, "-o", str(changed_output)], root, env)
    changed_cache = snapshot(cache)
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

    dependency_dir = work / "dependency"
    dependency_dir.mkdir()
    dependency_source = dependency_dir / "dep.ag"
    dependency_main = dependency_dir / "main.ag"
    dependency_cache = dependency_dir / "cache"
    dependency_output = dependency_dir / "app"
    dependency_source.write_text("i32 cached_value() {\n    return 1;\n}\n", encoding="utf-8")
    dependency_main.write_text(
        "import dep;\ni32 main() {\n    return cached_value() - 1;\n}\n",
        encoding="utf-8",
    )
    dependency_args = [
        str(dependency_main), "--cache-dir", str(dependency_cache),
        "--no-progress", "--verbose", "-o", str(dependency_output),
    ]
    dependency_first = run(binary, dependency_args, root, env)
    dependency_first_cache = snapshot(dependency_cache)
    dependency_second = run(binary, dependency_args, root, env)
    dependency_second_cache = snapshot(dependency_cache)
    dependency_source.write_text("i32 cached_value() {\n    return 2;\n}\n", encoding="utf-8")
    dependency_changed = run(binary, dependency_args, root, env)
    dependency_changed_cache = snapshot(dependency_cache)
    dependency_exit = subprocess.run(
        [str(dependency_output)], cwd=root, env=env, check=False
    ).returncode
    cache_cli = {}
    if check_corrupt_entry:
        info = run(binary, ["cache", "info", "--cache-dir", str(cache)], root, env)
        prune = run(binary, ["cache", "prune", "0", "--cache-dir", str(cache)], root, env)
        pruned_cache = snapshot(cache)
        refill = run(binary, [*base, "-o", str(changed_output)], root, env)
        flag_prune = run(
            binary, ["--cache-prune=0", "--cache-dir", str(cache)], root, env
        )
        flag_pruned_cache = snapshot(cache)
        refill_after_flag_prune = run(binary, [*base, "-o", str(changed_output)], root, env)
        subcommand_clean = run(
            binary, ["cache", "clean", "--cache-dir", str(cache)], root, env
        )
        subcommand_cleaned_cache = snapshot(cache)
        refill_after_subcommand_clean = run(
            binary, [*base, "-o", str(changed_output)], root, env
        )
        clean_alias = run(binary, ["--clean-cache", "--cache-dir", str(cache)], root, env)
        cleaned_cache = snapshot(cache)
        clean_command = run(binary, ["clean", "--cache-dir", str(cache)], root, env)
        flag_info = run(binary, ["--cache-info", "--cache-dir", str(cache)], root, env)
        invalid_prune = run(
            binary, ["cache", "prune", "unknown", "--cache-dir", str(cache)], root, env
        )
        cache_cli = {
            "info": info,
            "prune": prune,
            "refill": refill,
            "flag_prune": flag_prune,
            "flag_pruned_cache": flag_pruned_cache,
            "refill_after_flag_prune": refill_after_flag_prune,
            "subcommand_clean": subcommand_clean,
            "subcommand_cleaned_cache": subcommand_cleaned_cache,
            "refill_after_subcommand_clean": refill_after_subcommand_clean,
            "clean_alias": clean_alias,
            "clean_command": clean_command,
            "flag_info": flag_info,
            "invalid_prune": invalid_prune,
            "pruned_cache": pruned_cache,
            "cleaned_cache": cleaned_cache,
        }

    return {
        "first": first,
        "second": second,
        "uncached": uncached,
        "corrupt_rebuild": corrupt_rebuild,
        "corrupt_rebuild_cache": corrupt_rebuild_cache,
        "cycle": cycle,
        "dependency_first": dependency_first,
        "dependency_second": dependency_second,
        "dependency_changed": dependency_changed,
        "dependency_cache_hit": "reusing cached native object" in dependency_second.stderr,
        "dependency_first_cache": dependency_first_cache,
        "dependency_second_cache": dependency_second_cache,
        "dependency_changed_cache": dependency_changed_cache,
        "dependency_exit": dependency_exit,
        "cache_cli": cache_cli,
        "changed": changed,
        "cache_hit": "reusing cached native object" in second.stderr,
        "first_cache": first_cache,
        "second_cache": second_cache,
        "uncached_cache": uncached_cache,
        "changed_cache": changed_cache,
        "output": sha256(output),
        "uncached_output": sha256(uncached_output),
        "changed_output": sha256(changed_output),
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
            ("stage1", exercise(stage1, temp_root / "stage1", root, True)),
        )

        failures: list[str] = []
        for label, result in results:
            for key in ("first", "second", "uncached", "cycle", "changed"):
                command: RunResult = result[key]  # type: ignore[assignment]
                if command.returncode != 0:
                    failures.append(
                        f"{label}: {key} build failed ({command.returncode})"
                    )
            if result["first_cache"] != result["second_cache"]:
                failures.append(f"{label}: repeated cache tree differs")
            if result["output"] != result["uncached_output"]:
                failures.append(f"{label}: cached and uncached executables differ")
            if result["output"] == result["changed_output"]:
                failures.append(f"{label}: edited source produced the old executable")
            if result["second_cache"] == result["changed_cache"]:
                failures.append(f"{label}: edited source did not add a cache entry")
            if result["second_cache"] != result["uncached_cache"]:
                failures.append(f"{label}: --no-cache changed the cache")
            corrupt_rebuild = result["corrupt_rebuild"]
            if corrupt_rebuild is not None:
                command = corrupt_rebuild
                if command.returncode != 0:  # type: ignore[union-attr]
                    failures.append(f"{label}: corrupt cache recovery failed")
                if result["corrupt_rebuild_cache"] != result["second_cache"]:
                    failures.append(f"{label}: corrupt cache entry was not repaired")
            for key in ("dependency_first", "dependency_second", "dependency_changed"):
                command: RunResult = result[key]  # type: ignore[assignment]
                if command.returncode != 0:
                    failures.append(f"{label}: {key} build failed ({command.returncode})")
            if result["dependency_second_cache"] == result["dependency_changed_cache"]:
                failures.append(f"{label}: imported source edit did not invalidate the cache")
            if result["dependency_exit"] != 1:
                failures.append(
                    f"{label}: executable did not reflect imported source edit "
                    f"(exit {result['dependency_exit']})"
                )

        if not results[1][1]["first_cache"]:
            failures.append("stage1: incremental object cache stayed empty")
        if not results[1][1]["cache_hit"]:
            failures.append("stage1: repeated build did not report a cache hit")
        if not results[1][1]["dependency_cache_hit"]:
            failures.append("stage1: repeated imported-source build did not report a cache hit")
        corrupt_rebuild = results[1][1]["corrupt_rebuild"]
        if corrupt_rebuild is None or "reusing cached native object" in corrupt_rebuild.stderr:
            failures.append("stage1: corrupt object was accepted as a cache hit")
        cache_cli = results[1][1]["cache_cli"]
        for key in (
            "info", "prune", "refill", "flag_prune", "refill_after_flag_prune",
            "subcommand_clean", "refill_after_subcommand_clean", "clean_alias", "clean_command",
            "flag_info",
        ):
            if cache_cli[key].returncode != 0:  # type: ignore[union-attr]
                failures.append(f"stage1: cache CLI command {key} failed")
        if "Files:" not in cache_cli["info"].stdout:  # type: ignore[union-attr]
            failures.append("stage1: cache info omitted file statistics")
        if "\x1b[" not in cache_cli["info"].stdout:  # type: ignore[union-attr]
            failures.append("stage1: cache info omitted styled output")
        if cache_cli["pruned_cache"]:  # type: ignore[truthy-bool]
            failures.append("stage1: cache prune did not remove entries to the requested limit")
        if cache_cli["flag_pruned_cache"]:  # type: ignore[truthy-bool]
            failures.append("stage1: --cache-prune did not remove entries to the requested limit")
        if cache_cli["subcommand_cleaned_cache"]:  # type: ignore[truthy-bool]
            failures.append("stage1: cache clean did not clear entries")
        if cache_cli["cleaned_cache"]:  # type: ignore[truthy-bool]
            failures.append("stage1: --clean-cache did not clear entries")
        if cache_cli["invalid_prune"].returncode != 2:  # type: ignore[union-attr]
            failures.append("stage1: invalid cache prune size was accepted")

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
