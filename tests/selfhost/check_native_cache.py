#!/usr/bin/env python3
"""Check repeated and uncached native builds for stage0 and stage1 on Linux."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import textwrap
import time
from dataclasses import asdict, dataclass

REPO_ROOT = pathlib.Path(__file__).parents[2]
ANSI = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")
COMPILE_ROW = re.compile(
    r"^\s*\[\s*\d+\s*/\s*(\d+)\s*\]\s*\d+%\s+"
    r"(compiled(?:\s+in\s+\S+)?|\[cached\])\s+(.+?)\s+\((\d+) item(?:s)?"
)
COMPILE_START = re.compile(
    r"^\s*\[\s*\d+\s*/\s*\d+\s*\]\s*\d+%\s+compiling\s+(.+?)"
    r"(?:\s+\(\d+ items?\))?$"
)


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


def progress_modules(
    result: RunResult, source: pathlib.Path
) -> tuple[int, dict[str, str]]:
    rows = []
    for line in ANSI.sub("", result.stderr).splitlines():
        match = COMPILE_ROW.match(line)
        if match is None:
            continue
        status = "cached" if match.group(2) == "[cached]" else "compiled"
        target = match.group(3)
        if target in (str(source), source.name, source.stem):
            target = "<root>"
        rows.append((int(match.group(1)), target, status))
    totals = {total for total, _target, _status in rows}
    if not rows or len(totals) != 1:
        raise AssertionError(
            "native module progress omitted compile completion rows or used "
            f"inconsistent step totals: {result.stderr}"
        )
    modules = {target: status for _total, target, status in rows}
    if len(modules) != len(rows):
        raise AssertionError(f"{result.stderr}: duplicate module completion row")
    total_steps = totals.pop()
    if total_steps != len(modules) + 1:
        raise AssertionError(
            f"progress total {total_steps} does not include one link step after "
            f"{len(modules)} module completions: {result.stderr}"
        )
    return total_steps, modules


def progress_start_counts(result: RunResult, source: pathlib.Path) -> dict[str, int]:
    starts: dict[str, int] = {}
    for line in ANSI.sub("", result.stderr).splitlines():
        match = COMPILE_START.match(line)
        if match is None:
            continue
        target = match.group(1)
        if target in (str(source), source.name, source.stem):
            target = "<root>"
        starts[target] = starts.get(target, 0) + 1
    return starts


def progress_item_counts(result: RunResult, source: pathlib.Path) -> dict[str, int]:
    item_counts = {}
    for line in ANSI.sub("", result.stderr).splitlines():
        match = COMPILE_ROW.match(line)
        if match is None:
            continue
        target = match.group(3)
        if target in (str(source), source.name, source.stem):
            target = "<root>"
        if target in item_counts:
            raise AssertionError(f"{result.stderr}: duplicate module completion row")
        item_counts[target] = int(match.group(4))
    if not item_counts:
        raise AssertionError(f"native progress omitted item counts: {result.stderr}")
    return item_counts


def exercise_module_boundaries(
    binary: pathlib.Path,
    temp_root: pathlib.Path,
    root: pathlib.Path,
    compiler_label: str,
    require_all_cached: bool = False,
) -> dict[str, object]:
    work = temp_root / "module-boundaries"
    work.mkdir(parents=True, exist_ok=True)
    source = work / "main.ag"
    dependency = work / "dep.ag"
    source.write_text(
        textwrap.dedent(
            """\
            import std.mem.drop;
            import std.string;
            import std.mem.vec;
            import dep;

            i64 root_drop_count = 0;
            struct Probe { i64 value; }
            impl Drop for Probe {
                void drop(Probe* self) {
                    root_drop_count = root_drop_count + self.value;
                }
            }

            i32 main() {
                String original = String.from_str("forty two");
                String received = identity<String>(move original);
                Vec<String> values = Vec<String>.new();
                values.push(move received);
                {
                    Probe probe;
                    probe.value = 1;
                }
                if (root_drop_count != 1) { return 3; }
                ModuleValue measured;
                measured.value = 40;
                if (measured.increment(2) != 42 || measured.current() != 42) { return 5; }
                if (read_cfg_value() != 7) { return 6; }
                bump();
                if (module_counter != 1) { return 1; }
                if (!values.get_ptr(0).equals("forty two")) { return 2; }
                String extracted = values.pop();
                if (!extracted.equals("forty two")) { return 4; }
                @println("module-boundary-ok");
                return 42;
            }
            """
        ),
        encoding="utf-8",
    )
    dependency.write_text(
        textwrap.dedent(
            """\
            import std.string;
            i64 module_counter = 0;
            struct ModuleValue { i64 value; }
            impl ModuleValue {
                i64 increment(ModuleValue* self, i64 amount) {
                    self.value = self.value + amount;
                    return self.value;
                }
                i64 current(ModuleValue* self) { return self.value; }
            }
            if (@cfg(os.linux)) {
                i32 cfg_value() { return 7; }
            } else {
                i32 cfg_value() { return 8; }
            }
            T identity<T>(T value) { return move value; }
            void bump() { module_counter = module_counter + 1; }
            i32 read_cfg_value() { return cfg_value(); }
            """
        ),
        encoding="utf-8",
    )
    cache = work / "cache" / compiler_label
    env = os.environ.copy()
    base = [
        "build", str(source), "--cache-dir", str(cache),
        "--progress", "--verbose",
        "-I", str(root),
    ]
    outputs = {
        "cold": work / f"{compiler_label}-cold-app",
        "warm": work / f"{compiler_label}-warm-app",
        "uncached": work / f"{compiler_label}-uncached-app",
    }
    cold = run(binary, [*base, "-o", str(outputs["cold"])], root, env)
    warm = run(binary, [*base, "-o", str(outputs["warm"])], root, env)
    uncached = run(
        binary,
        [
            "build", str(source), "--no-cache", "--no-progress",
            "-I", str(root), "-o", str(outputs["uncached"]),
        ],
        root,
        env,
    )
    for label, result in (("cold", cold), ("warm", warm), ("uncached", uncached)):
        if result.returncode != 0:
            raise AssertionError(
                f"{binary.name}: {label} module-boundary build failed "
                f"({result.returncode})\n{result.stdout}\n{result.stderr}"
            )

    cold_progress = progress_modules(cold, source)
    warm_progress = progress_modules(warm, source)
    cold_item_counts = progress_item_counts(cold, source)
    if (
        cold_progress[0] != warm_progress[0]
        or cold_progress[1].keys() != warm_progress[1].keys()
    ):
        raise AssertionError(
            f"{binary.name}: cold and warm module progress differ: "
            f"{cold_progress!r} vs {warm_progress!r}"
        )
    if any(status != "compiled" for status in cold_progress[1].values()):
        raise AssertionError(f"{binary.name}: cold build did not compile every module")
    if require_all_cached:
        cold_starts = progress_start_counts(cold, source)
        if (
            not cold_starts
            or any(count != 1 for count in cold_starts.values())
            or not cold_starts.keys() <= cold_progress[1].keys()
        ):
            raise AssertionError(
                f"{binary.name}: verbose cold build should report at least one "
                f"compiling module and report each once, got {cold_starts!r}\n"
                f"{cold.stderr[:4000]}"
            )
    imported_modules = {
        name for name in cold_progress[1] if name != "<root>"
    }
    if "dep" not in imported_modules:
        raise AssertionError(
            f"{binary.name}: standalone dep boundary missing from cold progress: "
            f"{cold_progress!r}"
        )
    if "dep" not in cold_item_counts:
        raise AssertionError(f"{binary.name}: standalone dep item count missing")
    if any(warm_progress[1][name] != "cached" for name in imported_modules):
        raise AssertionError(f"{binary.name}: warm build missed imported module caches")
    if require_all_cached and any(
        status != "cached" for status in warm_progress[1].values()
    ):
        raise AssertionError(f"{binary.name}: warm build did not reuse every module")

    for label, output in outputs.items():
        executed = subprocess.run(
            [str(output)], cwd=root, env=env, capture_output=True,
            text=True, check=False,
        )
        if (executed.returncode, executed.stdout, executed.stderr) != (
            42, "module-boundary-ok\n", ""
        ):
            raise AssertionError(
                f"{binary.name}: {label} module-boundary executable returned "
                f"{executed.returncode}, stdout={executed.stdout!r}, "
                f"stderr={executed.stderr!r}"
            )

    simple_source = work / "simple.ag"
    simple_source.write_text("i32 main() { return 42; }\n", encoding="utf-8")
    simple_output = work / f"{compiler_label}-simple-app"
    simple_cache = work / "simple-cache" / compiler_label
    simple = run(
        binary,
        [
            "build", str(simple_source), "--cache-dir", str(simple_cache),
            "--progress", "--verbose", "-o", str(simple_output),
        ],
        root,
        env,
    )
    if simple.returncode != 0:
        raise AssertionError(
            f"{binary.name}: simple no-import build failed "
            f"({simple.returncode})\n{simple.stdout}\n{simple.stderr}"
        )
    simple_progress = progress_modules(simple, simple_source)
    if simple_progress != (2, {"<root>": "compiled"}):
        raise AssertionError(
            f"{binary.name}: simple program should report only its root compile "
            f"and link steps, got {simple_progress!r}\n{simple.stderr}"
        )
    executed = subprocess.run(
        [str(simple_output)], cwd=root, env=env, capture_output=True,
        text=True, check=False,
    )
    if (executed.returncode, executed.stdout, executed.stderr) != (42, "", ""):
        raise AssertionError(
            f"{binary.name}: simple executable returned {executed.returncode}, "
            f"stdout={executed.stdout!r}, stderr={executed.stderr!r}"
        )

    job_thread_count = None
    jobs_two_thread_count = None
    if require_all_cached:
        strace = shutil.which("strace")
        if strace is not None:
            available_cpus = sorted(os.sched_getaffinity(0))
            if len(available_cpus) > 1:
                affinity = set(available_cpus[:2])
                thread_counts = []
                for jobs in (1, 2):
                    trace = work / f"jobs-{jobs}.trace"
                    jobs_output = work / f"stage1-jobs-{jobs}-app"
                    traced = subprocess.run(
                        [
                            strace, "-f", "-e", "trace=clone,clone3", "-o", str(trace),
                            str(binary), "build", str(source), "--no-cache", "--no-progress",
                            "-j", str(jobs), "-I", str(root), "-o", str(jobs_output),
                        ],
                        cwd=root,
                        env=env,
                        capture_output=True,
                        text=True,
                        check=False,
                        preexec_fn=lambda: os.sched_setaffinity(0, affinity),
                    )
                    if traced.returncode != 0:
                        raise AssertionError(
                            f"{binary.name}: -j {jobs} build failed ({traced.returncode})\n"
                            f"{traced.stdout}\n{traced.stderr}"
                        )
                    trace_text = trace.read_text(encoding="utf-8")
                    thread_counts.append(sum(
                        "CLONE_THREAD" in line for line in trace_text.splitlines()
                    ))
                job_thread_count, jobs_two_thread_count = thread_counts
                if jobs_two_thread_count != job_thread_count + 1:
                    raise AssertionError(
                        f"{binary.name}: -j 1/-j 2 worker counts were "
                        f"{job_thread_count}/{jobs_two_thread_count}"
                    )
    invalid_jobs = run(
        binary,
        ["build", str(source), "--no-cache", "--no-progress", "-j", "0", "-I", str(root),
         "-o", str(work / "stage1-jobs-invalid-app")],
        root,
        env,
    ) if require_all_cached else None
    if invalid_jobs is not None and (
        invalid_jobs.returncode != 2 or "--jobs must be a positive integer" not in invalid_jobs.stderr
    ):
        raise AssertionError(f"{binary.name}: invalid -j 0 was not rejected")
    return {
        "cold_progress": cold_progress,
        "warm_progress": warm_progress,
        "simple_progress": simple_progress,
        "cold_item_counts": cold_item_counts,
        "jobs_thread_count": job_thread_count,
        "jobs_two_thread_count": jobs_two_thread_count,
    }


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
    progress_args = ["--progress"] if check_corrupt_entry else ["--no-progress"]
    base = [str(source), "--cache-dir", str(cache), *progress_args, "--verbose"]
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
            for cached_object in cached_objects:
                cached_object.write_bytes(b"\x7fELF")
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
        *progress_args, "--verbose", "-o", str(dependency_output),
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
        cache_root = cache / "native-object-v1"
        cache_root.mkdir(parents=True, exist_ok=True)
        external = work / "external-cache-target"
        external.mkdir()
        external_file = external / "keep.o"
        external_file.write_bytes(b"must remain untouched")
        (cache_root / "external-link").symlink_to(external, target_is_directory=True)
        symlink_clean = run(
            binary, ["cache", "clean", "--cache-dir", str(cache)], root, env
        )
        root_symlink_base = work / "root-symlink-cache"
        root_symlink_base.mkdir()
        root_symlink_external = work / "external-root-target"
        root_symlink_external.mkdir()
        root_symlink_file = root_symlink_external / "keep.o"
        root_symlink_file.write_bytes(b"must remain untouched")
        (root_symlink_base / "native-object-v1").symlink_to(
            root_symlink_external, target_is_directory=True
        )
        root_symlink_clean = run(
            binary,
            ["cache", "clean", "--cache-dir", str(root_symlink_base)],
            root,
            env,
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
            "symlink_clean": symlink_clean,
            "symlink_external_exists": external_file.exists(),
            "root_symlink_clean": root_symlink_clean,
            "root_symlink_external_exists": root_symlink_file.exists(),
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
        "dependency_cache_hit": "[cached]" in dependency_second.stderr,
        "dependency_first_cache": dependency_first_cache,
        "dependency_second_cache": dependency_second_cache,
        "dependency_changed_cache": dependency_changed_cache,
        "dependency_exit": dependency_exit,
        "cache_cli": cache_cli,
        "changed": changed,
        "cache_hit": "[cached]" in second.stderr,
        "first_cache": first_cache,
        "second_cache": second_cache,
        "uncached_cache": uncached_cache,
        "changed_cache": changed_cache,
        "output": sha256(output),
        "uncached_output": sha256(uncached_output),
        "changed_output": sha256(changed_output),
    }


def as_json(value: object) -> str:
    def normalize(item: object) -> object:
        if isinstance(item, RunResult):
            return asdict(item)
        if isinstance(item, dict):
            return {key: normalize(nested) for key, nested in item.items()}
        if isinstance(item, (list, tuple)):
            return [normalize(nested) for nested in item]
        return item

    return json.dumps(normalize(value), sort_keys=True)


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
        module_results = []
        for label, binary in (("stage0", stage0), ("stage1", stage1)):
            try:
                module_results.append(
                    (
                        label,
                        exercise_module_boundaries(
                            binary,
                            temp_root,
                            root,
                            label,
                            require_all_cached=label == "stage1",
                        ),
                    )
                )
            except AssertionError as error:
                failures.append(f"{label}: module-boundary check failed: {error}")
        if len(module_results) == 2:
            for phase in ("cold_progress", "warm_progress", "simple_progress"):
                stage0_progress = module_results[0][1][phase]
                stage1_progress = module_results[1][1][phase]
                if (
                    stage0_progress[0] != stage1_progress[0]
                    or stage0_progress[1].keys() != stage1_progress[1].keys()
                ):
                    stage0_names = set(stage0_progress[1])
                    stage1_names = set(stage1_progress[1])
                    failures.append(
                        f"stage0/stage1 {phase} module-boundary progress differs: "
                        f"total steps {stage0_progress[0]} vs {stage1_progress[0]}, "
                        f"module rows {len(stage0_names)} vs {len(stage1_names)}, "
                        f"only stage0={sorted(stage0_names - stage1_names)!r}, "
                        f"only stage1={sorted(stage1_names - stage0_names)!r}"
                    )
            stage0_counts = module_results[0][1]["cold_item_counts"]
            stage1_counts = module_results[1][1]["cold_item_counts"]
            for module_name in ("dep", "std.cpu"):
                if module_name in stage0_counts and module_name in stage1_counts:
                    if stage0_counts[module_name] != stage1_counts[module_name]:
                        failures.append(
                            f"stage0/stage1 {module_name} progress item count differs: "
                            f"{stage0_counts[module_name]} vs {stage1_counts[module_name]}"
                        )

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
        if corrupt_rebuild is None or "[cached]" in corrupt_rebuild.stderr:
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
        if cache_cli["symlink_clean"].returncode != 0:  # type: ignore[union-attr]
            failures.append("stage1: cache clean failed with an entry symlink")
        if not cache_cli["symlink_external_exists"]:
            failures.append("stage1: cache clean removed a file through an entry symlink")
        if cache_cli["root_symlink_clean"].returncode == 0:  # type: ignore[union-attr]
            failures.append("stage1: cache clean accepted a symlink cache root")
        if not cache_cli["root_symlink_external_exists"]:
            failures.append("stage1: cache clean removed a file through a root symlink")

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
            f"module_steps={module_results[0][1]['cold_progress'][0] if module_results else 'failed'}, "
            f"module_rows={len(module_results[0][1]['cold_progress'][1]) if module_results else 'failed'}, "
            f"cache_files={len(results[1][1]['first_cache'])}"  # type: ignore[arg-type]
        )
        return 0


if __name__ == "__main__":
    raise SystemExit(main())
