#!/usr/bin/env python3
"""Compare build times of Silver programs across compilers.

Modes (repeat --mode to run several):
  cold  `--no-cache` build in a fresh cache directory (the default).
  warm  rebuild with no change, after one build filled a private cache.
  edit  rebuild after appending a comment to a copy of the input, so the
        input changes on every run while every import stays cached.

No compiler reuses another's cache. Reports the median, min, and max wall
time per mode, compiler, file, and optimization level.

    python3 scripts/bench_compile.py --compiler base=/tmp/agc-old \
        --compiler new=target/release/agc --opt 0 --opt 2 --runs 10 \
        --mode cold --mode warm --mode edit examples/http_server.ag
"""

import argparse
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path


def build(compiler: str, source: Path, opt: str, workdir: Path, cache_args: list[str]) -> float:
    command = [compiler, "build", str(source), f"-O{opt}", "-o", str(workdir / "a.out"),
               "--no-progress", *cache_args]
    start = time.perf_counter()
    result = subprocess.run(command, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
    elapsed = time.perf_counter() - start
    if result.returncode != 0:
        sys.exit(f"build failed ({' '.join(command)}):\n{result.stderr}")
    return elapsed


def measure(mode: str, compiler: str, source: Path, opt: str, runs: int, workdir: Path) -> list[float]:
    cache = Path(tempfile.mkdtemp(prefix="cache-", dir=workdir))
    cache_args = ["--cache-dir", str(cache)]
    if mode == "cold":
        times = []
        for _ in range(runs):
            fresh = Path(tempfile.mkdtemp(prefix="cache-", dir=workdir))
            times.append(build(compiler, source, opt, workdir, ["--cache-dir", str(fresh), "--no-cache"]))
        return times
    if mode == "warm":
        build(compiler, source, opt, workdir, cache_args)
        return [build(compiler, source, opt, workdir, cache_args) for _ in range(runs)]
    if mode == "edit":
        copy = Path(tempfile.mkdtemp(prefix="edit-", dir=workdir)) / source.name
        shutil.copyfile(source, copy)
        build(compiler, copy, opt, workdir, cache_args)
        times = []
        for run in range(runs):
            with copy.open("a") as file:
                file.write(f"\n// bench edit {run}\n")
            times.append(build(compiler, copy, opt, workdir, cache_args))
        return times
    sys.exit(f"unknown mode {mode}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("sources", nargs="+", type=Path)
    parser.add_argument("--compiler", action="append", required=True, metavar="NAME=PATH")
    parser.add_argument("--opt", action="append", default=None, metavar="LEVEL")
    parser.add_argument("--mode", action="append", default=None, choices=["cold", "warm", "edit"])
    parser.add_argument("--runs", type=int, default=10)
    args = parser.parse_args()

    compilers = [spec.split("=", 1) for spec in args.compiler]
    print("mode\tcompiler\tfile\topt\truns\tmedian_s\tmin_s\tmax_s")
    with tempfile.TemporaryDirectory(prefix="silver-bench-") as tmp:
        workdir = Path(tmp)
        for mode in args.mode or ["cold"]:
            for source in args.sources:
                for opt in args.opt or ["0"]:
                    for name, path in compilers:
                        times = measure(mode, path, source, opt, args.runs, workdir)
                        print(f"{mode}\t{name}\t{source}\tO{opt}\t{args.runs}"
                              f"\t{statistics.median(times):.3f}\t{min(times):.3f}"
                              f"\t{max(times):.3f}", flush=True)


if __name__ == "__main__":
    main()
