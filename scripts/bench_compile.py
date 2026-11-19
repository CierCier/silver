#!/usr/bin/env python3
"""Compare cold (`--no-cache`) build times of Silver programs across compilers.

Each run uses a fresh cache directory, so no compiler reuses another's
objects. Reports the median, min, and max wall time per compiler, file, and
optimization level.

    python3 scripts/bench_compile.py --compiler base=/tmp/agc-old \
        --compiler new=target/release/agc --opt 0 --opt 2 --runs 10 \
        examples/http_server.ag
"""

import argparse
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path


def build_once(compiler: str, source: Path, opt: str, workdir: Path) -> float:
    cache = Path(tempfile.mkdtemp(prefix="cache-", dir=workdir))
    command = [compiler, "build", str(source), f"-O{opt}", "-o", str(workdir / "a.out"),
               "--cache-dir", str(cache), "--no-cache", "--no-progress"]
    start = time.perf_counter()
    result = subprocess.run(command, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True)
    elapsed = time.perf_counter() - start
    if result.returncode != 0:
        sys.exit(f"build failed ({' '.join(command)}):\n{result.stderr}")
    return elapsed


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("sources", nargs="+", type=Path)
    parser.add_argument("--compiler", action="append", required=True, metavar="NAME=PATH")
    parser.add_argument("--opt", action="append", default=None, metavar="LEVEL")
    parser.add_argument("--runs", type=int, default=10)
    args = parser.parse_args()

    compilers = [spec.split("=", 1) for spec in args.compiler]
    print("compiler\tfile\topt\truns\tmedian_s\tmin_s\tmax_s")
    with tempfile.TemporaryDirectory(prefix="silver-bench-") as tmp:
        workdir = Path(tmp)
        for source in args.sources:
            for opt in args.opt or ["0"]:
                for name, path in compilers:
                    times = [build_once(path, source, opt, workdir) for _ in range(args.runs)]
                    print(f"{name}\t{source}\tO{opt}\t{args.runs}\t{statistics.median(times):.3f}"
                          f"\t{min(times):.3f}\t{max(times):.3f}", flush=True)


if __name__ == "__main__":
    main()
