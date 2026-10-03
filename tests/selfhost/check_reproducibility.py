"""GATE-004: repeated and parallel builds must preserve output bytes.

Builds a multi-module fixture twice without cache, twice cached serially, and
twice cached with parallel module compilation. Cache/no-cache equality is
reported separately because those paths may legitimately emit different bytes.
For stage1, pass --stage0 to test the compiler's bridge path; the native backend
has a separate smoke gate and does not support multi-module builds.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import pathlib
import subprocess
import tempfile


def build(agc: pathlib.Path, source: pathlib.Path, out: pathlib.Path,
          env: dict[str, str], no_cache: bool, jobs: int) -> None:
    cmd = [str(agc), "build", str(source), "--jobs", str(jobs)]
    if no_cache:
        cmd.append("--no-cache")
    cmd += ["-o", str(out)]
    proc = subprocess.run(cmd, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, env=env, check=False)
    if proc.returncode != 0:
        raise AssertionError(f"build failed: rc={proc.returncode}\n{proc.stderr}")

def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--agc", required=True, type=pathlib.Path,
                        help="compiler binary to test (stage0 or stage1)")
    parser.add_argument("--stage0", type=pathlib.Path,
                        help="stage0 compiler used by a stage1 compiler's bridge")
    args = parser.parse_args()
    agc = args.agc.resolve()

    with tempfile.TemporaryDirectory(prefix="silver-repro-") as temporary:
        root = pathlib.Path(temporary)
        (root / "main.ag").write_text(
            "import alpha;\nimport beta;\n"
            "i32 main() { return alpha_value() + beta_value(); }\n"
        )
        (root / "alpha.ag").write_text("i32 alpha_value() { return 20; }\n")
        (root / "beta.ag").write_text("i32 beta_value() { return 22; }\n")
        env = os.environ.copy()
        if args.stage0 is not None:
            env["SILVER_STAGE0"] = str(args.stage0.resolve())
            env.pop("SILVER_STAGE1_NATIVE", None)
        builds = (
            ("no-cache", True, 1),
            ("no-cache", True, 1),
            ("cached-serial", False, 1),
            ("cached-serial", False, 1),
            ("cached-parallel", False, 4),
            ("cached-parallel", False, 4),
        )
        digests: list[str] = []
        for i, (_, no_cache, jobs) in enumerate(builds):
            out = root / f"out{i}"
            build(agc, root / "main.ag", out, env, no_cache, jobs)
            digests.append(sha256(out))

        for mode, indices in (
            ("no-cache repeats", (0, 1)),
            ("cached serial repeats", (2, 3)),
            ("cached parallel repeats", (4, 5)),
            ("serial vs parallel", (2, 4)),
        ):
            if digests[indices[0]] != digests[indices[1]]:
                print(f"REPRODUCIBILITY FAILED ({mode} differ):")
                for i, digest in enumerate(digests):
                    print(f"  build{i} ({builds[i][0]}): {digest}")
                return 1

        print("cache-mode comparison (categorized, not an identity invariant):")
        if digests[0] == digests[2]:
            print(f"  CACHE_MODE_EQUAL: {digests[0]}")
        else:
            print(f"  CACHE_MODE_DIFFERENCE: no-cache={digests[0]} cached={digests[2]}")
        print("repeat and parallel build byte-identity passed")

if __name__ == "__main__":
    raise SystemExit(main())
