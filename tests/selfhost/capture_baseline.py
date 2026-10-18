#!/usr/bin/env python3
"""Capture a reproducible self-host baseline from the current source snapshot.

Stage0 is rebuilt with cargo, stage1 is rebuilt from it (``--no-cache``), and the
integration corpus runs with each compiler, writing ``baseline.json`` and
per-command logs under an explicit output directory. A stale ``agc-stage1`` can
never be reported as fresh.

Source identity hashes ``HEAD``, ``git status --porcelain``, ``git diff --binary
HEAD``, and untracked file bytes, then re-checks it after the run: dirty trees are
labeled dirty and mid-run changes yield ``source-changed``. Skips and failures are
recorded with reasons; a failed corpus or generation run records its exit status
instead of aborting. Corpora run sequentially because TLS fixtures bind fixed
ports. Stage0 uses the harness's normal stage0 path; stage1 passes ``--compiler``.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timezone

SCHEMA = "silver-selfhost-baseline/v1"
HARNESS_REL = "tests/run_tests.py"
GENERATIONS_REL = "tests/selfhost/check_stage2.py"
ANSI_RE = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
RESULT_RE = re.compile(r"\s*\[(PASS|FAIL|SKIP)\]\s+(\S+)\s+\((.*)\)\s*$")
SUMMARY_RE = re.compile(r"\s*(Passed|Failed|Skipped):\s+(\d+)")


class EvidenceError(RuntimeError):
    """Raised when the output directory would clobber existing evidence."""


def utc_now() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def sha256_file(path: pathlib.Path) -> str:
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def run_logged(
    argv: list[str], cwd: pathlib.Path, env: dict[str, str],
    log_path: pathlib.Path, label: str,
) -> dict:
    """Run a command, streaming combined output to the console and a log file."""
    log_path.parent.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    with open(log_path, "w", encoding="utf-8", errors="replace") as log:
        log.write(f"$ {' '.join(shlex.quote(argument) for argument in argv)}\n")
        log.write(f"# cwd: {cwd}\n\n")
        log.flush()
        try:
            process = subprocess.Popen(
                argv, cwd=str(cwd), env=env, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT, text=True, errors="replace", bufsize=1,
            )
        except FileNotFoundError as error:
            exit_code = 127
            sys.stdout.write(f"failed to spawn {argv[0]}: {error}\n")
        else:
            assert process.stdout is not None
            for line in process.stdout:
                sys.stdout.write(line)
                sys.stdout.flush()
                log.write(line)
            exit_code = process.wait()
    return {
        "label": label,
        "argv": argv,
        "cwd": str(cwd),
        "exit_code": exit_code,
        "duration_s": round(time.monotonic() - started, 3),
        "log": str(log_path),
    }


def probe_binary(binary: pathlib.Path, cwd: pathlib.Path, env: dict[str, str]) -> dict:
    """Record a compiler's path, size, version, and SHA-256 identity."""
    identity = {"path": str(binary), "sha256": sha256_file(binary),
                "size": binary.stat().st_size}
    try:
        result = subprocess.run(
            [str(binary), "--version"], cwd=str(cwd), env=env, capture_output=True,
            text=True, timeout=60, check=False, errors="replace",
        )
        identity["version"] = (result.stdout + result.stderr).strip() or None
        identity["version_exit_code"] = result.returncode
    except (OSError, subprocess.TimeoutExpired):
        identity["version"] = None
        identity["version_exit_code"] = None
    return identity


def tool_versions(root: pathlib.Path, env: dict[str, str]) -> dict:
    """Record toolchain versions that affect build output."""
    def first_line(command: list[str]) -> str | None:
        if not shutil.which(command[0]):
            return None
        output = subprocess.run(
            command, cwd=str(root), env=env, capture_output=True, text=True,
            check=False, errors="replace",
        )
        combined = (output.stdout + output.stderr).strip()
        return combined.splitlines()[0] if combined else None

    return {
        "python": sys.version.split()[0],
        "uname": " ".join(os.uname()),
        "cpus": str(os.cpu_count() or 0),
        "cargo": first_line(["cargo", "--version"]),
        "rustc": first_line(["rustc", "--version"]),
        "cc": first_line(["cc", "--version"]),
        "llvm_config": first_line(["llvm-config", "--version"]),
    }


def snapshot_identity(root: pathlib.Path) -> dict:
    """Hash the full working-tree identity, including uncommitted changes."""
    def git(*arguments: str) -> str:
        result = subprocess.run(
            ["git", *arguments], cwd=str(root), capture_output=True, text=True,
            check=False, errors="replace",
        )
        if result.returncode != 0:
            raise RuntimeError(f"git {' '.join(arguments)} failed: {result.stderr.strip()}")
        return result.stdout

    head = git("rev-parse", "HEAD").strip()
    branch = git("rev-parse", "--abbrev-ref", "HEAD").strip()
    status = git("status", "--porcelain")
    diff = git("diff", "--binary", "HEAD")
    untracked = git("ls-files", "--others", "--exclude-standard").splitlines()

    digest = hashlib.sha256()
    for part in (head, status, diff):
        digest.update(part.encode("utf-8", "replace"))
        digest.update(b"\0")
    for relative in sorted(untracked):
        digest.update(relative.encode("utf-8", "replace"))
        digest.update(b"\0")
        # Fail loudly on an unreadable untracked file rather than hashing a
        # sentinel that would misrepresent the snapshot.
        with open(root / relative, "rb") as handle:
            for chunk in iter(lambda: handle.read(1 << 20), b""):
                digest.update(chunk)
        digest.update(b"\0")

    snapshot_id = digest.hexdigest()
    clean = status.strip() == ""
    modified = []
    for line in status.splitlines():
        if len(line) >= 4:
            entry = line[3:]
            modified.append(entry.split(" -> ", 1)[-1])
    return {
        "root": str(root),
        "commit": head,
        "branch": branch,
        "clean": clean,
        "dirty": not clean,
        "snapshot_id": snapshot_id,
        "commit_label": head if clean else f"{head}-dirty-{snapshot_id[:12]}",
        "modified_paths": modified,
        "untracked_files": len(untracked),
    }


def parse_harness_log(text: str) -> dict:
    """Extract pass/fail/skip counts and every skip reason from a harness log."""
    parsed: dict = {"passed": None, "failed": None, "skipped": None,
                    "skips": [], "failures": []}
    for line in ANSI_RE.sub("", text).splitlines():
        result = RESULT_RE.match(line)
        if result:
            status, name, detail = result.groups()
            if status == "SKIP":
                parsed["skips"].append({"name": name, "reason": detail})
            elif status == "FAIL":
                parsed["failures"].append({"name": name, "reason": detail})
            continue
        summary = SUMMARY_RE.match(line)
        if summary:
            parsed[summary.group(1).lower()] = int(summary.group(2))
    if parsed["skipped"] is not None and parsed["skipped"] != len(parsed["skips"]):
        parsed["skip_count_mismatch"] = True
    return parsed


def summary_problems(parsed: dict) -> list[str]:
    """Report a corpus summary that cannot be trusted as a full record."""
    problems = []
    if None in (parsed["passed"], parsed["failed"], parsed["skipped"]):
        problems.append("missing harness pass/fail/skip summary")
    if parsed.get("skip_count_mismatch"):
        problems.append("summary skip count does not match recorded skips")
    return problems


def parse_generation_log(text: str) -> dict:
    sections = {"stage2": [], "stage3": []}
    current = None
    for line in text.splitlines():
        if line.startswith("Running the full integration corpus with agc-"):
            current = line.rsplit("agc-", 1)[1]
        elif line.startswith("Building stage3 with stage2"):
            current = None
        elif current in sections:
            sections[current].append(line)
    return {label: parse_harness_log("\n".join(lines))
            for label, lines in sections.items()}


def prepare_out_dir(out: pathlib.Path) -> None:
    """Create the evidence directory, refusing to clobber prior evidence."""
    if out.exists() and not out.is_dir():
        raise EvidenceError(f"output path exists and is not a directory: {out}")
    if out.exists() and any(out.iterdir()):
        raise EvidenceError(
            f"refusing to overwrite existing evidence in {out}; choose a new directory"
        )
    out.mkdir(parents=True, exist_ok=True)
    (out / "logs").mkdir(exist_ok=True)


def corpus_argv(root: pathlib.Path, profile: str, jobs: int,
                compiler: pathlib.Path | None) -> list[str]:
    argv = [sys.executable, str(root / HARNESS_REL), "--no-tui", "--jobs", str(jobs)]
    if profile == "release":
        argv.append("--release")
    if compiler is not None:
        argv.extend(["--compiler", str(compiler)])
    return argv


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Capture a fresh stage0/stage1 self-host baseline.",
    )
    parser.add_argument("--out", required=True, type=pathlib.Path,
                        help="evidence output directory (must be new or empty)")
    parser.add_argument("--profile", choices=("debug", "release"), default="debug",
                        help="cargo profile for stage0 (default: debug)")
    parser.add_argument("--jobs", type=int, default=4,
                        help="integration corpus workers (default: 4)")
    parser.add_argument("--generations", action="store_true",
                        help="also run check_stage2.py --full-corpus --stage3")
    args = parser.parse_args(argv)
    if args.jobs < 1:
        parser.error("--jobs must be positive")

    root = pathlib.Path(__file__).resolve().parents[2]
    out = args.out.resolve()
    try:
        prepare_out_dir(out)
    except EvidenceError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    env = os.environ.copy()
    env["CARGO_TARGET_DIR"] = str(root / "target")
    commands: list[dict] = []
    failures: list[str] = []
    before = snapshot_identity(root)

    def record(label: str, command: list[str], log: str) -> dict:
        entry = run_logged(command, root, env, out / "logs" / log, label)
        entry["log"] = log
        commands.append(entry)
        return entry

    def corpus(label: str, log: str, compiler: pathlib.Path | None) -> dict:
        run = record(label, corpus_argv(root, args.profile, args.jobs, compiler), log)
        parsed = parse_harness_log(
            (out / "logs" / log).read_text(encoding="utf-8", errors="replace")
        )
        if run["exit_code"] != 0:
            failures.append(f"{label} failed")
        failures.extend(f"{label}: {problem}" for problem in summary_problems(parsed))
        return {"argv": run["argv"], "exit_code": run["exit_code"], "log": log, **parsed}

    entry: dict = {
        "schema": SCHEMA,
        "captured_utc": utc_now(),
        "profile": args.profile,
        "jobs": args.jobs,
        "source": before,
        "tools": tool_versions(root, env),
        "cargo_target_dir": env["CARGO_TARGET_DIR"],
        "commands": commands,
        "stage0": None,
        "stage1": None,
        "corpora": {},
        "generations": None,
        "status": "ok",
        "failures": failures,
    }

    build_argv = ["cargo", "build", "-p", "agc"]
    if args.profile == "release":
        build_argv.append("--release")
    build = record("cargo build stage0", build_argv, "01-cargo-build-stage0.log")

    # Never fall back to a pre-existing target/<profile>/agc after a failed build.
    stage0 = root / "target" / args.profile / "agc"
    if build["exit_code"] != 0:
        failures.append("stage0 cargo build failed")
    elif not stage0.is_file():
        failures.append(f"stage0 binary missing after build: {stage0}")
    else:
        entry["stage0"] = probe_binary(stage0, root, env)
        if entry["stage0"]["version_exit_code"] != 0:
            failures.append("stage0 version probe failed")

    # Always rebuild stage1 from this stage0; a cached stage1 is never accepted.
    # The binary is kept out of the evidence directory; its hash is the record.
    work = pathlib.Path(tempfile.mkdtemp(prefix="silver-baseline-"))
    stage1 = work / "agc-stage1"
    if entry["stage0"] is not None:
        built = record(
            "build stage1 with stage0",
            [str(stage0), "--no-cache", "build", str(root / "silver.toml"),
             "--bin", "agc", "-o", str(stage1)],
            "02-build-stage1.log",
        )
        if built["exit_code"] != 0 or not stage1.is_file():
            failures.append("stage1 build failed")
        else:
            entry["stage1"] = probe_binary(stage1, root, env)
            if entry["stage1"]["version_exit_code"] != 0:
                failures.append("stage1 version probe failed")

    if entry["stage1"] is not None:
        entry["corpora"]["stage0"] = corpus(
            "stage0 integration corpus", "03-corpus-stage0.log", None)
        entry["corpora"]["stage1"] = corpus(
            "stage1 integration corpus", "04-corpus-stage1.log", stage1)

    if args.generations:
        if entry["stage1"] is None:
            failures.append("generations skipped: stage1 unavailable")
        else:
            evidence = out / "generations"
            evidence.mkdir(exist_ok=True)
            run = record(
                "stage2/stage3 generations",
                [sys.executable, str(root / GENERATIONS_REL), "--stage1", str(stage1),
                 "--full-corpus", "--stage3", "--jobs", str(args.jobs),
                 "--evidence-dir", str(evidence)],
                "05-generations.log",
            )
            identity = {}
            for label in ("stage2", "stage3"):
                path = evidence / f"{label}.json"
                identity[label] = (json.loads(path.read_text(encoding="utf-8"))
                                   if path.is_file() else {"captured": False})
            corpora = parse_generation_log(
                (out / "logs" / "05-generations.log").read_text(
                    encoding="utf-8", errors="replace"))
            if run["exit_code"] != 0:
                failures.append("stage2/stage3 generations failed")
            else:
                for label in ("stage2", "stage3"):
                    if not identity[label].get("sha256"):
                        failures.append(f"generations: {label} compiler identity missing")
                for label, parsed in corpora.items():
                    failures.extend(f"{label}: {problem}"
                                    for problem in summary_problems(parsed))
            entry["generations"] = {"argv": run["argv"], "exit_code": run["exit_code"],
                                    "log": "05-generations.log", "corpora": corpora,
                                    "compilers": identity}

    after = snapshot_identity(root)
    entry["source_after"] = after
    entry["snapshot_stable"] = after["snapshot_id"] == before["snapshot_id"]
    entry["finished_utc"] = utc_now()
    if not entry["snapshot_stable"]:
        entry["status"] = "source-changed"
    elif failures:
        entry["status"] = "failed"

    baseline_path = out / "baseline.json"
    baseline_path.write_text(json.dumps(entry, indent=2) + "\n", encoding="utf-8")
    print(f"baseline: {baseline_path}")
    for name, parsed in entry["corpora"].items():
        print(f"  {name} corpus: {parsed['passed']}/{parsed['failed']}/"
              f"{parsed['skipped']} (exit {parsed['exit_code']})")
    print(f"  status: {entry['status']}  source: {before['commit_label']} "
          f"({'clean' if before['clean'] else 'dirty'})")
    for failure in failures:
        print(f"  failure: {failure}")

    shutil.rmtree(work, ignore_errors=True)
    return 2 if entry["status"] == "source-changed" else (1 if failures else 0)


if __name__ == "__main__":
    raise SystemExit(main())
