#!/usr/bin/env python3
"""Measure how much of the integration corpus the stage1-owned native backend
can compile and run WITHOUT the stage0 bridge.

Sets SILVER_STAGE0 to a missing binary so any fallback to the bridge fails.
Reports per-test pass/fail through the stage1 backend only. This is the
coverage gate for removing the bridge: it is green when every test that
passes through the bridge also passes through the stage1 backend.
"""

from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import sys


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    parser.add_argument("--filter", default="", help="only run tests matching this substring")
    parser.add_argument("--timeout", type=int, default=120)
    parser.add_argument("--expect-fail", action="store_true",
                        help="report only; exit 0 even with failures (triage mode)")
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    root = pathlib.Path(__file__).parents[2]

    env = os.environ.copy()
    env["SILVER_STAGE0"] = "/nonexistent-stage0-agc"
    env["SILVER_STAGE1_NATIVE"] = "1"
    # Server tests bind ports and never exit on their own; the integration
    # harness drives them specially, so they are out of scope here.
    skip = {"http_server_test", "http2_server_test", "https_server_test",
            "http_bench", "http_perf_test", "server_raw_test", "pool_test",
            "stream_test", "websocket_test", "sse_test", "tls_test",
            "dial_timeout_test", "timeout_test", "http_test", "http2_test",
            "http2_tls_test", "json_tcp_test", "cookie_test", "display_net_test"}

    passed: list[str] = []
    failed: list[tuple[str, str]] = []
    workdir = pathlib.Path("/tmp/opencode/native-cov-work")
    workdir.mkdir(parents=True, exist_ok=True)
    for path in sorted((root / "tests").glob("*.ag")):
        if args.filter and args.filter not in path.name:
            continue
        if path.stem in skip:
            continue
        out = pathlib.Path(f"/tmp/opencode/native-cov-{path.stem}")
        try:
            # cwd=/tmp blocks the relative target/debug/agc fallback, so only
            # the stage1-owned backend can succeed here.
            build = subprocess.run(
                [str(stage1), "build", str(path), "--no-cache", "-o", str(out)],
                cwd=str(workdir), capture_output=True, text=True, timeout=args.timeout,
            )
        except subprocess.TimeoutExpired:
            failed.append((path.name, "build timeout"))
            continue
        if build.returncode != 0:
            err = (build.stderr or build.stdout).strip().splitlines()
            failed.append((path.name, f"build rc={build.returncode}: {err[-1] if err else ''}"[:160]))
            continue
        try:
            run = subprocess.run([str(out)], capture_output=True, text=True, timeout=15,
                                 cwd=str(workdir))
        except subprocess.TimeoutExpired:
            failed.append((path.name, "run timeout"))
            continue
        if run.returncode != 0:
            failed.append((path.name, f"run rc={run.returncode}"))
            continue
        passed.append(path.name)

    print(f"stage1-backend coverage: {len(passed)} passed, {len(failed)} failed")
    for name, reason in failed:
        print(f"  FAIL {name}: {reason}")
    # GATE-002: real gate, not informational. Green means every bridge-passing
    # test also passes through the stage1 backend (the removal condition for
    # the bridge). Use --expect-fail to triage known gaps without going red.
    if failed and not args.expect_fail:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
