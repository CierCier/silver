#!/usr/bin/env python3
"""HTTP Performance Rig for the Silver HTTP Server.

Benchmarks the Silver HTTP server using ApacheBench (ab) within a nix-shell environment.
Compiles the server with optimization flags (-O2), runs it as a background service,
drives traffic with ApacheBench, parses metrics (RPS, latencies, error rates), and tears
down cleanly.
"""

import argparse
import json
import os
import re
import signal
import socket
import subprocess
import sys
import time
from pathlib import Path


def wait_for_server(host: str, port: int, timeout: float = 10.0) -> bool:
    """Poll until server accepts TCP connections on host:port."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            with socket.create_connection((host, port), timeout=0.5):
                return True
        except (ConnectionRefusedError, OSError, socket.timeout):
            time.sleep(0.1)
    return False


def build_server(project_root: Path, opt: str, output_path: Path) -> bool:
    """Compile examples/http_server.ag using agc."""
    example_path = project_root / "examples" / "http_server.ag"
    cmd = [
        "cargo",
        "run",
        "-p",
        "agc",
        "--",
        opt,
        str(example_path),
        "-o",
        str(output_path),
    ]
    print(f"==> Compiling {example_path.name} with {opt}...")
    start = time.time()
    proc = subprocess.run(cmd, cwd=str(project_root), capture_output=True, text=True)
    elapsed = time.time() - start
    if proc.returncode != 0:
        print(f"Compilation failed (exit {proc.returncode}) after {elapsed:.1f}s:")
        print(proc.stderr or proc.stdout)
        return False
    print(f"==> Built {output_path} in {elapsed:.1f}s")
    return True


def parse_ab_output(output: str) -> dict:
    """Extract metrics from ApacheBench output."""
    metrics = {
        "raw": output,
        "concurrency": None,
        "complete_requests": None,
        "failed_requests": 0,
        "requests_per_second": None,
        "time_per_request_ms": None,
        "time_per_request_concurrent_ms": None,
        "transfer_rate_kbytes_sec": None,
        "latencies_ms": {},
    }

    m = re.search(r"Concurrency Level:\s+(\d+)", output)
    if m:
        metrics["concurrency"] = int(m.group(1))

    m = re.search(r"Complete requests:\s+(\d+)", output)
    if m:
        metrics["complete_requests"] = int(m.group(1))

    m = re.search(r"Failed requests:\s+(\d+)", output)
    if m:
        metrics["failed_requests"] = int(m.group(1))

    m = re.search(r"Requests per second:\s+([0-9.]+)\s+\[#/sec\]", output)
    if m:
        metrics["requests_per_second"] = float(m.group(1))

    m = re.search(r"Time per request:\s+([0-9.]+)\s+\[ms\]\s+\(mean\)", output)
    if m:
        metrics["time_per_request_ms"] = float(m.group(1))

    m = re.search(r"Time per request:\s+([0-9.]+)\s+\[ms\]\s+\(mean, across all concurrent requests\)", output)
    if m:
        metrics["time_per_request_concurrent_ms"] = float(m.group(1))

    m = re.search(r"Transfer rate:\s+([0-9.]+)\s+\[Kbytes/sec\]", output)
    if m:
        metrics["transfer_rate_kbytes_sec"] = float(m.group(1))

    # Latency percentiles
    percentiles = ["50%", "66%", "75%", "80%", "90%", "95%", "98%", "99%", "100%"]
    for p in percentiles:
        m = re.search(rf"\s+{re.escape(p)}\s+(\d+)", output)
        if m:
            metrics["latencies_ms"][p] = int(m.group(1))

    return metrics


def run_benchmark(
    port: int,
    endpoint: str,
    requests: int,
    concurrency: int,
    keep_alive: bool,
) -> dict:
    """Run ApacheBench via nix-shell -p apacheHttpd."""
    url = f"http://127.0.0.1:{port}{endpoint}"
    ab_args = f"-n {requests} -c {concurrency} -l"
    if keep_alive:
        ab_args += " -k"
    ab_args += f" {url}"

    nix_cmd = [
        "nix-shell",
        "-p",
        "apacheHttpd",
        "--run",
        f"ab {ab_args}",
    ]

    print(f"==> Benchmarking {url} ({requests} requests, concurrency {concurrency}, keep-alive={keep_alive})...")
    start = time.time()
    proc = subprocess.run(nix_cmd, capture_output=True, text=True)
    duration = time.time() - start

    if proc.returncode != 0:
        print(f"ApacheBench failed (exit {proc.returncode}):")
        print(proc.stderr or proc.stdout)
        sys.exit(proc.returncode)

    metrics = parse_ab_output(proc.stdout)
    metrics["benchmark_duration_s"] = duration
    metrics["endpoint"] = endpoint
    metrics["url"] = url
    return metrics


def print_summary(metrics: dict):
    """Print a clean terminal dashboard for benchmark results."""
    rps = metrics.get("requests_per_second")
    concurrency = metrics.get("concurrency")
    complete = metrics.get("complete_requests")
    failed = metrics.get("failed_requests", 0)
    transfer = metrics.get("transfer_rate_kbytes_sec")
    lat_50 = metrics.get("latencies_ms", {}).get("50%", "N/A")
    lat_90 = metrics.get("latencies_ms", {}).get("90%", "N/A")
    lat_99 = metrics.get("latencies_ms", {}).get("99%", "N/A")
    lat_100 = metrics.get("latencies_ms", {}).get("100%", "N/A")
    mean_lat = metrics.get("time_per_request_concurrent_ms")

    print("\n" + "=" * 65)
    print("         SILVER HTTP SERVER PERFORMANCE SUMMARY")
    print("=" * 65)
    print(f"  Target Endpoint        : {metrics.get('url')}")
    print(f"  Completed Requests     : {complete:,}")
    print(f"  Failed Requests        : {failed}")
    print(f"  Concurrency Level      : {concurrency}")
    print(f"  Throughput (RPS)       : {rps:,.2f} req/sec" if rps else "  Throughput (RPS)       : N/A")
    if transfer:
        print(f"  Transfer Rate          : {transfer:,.2f} KB/sec")
    if mean_lat is not None:
        print(f"  Mean Latency           : {mean_lat:.3f} ms (per-request)")
    print("-" * 65)
    print("  Latency Percentiles (ms):")
    print(f"    50% (median) : {lat_50:>6} ms")
    print(f"    90%          : {lat_90:>6} ms")
    print(f"    95%          : {metrics.get('latencies_ms', {}).get('95%', 'N/A'):>6} ms")
    print(f"    99%          : {lat_99:>6} ms")
    print(f"    100% (max)   : {lat_100:>6} ms")
    print("=" * 65 + "\n")


def main():
    parser = argparse.ArgumentParser(
        description="Benchmark Silver HTTP server using ApacheBench in nix-shell"
    )
    parser.add_argument(
        "-n", "--requests", type=int, default=50000, help="Total number of requests (default: 50000)"
    )
    parser.add_argument(
        "-c", "--concurrency", type=int, default=100, help="Concurrency level (default: 100)"
    )
    parser.add_argument(
        "-e", "--endpoint", default="/plaintext", help="Endpoint to benchmark (default: /plaintext)"
    )
    parser.add_argument(
        "-p", "--port", type=int, default=8080, help="Server port (default: 8080)"
    )
    parser.add_argument(
        "--opt", default="-O2", help="Optimization flag (default: -O2)"
    )
    parser.add_argument(
        "--skip-build", action="store_true", help="Skip compilation and reuse existing binary"
    )
    parser.add_argument(
        "--bin", default="/tmp/silver_http_server", help="Server binary location"
    )
    parser.add_argument(
        "--no-keep-alive", action="store_true", help="Disable HTTP keep-alive (-k in ab)"
    )
    parser.add_argument(
        "--json", dest="json_out", help="Write benchmark results to specified JSON file"
    )

    args = parser.parse_args()

    project_root = Path(__file__).resolve().parent.parent
    server_bin = Path(args.bin)

    if not args.skip_build or not server_bin.exists():
        if not build_server(project_root, args.opt, server_bin):
            sys.exit(1)

    print(f"==> Launching Silver HTTP server ({server_bin})...")
    server_proc = subprocess.Popen(
        [str(server_bin)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        preexec_fn=os.setsid,
    )

    try:
        if not wait_for_server("127.0.0.1", args.port, timeout=10.0):
            print(f"Error: Server failed to start on 127.0.0.1:{args.port}")
            server_proc.poll()
            if server_proc.returncode is not None:
                _, err = server_proc.communicate()
                print(f"Server exited with code {server_proc.returncode}: {err.decode()}")
            sys.exit(1)

        print(f"==> Server is up on http://127.0.0.1:{args.port}")

        metrics = run_benchmark(
            port=args.port,
            endpoint=args.endpoint,
            requests=args.requests,
            concurrency=args.concurrency,
            keep_alive=not args.no_keep_alive,
        )

        print_summary(metrics)

        if args.json_out:
            with open(args.json_out, "w") as f:
                json.dump(metrics, f, indent=2)
            print(f"==> Saved metrics JSON to {args.json_out}")

    finally:
        print("==> Shutting down Silver HTTP server...")
        try:
            os.killpg(os.getpgid(server_proc.pid), signal.SIGTERM)
            server_proc.wait(timeout=2.0)
        except Exception:
            try:
                os.killpg(os.getpgid(server_proc.pid), signal.SIGKILL)
            except Exception:
                pass


if __name__ == "__main__":
    main()
