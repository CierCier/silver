#!/usr/bin/env python3
"""Exercise the stage1 AGM reader across supported versions and bad input."""

from __future__ import annotations

import argparse
import pathlib
import struct
import subprocess
import tempfile


SUPPORTED_VERSIONS = (2, 6, 7, 8, 9, 10, 11)


def u32(value: int) -> bytes:
    return struct.pack("<I", value)


def string(value: str) -> bytes:
    encoded = value.encode("utf-8")
    return u32(len(encoded)) + encoded


def optional(value: str | None) -> bytes:
    return b"\x00" if value is None else b"\x01" + string(value)


def artifact(version: int, export_kind: int = 1) -> bytes:
    data = bytearray(b"AGM\x00\x00" + bytes([version]))
    data += string("probe")
    data += string("artifact_probe")
    data += string("artifact_probe.ag")
    data += bytes(8)
    data += string("test")
    data += string("x86_64-unknown-linux-gnu")
    data += bytes((0, 0))
    data += u32(0)  # direct dependencies
    data += u32(0)  # transitive dependencies
    data += u32(1)  # exports

    data += bytes((export_kind,))  # export kind
    data += string("probe")
    data += string("fn() -> i32")
    data += u32(0)  # type parameters
    data += optional("probe")
    data += b"\x01\x02"  # ABI: optional, Silver
    data += b"\x00"  # variadic
    data += optional(None)  # type key
    data += u32(0)  # fields
    data += b"\x00"  # layout
    data += optional(None)  # enum backing type
    data += u32(0)  # variants
    data += u32(0)  # trait items

    if version in (8, 9, 11):
        data += optional(None)  # constant value
        data += b"\x00"  # mutable
    if version == 11:
        data += optional(None)  # implementation trait

    data += u32(0)  # native libraries
    if version in (7, 8, 9, 11):
        data += u32(0)  # native library paths
    if version == 11:
        data += u32(0)  # generic templates
    return bytes(data)


def run_check(stage1: pathlib.Path, root: pathlib.Path, source: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(stage1), "check", str(source)],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    root = pathlib.Path(__file__).parents[2]
    failures: list[str] = []

    with tempfile.TemporaryDirectory(prefix="silver-artifacts-", dir=root) as temporary:
        directory = pathlib.Path(temporary)

        for version in SUPPORTED_VERSIONS:
            name = f"artifact_v{version}"
            (directory / f"{name}.agm").write_bytes(artifact(version))
            source = directory / f"consumer_v{version}.ag"
            source.write_text(
                f"import {name};\n\ni32 main() {{ return probe(); }}\n",
                encoding="utf-8",
            )
            result = run_check(stage1, root, source)
            if result.returncode != 0:
                failures.append(
                    f"v{version} valid artifact failed ({result.returncode}): "
                    f"{result.stderr.splitlines()[0] if result.stderr else 'no stderr'}"
                )

        valid = artifact(11)
        bad_cases = {
            "bad_magic": b"not-an-agm",
            "unsupported_version": b"AGM\x00\x00\x05" + valid[6:],
            "bad_export_kind": artifact(11, export_kind=8),
            "truncated_header": valid[:16],
            "truncated_export": valid[: len(valid) // 2],
            "truncated_tail": valid[:-1],
            "invalid_utf8": bytes(bytearray(valid[:10]) + b"\xff" + valid[11:]),
        }
        for label, payload in bad_cases.items():
            name = f"artifact_{label}"
            (directory / f"{name}.agm").write_bytes(payload)
            source = directory / f"consumer_{label}.ag"
            source.write_text(
                f"import {name};\n\ni32 main() {{ return 0; }}\n",
                encoding="utf-8",
            )
            result = run_check(stage1, root, source)
            if result.returncode == 0:
                failures.append(f"{label} was accepted")
            elif "invalid module artifact" not in result.stderr:
                failures.append(f"{label} lacked the artifact diagnostic")

    if failures:
        for failure in failures:
            print(f"artifact check failed: {failure}")
        return 1
    print(f"artifact reader checks passed: {len(SUPPORTED_VERSIONS)} versions, {len(bad_cases)} bad inputs")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
