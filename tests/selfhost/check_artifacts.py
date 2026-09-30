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

def artifact_source_hash(payload: bytes) -> int:
    cursor = 6
    for _ in range(3):
        length = struct.unpack_from("<I", payload, cursor)[0]
        cursor += 4 + length
    return struct.unpack_from("<Q", payload, cursor)[0]


def fnv1a64(payload: bytes) -> int:
    result = 0xCBF29CE484222325
    for byte in payload:
        result = ((result ^ byte) * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return result



def optional(value: str | None) -> bytes:
    return b"\x00" if value is None else b"\x01" + string(value)


def artifact(version: int, export_kind: int = 1) -> bytes:
    data = bytearray(b"AGM\x00\x00" + bytes([version]))
    data += string("probe")
    data += string("artifact_probe")
    data += string("")
    data += bytes(8)
    data += string("foreign")
    data += string("unknown")
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


def run_check(
    compiler: pathlib.Path, root: pathlib.Path, source: pathlib.Path
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(compiler), "check", str(source)],
        cwd=root,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    args = parser.parse_args()
    stage0 = args.stage0.resolve()
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
        legacy_name = "artifact_v9_empty_trailer"
        (directory / f"{legacy_name}.agm").write_bytes(artifact(9) + bytes(4))
        legacy_consumer = directory / "consumer_v9_empty_trailer.ag"
        legacy_consumer.write_text(
            f"import {legacy_name};\ni32 main() {{ return probe(); }}\n",
            encoding="utf-8",
        )
        for compiler, label in ((stage0, "stage0"), (stage1, "stage1")):
            result = run_check(compiler, root, legacy_consumer)
            if result.returncode != 0:
                failures.append(
                    f"{label} rejected the legacy v9 empty trailer: {result.stderr}"
                )

        vendor_consumer = directory / "vendor_v9_consumer.ag"
        vendor_consumer.write_text(
            "import vendor.gfx.raylib;\nimport vendor.gfx.rlgl;\n"
            "i32 main() { return 0; }\n",
            encoding="utf-8",
        )
        vendor_result = subprocess.run(
            [str(stage1), "check", str(vendor_consumer), "-I", str(root)],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        if vendor_result.returncode != 0:
            failures.append(
                f"stage1 rejected vendored v9 AGM files: {vendor_result.stderr}"
            )

        valid = artifact(11)
        bad_cases = {
            "bad_magic": b"not-an-agm",
            "unsupported_version": b"AGM\x00\x00\x05" + valid[6:],
            "bad_export_kind": artifact(11, export_kind=8),
            "truncated_header": valid[:16],
            "truncated_export": valid[: len(valid) // 2],
            "truncated_tail": valid[:-1],
            "v9_nonzero_trailer": artifact(9) + b"\x00\x00\x00\x01",
            "v9_partial_trailer": artifact(9) + b"\x00",
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

        # Stage1 writer -> stage0 reader, using the same name-visibility path
        # as ordinary imported modules.
        stage1_artifact = directory / "stage1_fixture.agm"
        emitted = subprocess.run(
            [str(stage1), "artifact-fixture", str(stage1_artifact)],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        if emitted.returncode != 0:
            failures.append(f"stage1 AGM publisher failed: {emitted.stderr}")
        else:
            repeated_path = directory / "stage1_fixture_repeat.agm"
            repeated = subprocess.run(
                [str(stage1), "artifact-fixture", str(repeated_path)],
                cwd=root,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            if repeated.returncode != 0:
                failures.append(f"repeated stage1 AGM publication failed: {repeated.stderr}")
            else:
                first_bytes = stage1_artifact.read_bytes()
                repeat_bytes = repeated_path.read_bytes()
                if first_bytes != repeat_bytes:
                    failures.append("stage1 AGM publication was not byte-deterministic")
                positions = [
                    first_bytes.find(string("alpha")),
                    first_bytes.find(string("probe")),
                    first_bytes.find(string("zeta")),
                ]
                if min(positions) < 0 or positions != sorted(positions):
                    failures.append("stage1 AGM exports were not sorted by name")
            consumer = directory / "stage0_consumer.ag"
            consumer.write_text(
                "import stage1_fixture;\ni32 main() { return probe(); }\n",
                encoding="utf-8",
            )
            read_result = subprocess.run(
                [str(stage0), "check", str(consumer), "-I", str(directory)],
                cwd=root,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            if read_result.returncode != 0:
                failures.append(f"stage0 rejected stage1-written AGM: {read_result.stderr}")

        # The source-publication path must use the same cross-stage metadata.
        source_module = directory / "stage1_source.ag"
        source_module.write_text("i32 probe() { return 17; }\n", encoding="utf-8")
        stage1_source_artifact = directory / "stage1_source.agm"
        emitted_source = subprocess.run(
            [str(stage1), "artifact-source", str(source_module), str(stage1_source_artifact)],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        if emitted_source.returncode != 0:
            failures.append(f"stage1 source AGM publication failed: {emitted_source.stderr}")
        else:
            payload = stage1_source_artifact.read_bytes()
            if artifact_source_hash(payload) != fnv1a64(source_module.read_bytes()):
                failures.append("stage1 source AGM stored an incorrect source hash")
            consumer = directory / "stage1_source_consumer.ag"
            consumer.write_text(
                "import stage1_source;\ni32 main() { return probe(); }\n",
                encoding="utf-8",
            )
            read_result = subprocess.run(
                [str(stage0), "check", str(consumer), "-I", str(directory)],
                cwd=root,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            if read_result.returncode != 0:
                failures.append(f"stage0 rejected stage1 source AGM: {read_result.stderr}")

        unsupported_source = directory / "generic_source.ag"
        unsupported_source.write_text(
            "T identity<T>(T value) { return value; }\n", encoding="utf-8"
        )
        unsupported_artifact = directory / "generic_source.agm"
        unsupported_result = subprocess.run(
            [
                str(stage1),
                "artifact-source",
                str(unsupported_source),
                str(unsupported_artifact),
            ],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=30,
            check=False,
        )
        if unsupported_result.returncode == 0 or unsupported_artifact.exists():
            failures.append("stage1 published unsupported generic function metadata")



        # Stage0 writer -> stage1 reader, using a real source module artifact.
        source_module = directory / "stage0_fixture.ag"
        source_module.write_text("i32 probe() { return 17; }\n", encoding="utf-8")
        stage0_artifact = directory / "stage0_fixture.agm"
        emitted = subprocess.run(
            [
                str(stage0),
                "--no-cache",
                "--emit=module",
                str(source_module),
                "-o",
                str(stage0_artifact),
            ],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=60,
            check=False,
        )
        if emitted.returncode != 0:
            failures.append(f"stage0 AGM emission failed: {emitted.stderr}")
        else:
            consumer = directory / "stage1_consumer.ag"
            consumer.write_text(
                "import stage0_fixture;\ni32 main() { return probe(); }\n",
                encoding="utf-8",
            )
            read_result = subprocess.run(
                [str(stage1), "check", str(consumer), "-I", str(directory)],
                cwd=root,
                capture_output=True,
                text=True,
                timeout=30,
                check=False,
            )
            if read_result.returncode != 0:
                failures.append(f"stage1 rejected stage0-written AGM: {read_result.stderr}")
    if failures:
        for failure in failures:
            print(f"artifact check failed: {failure}")
        return 1
    print(
        f"artifact checks passed: {len(SUPPORTED_VERSIONS)} reader versions, "
        f"{len(bad_cases)} bad inputs, legacy v9 trailer, 3 cross-stage roundtrips"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
