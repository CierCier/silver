#!/usr/bin/env python3
"""Compare stage0 and stage1 on named declaration types."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import tempfile


def run(compiler: pathlib.Path, source: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(compiler), "--no-cache", "check", str(source)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()

    cases = {
        "valid": (
            """\
struct Pair {
    i64 first;
    i64 second;
}

i32 take(Pair value) {
    return 0;
}

i32 main() {
    Pair value;
    return take(value);
}
""",
            True,
        ),
        "generic-function": (
            """\
T identity<T>(T value) {
    return value;
}

i32 main() {
    return identity(1);
}
""",
            True,
        ),
        "generic-struct": (
            """\
struct Pair<T> {
    T first;
    T second;
}

i32 main() {
    Pair<i64> value;
    return 0;
}
""",
            True,
        ),
        "unknown-field": (
            """\
struct Pair {
    Missing second;
}
""",
            False,
        ),
        "unknown-parameter": (
            """\
i32 bad(Missing value) {
    return 0;
}
""",
            False,
        ),
        "unknown-generic": (
            """\
i32 bad(Missing<i64> value) {
    return 0;
}
""",
            False,
        ),
    }

    with tempfile.TemporaryDirectory(prefix="silver-decl-types-") as temporary:
        root = pathlib.Path(temporary)
        for name, (content, should_pass) in cases.items():
            source = root / f"{name}.ag"
            source.write_text(content)
            old = run(stage0, source)
            new = run(stage1, source)
            if (old.returncode == 0) != should_pass:
                raise AssertionError(
                    f"stage0 status for {name}: {old.returncode}\n{old.stderr}"
                )
            if (new.returncode == 0) != should_pass:
                raise AssertionError(
                    f"stage1 status for {name}: {new.returncode}\n{new.stderr}"
                )
            if not should_pass and "unknown type 'Missing'" not in new.stderr:
                raise AssertionError(
                    f"stage1 did not report the missing type for {name}:\n{new.stderr}"
                )

    print("declared type checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
