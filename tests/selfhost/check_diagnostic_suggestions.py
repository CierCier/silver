#!/usr/bin/env python3
"""Check stage1 typo suggestions for unresolved names and declaration types."""

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
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    compiler = parser.parse_args().stage1.resolve()

    cases = {
        "local": (
            """\
i32 main() {
    i32 counter = 1;
    return countr;
}
""",
            "unknown identifier 'countr', did you mean 'counter'?",
        ),
        "global": (
            """\
i32 counter = 1;
i32 main() { return countr; }
""",
            "unknown identifier 'countr', did you mean 'counter'?",
        ),
        "function": (
            """\
i32 calculate() { return 1; }
i32 main() { return calculte(); }
""",
            "unknown function 'calculte', did you mean 'calculate'?",
        ),
        "parameter-type": (
            """\
struct Pair { i64 value; }
i32 take(Piar item) { return 0; }
""",
            "unknown type 'Piar', did you mean 'Pair'?",
        ),
        "field-type": (
            """\
struct Pair { i64 value; }
struct Box { Piar item; }
""",
            "unknown type 'Piar', did you mean 'Pair'?",
        ),
        "alias-type": (
            """\
struct Pair { i64 value; }
type PairAlias = Piar;
""",
            "unknown type 'Piar', did you mean 'Pair'?",
        ),
    }

    with tempfile.TemporaryDirectory(prefix="silver-diagnostic-suggestions-") as temporary:
        root = pathlib.Path(temporary)
        for name, (content, expected) in cases.items():
            source = root / f"{name}.ag"
            source.write_text(content)
            result = run(compiler, source)
            if result.returncode == 0:
                raise AssertionError(f"stage1 accepted {name} typo:\n{result.stderr}")
            if expected not in result.stderr:
                raise AssertionError(
                    f"stage1 did not emit {expected!r} for {name}:\n{result.stderr}"
                )

    print("diagnostic suggestions passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
