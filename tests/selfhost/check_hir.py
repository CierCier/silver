#!/usr/bin/env python3
"""Focused checks for the retained HIR/type projection."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import tempfile


def run(compiler: pathlib.Path, source: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(compiler), "check", str(source)],
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

    valid = """\
i32 int_value() {
    return 7;
}

i32 good() {
    return int_value();
}

i32 takes_text(&str text) {
    return 0;
}

i32 main() {
    return takes_text("text");
}
"""
    invalid_literal = """\
i32 bad_literal() {
    return "not an integer";
}
"""
    invalid_call = """\
str text_value() {
    return "text";
}

i32 bad_call() {
    return text_value();
}
"""
    invalid_argument = """\
i32 takes_text(str text) {
    return 0;
}

i32 bad_argument() {
    return takes_text(1);
}
"""
    invalid_declarations = """\
i32 bad_one() {
    i32 value = "wrong";
    return 0;
}

i32 bad_two() {
    str text = 1;
    return 0;
}
"""
    invalid_arity = """\
i32 one(i32 value) {
    return value;
}

i32 bad_arity() {
    return one();
}
"""
    invalid_local_return = """\
i32 bad_local_return() {
    str text = "text";
    return text;
}
"""

    with tempfile.TemporaryDirectory(prefix="silver-hir-") as temporary:
        root = pathlib.Path(temporary)
        cases = {
            "valid": (valid, True),
            "invalid-literal": (invalid_literal, False),
            "invalid-call": (invalid_call, False),
            "invalid-argument": (invalid_argument, False),
            "invalid-declarations": (invalid_declarations, False),
            "invalid-arity": (invalid_arity, False),
            "invalid-local-return": (invalid_local_return, False),
        }
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
            if not should_pass and "type mismatch" not in new.stderr and "argument count mismatch" not in new.stderr:
                raise AssertionError(f"stage1 did not report a typed diagnostic:\n{new.stderr}")
            if name == "invalid-declarations" and new.stderr.count("type mismatch") != 2:
                raise AssertionError(f"expected one diagnostic per declaration:\n{new.stderr}")

    print("HIR checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
