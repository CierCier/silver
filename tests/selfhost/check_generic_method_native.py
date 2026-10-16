#!/usr/bin/env python3
"""Exercise inferred method specializations and bound diagnostics in stage1."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import tempfile


BOUNDS = """\
trait Marker { i64 marker(&self); }
impl Marker for i64 { i64 marker(&self) { return *self; } }
struct Marked { i64 value; }
impl Marker for Marked { i64 marker(&self) { return self.value; } }
struct Tagged<T> { T value; }
impl<T: Marker> Marker for Tagged<T> { i64 marker(&self) { return 1; } }
struct Methods { i32 unused; }
impl Methods {
    i64 width<T: Marker>(&self, &T _value) { return @size(T); }
    i64 constrained<T>(&self, &T _value) where T: Marker { return @size(T); }
}
"""


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    stage1 = args.stage1.resolve()
    with tempfile.TemporaryDirectory(prefix="silver-native-generic-method-") as temporary:
        work = pathlib.Path(temporary)

        def invoke(*arguments: str) -> subprocess.CompletedProcess[str]:
            return subprocess.run(
                [str(stage1), *arguments], capture_output=True, text=True,
                timeout=180, check=False,
            )

        def build_and_run(source: pathlib.Path, expected: int) -> None:
            output = work / source.stem
            built = invoke("build", str(source), "--no-cache", "-o", str(output))
            if built.returncode != 0:
                raise AssertionError(f"{source.name} build failed:\n{built.stdout}{built.stderr}")
            executed = subprocess.run(
                [str(output)], capture_output=True, text=True, timeout=30, check=False,
            )
            if (executed.returncode, executed.stdout, executed.stderr) != (expected, "", ""):
                raise AssertionError(
                    f"{source.name} runtime failed: rc={executed.returncode}, "
                    f"stdout={executed.stdout!r}, stderr={executed.stderr!r}"
                )

        accepted = work / "bounds.ag"
        accepted.write_text(BOUNDS + """\
i32 main() {
    Methods methods;
    i64 number = 17;
    Marked custom = { .value = 19 };
    Tagged<i64> tagged = { .value = 23 };
    if (methods.width(&number) != 8 || methods.width(&custom) != 8) { return 1; }
    if (methods.constrained(&custom) != 8) { return 2; }
    if (methods.width(&tagged) != 8) { return 3; }
    return 42;
}
""", encoding="utf-8")
        build_and_run(accepted, 42)
        for method, actual_type in (
            ("width", "Missing"),
            ("constrained", "Missing"),
            ("width", "Tagged<Missing>"),
        ):
            rejected = work / f"bad-{method}.ag"
            rejected.write_text(BOUNDS + f"""\
struct Missing {{ i64 value; }}
i32 main() {{
    Methods methods;
    {actual_type} missing;
    methods.{method}(&missing);
    return 0;
}}
""", encoding="utf-8")
            for command in ("check", "build"):
                result = invoke(command, str(rejected), "--no-cache", "-o", str(work / "bad"))
                diagnostic = result.stdout + result.stderr
                if result.returncode == 0 or "implement 'Marker'" not in diagnostic:
                    raise AssertionError(f"{command} accepted or misdiagnosed {method}:\n{diagnostic}")
                if f"{rejected}:" not in diagnostic:
                    raise AssertionError(f"bound diagnostic lost its source location:\n{diagnostic}")
        ownership = work / "ownership.ag"
        ownership.write_text("""\
import std.mem.drop;
struct Leaf { i64 value; }
impl Drop for Leaf { void drop(*self) {} }
struct Owner { Leaf item; }
impl Drop for Owner { void drop(*self) {} }
struct Methods { i32 unused; }
impl Methods { void take<T>(&self, T _value) {} }
i32 main() {
    Owner owner;
    owner.item.value = 7;
    Methods methods;
    methods.take(move owner.item);
    return 0;
}
""", encoding="utf-8")
        rejected = invoke("check", str(ownership), "--no-cache")
        diagnostic = rejected.stdout + rejected.stderr
        if rejected.returncode == 0 or "cannot move" not in diagnostic:
            raise AssertionError(f"generic method bypassed Drop ownership:\n{diagnostic}")
    print("stage1 native generic method runtime and bound checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
