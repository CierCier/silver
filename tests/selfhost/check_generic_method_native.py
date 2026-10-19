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
struct Marked { i64 value; i64 extra; }
impl Marker for Marked { i64 marker(&self) { return self.value; } }
struct Tagged<T> { T value; }
impl<T: Marker> Marker for Tagged<T> { i64 marker(&self) { return 1; } }
struct Holder { i64 value; i64 rows[2][3]; }
struct Methods { i32 unused; }
impl Methods {
    i64 width<T: Marker>(&self, &T _value) { return @size(T); }
    i64 constrained<T>(&self, &T _value) where T: Marker { return @size(T); }
    i64 by_value<T>(&self, T _value) { return @size(T); }
    i64 tuple_width<T>(&self, [T, i64] _value) { return @size(T); }
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
            checked = invoke("check", str(source), "--no-cache")
            if checked.returncode != 0:
                raise AssertionError(f"{source.name} check failed:\n{checked.stdout}{checked.stderr}")
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
    Marked custom = { .value = 19, .extra = 29 };
    Tagged<i64> tagged = { .value = 23 };
    if (methods.width(&number) != 8 || methods.width(&custom) != 16) { return 1; }
    if (methods.constrained(&custom) != 16) { return 2; }
    if (methods.width(&tagged) != 8) { return 3; }
    Holder holder = { .value = 31 };
    if (methods.width(&holder.value) != 8) { return 4; }
    if (methods.by_value(holder.value) != 8) { return 5; }
    holder.rows[1][2] = 31;
    if (methods.by_value(holder.rows) != 48) { return 7; }
    if (methods.by_value(holder.rows[1][2]) != 8) { return 8; }
    [i32, i64] pair = [7, 11];
    if (methods.tuple_width(pair) != 4) { return 6; }
    return 42;
}
""", encoding="utf-8")
        build_and_run(accepted, 42)
        (work / "bounds_module.ag").write_text(BOUNDS, encoding="utf-8")
        imported = work / "imported.ag"
        imported.write_text(
            accepted.read_text(encoding="utf-8").replace(BOUNDS, "import bounds_module;\n", 1),
            encoding="utf-8",
        )
        build_and_run(imported, 42)
        overloads = (
            "i64 pick<T>(&self, &T _value) { return @size(T); }\n",
            "i64 pick(&self, i64 value) { return value + 100; }\n",
        )
        for order in (overloads, overloads[::-1]):
            overloaded = work / "overloaded.ag"
            overloaded.write_text("struct Overloads {}\nimpl Overloads {\n" + "".join(order) + """\
    i64 shape<T>(&self, T _value) { return @size(T); }
    i64 shape<T>(&self, &T _value) { return @size(T) + 100; }
}
i32 main() {
    Overloads methods;
    i64 number = 7;
    i32 small = 3;
    if (methods.pick(number) != 107) { return 1; }
    if (methods.pick(&small) != 4) { return 2; }
    if (methods.shape(small) != 4) { return 3; }
    if (methods.shape(&small) != 104) { return 4; }
    return 42;
}
""", encoding="utf-8")
            build_and_run(overloaded, 42)
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
                if result.returncode != 2 or "implement 'Marker'" not in diagnostic:
                    raise AssertionError(f"{command} accepted or misdiagnosed {method}:\n{diagnostic}")
                if f"{rejected}:" not in diagnostic:
                    raise AssertionError(f"bound diagnostic lost its source location:\n{diagnostic}")
        display = work / "imported_display.ag"
        display.write_text("""\
import std.string;
struct Missing { i64 value; }
i32 main() {
    String output = String.new();
    Missing missing;
    output.push_display(&missing);
    return 0;
}
""", encoding="utf-8")
        for command in ("check", "build"):
            rejected = invoke(command, str(display), "--no-cache", "-o", str(work / "bad"))
            diagnostic = rejected.stdout + rejected.stderr
            if rejected.returncode != 2 or "implement 'Display'" not in diagnostic:
                raise AssertionError(f"{command} accepted or misdiagnosed imported Display bound:\n{diagnostic}")
            if f"{display}:" not in diagnostic:
                raise AssertionError(f"imported bound diagnostic lost its source location:\n{diagnostic}")

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
        if rejected.returncode != 2 or "cannot move" not in diagnostic:
            raise AssertionError(f"generic method bypassed Drop ownership:\n{diagnostic}")
    print("stage1 native generic method runtime and bound checks passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
