#!/usr/bin/env python3
"""Build stage2 natively, then exercise its frontend and compiler without stage0."""

from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import tempfile


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = pathlib.Path(__file__).parents[2]
    with tempfile.TemporaryDirectory(prefix="silver-stage2-") as temporary:
        work = pathlib.Path(temporary)
        env = os.environ.copy()
        env["SILVER_STAGE0"] = str(work / "missing-stage0")
        env["SILVER_STAGE1_NATIVE"] = "1"
        stage2 = work / "agc-stage2"

        def invoke(binary: pathlib.Path, arguments: list[str]) -> subprocess.CompletedProcess[str]:
            result = subprocess.run(
                [str(binary), *arguments], cwd=root, env=env,
                capture_output=True, text=True, timeout=300, check=False,
            )
            if result.returncode != 0:
                raise AssertionError(
                    f"{binary.name} {arguments!r} failed: {result.returncode}\n"
                    f"{result.stdout}\n{result.stderr}"
                )
            return result

        def expect_rejected(source: pathlib.Path, label: str) -> None:
            result = subprocess.run(
                [str(stage2), "build", str(source), "--no-cache", "-o",
                 str(work / f"rejected-{label}")],
                cwd=root, env=env, capture_output=True, text=True,
                timeout=300, check=False,
            )
            output = result.stdout + result.stderr
            if result.returncode == 0:
                raise AssertionError(f"stage2 unexpectedly built {label}")
            for fragment in ("cannot move", "because it implements Drop"):
                if fragment not in output:
                    raise AssertionError(
                        f"stage2 {label} diagnostic missing {fragment!r}:\n{output}"
                    )

        invoke(args.stage1.resolve(), ["build", str(root / "silver.toml"),
                                      "--bin", "agc", "--no-cache", "-o", str(stage2)])
        invoke(stage2, ["--version"])
        invoke(stage2, ["--help"])
        source = work / "main.ag"
        # Exceed the 128-token capacity that previously corrupted Vec<Token>.
        declarations = "".join(f"    i64 value_{index} = {index};\n" for index in range(64))
        source.write_text("i32 main() {\n" + declarations + "    return 42;\n}\n", encoding="utf-8")
        for command in ["lex", "parse", "check"]:
            invoke(stage2, [command, str(source)])
        app = work / "app"
        invoke(stage2, ["build", str(source), "--no-cache", "-o", str(app)])
        executed = subprocess.run([str(app)], timeout=30, check=False)
        if executed.returncode != 42:
            raise AssertionError(f"stage2-compiled program returned {executed.returncode}, expected 42")

        programs = {
            "arithmetic-control": """i32 main() {
    i32 total = 0;
    for (i32 i = 1; i <= 10; i = i + 1) {
        if (i % 2 == 0) { total = total + i; }
        else { total = total - i; }
    }
    return total + 37;
}
""",
            "generic-free-function": """T identity<T>(T value) {
    return value;
}

i32 main() {
    i64 first = identity((i64)17);
    i32 second = identity((i32)25);
    return (i32)first + second;
}
""",
            "function-pointer": """i32 add_one(i32 value) {
    return value + 1;
}

i32 main() {
    i32(i32) operation = add_one;
    return operation(41);
}
""",
            "aggregate-layout": """struct Cell {
    i64 left;
    i64 right;
}

i32 main() {
    Cell cells[2];
    cells[0].left = 7;
    cells[0].right = 11;
    cells[1].left = 13;
    cells[1].right = 17;
    return (i32)(cells[0].left + cells[0].right
        + cells[1].left + cells[1].right) - 6;
}
""",
        }
        programs["nested-aggregate-layout"] = (
            root / "tests/selfhost/semantic_regressions/stage2_nested_layout.ag"
        ).read_text(encoding="utf-8")
        programs["aggregate-init-drop"] = (
            root / "tests/aggregate_init_drop_test.ag"
        ).read_text(encoding="utf-8")
        programs["wrapper-drop-transfer"] = (
            root / "tests/wrapper_drop_transfer_test.ag"
        ).read_text(encoding="utf-8")
        programs["wrapper-implicit-transfer"] = (
            root / "tests/selfhost/semantic_regressions/wrapper_implicit_transfer_native.ag"
        ).read_text(encoding="utf-8")
        programs["enum-custom-drop"] = (
            root / "tests/enum_custom_drop_test.ag"
        ).read_text(encoding="utf-8")
        programs["direct-field-partial-move"] = (
            root / "tests/direct_field_partial_move_test.ag"
        ).read_text(encoding="utf-8")
        programs["destructor-order"] = (
            root / "tests/destructor_order_test.ag"
        ).read_text(encoding="utf-8")
        programs["nested-field-drop"] = (
            root / "tests/nested_field_drop_test.ag"
        ).read_text(encoding="utf-8")
        programs["drop-ancestor-generic-copy"] = (
            root / "tests/drop_ancestor_generic_copy_test.ag"
        ).read_text(encoding="utf-8")
        programs["drop-ancestor-generic-borrow"] = (
            root / "tests/drop_ancestor_generic_borrow_test.ag"
        ).read_text(encoding="utf-8")
        programs["intermediate-drop-activation"] = (
            root / "tests/intermediate_drop_activation_test.ag"
        ).read_text(encoding="utf-8")
        for name, contents in programs.items():
            program_source = work / f"{name}.ag"
            program_source.write_text(contents, encoding="utf-8")
            program = work / name
            invoke(stage2, ["build", str(program_source), "--no-cache", "-o", str(program)])
            executed = subprocess.run([str(program)], timeout=30, check=False)
            if executed.returncode != 42:
                raise AssertionError(
                    f"stage2-compiled {name} program returned {executed.returncode}, expected 42"
                )
        for fixture in (
            "drop_ancestor_direct_error_test.ag",
            "drop_ancestor_nested_error_test.ag",
            "drop_ancestor_implicit_transfer_error_test.ag",
            "drop_ancestor_by_value_error_test.ag",
            "drop_ancestor_return_error_test.ag",
            "drop_ancestor_manual_leaf_drop_error_test.ag",
            "drop_ancestor_generic_error_test.ag",
            "drop_ancestor_receiver_error_test.ag",
            "drop_ancestor_enum_payload_error_test.ag",
            "drop_ancestor_generic_function_error_test.ag",
            "drop_ancestor_method_argument_error_test.ag",
            "drop_ancestor_function_pointer_argument_error_test.ag",
        ):
            expect_rejected(root / "tests" / fixture, fixture.removesuffix(".ag"))
    print(
        "native stage2 frontend commands, runtime programs, and Drop-ancestor "
        "rejection builds passed without stage0"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
