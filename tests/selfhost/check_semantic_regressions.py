#!/usr/bin/env python3
"""Check focused stage1 semantic regressions against stage0 behavior."""

from __future__ import annotations

import argparse
import pathlib
import os
import subprocess
import tempfile


CASES = {
    "nested_generic.ag": (True, True, ""),
    "nested_unknown.ag": (False, False, "unknown type 'Missing'"),
    "scalar_alias_return.ag": (True, True, ""),
    "indexed_holder.ag": (True, True, ""),
    "imported_global_return.ag": (True, True, ""),
    "branch_move_return.ag": (True, True, ""),
    "branch_move_reuse.ag": (
        False, False, "use of moved value 'incoming'"
    ),
    "branch_move_fallthrough.ag": (
        False, False, "use of moved value 'incoming'"
    ),
    "local_reference_return.ag": (
        False, False, "returned reference does not outlive the function"
    ),
    "ambiguous_overload.ag": (
        False, False, "call to overloaded function 'g' is ambiguous"
    ),
    "unsigned_float_cast.ag": (True, True, ""),
}

DROP_DIAGNOSTIC = ("cannot move", "because it implements Drop")
DROP_ANCESTOR_CASES = {
    "tests/drop_ancestor_direct_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_nested_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_implicit_transfer_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
    "tests/drop_ancestor_by_value_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_return_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_manual_leaf_drop_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
    "tests/drop_ancestor_generic_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_receiver_error_test.ag": (False, False, DROP_DIAGNOSTIC),
    "tests/drop_ancestor_enum_payload_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
    "tests/drop_ancestor_generic_function_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
    "tests/drop_ancestor_generic_copy_test.ag": (True, True, ()),
    "tests/drop_ancestor_generic_borrow_test.ag": (True, True, ()),
    "tests/drop_ancestor_method_argument_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
    "tests/drop_ancestor_function_pointer_argument_error_test.ag": (
        False, False, DROP_DIAGNOSTIC
    ),
}


def check(
    compiler: pathlib.Path, source: pathlib.Path, *, stage1: bool, cwd: pathlib.Path
) -> subprocess.CompletedProcess[str]:
    env = os.environ.copy()
    if stage1:
        env["SILVER_STAGE0"] = "/nonexistent"
    return subprocess.run(
        ([str(compiler), "check", str(source), "--no-cache"] if stage1 else
         [str(compiler), "--no-cache", "check", str(source)]),
        cwd=cwd,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )


def check_drop_ancestor_imports(
    stage0: pathlib.Path, stage1: pathlib.Path, root: pathlib.Path
) -> int:
    module_source_text = """\
import std.mem.drop;
struct Leaf { i64 value; }
impl Drop for Leaf { void drop(Leaf* self) {} }
struct Managed { Leaf item; Leaf sibling; }
impl Drop for Managed { void drop(Managed* self) {} }
"""
    consumer_text = """\
import drop_ancestor_owners;

void consume(Leaf item) {}

i32 main() {
    Managed parent;
    parent.item.value = 1;
    parent.sibling.value = 2;
    consume(move parent.item);
    return 0;
}
"""
    stage1_env = os.environ.copy()
    stage1_env["SILVER_STAGE0"] = "/nonexistent"
    with tempfile.TemporaryDirectory(prefix="silver-drop-ancestor-import-") as name:
        work = pathlib.Path(name)
        module_source = work / "drop_ancestor_owners.ag"
        consumer = work / "consumer.ag"
        module_source.write_text(module_source_text, encoding="utf-8")
        consumer.write_text(consumer_text, encoding="utf-8")

        source_checks = (
            (
                "source import stage0",
                subprocess.run(
                    [str(stage0), "--no-cache", "check", "-I", str(work), str(consumer)],
                    cwd=root, capture_output=True, text=True, check=False,
                ),
            ),
            (
                "source import stage1",
                subprocess.run(
                    [str(stage1), "check", str(consumer), "--no-cache", "-I", str(work)],
                    cwd=root, env=stage1_env, capture_output=True, text=True,
                    check=False,
                ),
            ),
        )
        for label, result in source_checks:
            output = result.stdout + result.stderr
            if result.returncode == 0:
                raise AssertionError(
                    f"{label} accepted a field move through custom Drop"
                )
            for fragment in DROP_DIAGNOSTIC:
                if fragment not in output:
                    raise AssertionError(
                        f"{label} diagnostic missing {fragment!r}:\n{output}"
                    )

        emitted = subprocess.run(
            [str(stage0), "--no-cache", "--emit=module", str(module_source)],
            cwd=work, capture_output=True, text=True, check=False,
        )
        artifact = work / "drop_ancestor_owners.agm"
        if emitted.returncode != 0 or not artifact.is_file():
            raise AssertionError(
                "failed to emit custom-Drop ownership module: "
                f"{emitted.stdout}\n{emitted.stderr}"
            )
        module_source.unlink()
        imported = subprocess.run(
            [str(stage0), "--no-cache", "check", "-I", str(work), str(consumer)],
            cwd=root, capture_output=True, text=True, check=False,
        )
        output = imported.stdout + imported.stderr
        if imported.returncode == 0:
            raise AssertionError(
                "compiled AGM import accepted field move through custom Drop"
            )
        for fragment in DROP_DIAGNOSTIC:
            if fragment not in output:
                raise AssertionError(
                    f"compiled AGM import diagnostic missing {fragment!r}:\n{output}"
                )
    return 3


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--stage0", required=True, type=pathlib.Path)
    parser.add_argument("--stage1", required=True, type=pathlib.Path)
    args = parser.parse_args()
    root = pathlib.Path(__file__).parents[2]
    fixtures = root / "tests/selfhost/semantic_regressions"

    for name, (stage0_passes, stage1_passes, diagnostic) in CASES.items():
        source = fixtures / name
        old = check(args.stage0.resolve(), source, stage1=False, cwd=root)
        new = check(args.stage1.resolve(), source, stage1=True, cwd=root)
        if (old.returncode == 0) != stage0_passes:
            raise AssertionError(
                f"stage0 status mismatch for {name}: {old.returncode}\n{old.stderr}"
            )
        if (new.returncode == 0) != stage1_passes:
            raise AssertionError(
                f"stage1 status mismatch for {name}: {new.returncode}\n{new.stderr}"
            )
        if diagnostic and diagnostic not in new.stderr:
            raise AssertionError(
                f"stage1 diagnostic mismatch for {name}: expected {diagnostic!r}\n{new.stderr}"
            )
    for name, (stage0_passes, stage1_passes, fragments) in DROP_ANCESTOR_CASES.items():
        source = root / name
        old = check(args.stage0.resolve(), source, stage1=False, cwd=root)
        new = check(args.stage1.resolve(), source, stage1=True, cwd=root)
        if (old.returncode == 0) != stage0_passes:
            raise AssertionError(
                f"stage0 status mismatch for {name}: {old.returncode}\n{old.stderr}"
            )
        if (new.returncode == 0) != stage1_passes:
            raise AssertionError(
                f"stage1 status mismatch for {name}: {new.returncode}\n{new.stderr}"
            )
        for label, result, expected in (
            ("stage0", old, stage0_passes),
            ("stage1", new, stage1_passes),
        ):
            if expected:
                continue
            diagnostic = result.stdout + result.stderr
            for fragment in fragments:
                if fragment not in diagnostic:
                    raise AssertionError(
                        f"{label} diagnostic mismatch for {name}: "
                        f"missing {fragment!r}\n{diagnostic}"
                    )
    import_count = check_drop_ancestor_imports(
        args.stage0.resolve(), args.stage1.resolve(), root
    )
    print(
        "semantic regressions passed: "
        f"{len(CASES) + len(DROP_ANCESTOR_CASES)} fixtures and "
        f"{import_count} source/AGM import checks"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
