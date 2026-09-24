#!/usr/bin/env python3
"""Compare stage0 and stage1 frontend acceptance for semantic fixtures.

This is deliberately a status gate.  The stage1 checker is still a projection,
not the completed type checker, so comparing rendered diagnostics here would
mix implementation gaps with semantic regressions.  The cfg cases exercise
real branch and item selection; the remaining corpus catches accidental
regressions in the preliminary name, borrow, move, and Send passes.
"""

from __future__ import annotations

import argparse
import pathlib
import shutil
import subprocess
import sys
import tempfile


# The integration harness supplies these CPU cfg values to every native test.
# Keep the fixture-specific additions beside the path so failures show the
# complete command line.
OPTIONS = {
    "tests/cfg_test.ag": ["--cfg", "cfg_test_flag=1,cpu.sse41=1,cpu.avx512f=1"],
    "tests/cfg_derived_test.ag": ["-O2"],
    "tests/target_feature_test.ag": ["--cfg", "cpu.avx2"],
}


RAYGUI_STUB = """\
import vendor.gfx.raylib;

void WindowBox(Rectangle r, const i8* _text) { }
void GroupBox(Rectangle r, const i8* _text) { }
void Label(Rectangle r, const i8* _text) { }
i32 Button(Rectangle r, const i8* _text) { return 0; }
i32 CheckBox(Rectangle r, const i8* _text, bool* _checked) { return 0; }
i32 Slider(Rectangle r, const i8* _left, const i8* _right, f32* _value, f32 _min, f32 _max) { return 0; }
i32 ProgressBar(Rectangle r, const i8* _left, const i8* _right, f32* _value, f32 _min, f32 _max) { return 0; }
void Panel(Rectangle r, const i8* _text) { }
"""


def emit_module(stage0: pathlib.Path, root: pathlib.Path, source: pathlib.Path, output_dir: pathlib.Path) -> pathlib.Path:
    output_dir.mkdir(parents=True, exist_ok=True)
    result = subprocess.run(
        [str(stage0), "--no-cache", "--emit=module", "-I", str(root), str(source)],
        cwd=output_dir,
        capture_output=True,
        text=True,
        check=False,
    )
    artifact = output_dir / f"{source.stem}.agm"
    if result.returncode != 0 or not artifact.is_file():
        detail = (result.stderr or result.stdout).strip()
        raise RuntimeError(f"failed to emit {artifact}: {detail}")
    return artifact


def prepare_artifacts(stage0: pathlib.Path, root: pathlib.Path, temp: pathlib.Path) -> tuple[pathlib.Path, pathlib.Path]:
    module_dir = temp / "module"
    emit_module(stage0, root, root / "tests/modules/module_lib.ag", module_dir)

    mirror = temp / "mirror"
    (mirror / "vendor/gfx").mkdir(parents=True, exist_ok=True)
    for relative in ("vendor/gfx/raylib.agm", "vendor/gfx/rlgl.agm"):
        source = root / relative
        target = mirror / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)

    stub_source = temp / "raygui.ag"
    stub_source.write_text(RAYGUI_STUB, encoding="utf-8")
    emitted = emit_module(stage0, root, stub_source, temp / "raygui-build")
    shutil.copy2(emitted, mirror / "vendor/gfx/raygui.agm")
    return module_dir, mirror


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=pathlib.Path, default=pathlib.Path(__file__).parents[2])
    parser.add_argument("--stage0", type=pathlib.Path, required=True)
    parser.add_argument("--stage1", type=pathlib.Path, required=True)
    parser.add_argument("--include-std", action="store_true")
    parser.add_argument("--timeout", type=int, default=90)
    args = parser.parse_args()

    root = args.root.resolve()
    stage0 = args.stage0.resolve()
    stage1 = args.stage1.resolve()
    paths = []
    for directory in ("tests", "examples"):
        paths.extend(sorted((root / directory).glob("*.ag")))
    if args.include_std:
        paths.extend(sorted((root / "std").rglob("*.ag")))

    failures = []
    compared = 0
    with tempfile.TemporaryDirectory(prefix="silver-semantic-artifacts-") as temp:
        try:
            module_dir, mirror = prepare_artifacts(stage0, root, pathlib.Path(temp))
        except (OSError, RuntimeError) as error:
            print(f"semantic fixture setup failed: {error}", file=sys.stderr)
            return 1

        for path in paths:
            relative = path.relative_to(root).as_posix()
            options = ["--cfg", "cpu.sse41=1,cpu.avx2=1,cpu.avx512f=1"]
            options.extend(OPTIONS.get(relative, []))
            run_cwd = root
            if relative == "tests/module_import_test.ag":
                options.extend(["-I", str(module_dir)])
            elif relative == "examples/raygui_demo.ag":
                # Keep the unavailable external submodule out of the source
                # tree while exercising the artifact namespace deterministically.
                run_cwd = mirror
                options.extend(["-I", str(mirror), "-I", str(root)])
            try:
                old = subprocess.run(
                    [str(stage0), "--no-cache", "check", *options, str(path)],
                    cwd=run_cwd,
                    capture_output=True,
                    text=True,
                    timeout=args.timeout,
                    check=False,
                )
                new = subprocess.run(
                    [str(stage1), "check", str(path), *options],
                    cwd=run_cwd,
                    capture_output=True,
                    text=True,
                    timeout=args.timeout,
                    check=False,
                )
            except subprocess.TimeoutExpired as error:
                failures.append((relative, "timeout", str(error), ""))
                continue
            compared += 1
            if (old.returncode == 0) != (new.returncode == 0):
                failures.append(
                    (
                        relative,
                        f"stage0={old.returncode}, stage1={new.returncode}",
                        new.stderr[:500],
                        old.stderr[:500],
                    )
                )

        # A present but truncated artifact must not be treated as an empty
        # module. This catches the old silent-return path in the source loader.
        bad_dir = pathlib.Path(temp) / "bad-artifact"
        bad_dir.mkdir()
        bad_artifact = bad_dir / "module_lib.agm"
        bad_artifact.write_bytes((module_dir / "module_lib.agm").read_bytes()[:20])
        bad_consumer = bad_dir / "consumer.ag"
        bad_consumer.write_text(
            "import module_lib;\n\ni32 main() {\n    Pair<i64> value;\n    return 0;\n}\n",
            encoding="utf-8",
        )
        old_bad = subprocess.run(
            [str(stage0), "--no-cache", "check", "-I", str(bad_dir), str(bad_consumer)],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=args.timeout,
            check=False,
        )
        new_bad = subprocess.run(
            [str(stage1), "check", str(bad_consumer), "-I", str(bad_dir)],
            cwd=root,
            capture_output=True,
            text=True,
            timeout=args.timeout,
            check=False,
        )
        if old_bad.returncode == 0 or new_bad.returncode == 0:
            failures.append(
                (
                    "malformed-artifact-fixture",
                    f"stage0={old_bad.returncode}, stage1={new_bad.returncode}",
                    new_bad.stderr[:500],
                    old_bad.stderr[:500],
                )
            )

    if failures:
        for relative, summary, stage1_error, stage0_error in failures[:25]:
            print(f"{relative}: {summary}", file=sys.stderr)
            if stage1_error:
                print(f"  stage1: {stage1_error.splitlines()[0]}", file=sys.stderr)
            if stage0_error:
                print(f"  stage0: {stage0_error.splitlines()[0]}", file=sys.stderr)
        print(
            f"semantic parity failed: {len(failures)} mismatch(es), {compared} compared",
            file=sys.stderr,
        )
        return 1
    print(f"semantic status parity passed: {compared} file(s), 0 deferred-boundary skips")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
