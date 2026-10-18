#!/usr/bin/env python3
"""Focused tests for the self-host baseline recorder.

These test pure logic: harness-log parsing (including malformed summaries),
snapshot identity for dirty trees, evidence-directory protection, and the
stage0/stage1 corpus argument contract. They never build a compiler.
"""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

import capture_baseline as recorder  # noqa: E402


def git(root: pathlib.Path, *arguments: str) -> None:
    subprocess.run(
        ["git", "-c", "user.email=test@example.invalid",
         "-c", "user.name=Recorder Test", *arguments],
        cwd=str(root), check=True, capture_output=True, text=True,
    )


class ParseHarnessLogTests(unittest.TestCase):
    LOG = (
        "\x1b[1m== Summary ==\x1b[0m\n"
        "  \x1b[32m[PASS]\x1b[0m  alpha                        (12ms)\n"
        "  \x1b[31m[FAIL]\x1b[0m  beta                         (exit code 1, expected 0)\n"
        "  \x1b[33m[SKIP]\x1b[0m  gamma                        (requires Node.js)\n"
        "  \x1b[33m[SKIP]\x1b[0m  delta                        (in default skip list)\n"
        "  \x1b[32mPassed:   1\x1b[0m\n"
        "  \x1b[31mFailed:   1\x1b[0m\n"
        "  \x1b[33mSkipped:  2\x1b[0m\n"
    )

    def test_counts_and_every_skip_reason(self) -> None:
        parsed = recorder.parse_harness_log(self.LOG)
        self.assertEqual((parsed["passed"], parsed["failed"], parsed["skipped"]), (1, 1, 2))
        self.assertEqual(
            [(skip["name"], skip["reason"]) for skip in parsed["skips"]],
            [("gamma", "requires Node.js"), ("delta", "in default skip list")],
        )
        self.assertEqual(parsed["failures"],
                         [{"name": "beta", "reason": "exit code 1, expected 0"}])
        self.assertNotIn("skip_count_mismatch", parsed)

    def test_malformed_summary_yields_none_not_a_crash(self) -> None:
        parsed = recorder.parse_harness_log("  [PASS]  alpha  (1ms)\n")
        self.assertIsNone(parsed["passed"])
        self.assertIsNone(parsed["failed"])
        self.assertIsNone(parsed["skipped"])

    def test_skip_summary_disagreeing_with_lines_is_flagged(self) -> None:
        text = "  [SKIP]  gamma  (reason)\n  Skipped:  3\n"
        self.assertTrue(recorder.parse_harness_log(text)["skip_count_mismatch"])


class SnapshotIdentityTests(unittest.TestCase):
    def setUp(self) -> None:
        self._temporary = tempfile.TemporaryDirectory(prefix="silver-snapshot-test-")
        self.root = pathlib.Path(self._temporary.name)
        git(self.root, "init", "-q")
        (self.root / "src.ag").write_text("i32 main() { return 0; }\n", encoding="utf-8")
        git(self.root, "add", "src.ag")
        git(self.root, "commit", "-q", "-m", "initial")

    def tearDown(self) -> None:
        self._temporary.cleanup()

    def test_clean_tree_is_labeled_clean(self) -> None:
        snapshot = recorder.snapshot_identity(self.root)
        self.assertTrue(snapshot["clean"])
        self.assertEqual(snapshot["commit_label"], snapshot["commit"])

    def test_dirty_tree_is_never_labeled_clean(self) -> None:
        (self.root / "src.ag").write_text("i32 main() { return 1; }\n", encoding="utf-8")
        dirty = recorder.snapshot_identity(self.root)
        self.assertFalse(dirty["clean"])
        self.assertTrue(dirty["dirty"])
        self.assertNotEqual(dirty["commit_label"], dirty["commit"])
        self.assertIn("dirty", dirty["commit_label"])
        self.assertIn("src.ag", dirty["modified_paths"])

    def test_snapshot_id_tracks_uncommitted_content(self) -> None:
        (self.root / "src.ag").write_text("i32 main() { return 1; }\n", encoding="utf-8")
        first = recorder.snapshot_identity(self.root)
        self.assertEqual(first["snapshot_id"], recorder.snapshot_identity(self.root)["snapshot_id"])

        (self.root / "src.ag").write_text("i32 main() { return 2; }\n", encoding="utf-8")
        changed = recorder.snapshot_identity(self.root)
        self.assertEqual(first["commit"], changed["commit"])
        self.assertNotEqual(first["snapshot_id"], changed["snapshot_id"])

    def test_untracked_source_changes_snapshot_id(self) -> None:
        baseline = recorder.snapshot_identity(self.root)
        (self.root / "extra.ag").write_text("i32 extra() { return 0; }\n", encoding="utf-8")
        with_untracked = recorder.snapshot_identity(self.root)
        self.assertTrue(with_untracked["dirty"])
        self.assertNotEqual(baseline["snapshot_id"], with_untracked["snapshot_id"])

    def test_binary_edits_change_snapshot_id(self) -> None:
        binary = self.root / "binary.dat"
        binary.write_bytes(b"\x00first")
        git(self.root, "add", "binary.dat")
        git(self.root, "commit", "-q", "-m", "add binary")
        binary.write_bytes(b"\x00second")
        first = recorder.snapshot_identity(self.root)
        binary.write_bytes(b"\x00third")
        self.assertNotEqual(first["snapshot_id"],
                            recorder.snapshot_identity(self.root)["snapshot_id"])

    def test_quoted_untracked_names_are_hashed(self) -> None:
        for name in ("notes-é.ag", "tab\tname.ag", "line\nbreak.ag"):
            with self.subTest(name=name):
                path = self.root / name
                path.write_text("first", encoding="utf-8")
                first = recorder.snapshot_identity(self.root)
                path.write_text("second", encoding="utf-8")
                self.assertNotEqual(first["snapshot_id"],
                                    recorder.snapshot_identity(self.root)["snapshot_id"])
                path.unlink()


class GenerationLogTests(unittest.TestCase):
    def test_focused_counts_do_not_substitute_for_a_full_corpus(self) -> None:
        result = recorder.parse_generation_log("Passed: 30\nFailed: 0\nSkipped: 0\n")
        self.assertTrue(recorder.summary_problems(result["stage2"]))
        self.assertTrue(recorder.summary_problems(result["stage3"]))

    def test_keeps_separate_corpus_counts_and_skip_reasons(self) -> None:
        text = (
            "Running the full integration corpus with agc-stage2\n"
            "  [PASS] alpha (10ms)\n  [SKIP] optional (missing library)\n"
            "Passed: 1\nFailed: 0\nSkipped: 1\n"
            "Building stage3 with stage2\nPassed: 99\n"
            "Running the full integration corpus with agc-stage3\n"
            "Passed: 2\nFailed: 0\nSkipped: 0\n"
        )
        result = recorder.parse_generation_log(text)
        self.assertEqual(result["stage2"]["passed"], 1)
        self.assertEqual(result["stage3"]["passed"], 2)
        self.assertEqual(result["stage2"]["skips"],
                         [{"name": "optional", "reason": "missing library"}])


class PrepareOutDirTests(unittest.TestCase):
    def test_main_rejects_output_inside_checkout_before_writing(self) -> None:
        out = pathlib.Path(recorder.__file__).resolve().parents[2] / "baseline-evidence"
        with self.assertRaises(SystemExit) as error:
            recorder.main(["--out", str(out)])
        self.assertEqual(error.exception.code, 2)
        self.assertFalse(out.exists())

    def test_refuses_non_empty_directory(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            out = pathlib.Path(temporary)
            (out / "baseline.json").write_text("{}\n", encoding="utf-8")
            with self.assertRaises(recorder.EvidenceError):
                recorder.prepare_out_dir(out)

    def test_main_refuses_existing_evidence(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            out = pathlib.Path(temporary)
            (out / "baseline.json").write_text("{}\n", encoding="utf-8")
            self.assertEqual(recorder.main(["--out", str(out)]), 2)

    def test_accepts_empty_directory(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            out = pathlib.Path(temporary)
            recorder.prepare_out_dir(out)
            self.assertTrue((out / "logs").is_dir())


class CorpusArgumentTests(unittest.TestCase):
    ROOT = pathlib.Path("/silver-root")
    STAGE1 = pathlib.Path("/silver-stage1")

    def test_stage0_corpus_uses_normal_stage0_path(self) -> None:
        argv = recorder.corpus_argv(self.ROOT, "debug", 4, None)
        self.assertNotIn("--compiler", argv)
        self.assertNotIn("--release", argv)

    def test_stage1_corpus_passes_compiler_and_release(self) -> None:
        argv = recorder.corpus_argv(self.ROOT, "release", 4, self.STAGE1)
        self.assertIn("--release", argv)
        self.assertIn(str(self.STAGE1), argv)
        self.assertEqual(argv[argv.index("--compiler") + 1], str(self.STAGE1))


if __name__ == "__main__":
    unittest.main()
