"""The integration runner must distinguish a diagnostic from a broken compiler."""

import os
from pathlib import Path
import signal
import sys
import tempfile
from types import SimpleNamespace
import unittest

import run_tests


class CompileFailureTests(unittest.TestCase):
    @unittest.skipIf(os.name == "nt", "executable script compiler fixture requires POSIX")
    def test_only_the_intended_diagnostic_passes(self):
        cases = [
            ("diagnostic", "print('error: cannot borrow value'); raise SystemExit(2)", "PASS"),
            ("unrelated", "print('error: unknown type Vec'); raise SystemExit(2)", "FAIL"),
            ("source echo", "print('error: unknown type Vec\\n  | // cannot borrow value'); raise SystemExit(2)", "FAIL"),
            ("missing diagnostic", "raise SystemExit(2)", "FAIL"),
            ("accepted source", "raise SystemExit(0)", "FAIL"),
            ("signal", f"import os; os.kill(os.getpid(), {signal.SIGTERM})", "FAIL"),
            ("diagnostic then signal", f"import os; print('error: cannot borrow value', flush=True); os.kill(os.getpid(), {signal.SIGTERM})", "FAIL"),
            ("timeout", "import time; time.sleep(10)", "FAIL"),
        ]
        with tempfile.TemporaryDirectory(prefix="silver-runner-test-") as name:
            work = Path(name)
            source = work / "borrow_conflict_error_test.ag"
            source.write_text("// error_contains: cannot borrow value\ni32 main() { return 0; }\n")
            compiler = work / "compiler"
            services = SimpleNamespace(root=work)
            for label, program, expected in cases:
                with self.subTest(label=label):
                    compiler.write_text(f"#!{sys.executable}\n{program}\n")
                    compiler.chmod(0o755)
                    result = run_tests.run_single_test(
                        source, compiler, work, services, compile_timeout_secs=0.5,
                    )
                    self.assertEqual(result.status, expected, result)


if __name__ == "__main__":
    unittest.main()
