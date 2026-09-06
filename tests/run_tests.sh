#!/usr/bin/env bash
# Integration test runner for the Silver compiler.
# Delegates to the modern Python TUI test harness: tests/run_tests.py
exec python3 "$(dirname "$0")/run_tests.py" "$@"
