#!/usr/bin/env bash
# Runs every test suite in the dev environment. `nix run .#check` calls this
# inside `nix develop`; outside Nix, run it from a shell that has the LLVM 22
# toolchain.
#
# Steps, in order:
#   1. build the bootstrap workspace (stage0 agc, agsm, Rust FFI)
#   2. Rust tests for the workspace
#   3. stdlib suite with stage0, natively and (on Linux hosts) for the other
#      Linux architecture under qemu user mode
#   4. build stage1 with stage0, then the stdlib suite with stage1
#   5. build the native language server, then the LSP protocol suite
#
# Every step runs even after a failure, except those that need a failed build.
# The exit status is non-zero when any step failed.
set -uo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"

jobs="${CHECK_JOBS:-4}"
work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

# The tests import std from the checkout, not from a store copy.
export SILVER_SYSROOT="$root_dir"

results=()
failed=0

# step <name> <command...>: run a command and record whether it passed.
step() {
  local name="$1"
  shift
  printf '\n==> %s\n' "$name"
  if "$@"; then
    results+=("pass  $name")
    return 0
  fi
  results+=("FAIL  $name")
  failed=1
  return 1
}

# skip <name> <reason>: record a step that could not run.
skip() {
  printf '\n==> %s (skipped: %s)\n' "$1" "$2"
  results+=("skip  $1 ($2)")
  failed=1
}

summary() {
  printf '\n== Summary ==\n'
  printf '  %s\n' "${results[@]}"
  if [[ "$failed" -eq 0 ]]; then
    printf '\nAll checks passed.\n'
  else
    printf '\nSome checks failed.\n'
  fi
}

stage0="$root_dir/target/debug/agc"
stage1="$work_dir/agc-stage1"
aglsp="$work_dir/aglsp-native"

if ! step "build workspace" cargo build --locked --workspace; then
  summary
  exit 1
fi

step "rust tests" cargo test --locked --workspace --no-fail-fast

step "stdlib suite (stage0)" \
  python3 tests/run_tests.py --no-tui --jobs "$jobs"

case "$(uname -m)" in
  x86_64) cross_target="aarch64-unknown-linux-gnu" ;;
  aarch64) cross_target="x86_64-unknown-linux-gnu" ;;
  *) cross_target="" ;;
esac
cross_qemu="qemu-${cross_target%%-*}"
if [[ "$(uname -s)" != "Linux" ]]; then
  # qemu user mode runs foreign Linux binaries on a Linux kernel only.
  printf '\n==> stdlib suite (cross, qemu) (not applicable: needs a Linux host)\n'
  results+=("n/a   stdlib suite (cross, qemu) (needs a Linux host)")
elif [[ -z "$cross_target" ]]; then
  skip "stdlib suite (cross, qemu)" "no cross target for $(uname -m)"
elif ! command -v "$cross_qemu" > /dev/null; then
  skip "stdlib suite ($cross_target)" "$cross_qemu not found"
else
  step "stdlib suite ($cross_target)" \
    python3 tests/run_tests.py --no-tui --jobs "$jobs" --target "$cross_target"
fi

if step "build stage1" "$stage0" build silver.toml --bin agc --no-cache -o "$stage1"; then
  step "stdlib suite (stage1)" \
    python3 tests/run_tests.py --no-tui --jobs "$jobs" --compiler "$stage1"
else
  skip "stdlib suite (stage1)" "stage1 did not build"
fi

if step "build language server" "$stage0" build silver.toml --bin aglsp --no-cache -o "$aglsp"; then
  step "language server suite" python3 scripts/check_lsp.py --server "$aglsp"
else
  skip "language server suite" "language server did not build"
fi

summary
exit "$failed"
