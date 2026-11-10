#!/usr/bin/env bash
# dev-lsp.sh — rebuild the native Silver language server and run its checks.
#
# Usage:
#   scripts/dev-lsp.sh build                 build stage0 + the server into $AGLSP_BIN
#   scripts/dev-lsp.sh check [-k NAME] [-v]  build, then run the protocol suite
#   scripts/dev-lsp.sh bench [ARGS...]       build, then run scripts/benchmark_lsp.py
#   scripts/dev-lsp.sh list                  list protocol scenarios (no build)
#
# AGLSP_BIN overrides the server path (default /tmp/aglsp-native).
# Run inside `nix develop` (or direnv) for the LLVM 22 toolchain.
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root_dir"
server="${AGLSP_BIN:-/tmp/aglsp-native}"

build() {
  cargo build -p agc
  target/debug/agc build silver.toml --bin aglsp --no-cache -o "$server"
  echo "built $server"
}

case "${1:-}" in
  build)
    build
    ;;
  check)
    shift
    build
    python3 scripts/check_lsp.py --server "$server" "$@"
    ;;
  bench)
    shift
    build
    # A --workspace passed by the caller comes later and overrides this default.
    python3 scripts/benchmark_lsp.py --server "$server" --workspace "$root_dir" "$@"
    ;;
  list)
    python3 scripts/check_lsp.py --list
    ;;
  *)
    sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'
    if [ -n "${1:-}" ]; then exit 1; fi
    ;;
esac
