#!/usr/bin/env bash
# Build stage0, compile the Silver stage1 compiler with it, and run the
# frontend parity gate. The LLVM backend is intentionally not built here.
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
work=${SELFHOST_WORKDIR:-"${TMPDIR:-/tmp}/silver-selfhost"}
mkdir -p "$work"

stage0="$root/target/debug/agc"
if [[ ! -x "$stage0" ]]; then
    cargo build --manifest-path "$root/Cargo.toml" -p agc
fi
stage0="$root/target/debug/agc"

stage1="$work/agc-stage1"
"$stage0" --no-cache build "$root/silver.toml" -o "$stage1"
python3 "$root/tests/selfhost/check_workspace.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_hir.py" --stage0 "$stage0" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_declared_types.py" --stage0 "$stage0" --stage1 "$stage1"

python3 "$root/tests/selfhost/diff_tokens.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1" \
    "$@"
python3 "$root/tests/selfhost/check_parse.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1" \
    "$@"
python3 "$root/tests/selfhost/check_semantics.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1" \
    "$@"
python3 "$root/tests/selfhost/check_send.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1"
python3 "$root/tests/selfhost/check_artifacts.py" --stage1 "$stage1"
