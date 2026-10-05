#!/usr/bin/env bash
# Build stage0, compile the Silver stage1 compiler with it, and run the
# frontend parity gate. The LLVM backend is intentionally not built here.
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
work=${SELFHOST_WORKDIR:-"${TMPDIR:-/tmp}/silver-selfhost"}
mkdir -p "$work"

# STD-008: stage builds must never enable --leak-check (the compiler allocates
# heavily; leak reports would break every output comparison below). The flag
# is opt-in per test in tests/run_tests.py (LEAK_CHECK_TESTS), default off.
if [[ " $* " == *" --leak-check "* ]]; then
    echo "run_stage.sh: --leak-check must stay off in stage builds (STD-008)" >&2
    exit 2
fi

# STD-005: fail fast on unguarded inline asm (source-only, no build needed).
python3 "$root/tests/selfhost/check_asm_fallback.py" --root "$root"

stage0="$root/target/debug/agc"
if [[ ! -x "$stage0" ]]; then
    cargo build --manifest-path "$root/Cargo.toml" -p agc
fi
stage0="$root/target/debug/agc"
# Self-host parity checks may intentionally cross the compatibility bridge;
# select this repository's freshly built stage0 explicitly.
export SILVER_STAGE0="$stage0"
python3 "$root/tests/selfhost/check_float_format.py" --stage0 "$stage0"

stage1="$work/agc-stage1"
"$stage0" --no-cache build "$root/silver.toml" --bin agc -o "$stage1"

aglsp="$work/aglsp-stage1"
"$stage0" --no-cache build "$root/silver.toml" --bin aglsp -o "$aglsp"
python3 "$root/tests/selfhost/test_aglsp.py" "$aglsp"

agsm="$work/agsm-stage1"
"$stage0" --no-cache build "$root/silver.toml" --bin agsm -o "$agsm"
"$agsm" --help > /dev/null

python3 "$root/tests/selfhost/check_workspace.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_hir.py" --stage0 "$stage0" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_declared_types.py" --stage0 "$stage0" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_semantic_regressions.py" --stage0 "$stage0" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_diagnostic_order.py" \
    --stage0 "$stage0" \
    --stage1 "$stage1"
python3 "$root/tests/selfhost/check_gate003.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1"
python3 "$root/tests/selfhost/check_cfg_key_parity.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1"

python3 "$root/tests/selfhost/diff_tokens.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1" \
    "$@"
python3 "$root/tests/selfhost/diff_ast_spans.py" \
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
python3 "$root/tests/selfhost/check_artifacts.py" --stage0 "$stage0" --stage1 "$stage1"
