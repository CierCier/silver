#!/usr/bin/env bash
# Exercise the supported native stage1 boundary without allowing stage0 fallback.
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
if [[ $# -lt 1 || ! -x "$1" ]]; then
    echo "usage: $0 /path/to/agc-stage1" >&2
    exit 2
fi
stage1=$(realpath "$1")
work=$(mktemp -d "${TMPDIR:-/tmp}/silver-no-stage0.XXXXXX")
trap 'rm -rf "$work"' EXIT

export SILVER_STAGE0="$work/stage0-unavailable"
export SILVER_STAGE1_NATIVE=1
if [[ -e "$SILVER_STAGE0" ]]; then
    echo "run_native_no_stage0.sh: stage0 path unexpectedly exists" >&2
    exit 2
fi

"$stage1" --version
python3 "$root/tests/selfhost/check_native_backend.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_link_contract.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_format.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_assert.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_args.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_enum_layout.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_layout.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_generic_function_native.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_stage2.py" --stage1 "$stage1"
python3 "$root/tests/run_tests.py" --no-tui --jobs 4 --compiler "$stage1" \
    allocator_threads_test loop_stack_restore_test aggregate_init_drop_test \
    cascade_drop_test field_predrop_test intermediate_drop_activation_test \
    collections_set_test for_in_consume_test nested_field_drop_test native_for_in_continue_test \
    string_split_once_drop_test string_order_native_test tuple_local_destructure_native_test \
    slice_syntax_test destructure_let_test \
    partial_move_test reborrow_passthrough_test sha256_test \
    unsigned_narrow_shift_test packed_layout_test \
    collection_drop_glue_test enum_custom_drop_test map_tuple_test map_test \
    for_in_generic_test str_key_map_test \
    direct_field_partial_move_test drop_ancestor_direct_error_test \
    drop_ancestor_nested_error_test
