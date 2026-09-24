#!/usr/bin/env bash
# Build the Silver stage1 binary, then run the native integration corpus
# through its Linux command surface. During the backend migration stage1 keeps
# its verified frontend commands and delegates native compilation to stage0;
# this script makes that boundary executable and reproducible.
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
work=${SELFHOST_WORKDIR:-"${TMPDIR:-/tmp}/silver-selfhost"}

profile=debug
for argument in "$@"; do
    if [[ "$argument" == "--release" ]]; then
        profile=release
        break
    fi
done
stage0=${SILVER_STAGE0:-"$root/target/$profile/agc"}
if [[ ! -x "$stage0" ]]; then
    if [[ "$profile" == "release" ]]; then
        cargo build --manifest-path "$root/Cargo.toml" -p agc --release
    else
        cargo build --manifest-path "$root/Cargo.toml" -p agc
    fi
fi
stage0=$(realpath "$stage0")
mkdir -p "$work"

stage1="$work/agc-stage1"
"$stage0" --no-cache build "$root/silver.toml" -o "$stage1"

# Also build the stage1 compiler through the normal dependency cache. This is
# the path that exercises runtime ABI declarations across cached module objects.
cached_stage1="$work/agc-stage1-cached"
"$stage0" build "$root/silver.toml" -o "$cached_stage1"

# The bridge resolves this explicitly, rather than depending on the caller's
# working directory or a PATH lookup.
export SILVER_STAGE0="$stage0"
python3 "$root/tests/selfhost/check_bridge.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_bridge.py" --stage1 "$cached_stage1"

cached_rebuild="$work/agc-stage1-cached-rebuild"
"$cached_stage1" build "$root/silver.toml" -o "$cached_rebuild"
"$cached_rebuild" --version
python3 "$root/tests/selfhost/check_native_backend.py" --stage1 "$stage1"
python3 "$root/tests/selfhost/check_native_cache.py" \
    --root "$root" \
    --stage0 "$stage0" \
    --stage1 "$stage1"
python3 "$root/tests/run_tests.py" --no-tui --compiler "$stage1" "$@"
