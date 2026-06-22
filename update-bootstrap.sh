#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
std_dir="$root_dir/std"
vendor_dir="$root_dir/vendor"
bootstrap_dir="$root_dir/bootstrap"
bin_dir="$bootstrap_dir/bin"
include_root="$bootstrap_dir/include/silver"
lib_dir="$bootstrap_dir/lib/silver"

echo "Building agc (release)..."
cargo build -p agc --release

mkdir -p "$bin_dir" "$include_root" "$lib_dir"
cp "$root_dir/target/release/agc" "$bin_dir/agc"

rm -rf "$include_root" "$lib_dir"
mkdir -p "$include_root" "$lib_dir"

# Build std modules (flat naming for backward compat)
if [ -d "$std_dir" ]; then
  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    dest="$include_root/$rel"
    mkdir -p "$(dirname "$dest")"
    cp "$src" "$dest"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)

  # NOTE: box.ag is a draft and fails to compile as a module
  #       (generic `alloc<T>()` is not resolved across modules).
  #       Its import is commented out in std/mem.ag.
  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    case "$rel" in
      mem/box.ag) echo "  (skipping draft: $rel)"; continue ;;
      slice.ag) echo "  (compiler-builtin, skipping: $rel)"; continue ;;
      cffi.ag) echo "  (documentation-only, skipping: $rel)"; continue ;;
    esac
    dotted="${rel//\//.}"
    base="${dotted%.ag}"
    out="$lib_dir/$base.agm"
    echo "  std module: $rel -> $base.agm"
    "$bin_dir/agc" "$src" --emit=module -I "$include_root" -o "$out"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)
fi

# Build vendor modules (directory hierarchy)
if [ -d "$vendor_dir" ]; then
  while IFS= read -r -d '' src; do
    rel="${src#$root_dir/}"
    dest="$include_root/$rel"
    mkdir -p "$(dirname "$dest")"
    cp "$src" "$dest"
  done < <(find "$vendor_dir" -type f -name '*.ag' -print0)

  while IFS= read -r -d '' src; do
    rel="${src#$root_dir/}"
    base="${rel%.ag}"
    out="$lib_dir/$base.agm"
    mkdir -p "$(dirname "$out")"
    echo "  vendor module: $rel -> $base.agm"
    "$bin_dir/agc" "$src" --emit=module -I "$include_root" -o "$out"
  done < <(find "$vendor_dir" -type f -name '*.ag' -print0)
fi

echo "Bootstrap updated at $bootstrap_dir"
