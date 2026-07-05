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
mkdir -p "$bin_dir"

# Build std modules: source -> include/silver/std/... , compiled -> lib/silver/std/...
if [ -d "$std_dir" ]; then
  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    dest="$include_root/std/$rel"
    mkdir -p "$(dirname "$dest")"
    cp "$src" "$dest"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)

  # NOTE: slice.ag is a compiler-builtin; cffi.ag is documentation-only.
  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    case "$rel" in
      slice.ag) echo "  (compiler-builtin, skipping: $rel)"; continue ;;
      cffi.ag) echo "  (documentation-only, skipping: $rel)"; continue ;;
    esac
    module_path="std.${rel%.ag}"
    module_path="${module_path//\//.}"
    out="$lib_dir/${module_path//.//}.agm"
    mkdir -p "$(dirname "$out")"
    echo "  std module: $rel -> std/$rel"
    "$bin_dir/agc" "$src" --emit=module -I "$include_root" -o "$out"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)
fi

# Build vendor modules: source -> include/silver/vendor/... , compiled -> lib/silver/vendor/...
if [ -d "$vendor_dir" ]; then
  while IFS= read -r -d '' src; do
    rel="${src#$root_dir/vendor/}"
    dest="$include_root/vendor/$rel"
    mkdir -p "$(dirname "$dest")"
    cp "$src" "$dest"
  done < <(find "$vendor_dir" -type f -name '*.ag' -print0)

  while IFS= read -r -d '' src; do
    rel="${src#$root_dir/vendor/}"
    module_path="vendor.${rel%.ag}"
    module_path="${module_path//\//.}"
    out="$lib_dir/${module_path//.//}.agm"
    mkdir -p "$(dirname "$out")"
    echo "  vendor module: vendor/$rel -> vendor/$rel"
    "$bin_dir/agc" "$src" --emit=module -I "$include_root" -o "$out"
  done < <(find "$vendor_dir" -type f -name '*.ag' -print0)
fi
echo "Bootstrap updated at $bootstrap_dir"
