#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
std_dir="$root_dir/std"
bootstrap_dir="$root_dir/bootstrap"
bin_dir="$bootstrap_dir/bin"
include_dir="$bootstrap_dir/include/silver"
lib_dir="$bootstrap_dir/lib/silver"

echo "Building agc (release)..."
cargo build -p agc --release

mkdir -p "$bin_dir" "$include_dir" "$lib_dir"
cp "$root_dir/target/release/agc" "$bin_dir/agc"

rm -rf "$include_dir" "$lib_dir"
mkdir -p "$include_dir" "$lib_dir"

if [ -d "$std_dir" ]; then
  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    dest="$include_dir/$rel"
    mkdir -p "$(dirname "$dest")"
    cp "$src" "$dest"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)

  while IFS= read -r -d '' src; do
    rel="${src#$std_dir/}"
    dotted="${rel//\//.}"
    base="${dotted%.ag}"
    out="$lib_dir/$base.agbm"
    "$bin_dir/agc" "$src" --emit=module -I "$include_dir" -o "$out"
  done < <(find "$std_dir" -type f -name '*.ag' -print0)
fi

echo "Bootstrap updated at $bootstrap_dir"
