#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
dist_dir="$root_dir/dist"
target_triple="${TARGET_TRIPLE:-x86_64-unknown-linux-gnu}"
release_name="${1:-$(git -C "$root_dir" describe --tags --exact-match 2>/dev/null || git -C "$root_dir" rev-parse --short HEAD)}"
bundle_name="silver-linux-${target_triple}-${release_name}"
stage_dir="$dist_dir/$bundle_name"
archive_path="$dist_dir/$bundle_name.tar.gz"

echo "Building bootstrap artifacts..."
bash "$root_dir/update-bootstrap.sh"

echo "Preparing Linux bundle at $archive_path"
rm -rf "$stage_dir"
mkdir -p "$stage_dir/bin" "$stage_dir/include" "$stage_dir/lib"

cp "$root_dir/target/release/agc" "$stage_dir/bin/agc"
cp -R "$root_dir/bootstrap/include/silver" "$stage_dir/include/silver"
cp -R "$root_dir/bootstrap/lib/silver" "$stage_dir/lib/silver"
cp "$root_dir/README.md" "$stage_dir/README.md"
cp "$root_dir/LICENSE" "$stage_dir/LICENSE"

tar -C "$dist_dir" -czf "$archive_path" "$bundle_name"

echo "Bundle created: $archive_path"
