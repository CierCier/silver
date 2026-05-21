#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tag_name="${1:-}"

if [ -z "$tag_name" ]; then
  echo "usage: $0 <tag>"
  exit 1
fi

if ! command -v gh >/dev/null 2>&1; then
  echo "error: gh CLI is required"
  exit 1
fi

if ! command -v git >/dev/null 2>&1; then
  echo "error: git is required"
  exit 1
fi

if ! git -C "$root_dir" diff --quiet || ! git -C "$root_dir" diff --cached --quiet; then
  echo "error: working tree has uncommitted changes"
  exit 1
fi

if git -C "$root_dir" rev-parse --verify "refs/tags/$tag_name" >/dev/null 2>&1; then
  echo "error: tag '$tag_name' already exists"
  exit 1
fi

git -C "$root_dir" tag -a "$tag_name" -m "Release $tag_name"
git -C "$root_dir" push origin "$tag_name"

echo "Building release bundle for $tag_name..."
bash "$root_dir/scripts/create-linux-bundle.sh" "$tag_name"

archive_path="$root_dir/dist/silver-linux-x86_64-unknown-linux-gnu-${tag_name}.tar.gz"

if [ ! -f "$archive_path" ]; then
  echo "error: expected bundle not found at $archive_path"
  exit 1
fi

release_notes_file="$(mktemp)"
trap 'rm -f "$release_notes_file"' EXIT

previous_tag="$(git -C "$root_dir" describe --tags --abbrev=0 "${tag_name}^" 2>/dev/null || true)"

if [ -z "$previous_tag" ]; then
  git -C "$root_dir" log --pretty=format:"- %s (%h)" > "$release_notes_file"
else
  git -C "$root_dir" log "${previous_tag}..${tag_name}" --pretty=format:"- %s (%h)" > "$release_notes_file"
fi

gh release create "$tag_name" "$archive_path" \
  --title "$tag_name" \
  --notes-file "$release_notes_file"
