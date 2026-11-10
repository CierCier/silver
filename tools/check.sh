#!/usr/bin/env bash
# Checks the editor tools: the tree-sitter grammar, the Zed extension, and the
# VS Code extension. Run from anywhere; needs node, npm, a C compiler, and cargo.
#
#   tools/check.sh              all checks
#   tools/check.sh --no-vscode  skip the VS Code npm install and tests
set -euo pipefail

tools="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo="$(cd "$tools/.." && pwd)"
grammar="$tools/tree-sitter-silver"
run_vscode=1
[ "${1:-}" = "--no-vscode" ] && run_vscode=0

step() { printf '\n== %s\n' "$*"; }

step "tree-sitter: generated parser matches grammar.js"
cd "$grammar"
npm ci --no-audit --no-fund --silent
# ABI 14 keeps the parser loadable by older Zed releases.
npx tree-sitter generate --abi 14
if ! git -C "$repo" diff --quiet -- tools/tree-sitter-silver/src; then
  git -C "$repo" --no-pager diff --stat -- tools/tree-sitter-silver/src
  echo "error: src/ is stale. Run 'npm run generate' in tools/tree-sitter-silver and commit it." >&2
  exit 1
fi

step "tree-sitter: corpus and highlight tests"
npx tree-sitter test

step "tree-sitter: every .ag file in the repository parses without errors"
list="$(mktemp)"
trap 'rm -f "$list"' EXIT
git -C "$repo" ls-files -z '*.ag' | while IFS= read -r -d '' file; do
  printf '%s\n' "$repo/$file"
done > "$list"
# `tree-sitter parse` exits non-zero when any file has an ERROR or MISSING node.
if ! parsed="$(npx tree-sitter parse --quiet --stat --paths "$list")"; then
  printf '%s\n' "$parsed" | grep -E '\((ERROR|MISSING)' | head -n 20
  echo "error: the files above do not parse cleanly" >&2
  exit 1
fi
printf '%s\n' "$parsed" | grep '^Total parses' || true

step "zed: grammar rev contains the current grammar"
rev="$(sed -n 's/^rev = "\([0-9a-f]*\)"$/\1/p' "$tools/zed/extension.toml")"
# After a squash merge the pinned commit is on no branch, but GitHub still
# serves it by SHA (through the pull request ref), as Zed fetches it.
if ! git -C "$repo" cat-file -e "$rev^{commit}" 2>/dev/null; then
  git -C "$repo" fetch --quiet --no-tags origin "$rev" 2>/dev/null || true
fi
if ! git -C "$repo" cat-file -e "$rev^{commit}" 2>/dev/null; then
  echo "error: tools/zed/extension.toml rev $rev is not a commit in this clone or on origin." >&2
  exit 1
fi
if ! git -C "$repo" diff --quiet "$rev" -- tools/tree-sitter-silver/grammar.js tools/tree-sitter-silver/src; then
  echo "error: the grammar changed after rev $rev. Commit it, then set rev to that commit." >&2
  exit 1
fi

step "zed: extension crate"
cargo clippy --quiet --manifest-path "$tools/zed/Cargo.toml" -- -D warnings

if [ "$run_vscode" = 1 ]; then
  step "vscode: type check and grammar tests"
  cd "$tools/vscode"
  npm ci --no-audit --no-fund --silent
  npm test --silent
  npx vsce package --out "$(mktemp -d)/silver.vsix" > /dev/null
  echo "vsce package: ok"
fi

step "all editor tool checks passed"
