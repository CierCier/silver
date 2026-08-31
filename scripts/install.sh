#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Default directories
prefix="${PREFIX:-${SILVER_INSTALL_DIR:-$HOME/.local/share/silver}}"
bin_dir=""
symlink_bin_dir=""
create_symlinks=true
build_profile="release"
cargo_flags=(--release)

print_help() {
  cat << 'HELP'
Silver Compiler Installer

Usage: install.sh [OPTIONS]

Installs the Silver compiler (agc), Language Server (aglsp),
Submodule Generator (agsm), and the standard & vendor include files.

Options:
  --prefix <DIR>          Installation prefix (default: ~/.local/share/silver)
  --bin-dir <DIR>         Directory for binary executables (default: <prefix>/bin)
  --symlink-bin <DIR>     Create convenience symlinks in <DIR> (default: ~/.local/bin if present)
  --no-symlinks           Skip creating convenience symlinks in PATH directories
  --debug                 Build debug binaries instead of release
  -h, --help              Show this help message

Environment Variables:
  PREFIX                  Overrides default installation prefix
  SILVER_INSTALL_DIR      Alternative to PREFIX
HELP
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --prefix)
      prefix="$2"
      shift 2
      ;;
    --bin-dir)
      bin_dir="$2"
      shift 2
      ;;
    --symlink-bin)
      symlink_bin_dir="$2"
      create_symlinks=true
      shift 2
      ;;
    --no-symlinks)
      create_symlinks=false
      shift
      ;;
    --debug)
      build_profile="debug"
      cargo_flags=()
      shift
      ;;
    -h|--help)
      print_help
      exit 0
      ;;
    *)
      echo "error: unknown argument '$1'" >&2
      print_help >&2
      exit 1
      ;;
  esac
done

if [[ -z "$bin_dir" ]]; then
  bin_dir="$prefix/bin"
fi

include_dir="$prefix/include"

echo "=== Building Silver Toolchain ($build_profile) ==="
cargo build "${cargo_flags[@]}" -p agc -p aglsp -p agsm

echo "=== Installing to $prefix ==="
mkdir -p "$bin_dir"
mkdir -p "$include_dir"

# 1. Install binaries
for bin_name in agc aglsp agsm; do
  src_bin="$root_dir/target/$build_profile/$bin_name"
  dest_bin="$bin_dir/$bin_name"
  if [[ -f "$src_bin" ]]; then
    echo "  Installing binary: $dest_bin"
    cp "$src_bin" "$dest_bin"
    chmod +x "$dest_bin"
  else
    echo "  warning: binary not found at $src_bin" >&2
  fi
done

# 2. Install includes (std and vendor)
echo "  Installing includes to $include_dir/{std,vendor}"
rm -rf "$include_dir/std" "$include_dir/vendor"

if [[ -d "$root_dir/std" ]]; then
  cp -R "$root_dir/std" "$include_dir/std"
fi

if [[ -d "$root_dir/vendor" ]]; then
  cp -R "$root_dir/vendor" "$include_dir/vendor"
fi

# 3. Create optional symlinks in user bin directory (e.g. ~/.local/bin)
if [[ "$create_symlinks" == "true" ]]; then
  target_symlink_dir="${symlink_bin_dir:-$HOME/.local/bin}"
  if [[ -d "$target_symlink_dir" || -n "$symlink_bin_dir" ]]; then
    mkdir -p "$target_symlink_dir"
    echo "=== Creating symlinks in $target_symlink_dir ==="
    for bin_name in agc aglsp agsm; do
      if [[ -f "$bin_dir/$bin_name" ]]; then
        ln -sf "$bin_dir/$bin_name" "$target_symlink_dir/$bin_name"
        echo "  Symlinked: $target_symlink_dir/$bin_name -> $bin_dir/$bin_name"
      fi
    done
  fi
fi

echo ""
echo "Silver toolchain installed successfully!"
echo "  Prefix:   $prefix"
echo "  Binaries: $bin_dir"
echo "  Includes: $include_dir/{std, vendor}"
echo ""
echo "Ensure your PATH includes the binary directory:"
echo "  export PATH=\"$bin_dir:\$PATH\""
