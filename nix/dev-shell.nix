{ pkgs, silver }:
pkgs.mkShell {
  inputsFrom = [ silver ];
  packages =
    with pkgs;
    [
      cargo
      rustc
      rustfmt
      clippy
      rust-analyzer
      openssl
      nodejs
      go
      llvmPackages_22.lld
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      gdb
      patchelf
      # Runs aarch64 test binaries on x86_64 hosts (run_tests.py --target).
      qemu-user
    ]
    # gdb does not run on Apple silicon; lldb is the native debugger there.
    ++ lib.optional stdenv.hostPlatform.isDarwin llvmPackages_22.lldb;

  LLVM_SYS_221_PREFIX = "${pkgs.llvmPackages_22.llvm.dev}";
  LIBCLANG_PATH = "${pkgs.llvmPackages_22.libclang.lib}/lib";
  OPENSSL_LIB_DIR = "${pkgs.lib.getLib pkgs.openssl}/lib";
  OPENSSL_DIR = "${pkgs.openssl.dev}";
  RUSTFLAGS = silver.passthru.rustflags;
  SILVER_SYSROOT = builtins.toString ../.;
}
