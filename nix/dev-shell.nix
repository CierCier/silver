{ pkgs, silver }:
pkgs.mkShell {
  inputsFrom = [ silver ];
  packages = with pkgs; [
    cargo
    rustc
    rustfmt
    clippy
    rust-analyzer
    gdb
    openssl
    nodejs
    go
    llvmPackages_22.lld
  ];

  LLVM_SYS_221_PREFIX = "${pkgs.llvmPackages_22.llvm.dev}";
  LIBCLANG_PATH = "${pkgs.llvmPackages_22.libclang.lib}/lib";
  OPENSSL_LIB_DIR = "${pkgs.lib.getLib pkgs.openssl}/lib";
  OPENSSL_DIR = "${pkgs.openssl.dev}";
  SILVER_SYSROOT = builtins.toString ../.;
}
