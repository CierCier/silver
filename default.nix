{
  system ? builtins.currentSystem,
  nixpkgs ? fetchTarball "https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable",
  pkgs ? import nixpkgs {
    config = {};
    overlays = [];
    inherit system;
  },
}:
let
  silver = pkgs.callPackage ./package.nix {};

  shell = pkgs.mkShell {
    inputsFrom = [ silver ];
    packages = builtins.attrValues {
      inherit (pkgs)
        cargo
        rustc
        rustfmt
        clippy
        rust-analyzer
        gdb
        openssl
        nodejs
        go
      ;
      inherit (pkgs.llvmPackages_22)
        lld
      ;
    };

    LLVM_SYS_221_PREFIX = "${pkgs.llvmPackages_22.llvm.dev}";
    LIBCLANG_PATH = "${pkgs.llvmPackages_22.libclang.lib}/lib";
    OPENSSL_LIB_DIR = "${pkgs.lib.getLib pkgs.openssl}/lib";
    OPENSSL_DIR = "${pkgs.openssl.dev}";
    SILVER_SYSROOT = builtins.toString ./.;
  };

  result = pkgs.lib.recursiveUpdate silver {
    inherit silver shell;
    default = silver;
  };
in
result
