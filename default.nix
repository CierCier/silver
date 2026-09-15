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
      ;
      inherit (pkgs.llvmPackages_22)
        lld
      ;
    };

    LLVM_SYS_221_PREFIX = "${pkgs.llvmPackages_22.llvm.dev}";
    LIBCLANG_PATH = "${pkgs.llvmPackages_22.libclang.lib}/lib";
    SILVER_SYSROOT = builtins.toString ./.;
  };

  result = pkgs.lib.recursiveUpdate silver {
    inherit silver shell;
    default = silver;
  };
in
result
