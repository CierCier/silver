{
  description = "The Silver systems programming language compiler, tooling, and standard library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    {
      self,
      nixpkgs,
      systems,
    }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
    in
    {
      packages = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = { };
            overlays = [ ];
          };
          silver = pkgs.callPackage ./package.nix { };
        in
        {
          inherit silver;
          default = silver;
        }
      );

      apps = eachSystem (
        system:
        let
          silver = self.packages.${system}.silver;
        in
        {
          default = {
            type = "app";
            program = "${silver}/bin/agc";
            meta.description = "Silver compiler driver";
          };
          agc = {
            type = "app";
            program = "${silver}/bin/agc";
            meta.description = "Silver compiler driver";
          };
          aglsp = {
            type = "app";
            program = "${silver}/bin/aglsp";
            meta.description = "Silver Language Server Protocol daemon";
          };
          agsm = {
            type = "app";
            program = "${silver}/bin/agsm";
            meta.description = "Silver submodule header extractor and source map tool";
          };
        }
      );

      devShells = eachSystem (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = { };
            overlays = [ ];
          };
          silver = self.packages.${system}.silver;
        in
        {
          default = pkgs.mkShell {
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
        }
      );

      overlays.default = final: prev: {
        silver = final.callPackage ./package.nix { };
      };
    };
}
