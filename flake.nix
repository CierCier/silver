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
          silver = pkgs.callPackage ./nix/package.nix { };
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
          pkgs = import nixpkgs {
            inherit system;
            config = { };
            overlays = [ ];
          };
          # Runs scripts/check.sh from the working tree inside the dev shell, so
          # the tests see local edits and the same toolchain as `nix develop`.
          check = pkgs.writeShellScriptBin "silver-check" ''
            set -euo pipefail
            root="$(${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null)" || {
              echo "silver-check: run this from inside the Silver checkout" >&2
              exit 1
            }
            cd "$root"
            exec nix develop . --command bash scripts/check.sh "$@"
          '';
        in
        {
          check = {
            type = "app";
            program = "${check}/bin/silver-check";
            meta.description = "Run every Silver test suite (Rust, stdlib stage0 and stage1, language server)";
          };
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
          default = import ./nix/dev-shell.nix { inherit pkgs silver; };
        }
      );

      overlays.default = final: prev: {
        silver = final.callPackage ./nix/package.nix { };
      };
    };
}
