{
  system ? builtins.currentSystem,
  nixpkgs ? builtins.fetchTree (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes.nixpkgs.locked,
  pkgs ? import nixpkgs {
    config = {};
    overlays = [];
    inherit system;
  },
}:
let
  silver = pkgs.callPackage ./package.nix {};

  shell = import ./dev-shell.nix { inherit pkgs silver; };

  result = pkgs.lib.recursiveUpdate silver {
    inherit silver shell;
    default = silver;
  };
in
result
