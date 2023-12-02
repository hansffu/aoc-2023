{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports =
        [ inputs.haskell-flake.flakeModule inputs.treefmt-nix.flakeModule ];

      perSystem = { self', pkgs, config, ... }: {

        haskellProjects.default = {

          devShell = {
            hlsCheck.enable = true;
          };
          autoWire = [ "packages" "apps" "checks" ];
        };
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.advent-of-code;
        devShells.default = pkgs.mkShell {
          name = "AoC";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [ just ];
        };
      };
    };
}
