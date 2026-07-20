{
  description = "Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        haskell = pkgs.haskellPackages;

        treefmt = treefmt-nix.lib.evalModule pkgs ./nix/formatter.nix;
      in
      {
        formatter = treefmt.config.build.wrapper;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            haskell.ghc
            haskell.cabal-install

            haskell.haskell-language-server
            haskell.ghcid
            haskell.hlint
            haskell.fourmolu

            pkg-config
            zlib
            git
            just

            treefmt.config.build.wrapper
          ];
        };
      }
    );
}
