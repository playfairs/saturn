{ pkgs, ... }:

{
  projectRootFile = "flake.nix";

  programs = {
    nixfmt.enable = true;

    fourmolu = {
      enable = true;
      package = pkgs.haskellPackages.fourmolu;
    };

    prettier.enable = true;

    taplo.enable = true;

    shfmt = {
      enable = true;
      indent_size = 2;
    };
  };
}
