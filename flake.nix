{
  description = "Nix template for Haskell projects";
  nixConfig = {
    extra-substituters = "https://horizon.cachix.org";
    extra-trusted-public-keys = "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0=";
  };
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixos-unified.url = "github:srid/nixos-unified";
    haskell-flake.url = "github:srid/haskell-flake";

    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.flake = false;

    horizon-core.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-core?ref=lts/ghc-9.10.x";
  };

  outputs = inputs:
    # This will import ./nix/modules/flake/*.nix
    # cf. https://nixos-unified.org/autowiring.html#flake-parts
    #
    # To write your own Nix, add or edit files in ./nix/modules/flake/
    inputs.nixos-unified.lib.mkFlake
      { inherit inputs; root = ./.; };
}
