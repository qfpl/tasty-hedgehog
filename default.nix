let
  latest =
    fetchTarball https://github.com/nixos/nixpkgs-channels/archive/129f8d7e999b1a1f0fceaecadca30211e34d85a6.tar.gz;
in
{ pkgs ? import latest { }, ghc ? "ghc802" }:
pkgs.haskell.packages.${ghc}.callPackage ./tasty-hedgehog.nix { }
