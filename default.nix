{ pkgs ? import <nixpkgs> { }, ghc ? "ghc802" }:
pkgs.haskell.packages.${ghc}.callPackage ./tasty-hedgehog.nix { }
