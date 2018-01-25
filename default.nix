{ nixpkgs ? import <nixpkgs> { }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellOverrides7103 = { 
    overrides = self: super: {
      semigroups = self.callHackage "semigroups" "0.18.3" {};
      wl-pprint-annotated = self.callHackage "wl-pprint-annotated" "0.1.0.0" {};
      transformers = self.callHackage "transformers" "0.5.4.0" {};
    };
  };

  haskellOverrides = { 
    overrides = self: super: {
    };
  };

  modifiedHaskellPackages = 
    haskellPackages.override (if compiler == "ghc7103" then haskellOverrides7103 else haskellOverrides);
in 
  modifiedHaskellPackages.callPackage ./tasty-hedgehog.nix {}
