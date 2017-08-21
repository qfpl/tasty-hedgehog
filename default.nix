{ nixpkgs ? import <nixpkgs> { }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  hedgehog = pkgs.stdenv.mkDerivation {
    name = "hedgehog-0.5";
    src = pkgs.fetchFromGitHub {
      owner = "hedgehogqa";
      repo = "haskell-hedgehog";
      rev = "2591bd243751a21f0d7bf57fd54f1c30d07aa270";
      sha256 = "0dxkh97070kvfcsr5snj27fs2ckalsvqp14mwyjp4dywawdqs6n7";
    };
    phases = ["installPhase"];
    installPhase = ''
      mkdir $out
      cp -r $src/hedgehog/* $out/
      rm $out/LICENSE
      cp $src/LICENSE $out/
    '';
  };

  haskellOverrides7103 = { 
    overrides = self: super: {
      semigroups = self.callHackage "semigroups" "0.18.3" {};
      wl-pprint-annotated = self.callHackage "wl-pprint-annotated" "0.1.0.0" {};
      transformers = self.callHackage "transformers" "0.5.4.0" {};
      hedgehog = self.callCabal2nix "hedgehog" hedgehog {};
    };
  };

  haskellOverrides = { 
    overrides = self: super: {
      hedgehog = self.callCabal2nix "hedgehog" hedgehog {};
    };
  };

  modifiedHaskellPackages = 
    haskellPackages.override (if compiler == "ghc7103" then haskellOverrides7103 else haskellOverrides);
in 
  modifiedHaskellPackages.callPackage ./tasty-hedgehog.nix {}
