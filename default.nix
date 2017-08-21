{ nixpkgs ? import <nixpkgs> { }, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
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

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      hedgehog = self.callCabal2nix "hedgehog" hedgehog {};
    };
  };

in 
  modifiedHaskellPackages.callPackage ./tasty-hedgehog.nix {}
