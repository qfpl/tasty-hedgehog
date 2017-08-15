{ mkDerivation, base, hedgehog, stdenv, tagged, tasty }:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  license = stdenv.lib.licenses.bsd3;
}
