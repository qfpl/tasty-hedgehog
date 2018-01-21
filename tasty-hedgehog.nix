{ mkDerivation, base, hedgehog, stdenv, tagged, tasty
, tasty-expected-failure
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  license = stdenv.lib.licenses.bsd3;
}
