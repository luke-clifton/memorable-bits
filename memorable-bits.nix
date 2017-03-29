{ mkDerivation, base, binary, bits, bytes, bytestring, cryptonite
, data-dword, doctest, hashable, HUnit, memory, mtl, network-ip
, optparse-applicative, random, split, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck
}:
mkDerivation {
  pname = "memorable-bits";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bits bytes bytestring cryptonite data-dword hashable
    memory mtl network-ip random split
  ];
  executableHaskellDepends = [
    base bytestring cryptonite optparse-applicative
  ];
  testHaskellDepends = [
    base doctest HUnit tasty tasty-hunit tasty-quickcheck
    tasty-smallcheck
  ];
  description = "Generate human memorable strings from binary data";
  license = stdenv.lib.licenses.bsd2;
}
