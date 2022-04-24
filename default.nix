{ mkDerivation, array, base, bytestring, directory, filepath, lib
, llvm-hs llvm-hs-pure, mtl, prettyprinter, tasty, tasty-golden
, tasty-hspec, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "llvm-hs-pretty";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base bytestring llvm-hs-pure text prettyprinter
  ];
  testHaskellDepends = [
    base directory filepath llvm-hs llvm-hs-pure mtl tasty
    tasty-golden tasty-hspec tasty-hunit text transformers
  ];
  doHaddock = false;
  doCheck = true;
  homepage = "https://github.com/llvm-hs/llvm-hs-pretty";
  description = "Pretty printer for LLVM IR";
  license = lib.licenses.mit;
}
