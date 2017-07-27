{ mkDerivation, base, bytestring, directory, filepath, llvm-hs
, llvm-hs-pure, mtl, pretty-show, stdenv, tasty, tasty-golden
, tasty-hspec, tasty-hunit, text, transformers, wl-pprint-text
}:
mkDerivation {
  pname = "llvm-hs-pretty";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring llvm-hs-pure text wl-pprint-text
  ];
  testHaskellDepends = [
    base directory filepath llvm-hs llvm-hs-pure mtl pretty-show tasty
    tasty-golden tasty-hspec tasty-hunit text transformers
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/llvm-hs/llvm-hs-pretty";
  description = "Pretty printer for LLVM IR";
  license = stdenv.lib.licenses.mit;
}
