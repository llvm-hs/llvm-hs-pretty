let
  pkgs = import <nixpkgs> {};
  zlib = pkgs.zlib;
  ncurses = pkgs.ncurses;
  haskellPackages = pkgs.haskellPackages;
  cabal = haskellPackages.cabal;
  cabalInstall = haskellPackages.cabalInstall;

in cabal.mkDerivation (self: {
  pname = "llvm-pp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  buildDepends = [
    zlib
    ncurses
    haskellPackages.mtl
    haskellPackages.transformers
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    #haskellPackages.pretty
    haskellPackages.cabalInstall
  ];
  buildTools = [ 
  ];
  testDepends = [
    haskellPackages.transformers
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    #haskellPackages.pretty
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
