let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "68cc97d306d3187c142cfb2378852f28d47bc098";
    sha256 = "07zxbk4g4d51hf7dhsj6h7jy5c2iccm2lwaashj36inkhh9lrqa3";
  };
in

{ nixpkgs ? default_nixpkgs }:

let

  orig_pkgs = import nixpkgs {};

  llvm-hs-repo = orig_pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "76cd4d5107862401a7ebbe1bb9cc1cf172fa1d66";
    sha256 = "0bnh0yyjflhvc8vjrqsa25k7issnvkvgx149bnq7avka5mx2m99m";
  };

  hsOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = self': super': {
        llvm-hs-pure = super'.callPackage (import "${llvm-hs-repo}/llvm-hs-pure") {};
        llvm-hs = super'.callPackage (import "${llvm-hs-repo}/llvm-hs") {
          llvm-config = self.llvm_4;
        };
        llvm-hs-pretty = super'.callPackage ./. {};
      };
    };
  };

  pkgs = import orig_pkgs.path { overlays = [ hsOverlay ]; };

  env =
    let
      # Determine if a package is a Haskell package or not.  Stolen from:
      # <nixpkgs/pkgs/development/haskell-modules/generic-builder.nix>
      isHaskellPkg = x: (x ? pname) && (x ? version) && (x ? env);
      isSystemPkg = x: !isHaskellPkg x;

      allDependencies =
        let inherit (pkgs.haskellPackages) llvm-hs-pretty; in
        builtins.concatLists [
          llvm-hs-pretty.nativeBuildInputs
          llvm-hs-pretty.propagatedNativeBuildInputs
        ]
      ;
      haskellDependencies = builtins.filter isHaskellPkg allDependencies;
      systemDependencies = builtins.filter isSystemPkg allDependencies;

      ghc = pkgs.haskellPackages.ghcWithPackages
        (ps: with ps; [ cabal-install ] ++ haskellDependencies)
      ;
    in
    pkgs.stdenv.mkDerivation {
      name = "llvm-hs-env";
      buildInputs = [ ghc ] ++ systemDependencies;
      shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
    }
  ;

in

env
