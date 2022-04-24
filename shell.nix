let
  default_nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a7ecde854aee5c4c7cd6177f54a99d2c1ff28a31"; # 21.11
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  };
in

{ nixpkgs ? default_nixpkgs, llvm_hs_deps_from_source ? false }:

let

  orig_pkgs = import nixpkgs {};

  llvm-hs-repo = orig_pkgs.fetchFromGitHub {
    owner = "llvm-hs";
    repo = "llvm-hs";
    rev = "442bc488c39f0264930c95e2c98b5cf055d53e8e";
    sha256 = "1xdxy9gcgs8y32hvvmx2bl9i3h6z967v77g4yp3blqwc2kmbrpg8";
  };

  hsOverlay = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = self': super': {
        llvm-hs-pure = super'.callPackage (import "${llvm-hs-repo}/llvm-hs-pure") {};
        llvm-hs = super'.callPackage (import "${llvm-hs-repo}/llvm-hs") {
          llvm-config = self.llvm_9;
        };
        llvm-hs-pretty = super'.callPackage ./. {};
      };
    };
  };

  pkgs = import orig_pkgs.path { overlays = if llvm_hs_deps_from_source then [ hsOverlay ] else []; };

  env =
    let
      # Determine if a package is a Haskell package or not.  Stolen from:
      # <nixpkgs/pkgs/development/haskell-modules/generic-builder.nix>
      isHaskellPkg = x: (x ? pname) && (x ? version) && (x ? env);
      isSystemPkg = x: !isHaskellPkg x;

      allDependencies =
        let inherit (pkgs.haskellPackages) llvm-hs-pretty; in
        builtins.concatLists [
          llvm-hs-pretty.buildInputs
          llvm-hs-pretty.propagatedBuildInputs
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
