{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  haskellPackages =
    ( if compiler == "default"
      then nixpkgs.pkgs.haskellPackages
      else nixpkgs.pkgs.haskell.packages.${compiler}
    );
in
  haskellPackages.callPackage ./memorable-bits.nix { }
