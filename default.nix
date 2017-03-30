let
  default-nixpkgs = fetchTarball https://github.com/NixOS/nixpkgs/archive/50cce50143c8395738702e735c682e4afd965f1c.tar.gz ;
in
{ nixpkgs ? import default-nixpkgs {}, compiler ? "default" }:
let
  haskellPackages =
    ( if compiler == "default"
      then nixpkgs.pkgs.haskellPackages
      else nixpkgs.pkgs.haskell.packages.${compiler}
    );
in
  haskellPackages.callPackage ./memorable-bits.nix { }
