{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  bits-src = nixpkgs.pkgs.fetchFromGitHub
    {
      owner = "luke-clifton";
      repo = "bits";
      rev = "be7eea3931ebc951fd140c4829153067b0c8e0ac";
      sha256 = "1v7hcswh4bqplj8pfs63n0hl069w5w2k9wdlz3j2y5cpv7fpgak8";
    };
  haskellPackages =
    nixpkgs.pkgs.haskell.packages.${compiler}.override
      {
        overrides = self: super:
        {
          bits = self.callCabal2nix "bits" bits-src {};
        };
      };
in
  haskellPackages.callPackage ./memorable-bits.nix { }
