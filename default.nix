let
  default-nixpkgs = fetchTarball https://github.com/NixOS/nixpkgs/archive/50cce50143c8395738702e735c682e4afd965f1c.tar.gz ;
in
{ nixpkgs ? import default-nixpkgs {}, compiler ? "default" }:
let
  # Until new version of bits is released on Hackage we need to use this.
  bits-src = nixpkgs.pkgs.fetchFromGitHub
    {
      owner = "luke-clifton";
      repo = "bits";
      rev = "be7eea3931ebc951fd140c4829153067b0c8e0ac";
      sha256 = "1v7hcswh4bqplj8pfs63n0hl069w5w2k9wdlz3j2y5cpv7fpgak8";
    };

  haskellPackages =
    ( if compiler == "default"
      then nixpkgs.pkgs.haskellPackages
      else nixpkgs.pkgs.haskell.packages.${compiler}
    ).override
      {
        overrides = self: super:
        {
          bits = self.callCabal2nix "bits" bits-src {};
        };
      };
in
  haskellPackages.callPackage ./memorable-bits.nix { }
