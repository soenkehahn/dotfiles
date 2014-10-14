{pkgs ? import <nixpkgs> {}}:
let
  nhc = pkgs.haskellPackages.buildLocalCabalWithArgs {
    name = "nhc";
    cabalDrvArgs = {
      doCheck = false;
    };
    src = pkgs.fetchgit {
      url = "https://github.com/soenkehahn/nhc";
      rev = "0689ae2c0851d120a5b21332d42778f33f07af55";
      sha256 = "900fc8dc33b0f2e5bb8bf05dbdbc27996527a7aa494ace1bea1ed6e7a50634ad";
    };
  };
in
pkgs.stdenv.mkDerivation {
  name = "profile";
  buildInputs = [
    pkgs.haskellPackages.cabalInstall
    pkgs.haskellPackages.ghcPlain
    nhc
  ];
  NIX_CUSTOM_PROFILE = "new-haskell";
  buildCommand = ''
    echo not for building, just a profile!
    barf
  '';
}
