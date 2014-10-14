{pkgs ? import <nixpkgs> {}}:
let
  nhc = pkgs.haskellPackages.buildLocalCabalWithArgs {
    name = "nhc";
    cabalDrvArgs = {
      doCheck = false;
    };
    src = pkgs.fetchgit {
      url = "git@github.com:soenkehahn/nhc.git";
      rev = "730b9ef127e615105e21c609985b2f802a8338bf";
      sha256 = "1da4eaaca6bbab42c305541f2252ff193924daeb8b896cf1b4dea783e08ea64a";
    };
  };
in
pkgs.stdenv.mkDerivation {
  name = "profile";
  buildInputs = [
    pkgs.haskellPackages.cabalInstall
    pkgs.haskellPackages.ghc
    nhc

    # useful haskell libraries
    pkgs.haskellPackages.interpolate
    pkgs.haskellPackages.safe
  ];
  NIX_CUSTOM_PROFILE = "old-haskell";
  buildCommand = ''
    echo not for building, just a profile!
    barf
  '';
}
