let
  pkgs = (import ./nixpkgs.nix).stable;
  src = pkgs.fetchFromGitHub {
    owner = "hspec";
    repo = "sensei";
    rev = "433b3c8";
    sha256 = "sha256:0lf9zjdy9hgd4ra7hp10pvzcp87lf2myz09m3362vjv79d94kg91";
    fetchSubmodules = true;
  };
  cabal2nixOutput =
    { mkDerivation, ansi-terminal, base, base-compat, bytestring
    , directory, filepath, fsnotify, hpack, hspec, hspec-meta
    , hspec-wai, http-client, http-types, interpolate, mockery, network
    , process, silently, stdenv, stm, text, time, unix, wai, warp
    }:
    mkDerivation {
      pname = "sensei";
      version = "0.4.0";
      inherit src;
      isLibrary = false;
      isExecutable = true;
      libraryToolDepends = [ hpack ];
      executableHaskellDepends = [
        ansi-terminal base base-compat bytestring directory filepath
        fsnotify http-client http-types network process stm text time unix
        wai warp
      ];
      testHaskellDepends = [
        ansi-terminal base base-compat bytestring directory filepath
        fsnotify hspec hspec-meta hspec-wai http-client http-types
        interpolate mockery network process silently stm text time unix wai
        warp
      ];
      preConfigure = "hpack";
      homepage = "https://github.com/hspec/sensei#readme";
      description = "Automatically run Hspec tests on file modifications";
      license = stdenv.lib.licenses.mit;
    };
in

pkgs.pkgs.haskell.packages.ghc864.callPackage cabal2nixOutput { }
