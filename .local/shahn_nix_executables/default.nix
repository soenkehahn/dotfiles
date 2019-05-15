let
  packages-source = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/7cd2e4ebe8ca91f829b405451586868744270100.tar.gz";
    sha256 = "0h4lacvqmk356ihc7gnb44dni6m5qza23vlgl6w6jdhr9pjcmdcm";
  };
  packages = import packages-source {};
in packages.buildEnv {
  name = "shahn_nix_executables";
  paths = (with packages; [
    ag
    fd
    nodejs
    nodePackages.parcel-bundler
    yarn
    (callPackage ./i3-lock-and-suspend.nix {})
  ]);
}
