let
  pkgs = (import ./nixpkgs.nix).stable;
in pkgs.buildEnv {
  name = "shahn_nix_executables";
  paths = (with pkgs; [
    ag
    fd
    nodejs
    nodePackages.parcel-bundler
    rustup
    yarn

    (import ./i3-lock-and-suspend.nix)
    (import ./just.nix)
  ]);
}
