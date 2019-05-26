let
  pkgs = (import ./nixpkgs.nix).stable;
in pkgs.buildEnv {
  name = "shahn_nix_executables";
  paths = (with pkgs; [
    ag
    blackbox
    fd
    nodejs
    nodePackages.parcel-bundler
    yarn

    (import ./atom.nix)
    (import ./cargo-script.nix)
    (import ./i3-lock-and-suspend.nix)
    (import ./just.nix)
    (import ./rustup.nix)
    (import ./sl.nix)
  ]);
}
