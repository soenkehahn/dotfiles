let
  allPkgs = (import ./nixpkgs.nix);
  pkgs = allPkgs.stable;
in pkgs.buildEnv {
  name = "shahn_nix_executables";
  paths = (with pkgs; [
    ag
    blackbox
    docker-compose
    fd
    fzf
    ipfs
    ngrok
    nodejs
    nodePackages.parcel-bundler
    nodePackages.prettier
    pandoc
    terraform
    yarn

    allPkgs.unstable.just

    (import ./atom.nix)
    (import ./cargo-script.nix)
    (import ./cargo-bump.nix)
    (import ./i3-lock-and-suspend.nix)
    (import ./i3-pretty-tree.nix)
    (import ./rustup.nix)
    (import ./sensei.nix)
    (import ./sl.nix)
    (import ./toast.nix)
  ]);
}
