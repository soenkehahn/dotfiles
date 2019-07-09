with (import ./nixpkgs.nix);
stable.buildEnv {
  name = "shahn_nix_executables";
  paths = (
    (with stable; [
      ag
      blackbox
      cabal2nix
      docker-compose
      fd
      fzf
      ipfs
      ngrok
      nodejs
      nodePackages.parcel-bundler
      pandoc
      terraform
      yarn
    ]) ++
    [
      unstable.just
    ] ++
    (with (np "e3190b2f9ec" "0x39sb1rrl5knzhn65558y28315sg77725hw09xn5lxil63b6mgl"); [
      nodePackages.prettier
    ]) ++
    [
      (import ./atom.nix)
      (import ./cargo-bump.nix)
      (import ./cargo-script.nix)
      (import ./el.nix)
      (import ./i3-lock-and-suspend.nix)
      (import ./i3-pretty-tree.nix)
      (import ./rustup.nix)
      (import ./sensei.nix)
      (import ./sl.nix)
      (import ./toast.nix)
    ]
  );
}
