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
      pandoc
      terraform
      yarn
    ]) ++
    (with (np "e3190b2f9ec" "0x39sb1rrl5knzhn65558y28315sg77725hw09xn5lxil63b6mgl"); [
      nodejs
      nodePackages.parcel-bundler
      nodePackages.prettier
    ]) ++
    (with (np "fd2b2b5cd56" "0jgy1dplp007la5waknrijzxh6ql88lbigyr7q8n9m7n92x736l9"); [
      atom
      just
    ]) ++
    [
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
