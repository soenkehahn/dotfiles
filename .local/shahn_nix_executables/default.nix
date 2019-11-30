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
      unison
    ]) ++
    (with (np "fd2b2b5cd56" "0jgy1dplp007la5waknrijzxh6ql88lbigyr7q8n9m7n92x736l9"); [
      bat
      broot
      nodejs
      nodePackages.parcel-bundler
      nodePackages.prettier
    ]) ++
    (with (np "d8a57e3614c" "03nakvzsg9flmd0dlixff87bl9grk95nwa6pj6n69qadapk14y3s"); [
      just
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
