with (import ./nixpkgs.nix);
stable.buildEnv {
  name = "shahn_nix_executables";
  paths = (
    (with (np "4dc1154e" "1vr38m1nzwmad2r76i9rh1ccjrfzhyvbbzc10c2j4ys0m3yrr9qg"); [
      ag
      age
      blackbox
      cabal2nix
      docker-compose
      fd
      ipfs
      ngrok
      ormolu
      pandoc
      terraform
      (pkgs.writeShellScriptBin "nix" ''
        exec ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
      '')
    ]) ++
    (with (np "1692997153e4b8300ce708ae20d37e268804a0fb" "19baj7ykzb26yv808xnv6w45fbx1hs6x8w5awfg8q85w9zh3rcsc"); [
      nodejs
      nodePackages.js-beautify
      nodePackages.parcel-bundler
      nodePackages.prettier
      yarn
    ]) ++
    [
      (import ./cargo-bump.nix)
      (import ./cargo-script.nix)
      (import ./el.nix)
      (import ./i3-lock-and-suspend.nix)
      (import ./i3-pretty-tree.nix)
      (import ./sensei.nix)
      (import ./sl.nix)
    ]
  );
}
