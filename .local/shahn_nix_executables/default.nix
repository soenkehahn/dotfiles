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
      ngrok
      ormolu
      pandoc
      (pkgs.writeShellScriptBin "nix" ''
        exec ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
      '')
    ]) ++
    (with (np "1692997153e4b8300ce708ae20d37e268804a0fb" "19baj7ykzb26yv808xnv6w45fbx1hs6x8w5awfg8q85w9zh3rcsc"); [
      nodePackages.prettier
      bandwhich
    ]) ++
    (with (np "c53014a0529cc4c7b30a72ee9f29a96b0cec7e65" "1hsix26wg3dd3r57dc9b8hk0vhs316gyq8fmwq44817yj49zc0pf"); [
      terraform
    ]) ++
    (with (np "a385dd1a" "1hbwvc00p9jf06p7vxx96dy9ddxcm6py04nwfl02rxbn80z278nh"); [
      ipfs
    ]) ++
    [
      (import ./cargo-bump.nix)
      (import ./cargo-script.nix)
      (import ./i3-lock-and-suspend.nix)
      (import ./i3-pretty-tree.nix)
    ]
  );
}
