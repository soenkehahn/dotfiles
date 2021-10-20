with (import ./nixpkgs.nix);
stable.buildEnv {
  name = "shahn_nix_executables";
  paths = (
    (with (np "1cafe467b1b" "04qwnvx31x3d8mibyvjvj5n2y1cw367r3hyw5pdak0r91gzf1185"); [
      # node 8
    ]) ++
    (with (np "1ac2c6b054a257ed7c849b7dea7ebcb7045e1f59" "1jz3vvl1fvkjgyhr4r22mglp5zld3zfnvmlxb2xgprgixz3pw6g9"); [
      # node 10
    ]) ++
    (with (np "fccf069" "0if0n2gy0s46q078qg4xrzyq3fz1an888plgdp1hhs1bz7c8mh9x"); [
    ]) ++
    (with (np "42aad3948cd" "1kml10ri1ka9z7n5z8i694l04ym2rczyg25avvn51y2yfcxjh85v"); [
      ag
      age
      blackbox
      cabal2nix
      docker-compose
      fd
      ngrok
      nodejs
      nodePackages.js-beautify
      nodePackages.parcel-bundler
      nodePackages.prettier
      ormolu
      pandoc
      terraform
      yarn
    ]) ++
    (with (np "34b37ad5" "09hsmbpiycyffjq7k49g8qav4fwlj6nrp9nzxpk5rm1rws8h9z61"); [
      ipfs
    ]) ++
    (with (np "ef64a9a4" "0bj4nz6x04wc75rajyi29j64l6b88q2y5a1vlprrxlpyikklq68g"); [
      nix-query-tree-viewer
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
