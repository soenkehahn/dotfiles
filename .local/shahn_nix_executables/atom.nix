let
  pkgsSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/fab6d810b5ac32f6d20fa2a77b97e8fbe0ffbd90.tar.gz";
    sha256 = "sha256:1k5sgd3s6b1ka7fpssc0h8xv6jmgx7lfc8myczhsa1vkmga2kwvh";
  };
  pkgs = import pkgsSrc {};
in

pkgs.runCommand "atom" {} ''
  mkdir -p $out/bin
  ln -s ${pkgs.atom-beta}/bin/atom-beta $out/bin/atom
''
