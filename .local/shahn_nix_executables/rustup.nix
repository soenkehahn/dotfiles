with (import ./nixpkgs.nix).stable;

let
  src = fetchurl {
    url = "https://sh.rustup.rs";
    sha256 = "sha256:0hgk9ifw1br8k0q8ddiw0hslalvknmdjs13jr4p038lj1pz7yk1p";
    executable = true;
  };
in

runCommand "rustup.sh" {} ''
  mkdir -p $out/bin
  ln -s ${src} $out/bin/rustup.sh
''
