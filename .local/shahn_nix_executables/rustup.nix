with (import ./nixpkgs.nix).stable;

let
  src = fetchurl {
    url = "https://sh.rustup.rs";
    sha256 = "sha256:0rqgnpw2hxfh1m2lz6z7vn3b5dhvkh712161gr1zppq2x6ab44lh";
    executable = true;
  };
in

runCommand "rustup.sh" {} ''
  mkdir -p $out/bin
  ln -s ${src} $out/bin/rustup.sh
''
