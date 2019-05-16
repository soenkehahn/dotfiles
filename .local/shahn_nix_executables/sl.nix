with (import ./nixpkgs.nix).stable;

let
  src = fetchFromGitHub {
    owner = "mtoyoda";
    repo = "sl";
    rev = "923e7d7";
    sha256 = "sha256:173gxk0ymiw94glyjzjizp8bv8g72gwkjhacigd1an09jshdrjb4";
  };
in

runCommand "sl" { buildInputs = [ gcc ncurses ]; } ''
  cp -r ${src} src
  chmod u+w -R src
  cd src
  make
  mkdir -p $out/bin
  cp sl $out/bin/
''
