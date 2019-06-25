with (import ./nixpkgs.nix).stable;
let
  src = fetchFromGitHub {
    owner = "soenkehahn";
    repo = "el";
    rev = "4e0e973";
    sha256 = "sha256:1ahwc4rnlnf69n5jbrmjp0vzg1laph24anpfv3qajxn8ggfhw8ii";
  };
in

haskellPackages.callCabal2nix "el" src {}
