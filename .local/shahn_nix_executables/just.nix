with (import ./nixpkgs.nix).unstable;

rustPlatform.buildRustPackage {
  name = "just";
  src = fetchFromGitHub {
    owner = "casey";
    repo = "just";
    rev = "d46e6d8";
    sha256 = "0iz7cwgb4fl34z16fhij7k2jzhpvgvrdjpa75b0q6693krrp3869";
  };
  cargoSha256 = "sha256:0i7qa6qwvql9nv88vg6d7b1bh50xx53phd341fxba4a27kbmalqg";
  doCheck = false;
}
