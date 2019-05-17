with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "cargo-script";
  src = fetchFromGitHub {
    owner = "DanielKeep";
    repo = "cargo-script";
    rev = "614e60e";
    sha256 = "sha256:0pdgffk0pzpjzzahcjwx6virgkkwv6yw8fvqgfgbvkmb1syxy7gq";
  };
  cargoSha256 = "sha256:1cgqk1dj8ln1x0ll63h83alrazhic20bqjzrn0kmphf136d9z8fm";
  doCheck = false;
}
