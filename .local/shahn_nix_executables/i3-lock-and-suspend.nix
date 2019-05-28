with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "i3-lock-and-suspend";
  src = fetchFromGitHub {
    owner = "soenkehahn";
    repo = "i3-lock-and-suspend";
    rev = "6d67b2a";
    sha256 = "sha256:05554a93qkkbf2h35ffjy94466ygin0s00clg3mrpwnwg7by0vwl";
  };
  cargoSha256 = "0jacm96l1gw9nxwavqi1x4669cg6lzy9hr18zjpwlcyb3qkw9z7f";
}
