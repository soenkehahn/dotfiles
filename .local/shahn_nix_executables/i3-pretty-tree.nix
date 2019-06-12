with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "i3-pretty-tree";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "soenkehahn";
    repo = "i3-pretty-tree";
    rev = "90d870e";
    sha256 = "sha256:0sxi2838dn1f7rjr7nwmq4m069vzyyl3bzfja0sd6jnxfd4jxycj";
  };

  cargoSha256 = "sha256:1gfnpcs24ax7q4rfb12z1d2zvsiwc721a7w20vl4iksd66kn4fns";
}
