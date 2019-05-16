with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "i3-lock-and-suspend";
  src = fetchFromGitHub {
    owner = "soenkehahn";
    repo = "i3-lock-and-suspend";
    rev = "e0466a3961da0e7396588d81bb8ffc63158b9955";
    sha256 = "1as7xh4d7mm1wxlh7219w327dgm1xkx6ajin531z93jffpf9ipwd";
  };
  cargoSha256 = "0jacm96l1gw9nxwavqi1x4669cg6lzy9hr18zjpwlcyb3qkw9z7f";
}
