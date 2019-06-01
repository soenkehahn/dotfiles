with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "cargo-bump";
  src = fetchFromGitHub {
    owner = "wraithan";
    repo = "cargo-bump";
    rev = "85c8a2f";
    sha256 = "sha256:1r3li597z3hxr41a9xc6zf2bfj8djznlsnrb4ccj6n38rzlwh22c";
  };
  cargoSha256 = "sha256:0g5yc9gvq0qmfwbb52ddc0x70fy4s09xq1xybwip8f73q6c7wp4n";
  doCheck = false;
}
