with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "rustup";
  src = fetchFromGitHub {
    owner = "rust-lang";
    repo = "rustup.rs";
    rev = "c67a740";
    sha256 = "sha256:0sm08xvfmk9m6j7fhr39gcp3r47pky431i8fk3g6pdk6mssnwl7i";
  };
  cargoSha256 = "sha256:02gv34asqnj36w4l6p4dpxlg14bmcbv8qns11bi8sxchdvvl2z31";
  buildInputs = [ pkgconfig openssl ];
  doCheck = false;
}
