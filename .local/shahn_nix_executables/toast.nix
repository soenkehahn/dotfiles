with (import ./nixpkgs.nix).stable;

rustPlatform.buildRustPackage {
  name = "toast";
  src = fetchFromGitHub {
    owner = "stepchowfun";
    repo = "toast";
    rev = "de65899";
    sha256 = "sha256:1290bbcsfq32gmf3hag19cz1c3gpq2vabxcgvg7w468mmiz3gjxf";
  };
  cargoSha256 = "sha256:1ji4wpsl24ivi23f682v47fgaw75n7mjv543ag8dx1ak2qfxgpj5";
  doCheck = false;
}
