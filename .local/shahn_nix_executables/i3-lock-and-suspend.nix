with (import ./nixpkgs.nix);
with (np "42aad3948cd" "1kml10ri1ka9z7n5z8i694l04ym2rczyg25avvn51y2yfcxjh85v");

rustPlatform.buildRustPackage {
  name = "i3-lock-and-suspend";
  src = fetchFromGitHub {
    owner = "soenkehahn";
    repo = "i3-lock-and-suspend";
    rev = "153c41da3143398765b8b8d79d70a1aec952c4b2";
    sha256 = "sha256:00p2ygvq624gxcapn3ccxcilqiyy07ykvxm7lsq94z5r2847550p";
  };
  cargoSha256 = "1vha6mz5gj2ll4hgvdj1xjab5q3rlpdwdi3dyp07l33g3xwkq4wb";
}
