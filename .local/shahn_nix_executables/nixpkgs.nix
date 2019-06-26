{
  stable =
    let
      pkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/3ddd23719bb.tar.gz";
        sha256 = "sha256:1q8ghjkjnsrwi9v2iwkvaszbvmq1np5yjjrpf1y6gx25lk43ahbn";
      };
    in import pkgsSrc {};
  unstable =
    let
      pkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/83ba5afcc96.tar.gz";
        sha256 = "sha256:0swh1i3rm8a7fij6drz11s5nyzr145yh4n17k0572pp8knpxw762";
      };
    in import pkgsSrc {};
}
