let
  np = commit : sha256 :
    let
      pkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
        sha256 =
          if sha256 == null || sha256 == ""
            then "0000000000000000000000000000000000000000000000000000"
            else sha256;
      };
    in import pkgsSrc {};
in
{
  stable = np "3ddd23719bb" "1q8ghjkjnsrwi9v2iwkvaszbvmq1np5yjjrpf1y6gx25lk43ahbn";
  unstable = np "83ba5afcc96" "0swh1i3rm8a7fij6drz11s5nyzr145yh4n17k0572pp8knpxw762";
  inherit np;
}
