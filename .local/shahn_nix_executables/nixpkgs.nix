{
  stable =
    let
      pkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/7cd2e4ebe8ca91f829b405451586868744270100.tar.gz";
        sha256 = "0h4lacvqmk356ihc7gnb44dni6m5qza23vlgl6w6jdhr9pjcmdcm";
      };
    in import pkgsSrc {};
  unstable =
    let
      pkgsSrc = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/bc94dcf.tar.gz";
        sha256 = "sha256:1siqklf863181fqk19d0x5cd0xzxf1w0zh08lv0l0dmjc8xic64a";
      };
    in import pkgsSrc {};
}
