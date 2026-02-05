{ pkgs, lib, ... }:
let
  nushellWithPlugins = plugins:
    let
      pluginList = lib.concatMapStringsSep " " lib.getExe plugins;
    in
    pkgs.writeShellScriptBin "nu" ''
      exec ${lib.getExe pkgs.nushell} \
        --plugins '[${pluginList}]' \
        "$@"
    '';
  nushell = nushellWithPlugins [
    pkgs.nushellPlugins.formats
  ];
in
{
  inherit nushellWithPlugins nushell;
}
