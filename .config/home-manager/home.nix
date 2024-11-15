{ pkgs, extraFlakesToInstall, inputs, system, ... }:
let
  wrapInNixGL = exe:
    pkgs.writeScriptBin exe ''
      #!/usr/bin/env bash

      set -eu

      if which nixGL ; then
        exec nixGL ${pkgs.${exe}}/bin/${exe} "$@"
      else
        notify-send "nixGL not installed"
        exec ${pkgs.${exe}}/bin/${exe} "$@"
      fi
    ''
  ;
in
{
  programs.home-manager.enable = true;

  home.username = "shahn";
  home.homeDirectory = "/home/shahn";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  home.packages = [
    pkgs.age
    pkgs.as-tree
    pkgs.atuin
    pkgs.bat
    pkgs.bottom
    pkgs.choose
    pkgs.chromium
    pkgs.clementine
    pkgs.d2
    pkgs.dhall
    pkgs.dhall-lsp-server
    pkgs.direnv
    pkgs.du-dust
    pkgs.element-desktop
    pkgs.fd
    pkgs.fzf
    pkgs.gittyup
    pkgs.helix
    pkgs.hexyl
    pkgs.jless
    pkgs.jq
    pkgs.just
    pkgs.musikcube
    pkgs.neovim
    pkgs.nethogs
    pkgs.nix-direnv
    pkgs.nix-output-monitor
    pkgs.nixpkgs-fmt
    pkgs.nodejs
    pkgs.nushell
    pkgs.ormolu
    pkgs.potrace
    pkgs.rage
    pkgs.remmina
    pkgs.ripgrep
    pkgs.sd
    pkgs.simple-http-server
    pkgs.spotdl
    pkgs.starship
    pkgs.sway
    pkgs.swaynotificationcenter
    pkgs.sysbench
    pkgs.tor-browser
    pkgs.uni
    pkgs.xdg-desktop-portal-wlr
    pkgs.yq
    pkgs.zellij
    (
      let pkgs = import inputs.nixpkgs_older { inherit system; };
      in pkgs.qmk
    )
    (
      pkgs.writeScriptBin "switch-colortheme" ''
        set -eu

        export PATH="${pkgs.nodejs}/bin:$PATH"
        export PATH="${pkgs.nodePackages.prettier}/bin:$PATH"

        ${./switch-colortheme}
      ''
    )
    (wrapInNixGL "alacritty")
    (wrapInNixGL "firefox")
  ] ++ extraFlakesToInstall
  ++ import ./commands.nix { inherit system pkgs inputs; };
}
