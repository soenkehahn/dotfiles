{ pkgs, extraFlakesToInstall, ... }:

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
    pkgs.as-tree
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
    pkgs.jless
    pkgs.jq
    pkgs.just
    pkgs.neovim
    pkgs.nethogs
    pkgs.nix
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.nix-tree
    pkgs.nushell
    pkgs.potrace
    pkgs.rage
    pkgs.remmina
    pkgs.ripgrep
    pkgs.sd
    pkgs.signal-desktop
    pkgs.starship
    pkgs.swaynotificationcenter
    pkgs.yq
    (
      pkgs.writeScriptBin "switch-colortheme" ''
        set -eu

        export PATH="${pkgs.nodejs}/bin:$PATH"
        export PATH="${pkgs.nodePackages.prettier}/bin:$PATH"

        ${./switch-colortheme}
      ''
    )
    (
      pkgs.writeScriptBin "firefox" ''
        #!/usr/bin/env bash

        set -eu

        if which nixGL ; then
          exec nixGL ${pkgs.firefox}/bin/firefox "$@"
        else
          notify-send "nixGL not installed"
          exec ${pkgs.firefox}/bin/firefox "$@"
        fi
      ''
    )
  ] ++ extraFlakesToInstall;

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions;
      [
        github.copilot
        haskell.haskell
        jnoortheen.nix-ide
        justusadam.language-haskell
        ms-vsliveshare.vsliveshare
        skellock.just
        vscodevim.vim
      ] ++
      (pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "abracadabra";
          publisher = "nicoespeon";
          version = "8.1.4";
          sha256 = "sha256-WYGd6ZxKAcE+xQBB+MWj+66eBgYGaY2LgdBVwrXdcFg=";
        }
        {
          name = "base16";
          publisher = "technosophos";
          version = "0.1.3";
          sha256 = "sha256-w6OLxpbR3Ql0CKm76UIgjWCSpkv+sf9b36YauzPVAzk=";
        }
        {
          name = "nix";
          publisher = "martinring";
          version = "0.0.1";
          sha256 = "sha256-s+Gcr65T2S58SOtUhUdcHqei7qmCU0lGS3hIwagpTO0=";
        }
      ])
    ;
  };
}
