{ pkgs, system, inputs, extraFlakesToInstall, ... }:

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
  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.packages = [
    pkgs.bat
    pkgs.bottom
    pkgs.chromium
    pkgs.clementine
    pkgs.d2
    pkgs.direnv
    pkgs.element-desktop
    pkgs.firefox
    pkgs.jless
    pkgs.jq
    pkgs.just
    pkgs.nethogs
    pkgs.nix
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.nix-tree
    pkgs.nodejs
    pkgs.potrace
    pkgs.remmina
    pkgs.sd
    pkgs.signal-desktop
    pkgs.starship
    pkgs.swaynotificationcenter
    pkgs.yq
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
