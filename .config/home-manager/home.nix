{ pkgs, system, jj, ... }:

{
  programs.home-manager.enable = false;

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
    pkgs.clementine
    pkgs.just
    pkgs.nil
    pkgs.nixpkgs-fmt
    pkgs.nodejs
    jj.packages.${system}.default
  ];
}
