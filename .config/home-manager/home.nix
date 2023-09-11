{ pkgs, system, inputs, ... }:

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
    inputs.jj.packages.${system}.default
    inputs.nil.packages.${system}.default
    pkgs.chromium
    pkgs.clementine
    pkgs.d2
    pkgs.element-desktop
    pkgs.just
    pkgs.nethogs
    pkgs.nix-direnv
    pkgs.nixpkgs-fmt
    pkgs.nodejs
    pkgs.potrace
    pkgs.remmina
    pkgs.signal-desktop
    pkgs.vscodium
  ];
}
