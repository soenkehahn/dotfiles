{ pkgs, extraFlakesToInstall, inputs, system, ... }:
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
    pkgs.alacritty
    pkgs.as-tree
    pkgs.atuin
    pkgs.bat
    pkgs.bluetuith
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
    pkgs.firefox
    pkgs.fzf
    pkgs.gimp
    pkgs.gittyup
    pkgs.haskellPackages.nix-derivation
    pkgs.hexyl
    pkgs.hwatch
    pkgs.hyprpicker
    pkgs.jless
    pkgs.jq
    pkgs.just
    pkgs.lazyjj
    pkgs.mpd
    pkgs.musikcube
    pkgs.mympd
    pkgs.neovim
    pkgs.nethogs
    pkgs.niri
    pkgs.nix-direnv
    pkgs.nix-output-monitor
    pkgs.nixpkgs-fmt
    pkgs.nix-tree
    pkgs.nodejs
    pkgs.nodePackages.prettier
    pkgs.nomacs
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
    pkgs.watchexec
    pkgs.xdg-desktop-portal-wlr
    pkgs.xwayland-satellite
    pkgs.yq
    pkgs.zellij
    (
      let pkgs = import inputs.nixpkgs_23_05 { inherit system; };
      in pkgs.qmk
    )
  ] ++ extraFlakesToInstall
  ++ import ./commands.nix { inherit system pkgs inputs; };
}
