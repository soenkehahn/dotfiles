{ pkgs, extraFlakesToInstall, inputs, system, jail, ... }:
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
    pkgs.github-cli
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
    pkgs.nix-tree
    pkgs.nixpkgs-fmt
    pkgs.nodePackages.prettier
    pkgs.nodejs
    pkgs.nushell
    pkgs.ormolu
    pkgs.potrace
    pkgs.rage
    pkgs.remmina
    pkgs.ripgrep
    pkgs.sd
    pkgs.simple-http-server
    pkgs.starship
    pkgs.sway
    pkgs.swaynotificationcenter
    pkgs.sysbench
    pkgs.uni
    pkgs.watchexec
    pkgs.wl-mirror
    pkgs.xdg-desktop-portal-wlr
    pkgs.xwayland-satellite
    pkgs.yq
    pkgs.zeal
    pkgs.zellij
    (
      let pkgs = import inputs.nixpkgs_23_05 { inherit system; };
      in pkgs.qmk
    )
    (pkgs.rustPlatform.buildRustPackage rec {
      pname = "tinted-builder-rust";
      version = "0.13.1";
      src = inputs.tinted-builder-rust;
      cargoLock = {
        lockFile = "${src}/Cargo.lock";
      };
      doCheck = false;
    })
  ] ++
  (
    let pkgs = import inputs.nixpkgs-unstable { inherit system; };
    in [
      pkgs.spotdl
      pkgs.yt-dlp
    ]
  ) ++ extraFlakesToInstall
  ++ import ./commands.nix { inherit system pkgs inputs jail; };
}
