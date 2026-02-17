{ pkgs, lib, extraFlakesToInstall, inputs, system, jail, ... }:
{
  programs.home-manager.enable = true;

  nix.registry = {
    nixpkgs = {
      from = {
        type = "indirect";
        id = "nixpkgs";
      };
      to = {
        type = "path";
        path = toString inputs.nixpkgs;
      };
    };
  };

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

  home.packages =
    [
      pkgs.age
      pkgs.alacritty
      pkgs.as-tree
      pkgs.atuin
      pkgs.bat
      pkgs.bluetui
      pkgs.bottom
      pkgs.cargo-limit
      pkgs.choose
      pkgs.d2
      pkgs.delta
      pkgs.dhall
      pkgs.dhall-lsp-server
      pkgs.direnv
      pkgs.dive
      pkgs.dust
      pkgs.element-desktop
      pkgs.fd
      pkgs.fzf
      pkgs.gimp
      pkgs.github-cli
      pkgs.gittyup
      pkgs.graphviz
      pkgs.haskellPackages.nix-derivation
      pkgs.hexyl
      pkgs.hwatch
      pkgs.hyprpicker
      pkgs.i3status
      pkgs.imv
      pkgs.inkscape
      pkgs.jjui
      pkgs.jless
      pkgs.jq
      pkgs.just
      pkgs.jwt-cli
      pkgs.kanshi
      pkgs.kdePackages.okular
      pkgs.lazyjj
      pkgs.libreoffice
      pkgs.meld
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
      pkgs.ormolu
      pkgs.potrace
      pkgs.rage
      pkgs.ranger
      pkgs.remmina
      pkgs.ripgrep
      pkgs.rofi
      pkgs.sd
      pkgs.simple-http-server
      pkgs.starship
      pkgs.sway
      pkgs.swaynotificationcenter
      pkgs.sysbench
      pkgs.uni
      pkgs.vlc
      pkgs.watchexec
      pkgs.wl-clipboard
      pkgs.wl-mirror
      pkgs.xdg-desktop-portal-wlr
      pkgs.xwayland-satellite
      pkgs.yq
      pkgs.zeal
      pkgs.zellij
    ] ++
    (
      let pkgs = import inputs.nixpkgs-unstable { inherit system; };
      in [
        pkgs.spotdl
        pkgs.yt-dlp
      ]
    ) ++
    [
      (pkgs.runCommand "qmv" { } ''
        mkdir -p $out/bin
        ln -s ${pkgs.renameutils}/bin/qmv $out/bin/qmv
      '')
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
      (import ./nushell.nix { inherit pkgs lib; }).nushell
    ] ++
    extraFlakesToInstall ++
    import ./commands.nix { inherit system pkgs lib inputs jail; };
}
