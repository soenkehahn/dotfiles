{ pkgs, lib, inputs, system, ... }:
{
  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "claude-code"
  ];

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
    nixpkgs-unstable = {
      from = {
        type = "indirect";
        id = "nixpkgs-unstable";
      };
      to = {
        type = "path";
        path = toString inputs.nixpkgs-unstable;
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
      pkgs.alacritty-graphics
      pkgs.amarok
      pkgs.as-tree
      pkgs.atool
      pkgs.atuin
      pkgs.bat
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
      pkgs.entr
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
      pkgs.jless
      pkgs.jq
      pkgs.just
      pkgs.just-lsp
      pkgs.jwt-cli
      pkgs.kanshi
      pkgs.kdePackages.okular
      pkgs.kdlfmt
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
      pkgs.nixfmt
      pkgs.nixpkgs-fmt
      pkgs.nodePackages.prettier
      pkgs.nodejs
      pkgs.ormolu
      pkgs.potrace
      pkgs.pwgen
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
      pkgs.yazi
      pkgs.yq
      pkgs.zeal
    ] ++
    (
      let pkgs = import inputs.nixpkgs-unstable { inherit system; };
      in [
        pkgs.bluetui
        pkgs.jjui
        pkgs.jujutsu
        pkgs.spotdl
        pkgs.wiremix
        pkgs.yt-dlp
        pkgs.zellij
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
      (import ./nushell.nix { inherit pkgs lib; }).nushell
    ] ++
    (pkgs.lib.lists.map (flake: flake.packages.${system}.default) [
      inputs.aegis
      inputs.coding
      inputs.debug-tools
      inputs.helix
      inputs.i3-pretty-tree
      inputs.nil
      inputs.set-colortheme
      inputs.sway-switch-outputs
      inputs.treetop
    ]) ++ [
      inputs.is-cached.packages.${system}.main_pkg
    ]
    ++
    import ./commands.nix { inherit system pkgs lib inputs; };
}
