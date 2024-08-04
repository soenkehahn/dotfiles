{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs_older.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.url = "github:nixos/nixpkgs/5ba549eafcf3e33405e5f66decd1a72356632b96";
    };
    i3-pretty-tree = {
      url = "github:soenkehahn/i3-pretty-tree";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sway-switch-outputs.url = "github:soenkehahn/sway-switch-outputs";
    debug-tools.url = "github:garnix-io/debug-tools";
    shellac.url = "github:soenkehahn/shellac";
    porc.url = "github:soenkehahn/porc";
    nix.url = "github:nixos/nix/2.20.1";
    nix-tree.url = "github:utdemir/nix-tree";
    cradle = {
      url = "github:garnix-io/cradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    crate2nix.url = "github:nix-community/crate2nix";
    tinty = {
      url = "github:tinted-theming/tinty";
      flake = false;
    };
    set-colortheme.url = "github:soenkehahn/set-colortheme";
    is-cached.url = "github:soenkehahn/is_cached";
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      homeConfigurations."shahn" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = {
          inherit system inputs;
          extraFlakesToInstall = pkgs.lib.lists.map (flake: flake.packages.${system}.default) [
            inputs.i3-pretty-tree
            inputs.nil
            inputs.nix
            inputs.porc
            inputs.set-colortheme
            inputs.shellac
            inputs.sway-switch-outputs
          ] ++ [
            inputs.debug-tools.packages.${system}.main_pkg
            inputs.is-cached.packages.${system}.main_pkg
            inputs.nix-tree.defaultPackage.${system}
          ];
        };
      };
      apps.x86_64-linux = (
        let
          f = acc: derivation:
            acc
            // {
              "${derivation.name}" = {
                type = "app";
                program = pkgs.lib.getExe derivation;
              };
            };
        in
        pkgs.lib.foldl f { } (import ./commands.nix { inherit system pkgs inputs; })
      );
    };
}
