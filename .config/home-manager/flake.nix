{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs_older.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs_unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nil.url = "github:oxalica/nil";
    i3-pretty-tree = {
      url = "github:soenkehahn/i3-pretty-tree";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sway-switch-outputs.url = "github:soenkehahn/sway-switch-outputs";
    debug-tools.url = "github:garnix-io/debug-tools";
    shellac.url = "github:soenkehahn/shellac";
    porc.url = "github:soenkehahn/porc";
    nix-tree.url = "github:utdemir/nix-tree";
    cradle = {
      url = "github:garnix-io/cradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    set-colortheme.url = "github:soenkehahn/set-colortheme";
    is-cached.url = "github:soenkehahn/is_cached";
    jj = {
      url = "github:martinvonz/jj/v0.25.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    atuin = {
      url = "github:atuinsh/atuin/v18.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs_unstable, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      pkgs_unstable = import nixpkgs_unstable { inherit system; };
    in
    {
      homeConfigurations."shahn" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
        extraSpecialArgs = {
          inherit system inputs pkgs_unstable;
          extraFlakesToInstall = pkgs.lib.lists.map (flake: flake.packages.${system}.default) [
            inputs.atuin
            inputs.i3-pretty-tree
            inputs.jj
            inputs.nil
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
