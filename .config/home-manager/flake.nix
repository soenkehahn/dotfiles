{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs_older.url = "github:nixos/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    i3-pretty-tree = {
      url = "github:soenkehahn/i3-pretty-tree";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    sway-switch-outputs = {
      url = "github:soenkehahn/sway-switch-outputs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    debug-tools = {
      url = "github:garnix-io/debug-tools";
      inputs.nixpkgs-repo.follows = "nixpkgs";
    };
    porc = {
      url = "github:soenkehahn/porc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    cradle = {
      url = "github:garnix-io/cradle";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    set-colortheme = {
      url = "github:soenkehahn/set-colortheme";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    is-cached.url = "github:soenkehahn/is_cached";
    jj = {
      url = "github:martinvonz/jj/v0.30.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    atuin = {
      url = "github:atuinsh/atuin/v18.4.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    coding = {
      url = "github:soenkehahn/coding";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
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
            inputs.atuin
            inputs.coding
            inputs.i3-pretty-tree
            inputs.jj
            inputs.nil
            inputs.porc
            inputs.set-colortheme
            inputs.sway-switch-outputs
          ] ++ [
            inputs.debug-tools.packages.${system}.main_pkg
            inputs.is-cached.packages.${system}.main_pkg
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
