{
  inputs.get-flake.url = "github:ursi/get-flake";

  outputs = { get-flake, ... }: get-flake ./.config/home-manager;
}
