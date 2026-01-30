{
  description = "NixOS system configuration(s)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { nixpkgs, ... }: {
    nixosConfigurations.amdpc = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./nixos/configuration.nix ]; # TODO: mkdir amdpc
    };
    nixosConfigurations.acer  = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [./nixos/acer/configuration.nix ];
    };
  };
}
