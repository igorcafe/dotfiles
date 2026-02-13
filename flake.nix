{
  description = "NixOS system configuration(s)";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }: {
    nixosConfigurations.amdpc = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./nixos/amdpc/configuration.nix ];
    };
    nixosConfigurations.acer  = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [./nixos/acer/configuration.nix ];
    };
  };
}
