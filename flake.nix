{
  description = "My configuration.nix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , ...
    } @ inputs:
    let
      system = "x86_64-linux";
      username = "user";
      inherit (self) outputs;
    in
    {
      nixosConfiguration.nixos = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs outputs; };
        system = "${system}";
        modules = [ ./nixos/configuration.nix ];
      };

      homeConfigurations."${username}" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system};
        extraSpecialArgs = { inherit inputs outputs; };
        modules = [ ./home-manager/home.nix ];
      };
    };
}
