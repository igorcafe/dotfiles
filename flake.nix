{
  description = "My configuration.nix";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # # telega
    # telega-overlay = {
    #   url = "github:ipvych/telega-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs = { nixpkgs, ... }: {
    nixosConfigurations = {
      amdpc = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [ ./nixos/configuration.nix ];
      };
      # intelpc = nixpkgs-stable.lib.nixosSystem {
      #   system = "x86_64-linux";
      #   modules = [ ./nixos/configuration_intelpc.nix ];
      # };
    };
  };
}
