{
  description = "Tapak";

  inputs = {
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_3;
        });
      in
      rec {
        packages = pkgs.callPackage ./nix/default.nix { nix-filter = nix-filter.lib; };
        defaultPackage = packages.tapak;
        devShells = {
          default = pkgs.callPackage ./nix/shell.nix { inherit packages; };
        };
      }
    );
}
