{
  description = "Tapak";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
    }:
    let
      # There is also pkgs.ocaml.meta.platforms
      # flakeExposed is a subset of that.
      allSystems = nixpkgs.lib.systems.flakeExposed;
      # withPkgs: ({ Nixpkgs, System } -> AttrSet) -> PerSystemAttrSet
      withPkgs =
        pkgsCallback:
        nixpkgs.lib.genAttrs allSystems (
          system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                (import ./nix/overlays)
                (_final: _prev: {
                  treefmt-nix = import treefmt-nix;
                })
                (import ./nix/overlays/development.nix)
              ];
            };
          in
          pkgsCallback { inherit pkgs system; }
        );
    in
    {
      packages = withPkgs (
        { pkgs, system }:
        {
          default = self.packages.${system}.tapak;

          inherit (pkgs.ocamlPackages)
            tapak
            tapak-compressions
            tapak-ppx
            ;
        }
      );

      devShells = withPkgs (
        { pkgs, ... }:
        {
          default = pkgs.tapak.dev-shell;
        }
      );

      overlays.default = import ./nix/overlays;

      formatter = withPkgs ({ pkgs, ... }: pkgs.tapak.treefmt);

      checks = withPkgs (
        { pkgs, ... }:
        {
          formatting = pkgs.tapak.checks.formatting;
        }
      );
    };
}
