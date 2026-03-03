{
  description = "Tapak";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.11";
    cows = {
      url = "github:syaiful6/cows";
      flake = false;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      cows,
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
                (import "${cows}/nix/overlays/default.nix")
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
            sch
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
