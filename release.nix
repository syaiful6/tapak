{
  system ? builtins.currentSystem,
  doCheck ? true,
}:
let
  flakeLock = builtins.fromJSON (builtins.readFile ./flake.lock);
  # A helper to fetch evaluation dependencies given a flake input name
  fetchGitHub =
    flakeInput:
    builtins.fetchTarball {
      url = "https://github.com/${flakeLock.nodes.${flakeInput}.locked.owner}/${
        flakeLock.nodes.${flakeInput}.locked.repo
      }/archive/${flakeLock.nodes.${flakeInput}.locked.rev}.tar.gz";
      sha256 = flakeLock.nodes.${flakeInput}.locked.narHash;
    };
  nixpkgsSrc = fetchGitHub "nixpkgs";
  treefmtSrc = fetchGitHub "treefmt-nix";

  pkgs = import nixpkgsSrc {
    inherit system;
    overlays = [
      (import ./nix/overlays)
      (_final: prev: {
        treefmt-nix = import treefmtSrc;
        # In the default overlay the projects packages inherit the tapak.doCheck attribute
        # A user can disable the checks by evaluating this file, release.nix, running:
        # $ nix-build release.nix --arg doCheck false
        tapak = prev.tapak.overrideScope (
          _final': _prev': {
            inherit doCheck;
          }
        );
      })
      (import ./nix/overlays/development.nix)
    ];
  };

  # Generate outputs for the projects packages
  # for each listed OCaml package set version.
  ocamlPackageSets = [
    "ocamlPackages" # for NixOS 25.11 this defaults to OCaml version 5.3
    "ocamlPackages_5_4"
    "ocamlPackages_5_3"
    "ocamlPackages_5_2"
  ];
  packageNames = [
    "tapak"
    "tapak-compressions"
    "tapak-ppx"
    "sch"
  ];
  outputs = pkgs.lib.genAttrs ocamlPackageSets (
    ocamlPackages: pkgs.lib.genAttrs packageNames (package: pkgs.ocaml-ng.${ocamlPackages}.${package})
  );
in
outputs
// {
  # Re-export the projects packages for the latest stable OCaml package set in
  # a flat hierarchy for convenience. Example:
  # $ nix-build release.nix -A tapak
  inherit (pkgs.ocamlPackages)
    tapak
    tapak-compressions
    tapak-ppx
    sch
    ;
  inherit (pkgs.tapak)
    dev-shell
    ;
  checks.formatting = pkgs.tapak.checks.formatting;
}
