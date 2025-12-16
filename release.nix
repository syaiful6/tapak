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
    overlays = [
      (import ./nix/overlays)
      (_final: prev: {
        treefmt-nix = import treefmtSrc;
      })
      (import ./nix/overlays/development.nix)
    ];
  };
in
{
  inherit (pkgs.ocamlPackages)
    tapak
    tapak-compressions
    tapak-ppx
    simdutf
    ;
  inherit (pkgs.tapak)
    dev-shell
    ;
  checks.formatting = pkgs.tapak.checks.formatting;
}
