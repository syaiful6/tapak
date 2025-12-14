let
  flakeLock = builtins.fromJSON (builtins.readFile ./flake.lock);
  nixpkgsSrc = builtins.fetchTarball {
    url = "https://github.com/${flakeLock.nodes.nixpkgs.locked.owner}/${flakeLock.nodes.nixpkgs.locked.repo}/archive/${flakeLock.nodes.nixpkgs.locked.rev}.tar.gz";
    sha256 = flakeLock.nodes.nixpkgs.locked.narHash;
  };
  pkgs = import nixpkgsSrc {
    overlays = [
      (import ./nix/overlays)
    ];
  };
in
{
  inherit (pkgs.ocamlPackages) tapak;
}
