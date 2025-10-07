{ lib, stdenv, ocamlPackages, nix-filter, doCheck ? true, pkgs }:

with ocamlPackages;

let
  genSrc = { dirs, files }:
    with nix-filter; filter {
      root = ./..;
      include = [ "dune-project" ] ++ files ++ (builtins.map inDirectory dirs);
    };
  buildTapak = args: buildDunePackage ({
    version = "0.1.0";
    doCheck = doCheck;
    duneVersion = "3";
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.openssl pkgs.postgresql ];
    PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [ pkgs.openssl pkgs.postgresql ];
  } // args);
in

{
  tapak = buildTapak {
    pname = "tapak";
    src = genSrc {
      dirs = [ "src" ];
      files = [ "tapak.opam" ];
    };
    propagatedBuildInputs = [
      eio
      hmap
      logs
      piaf
    ];
  };
}
