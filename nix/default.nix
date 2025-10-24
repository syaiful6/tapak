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
    buildInputs = [ pkgs.openssl pkgs.brotli pkgs.zstd ];
    checkInputs = [ alcotest ];
    PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
      pkgs.openssl
      pkgs.brotli
      pkgs.zstd
    ];
  } // args);
in

rec {
  simdutf = buildTapak {
    pname = "simdutf";
    src = genSrc {
      dirs = [ "pkg/simdutf" ];
      files = [ "simdutf.opam" ];
    };
    propagatedBuildInputs = [ ctypes dune dune-configurator ];
  };

  tapak = buildTapak {
    pname = "tapak";
    src = genSrc {
      dirs = [ "src" "pkg/kernel" ];
      files = [ "tapak.opam" ];
    };
    propagatedBuildInputs = [
      eio
      angstrom
      hmap
      logs
      piaf
      uri
      ptime
      yojson
      mirage-crypto
      mirage-crypto-rng
    ];
  };

  tapak-compressions = buildTapak {
    pname = "tapak-compressions";
    src = genSrc {
      dirs = [ "pkg/compressions" ];
      files = [ "tapak-compressions.opam" ];
    };
    propagatedBuildInputs = [
      piaf
      camlzip
      dune-configurator
    ];
  };

  tapak-ppx = buildTapak {
    pname = "tapak-ppx";
    src = genSrc {
      dirs = [ "pkg/ppx" ];
      files = [ "tapak-ppx.opam" ];
    };
    propagatedBuildInputs = [
      tapak
      ppxlib
    ];
  };
}
