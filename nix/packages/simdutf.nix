{
  buildDunePackage,
  lib,
  dune-configurator,
  ctypes,
  alcotest,
  doCheck ? true,
  fetchurl,
  simdutf-h ? fetchurl {
    url = "https://github.com/simdutf/simdutf/releases/download/v7.5.0/simdutf.h";
    hash = "sha256-Sh4ktRu6wfLcgnShdXdaxPB9E83IjkINzsibUqFrFD8=";
  },
  simdutf-cpp ? fetchurl {
    url = "https://github.com/simdutf/simdutf/releases/download/v7.5.0/simdutf.cpp";
    hash = "sha256-9GNP6/19MkljZHDQY5PWZeAd4X3S8pgyq+MjQ8sCU/k=";
  },
}:

buildDunePackage {
  pname = "simdutf";
  version = "7.5.0";

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../pkg/simdutf
        ../../simdutf.opam
        ../../dune-project
      ];
    };

  DOWNLOAD_SIMDUTF = "false";

  preBuild = ''
    ln -s ${simdutf-h} pkg/simdutf/c/simdutf.h
    ln -s ${simdutf-cpp} pkg/simdutf/c/simdutf.cpp
  '';

  buildInputs = [
    dune-configurator
  ];

  propagatedBuildInputs = [
    ctypes
  ];

  inherit doCheck;
  checkInputs = [
    alcotest
  ];
}
