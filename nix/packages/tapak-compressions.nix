{
  buildDunePackage,
  lib,
  tapak,
  dune-configurator,
  pkg-config,
  piaf,
  alcotest,
  brotli,
  zstd,
  zlib,
  bytesrw,
  doCheck ? true,
}:

buildDunePackage {
  pname = "tapak-compressions";
  inherit (tapak) version;

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../pkg/compressions
        ../../tapak-compressions.opam
        ../../dune-project
      ];
    };

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    dune-configurator
    brotli
    zstd
    zlib
  ];

  propagatedBuildInputs = [
    tapak
    piaf
    bytesrw
  ];

  inherit doCheck;
  checkInputs = [
    alcotest
  ];
}
