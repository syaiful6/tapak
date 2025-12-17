{
  buildDunePackage,
  lib,
  tapak,
  dune-configurator,
  piaf,
  camlzip,
  alcotest,
  brotli,
  zstd,
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
        ../../pkg/kernel
        ../../pkg/compressions
        ../../tapak-compressions.opam
        ../../dune-project
      ];
    };

  buildInputs = [
    dune-configurator
    brotli
    zstd
  ];

  propagatedBuildInputs = [
    tapak
    piaf
    camlzip
  ];

  inherit doCheck;
  checkInputs = [
    alcotest
  ];
}
