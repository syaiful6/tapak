{
  buildDunePackage,
  lib,
  bytesrw,
  eio,
  eio_main,
  angstrom,
  hmap,
  logs,
  re,
  cohttp,
  cohttp-eio,
  cows,
  magic-mime,
  uri,
  ptime,
  saturn,
  jsont,
  multipart_form,
  sch,
  mirage-crypto,
  mirage-crypto-rng,
  alcotest,
  doCheck ? true,
}:

buildDunePackage {
  pname = "tapak";
  version = "0.1.0";

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../src
        ../../tapak.opam
        ../../dune-project
      ];
    };

  propagatedBuildInputs = [
    eio
    eio_main
    angstrom
    bytesrw
    hmap
    logs
    re
    cohttp
    cohttp-eio
    cows
    magic-mime
    uri
    ptime
    saturn
    jsont
    multipart_form
    sch
    mirage-crypto
    mirage-crypto-rng
  ];

  inherit doCheck;
  checkInputs = [ alcotest ];
}
