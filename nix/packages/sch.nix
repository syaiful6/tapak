{
  buildDunePackage,
  lib,
  bytesrw,
  jsont,
  re,
  uri,
  ipaddr,
  alcotest,
  doCheck ? true,
}:

buildDunePackage {
  pname = "sch";
  version = "0.1.0";

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../pkg/sch
        ../../sch.opam
        ../../dune-project
      ];
    };

  buildInputs = [];

  propagatedBuildInputs = [ bytesrw jsont re uri ipaddr ];

  inherit doCheck;
  checkInputs = [
    alcotest
  ];
}
