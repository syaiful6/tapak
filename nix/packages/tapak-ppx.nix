{
  buildDunePackage,
  lib,
  tapak,
  ppxlib,
  alcotest,
  ocamlformat,
  doCheck ? true,
}:

buildDunePackage {
  pname = "tapak-ppx";
  inherit (tapak) version;

  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../pkg/ppx
        ../../tapak-ppx.opam
        ../../dune-project
      ];
    };

  propagatedBuildInputs = [
    tapak
    ppxlib
  ];

  inherit doCheck;
  nativeCheckInputs = [
    ocamlformat
  ];
  checkInputs = [
    alcotest
  ];
}
