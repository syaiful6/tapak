{
  lib,
  buildDunePackage,
  melange,
}:

buildDunePackage {
  pname = "sch-melange";
  version = "0.1.0";
  src =
    let
      fs = lib.fileset;
    in
    fs.toSource {
      root = ../..;
      fileset = fs.unions [
        ../../pkg/sch/melange
        ../../pkg/sch/src/common
        ../../sch-melange.opam
        ../../dune-project
      ];
    };

  nativeBuildInputs = [ melange ];
  propagatedBuildInputs = [ melange ];
  doCheck = false;
}
