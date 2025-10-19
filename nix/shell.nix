{ ocamlPackages, packages, pkgs }:

with ocamlPackages;

pkgs.mkShell {
  inputsFrom = with packages; [ simdutf tapak tapak-compressions ];
  buildInputs = [
    ocaml
    dune
    ocaml-lsp
    ocamlformat
    alcotest
    utop
    odoc
    pkgs.openssl
    pkgs.postgresql
    pkgs.pkg-config
    pkgs.systemfd
    pkgs.watchexec
  ];
}
