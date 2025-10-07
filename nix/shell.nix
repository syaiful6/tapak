{ ocamlPackages, packages, pkgs }:

with ocamlPackages;

pkgs.mkShell {
  inputsFrom = with packages; [ tapak ];
  buildInputs = [
    ocaml
    dune
    ocaml-lsp
    ocamlformat
    utop
    pkgs.openssl
    pkgs.postgresql
    pkgs.pkg-config
  ];
}
