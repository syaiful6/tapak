{
  mkShell,
  treefmt,
  ocamlPackages,
  systemfd,
  watchexec,
}:
mkShell {
  inputsFrom = with ocamlPackages; [
    simdutf
    tapak
    tapak-compressions
    tapak-ppx
  ];
  buildInputs =
    (with ocamlPackages; [
      ocaml-lsp
      ocamlformat
      utop
      odoc
      reason
      benchmark
    ])
    ++ [
      treefmt
      systemfd
      watchexec
    ];
}
