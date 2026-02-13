{
  mkShell,
  treefmt,
  ocamlPackages,
  systemfd,
  watchexec,
  pkg-config,
}:
mkShell {
  inputsFrom = with ocamlPackages; [
    sch
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
      pkg-config
    ];
}
