{
  mkShell,
  treefmt,
  ocamlPackages,
  systemfd,
  watchexec,
  pkg-config,
  pkgs,
}:
let
  melange-jest = ocamlPackages.callPackage ./melange-jest.nix { };
in
mkShell {
  inputsFrom = with ocamlPackages; [
    sch
    sch-melange
    tapak
    tapak-compressions
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
      melange-jest
      pkgs.nodejs_latest
    ];
}
