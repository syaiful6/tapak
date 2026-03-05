final: prev:
let
  treefmtEval = final.treefmt-nix.evalModule final {
    projectRootFile = "dune-project";
    programs.nixfmt.enable = true;
    programs.ocamlformat = {
      enable = true;
      configFile = ../../.ocamlformat;
    };
    settings.formatter = { };
  };
in
{
  tapak = prev.tapak.overrideScope (
    final': prev': {
      checks.formatting = treefmtEval.config.build.check (final.lib.cleanSource ../../.);
      treefmt = treefmtEval.config.build.wrapper;
      dev-shell = final'.callPackage ../packages/dev-shell.nix { };
      docs = final.stdenv.mkDerivation {
        name = "tapak-docs";
        src = final.lib.cleanSource ../../.;
        nativeBuildInputs = with final.ocaml-ng.ocamlPackages_5_3; [
          dune_3
          ocaml
          findlib
        ];
        buildInputs = with final.ocaml-ng.ocamlPackages_5_3; [
          odoc
          tapak
          sch
          tapak-compressions
        ];
        buildPhase = ''
          dune build @doc
        '';
        installPhase = ''
          mkdir -p $out
          cp -r _build/default/_doc/_html/* $out/
        '';
      };
    }
  );
}
