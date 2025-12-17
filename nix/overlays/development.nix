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
      checks.formatting = treefmtEval.config.build.check ../../.;
      treefmt = treefmtEval.config.build.wrapper;
      dev-shell = final'.callPackage ../packages/dev-shell.nix { };
    }
  );
}
