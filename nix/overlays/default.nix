final: prev:
let
  ocamlOverlay = final': prev': {
    tapak = final'.callPackage ../packages/tapak.nix {
    };
    tapak-compressions = final'.callPackage ../packages/tapak-compressions.nix {
    };
    tapak-ppx = final'.callPackage ../packages/tapak-ppx.nix {
    };
    simdutf = final'.callPackage ../packages/simdutf.nix {
    };
  };
in
{
  ocaml-ng.ocamlPackages_4_14 = prev.ocaml-ng.ocamlPackages_4_14.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_2 = prev.ocaml-ng.ocamlPackages_5_2.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_3 = prev.ocaml-ng.ocamlPackages_5_3.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages_5_4 = prev.ocaml-ng.ocamlPackages_5_4.overrideScope ocamlOverlay;
  ocaml-ng.ocamlPackages = prev.ocaml-ng.ocamlPackages.overrideScope ocamlOverlay;

  tapak = final.lib.makeScope final.newScope (self: {
  });
}
