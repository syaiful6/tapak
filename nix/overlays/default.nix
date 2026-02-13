final: prev:
let
  ocamlOverlay = final': prev': {
    # fix for zlib and zstd linking issues, it needs pkg-config to find them
    bytesrw = prev'.bytesrw.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [
        final.pkg-config
      ];
      propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
        final.zlib
        final.zstd
      ];
    });
    sch = final'.callPackage ../packages/sch.nix {
      inherit (final.tapak) doCheck;
    };
    tapak = final'.callPackage ../packages/tapak.nix {
      inherit (final.tapak) doCheck;
    };
    tapak-compressions = final'.callPackage ../packages/tapak-compressions.nix {
      inherit (final.tapak) doCheck;
    };
    tapak-ppx = final'.callPackage ../packages/tapak-ppx.nix {
      inherit (final.tapak) doCheck;
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
    doCheck = true;
  });
}
