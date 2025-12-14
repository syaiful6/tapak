final: prev: {
  ocamlPackages = prev.ocamlPackages.overrideScope (
    final': prev': {
      tapak = final'.callPackage ../packages/tapak.nix { };
      tapak-compressions = final'.callPackage ../packages/tapak-compressions.nix { };
    }
  );
}
