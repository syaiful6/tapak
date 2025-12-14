final: prev: {
  ocamlPackages = prev.ocamlPackages.overrideScope (
    final': prev': {
      tapak = final'.callPackage ../packages/tapak.nix { };
      tapak-compressions = final'.callPackage ../packages/tapak-compressions.nix { };
      tapak-ppx = final'.callPackage ../packages/tapak-ppx.nix { };
      simdutf = final'.callPackage ../packages/simdutf.nix {
      };
    }
  );
  tapak = final.lib.makeScope final.newScope (self: {
    dev-shell = self.callPackage ../packages/dev-shell.nix { };
  });
}
