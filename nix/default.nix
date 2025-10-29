{ inputs, lib, ... }:

let
  genSrc =
    { dirs, files }:
    let
      root = ./..;

      mkDirMatcher =
        dirs: args:
        let
          rootPath = toString args.root;
          getParents =
            path:
            let
              parts = lib.filter (p: p != "") (lib.splitString "/" path);
              numParts = builtins.length parts;
              mkPaths =
                n: if n == 0 then [ ] else [ (lib.concatStringsSep "/" (lib.take n parts)) ] ++ (mkPaths (n - 1));
            in
            mkPaths (numParts - 1);

          fullDirs = dirs;
          parentDirs = lib.unique (lib.concatMap getParents dirs);

        in
        path: type:
        let
          pathStr = toString path;
          relPath = lib.removePrefix (rootPath + "/") pathStr;
        in
        builtins.any (dir: relPath == dir || lib.hasPrefix (dir + "/") relPath) fullDirs
        || (type == "directory" && builtins.elem relPath parentDirs);
    in
    inputs.nix-filter.lib.filter {
      inherit root;
      include = [ "dune-project" ] ++ files ++ [ (mkDirMatcher dirs) ];
    };
in

{

  perSystem =
    {
      pkgs,
      inputs',
      self',
      ...
    }:
    let
      buildTapak =
        args:
        pkgs.ocamlPackages.buildDunePackage (
          {
            version = "0.1.0";
            doCheck = false;
            duneVersion = "3";
            nativeBuildInputs = [ pkgs.pkg-config ];
            buildInputs = [ pkgs.openssl ];
            checkInputs = [ pkgs.ocamlPackages.alcotest ];
            PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
              pkgs.openssl
            ];
          }
          // args
        );

    in
    {
      _module.args.pkgs = inputs'.nixpkgs.legacyPackages.extend (
        _self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_3;
        }
      );
      packages =
        with pkgs.ocamlPackages;
        with self'.packages;
        {
          simdutf =
            let
              simdutf-h = pkgs.fetchurl {
                url = "https://github.com/simdutf/simdutf/releases/download/v7.5.0/simdutf.h";
                sha256 = "sha256-Sh4ktRu6wfLcgnShdXdaxPB9E83IjkINzsibUqFrFD8=";
              };
              simdutf-cpp = pkgs.fetchurl {
                url = "https://github.com/simdutf/simdutf/releases/download/v7.5.0/simdutf.cpp";
                sha256 = "sha256-9GNP6/19MkljZHDQY5PWZeAd4X3S8pgyq+MjQ8sCU/k=";
              };
            in
            buildTapak {
              pname = "simdutf";
              src = genSrc {
                dirs = [ "pkg/simdutf" ];
                files = [ "simdutf.opam" ];
              };
              DOWNLOAD_SIMDUTF = "false";
              preBuild = ''
                cp ${simdutf-h} pkg/simdutf/c/simdutf.h
                cp ${simdutf-cpp} pkg/simdutf/c/simdutf.cpp
                chmod +w pkg/simdutf/c/simdutf.h pkg/simdutf/c/simdutf.cpp
              '';
              buildInputs = [ dune-configurator ];
              propagatedBuildInputs = [ ctypes ];
            };

          tapak = buildTapak {
            pname = "tapak";
            src = genSrc {
              dirs = [
                "src"
                "pkg/kernel"
              ];
              files = [ "tapak.opam" ];
            };
            propagatedBuildInputs = [
              eio
              angstrom
              hmap
              logs
              piaf
              uri
              ptime
              yojson
              mirage-crypto
              mirage-crypto-rng
            ];
          };

          tapak-compressions = buildTapak {
            pname = "tapak-compressions";
            src = genSrc {
              dirs = [
                "pkg/compressions"
                "pkg/kernel"
              ];
              files = [ "tapak-compressions.opam" ];
            };
            buildInputs = [
              dune-configurator
              pkgs.brotli
              pkgs.zstd
            ];
            PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
              pkgs.brotli
              pkgs.zstd
            ];
            propagatedBuildInputs = [
              tapak
              piaf
              camlzip
            ];
          };

          tapak-ppx = buildTapak {
            pname = "tapak-ppx";
            src = genSrc {
              dirs = [ "pkg/ppx" ];
              files = [ "tapak-ppx.opam" ];
            };
            nativeBuildInputs = [
              alcotest
              ocamlformat
            ];
            propagatedBuildInputs = [
              tapak
              ppxlib
            ];
          };

        };
      # defaultPackage = self'.packages.tapak;
      devShells = {
        default = pkgs.callPackage ./shell.nix { packages = self'.packages; };
      };
    };
}
