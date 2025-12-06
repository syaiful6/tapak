{ lib, stdenv, ocamlPackages, nix-filter, doCheck ? false, pkgs }:

with ocamlPackages;

let
  genSrc = { dirs, files }:
    let
      root = ./..;

      mkDirMatcher = dirs: args:
        let
          rootPath = toString args.root;
          getParents = path:
            let
              parts = lib.filter (p: p != "") (lib.splitString "/" path);
              numParts = builtins.length parts;
              mkPaths = n:
                if n == 0 then []
                else [ (lib.concatStringsSep "/" (lib.take n parts)) ] ++ (mkPaths (n - 1));
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
          builtins.any (dir:
            relPath == dir || lib.hasPrefix (dir + "/") relPath
          ) fullDirs
          || (type == "directory" && builtins.elem relPath parentDirs);
    in
    nix-filter.filter {
      inherit root;
      include = [ "dune-project" ] ++ files ++ [ (mkDirMatcher dirs) ];
    };
  buildTapak = args: buildDunePackage ({
    version = "0.1.0";
    doCheck = doCheck;
    duneVersion = "3";
    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = [ pkgs.openssl ];
    checkInputs = [ alcotest ];
    PKG_CONFIG_PATH = pkgs.lib.makeSearchPath "lib/pkgconfig" [
      pkgs.openssl
    ];
  } // args);
in

rec {
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
      dirs = [ "src" "pkg/kernel" "pkg/channel" ];
      files = [ "tapak.opam" ];
    };
    propagatedBuildInputs = [
      eio
      angstrom
      hmap
      logs
      re
      piaf
      magic-mime
      uri
      ptime
      saturn
      yojson
      mirage-crypto
      mirage-crypto-rng
    ];
  };

  tapak-compressions = buildTapak {
    pname = "tapak-compressions";
    src = genSrc {
      dirs = [ "pkg/compressions" "pkg/kernel" ];
      files = [ "tapak-compressions.opam" ];
    };
    buildInputs = [ dune-configurator pkgs.brotli pkgs.zstd ];
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
    nativeBuildInputs = [ alcotest ocamlformat ];
    propagatedBuildInputs = [
      tapak
      ppxlib
    ];
  };
}
