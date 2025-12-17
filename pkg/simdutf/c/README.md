# simdutf C Bindings

This directory contains the C/C++ interface for the simdutf library bindings.

## Automatic Download

The simdutf single-header files (`simdutf.h` and `simdutf.cpp`) are **automatically downloaded** during the build process via the `download_simdutf.sh` script. This ensures you always have the correct version without vendoring large source files in the repository.

### Current Version

**simdutf 7.5.0**

### How It Works

1. When you run `dune build`, the build system checks if `simdutf.h` and `simdutf.cpp` exist in the `_build` directory
2. If they don't exist, `download_simdutf.sh` downloads them from the [simdutf GitHub releases](https://github.com/simdutf/simdutf/releases)
3. The files are cached in `_build` and reused for subsequent builds

### Updating simdutf Version

To update to a newer version of simdutf:

1. Edit `download_simdutf.sh` and update the `VERSION` variable
2. Run `dune clean` to remove cached files
3. Run `dune build` to download and build with the new version

#### Updating the simdutf Version for the Nix uild

Edit the `nix/overlays/default.nix` file and change the `simdutf-version` attribute in the `tapak` scope.
Invalidate the `hash` attribute of the two `fetchurl` calls for the `simdutf-h` and `simdutf-cpp` packages by setting `hash` to either to an empty string i.e. `""` or use `final.lib.fakeHash` .
This will cause Nix to download the new updated source and print the new expected output hash (this principle is called trust on first use (TOFU), make sure you are in a trusted network).
The output will look something like:
```console
error: hash mismatch in fixed-output derivation '/nix/store/y1zr7iddgyivabjvlgr6bqdy76spz1i1-simdutf.cpp.drv':
         specified: sha256-8GNP6/19MkljZHDQY5PWZeAd4X3S8pgyq+MjQ8sCU/k=
            got:    sha256-9GNP6/19MkljZHDQY5PWZeAd4X3S8pgyq+MjQ8sCU/k=
error: Cannot build '/nix/store/bd1m10mi1sc0akq4hywar1ibgl99gj83-ocaml5.3.0-simdutf-7.5.0.drv'.
```
Copy the respective expected output hases (`got: ...`) and set it to the `hash` attribute.
