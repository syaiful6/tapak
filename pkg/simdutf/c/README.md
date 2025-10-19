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

### Manual Download (Optional)

If you prefer to download the files manually or are building offline:

```bash
cd pkg/simdutf/c
VERSION="7.5.0"
curl -L -O "https://github.com/simdutf/simdutf/releases/download/v${VERSION}/simdutf.h"
curl -L -O "https://github.com/simdutf/simdutf/releases/download/v${VERSION}/simdutf.cpp"
```

Note: The downloaded files in the source directory are gitignored and will be automatically used by the build system if present.

## Files

- `download_simdutf.sh` - Script to download simdutf single-header files
- `simdutf_capi.h` / `simdutf_capi.cpp` - OCaml-specific C API wrapper for simdutf
- `dune` - Build configuration with download rule
