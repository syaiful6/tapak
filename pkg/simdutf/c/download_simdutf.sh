#!/bin/sh
set -e

VERSION="7.5.0"
BASE_URL="https://github.com/simdutf/simdutf/releases/download/v${VERSION}"

# Download if files don't exist
if [ ! -f simdutf.h ] || [ ! -f simdutf.cpp ]; then
    echo "Downloading simdutf v${VERSION}..."
    curl -L -O "${BASE_URL}/simdutf.h"
    curl -L -O "${BASE_URL}/simdutf.cpp"
    echo "Download complete!"
else
    echo "simdutf files already exist, skipping download."
fi
