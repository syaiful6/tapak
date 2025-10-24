#!/bin/bash

set -eo pipefail

tapak-ppx --impl "$1" -o temp.ml

cat temp.ml
