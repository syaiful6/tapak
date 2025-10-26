#!/bin/sh

set -e

tapak-ppx --impl "$1" | ocamlformat - --enable-outside-detected-project --impl
