#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

apk update && apk add pkgconf
set -e # Exit on failure

# dune build --build-dir /tmp/codecrafters-build-http-server-ocaml
uname -a
which apt
which apk
dune build 
