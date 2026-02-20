#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

# dune build --build-dir /tmp/codecrafters-build-http-server-ocaml
dune build 
