#!/usr/bin/env sh

set -eux

warnings="-w A-40-42-70-30-44 -error-style short"
ocamlopt -o anvil.exe $warnings anvil.ml
