#!/usr/bin/env sh

set -eux

warnings="-w A-40-42-70-30-44 -error-style short"
ocamlopt $warnings anvil.mli -o anvil.cmi 
ocamlopt $warnings anvil.ml -a -cmi-file anvil.cmi -o anvil.cmxa 

ocamlopt $warnings -I . anvil.cmx tests/test_basics.ml -o tests/test_basics.exe
(cd tests && ./test_basics.exe)
