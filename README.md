# ocaml2lang
Pretty Printer from OCaml to languages in Iris (heaplang and anerislang)


## How to update the inline expect tests

```
# display the diff, but do not apply it
dune runtest --promote
# automatically update the expect blocks
dune runtest --auto-promote
# continuously build while updating the expect blocks
# (useful if the editor automatically updates the file while editing)
dune runtest -w --auto-promote
```
