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


## Main test folder

```
# main test folder are:
ocaml2lang/sources/aneris_examples/
ocaml2lang/sources/aneris_examples/vendor/aneris/

```

## TODOS

```
- code generation with dependencies-
- serialization:
  + record definitions,
  + record values
  + formal params as local gvars
- fix Network as module
- notation for lists
- parallel composition
- indentation (protect expressions)
- toolchain command from sources to make of Coq
- generate backup files *.v if *.v already exists
- write a doc describing technical solution


# toolchain compilation:
  Include into Makefile a command to run the generation of coq files from ml sources, e.g.
   
   all: Makefile.coq
        cd vendor/aneris && $(MAKE)
        cd vendor/monotone/iris-src && $(MAKE)
        cd vendor/record-update && $(MAKE)
        +make -f Makefile.coq all
        
   from-sources:
        cd vendor/aneris && $(MAKE) from-sources
        +make -f Makefile.coq all
```
