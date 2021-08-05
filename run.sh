#!/bin/bash
cd lib
dune build
cd ..
cd bin
dune build main.exe
