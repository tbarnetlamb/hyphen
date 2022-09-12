#!/bin/bash

#This script is for OSX, hopefully runs up to line 20, Assuming GHC and Python 3
#I have no way of testing this

#Ensure we can cabal install
echo "Get the stuff we need to do a Cabal install"
cabal install --reinstall --force-reinstalls --enable-shared text
cabal install --reinstall --force-reinstalls --enable-shared transformers
cabal install --reinstall --force-reinstalls --enable-shared mtl
cabal install --reinstall --force-reinstalls --enable-shared parsec
cabal install --reinstall --force-reinstalls --enable-shared hashable
cabal install --reinstall --force-reinstalls --enable-shared unordered-containers
cabal install --enable-shared --reinstall ghc-paths

#move to the file the python things are in
cd hyphen
echo "building hyphen"
build-extn.py
echo "Testing Hyphen"
cd ..
python3 hyphen_examples.py
echo "done"