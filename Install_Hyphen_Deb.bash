#!/bin/bash

#This script is for Debian based linux versions, Sudo expected, tested on ubuntu 22.04.1, runs up to line 25


#Ensure Python3 is installed with dev packages
echo "get python 3 with the dev"
sudo apt-get install python3-dev ghc-dynamic ghc python3
#Ensure we can cabal install
echo "Get the stuff we need to do a Cabal install"
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh


#now we do the actual installing
#Get hyphen
#echo "downloading hyphen"
#git clone https://github.com/tbarnetlamb/hyphen.git

#Calling what's needed
cabal install --enable-shared
#move to the file the python things are in
#cd hyphen
cd hyphen
echo "building hyphen"
build-extn.py
echo "Testing Hyphen"
cd ..
python3 hyphen_examples.py
echo "done"