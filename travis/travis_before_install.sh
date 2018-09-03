#!/bin/bash

if [[ "$TRAVIS_OS_NAME" == "linux" ]]
then
    sudo apt-get install -y python${PY_VER} python${PY_VER}-dev ghc-dynamic ghc cabal-install
    cabal update
fi

if [[ "$TRAVIS_OS_NAME" == "osx" ]]
then
    brew install ghc@$OSX_GHC_VER cabal-install
    cabal update
fi
