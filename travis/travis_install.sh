#!/bin/bash

if [[ $(cabal --numeric-version |  cut -d. -f1) -le 2 ]]
then
    CABAL_INSTALL_COMMAND=install
else
    CABAL_INSTALL_COMMAND=v1-install
fi

if [[ "$BUILD_TYPE" == "dynamic" ]]
then
  cabal --version
  cabal $CABAL_INSTALL_COMMAND --reinstall --force-reinstalls --enable-shared text transformers mtl parsec hashable unordered-containers ghc-paths
fi

if [[ "$BUILD_TYPE" == "static" ]]
then
  cabal --version
  cabal $CABAL_INSTALL_COMMAND text transformers mtl parsec hashable unordered-containers ghc-paths
fi
