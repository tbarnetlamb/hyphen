#!/bin/bash

if [[ "$BUILD_TYPE" == "dynamic" ]]
then
  cabal --version
  cabal v1-install --reinstall --force-reinstalls --enable-shared text transformers mtl parsec hashable unordered-containers ghc-paths
fi

if [[ "$BUILD_TYPE" == "static" ]]
then
  cabal --version
  cabal v1-install text transformers mtl parsec hashable unordered-containers ghc-paths
fi
