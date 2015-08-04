# packages needed

apt-get install python3-dev
apt-get install ghc-dynamic

# haskell packages needed

cabal install --reinstall --force-reinstalls --enable-shared text
cabal install --reinstall --force-reinstalls --enable-shared transformers
cabal install --reinstall --force-reinstalls --enable-shared mtl
cabal install --reinstall --force-reinstalls --enable-shared parsec
cabal install --reinstall --force-reinstalls --enable-shared hashable
cabal install --reinstall --force-reinstalls --enable-shared unordered-containers
cabal install --enable-shared --reinstall ghc-paths
