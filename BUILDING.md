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

On windows 64, need to do 
C:\Users\zakh\tom-stuff\hyphen>C:\Users\zakh\Downloads\bin\gendef.exe c:\Python34\python34.dll
dlltool --dllname python27.dll --def python27.def --output-lib libpython27.a
https://github.com/kivy/kivy/wiki/Creating-a-64-bit-development-environment-with-MinGW-on-Windows
