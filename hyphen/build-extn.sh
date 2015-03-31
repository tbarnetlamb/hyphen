#!/usr/bin/bash

wd=$(dirname "$0")

ghc -dynamic -shared -fPIC -i"$wd/lowlevel_src" "$wd/lowlevel_src/Hyphen.hs" "$wd/lowlevel_src/hyphen_c.c" -o "$wd/hslowlevel.so" -hidir "$wd/lowlevel_inter" -odir "$wd/lowlevel_inter" -no-hs-main -lHSrts-ghc7.6.3 -optl -Wl,-exported_symbol,_PyInit_hslowlevel,-L/opt/local/Library/Frameworks/Python.framework/Versions/3.3/lib -lpython3.3 -I/opt/local/Library/Frameworks/Python.framework/Versions/3.3/include/python3.3m -fwarn-unused-imports
