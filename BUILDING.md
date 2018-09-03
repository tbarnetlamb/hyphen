Building Hyphen
===============

Hyphen has been built (and tested) on Mac OS X, Ubuntu, Windows (32
bit and 64 bit) and Windows+Cygwin (32 bit and 64 bit). Here are some
notes on getting things to work.

General
-------

In general, you are meant to build hyphen by executing
`hyphen/build-extn.py`. This will try to invoke GHC to build the
Haskell, invoke a C compiler (usually via GHC) to build the C code,
and link everything. However, there is one important proviso: the
issue of dynamic modules.

Dynamic modules
-------

We all know GHC allows you to import functionality from
packages. There are in fact two ways you can do this. One is to
*statically* link the packages into the executable being built. The
other is to use *dynamic linking*.

If you are building a C library which will itself be dynamically
linked in to other C libraries/executables (and this is what a python
extension is, in operating system terms), then you are meant to use
dynamic linking within Haskell as well. (At least, according to the
GHC documentation [here][ghc-c-lib-guide].) For this reason, we
recommend that you use dynamic linking when building Hyphen if
possible. Unfortunately, it seems GHC just can't handle dynamic
building on Windows or Cygwin, so on these platforms, we recommend a
static build, which seems to actually work. (FWIW, I think problems
would arise only if you upgraded some of the packages used by hyphen
but then didn't rebuild hyphen, so the hyphen binary would be using
the old versions of the packages but when it tried to load the
packages at runtime from python it would pick up the new. This would
cause an error message for mismatched package versions. But I can't
promise that's the only problem you'll see.)

[ghc-c-lib-guide]: https://downloads.haskell.org/~ghc/7.0.4/docs/html/users_guide/using-shared-libs.html)

To tell `build-extn.py` to attempt a dynamic build, run with the
`--dynamic` flag. To attempt static, run with `--static`. If neither
flag is provided, we attempt static on win32 and dynamic elsewhere.

So far, it sounds like dynamic building is not only the Right Thing,
but also should be easy to do (except on poor windows). The reason
it's actually a bit nontrivial is that the Haskell platform doesn't
compile dynamic versions of libraries by default. So you need to build
various standard modules (e.g. `Data.Text`) in dynamic form. Worse,
cabal is a little bit dumb about this. If you tell it to build a
module with dynamic goodness enabled, but it's already been built
without, then it won't build anything (because the module 'has already
been built'): it doesn't realize it needs to do a rebuild to get the
dynamic version. You have to tell it to do a rebuild from scratch,
which prompts lots of warnings that rebuilding modules is dangerous.

Here's what I had to do:

    cabal install --reinstall --force-reinstalls --enable-shared text
    cabal install --reinstall --force-reinstalls --enable-shared transformers
	cabal install --reinstall --force-reinstalls --enable-shared mtl
	cabal install --reinstall --force-reinstalls --enable-shared parsec
	cabal install --reinstall --force-reinstalls --enable-shared hashable
	cabal install --reinstall --force-reinstalls --enable-shared unordered-containers
	cabal install --enable-shared --reinstall ghc-paths

BTW, even though on windows (or on other platforms if you attempt a
static build) it is not necessary to make sure you have installed
dynamic versions of all the above modules, you still have to make sure
that they are installed. So in such cases you still need to do:

    cabal install text transformers mtl parsec hashable unordered-containers ghc-paths

Mac OS X
--------

You will need to have a recent GHC and Python (in my case, both were
installed from macports).

You will then need to do all the `cabal install --enable-shared`
(unless you want to build a non-dynamic version of hyphen).

Finally, you should be able to do `build-extn.py` as per the general instructions.

Ubuntu
------

Before you build, you obviously need to make sure you have python3 and
ghc installed. You also need to make sure you have the ghc-dynamic
package (necessary to allow ghc to build and use dynamic modules) and
the python3-dev package (necessary to build python extensions). So do:

    apt-get install python3-dev ghc-dynamic ghc python3

You will then need to do all the `cabal install --enable-shared`
(unless you want to build a non-dynamic version of hyphen).

Finally, you should be able to do `build-extn.py` as per the general instructions.

Windows
-------

There are two different ways you might have your setup on windows. One
possibility is that you installed python directly from the python.org
installer and haskell from the Haskell platform installer. This is
probably the usual setup. Another possibility is that you might have
installed python from cygwin. (You can't install GHC from cygwin
though, so we imagine that was still installed from the Haskell
platform.)

Further complexity arises from the fact that both of the above exist
in two subvariants each---32 or 64 bit windows. (But be careful: if
you are using a 64 bit python or cygwin, you'd better be using a 64
bit Haskell as well, and vice versa.)

In general, all of these options work. The one fiddly thing is that in
some cases you have to build an `.a` file for the python dll. (IIRC
this was necessary for all options except the 32 bit, non-cygwin
case---but I maybe am incorrect on this point.)

To do this you need to download `gendef.exe` and `dlltool`, then do
something like the following (which is for the non-cygwin, 64-bit
case):

    C:\path\to\gendef.exe c:\Python34\python34.dll

which writes a file `python34.def`; then do

    dlltool --dllname python34.dll --def path\to\python34.def --output-lib libpython34.a

which writes a file `libpython34.a` which needs to live in the same
directory as hyphen's sources.

If you were working on cygwin, you'd do roughly the same thing except
you would replace `c:\Python34\python34.dll` with the path to the
cygwin python dll.

See
[here](https://github.com/kivy/kivy/wiki/Creating-a-64-bit-development-environment-with-MinGW-on-Windows)
for more.
