"""This file contains logic for finding Haskell source that we might
want to compile and import.

In general, the preferred way of accessing Haskell code from Python
via hyphen is to install the Haskell code as a ghc-pkg registered
library; such libraries can always be directly imported into Python
with hyphen.

But you might want to have some Haskell source code in the same
directory as your python source, and then magically import Haskell
functions from that source. This is possible, but there are some
limitations. The basic issue is that to ensure type-safety, the
low-level layer must enforce that we only ever compile and import from
source *once* per program run. (Reason: Haskell will recompile the
source if it has changed, which might lead us to have
binary-incompatible objects floating around which the type system
thinks can be freely inter-substituted, leading to Problems.) So we
have to have a system for deciding which Haskell modules we want
compiled as part of this 'one shot' compilation.

We provide two options for this.

Option one is enabled by calling
hyphen.find_and_load_haskell_source. This looks in the directory of
the running script for Haskell source, and (if any is found) we will
also check subdirectories for more source, and (if any is found in a
subdirectory) we check sub-sub directories recursively. All the source
we find, we try to compile; if there are .hs files lying around which
are not valid source, we will get errors. Once compiled, we can import
the contents of these files from the hs.* namespace as usual.

This is meant to cover the case when you're writing a little script
and you want to import a little bespoke Haskell routine. It assumes
that you can control the contents of the directory where your script
lives; not an unreasonable assumption.

Option two is not really recommended. It covers the case where you're
writing a python library which is being imported from somewhere on the
python path and which (in turn) wants to import a little Haskell piece
of code that lives in the same directory. (As we've said, the
recommended way of handling this is to install the Haskell code as a
Haskell library visible form ghc-pkg, then to import it from
python... but we'll assume this isn't possible for you.) To enable
option two, call find_and_load_haskell_source(check_full_path=True) as
your python library module is imported. This will check the *entire
python path* for Haskell files (using the same rules as were used to
check the script directory above, including recursively reading
subdirectories). We then compile them and again they may be
imported. This is (a) somewhat slow, and (b) runs the risk that we'll
come across an .hs file somewhere in the path which isn't valid
Haskell and die.

[There is also option 0: don't call find_and_load_haskell_source, and
just access stuff from ghc-pkg visible libraries; and option 3: write
your own code, similar to the code in this module, which interfaces
directly with the low level layer, and sends it a precisely-chosen set
of .hs files to compile.]

Once find_and_load_haskell_source has been called,
find_and_load_haskell_source(check_full_path=True) cannot be called,
because we only get one shot at importing. (But if
find_and_load_haskell_source(check_full_path=True) has been called, we
can still do find_and_load_haskell_source because the former finds a
strict superset of the stuff found by the latter.)

"""

from __future__           import absolute_import

import os, os.path, sys

from hyphen.caches import fetch_source_modules

haskell_source_seeking_status = "NEVER_LOADED"
        
def find_and_load_haskell_source(check_full_path=False):
    """Look for Haskell source that we might wish to compile and import
    into our python program. (If this is not done, we can import
    system-installed Haskell libraries into our python program, but
    not import functions from Haskell source that happens to be in the
    same directory as our Python program.)

    See module doctring for more information.
    """
    global haskell_source_seeking_status
    assert haskell_source_seeking_status in [
        "NEVER_LOADED", "LOADED_SCRIPT_DIR", "LOADED_FULL_PATH"]
    if haskell_source_seeking_status != "NEVER_LOADED":
        # The low-level layer will enforce that import_src not be
        # called more than once per program run (to ensure that we
        # don't hae binary-incompatible objects with the same types
        # floating about, which could happen if someone changed the
        # source between import_src calls.)
        #
        # So whatever happens, we don't want to run again.
        if haskell_source_seeking_status == "LOADED_FULL_PATH":
            # Last time we loaded everything anyway, so (since we're
            # an idempotent) we don't need to run again
            return
        elif not check_full_path:
            # Last time only loaded script dir, but we were only asked
            # to load the script dir, so we're ok
            return
        else: # haskell_source_seeking_status == "LOADED_SCRIPT_DIR" and check_full_path
            raise ValueError(
                "It is not possible to find_and_load_haskell_source with check_full_path=1 "
                "once you have called it with check_full_path=0.")

    directories_checked, found_modules = set(), set()
    if check_full_path:
        directories_to_check = {
            os.path.realpath(item) for item in sys.path if os.path.isdir(item) or item == ''}
    else:
        directories_to_check = {os.path.realpath(sys.path[0])}
    while directories_to_check:
        to_check = directories_to_check.pop()
        if to_check in directories_checked:
            continue
        directories_checked.add(to_check)
        if not os.path.exists(to_check):
            continue
        directories_found_here, hsfiles_found_yet = set(), False
        for filename in os.listdir(to_check):
            fullpath = os.path.realpath(os.path.join(to_check, filename))
            if os.path.isdir(filename):
                directories_found_here.add(fullpath)
            elif filename.endswith('.hs'):
                found_modules.add(fullpath)
                hsfiles_found_yet = True
        if hsfiles_found_yet:
            for dirname in directories_found_here:
                if dirname not in directories_checked:
                    directories_to_check.add(dirname)

    if found_modules:
        fetch_source_modules(found_modules)
        if check_full_path:
            haskell_source_seeking_status = "LOADED_FULL_PATH"
        else:
            haskell_source_seeking_status = "LOADED_SCRIPT_DIR"
