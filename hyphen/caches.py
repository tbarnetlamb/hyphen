"""This module is a thin wrapper around hslowlevel's import_lib and
access_basics functions. import_lib provides access to the types and
objects exposed by a Haskell module contained in any library, while
access_basics provides direct access to types and objects which are
baked into Haskell itself. The wrapper provides caching, because
import_lib and access_basics are both slow enough that we want to
avoid duplicative calls.
"""

from __future__ import absolute_import
from hyphen     import hslowlevel

module_cache        = {}

def fetch_lib_module(name):
    """Thin wrapper around import_lib in hslowlevel, caching the results
    so we never call it twice with the same module. Can be called with
    'prim' in place of a module name, in which case we return a cached
    copy of the result of hslowlevel.access_basics.

    """
    if name in module_cache:
        return module_cache[name]
    # print('reading ', name, '...')
    elif name == 'prim':
        result = module_cache[name] = hslowlevel.access_basics()
    else:
        result = module_cache[name] = hslowlevel.import_lib(name)[name]
    if name == 'GHC.Types':
        result[1]['[]'] = hs_List
    return result

def precache_modules(names):
    """hslowlevel.import_lib is somewhat faster if invoked in batch mode
    with several modules at once. This function does precisely that;
    names should be a tuple of module names for which we wish to
    involke import_lib. After the batch import, the imported modules
    are put into the cache where they can be found by future
    fetch_lib_module calls.

    """
    module_cache.update(hslowlevel.import_lib(*names))
