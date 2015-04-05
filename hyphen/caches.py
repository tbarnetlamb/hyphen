from __future__ import absolute_import
from hyphen     import hslowlevel

module_cache        = {}

def fetch_lib_module(name):
    if name in module_cache:
        return module_cache[name]
    # print('reading ', name, '...')
    if name == 'prim':
        result = module_cache[name] = hslowlevel.access_basics()
    else:
        result = module_cache[name] = hslowlevel.import_lib(name)[name]
    return result

def precache_modules(names):
    module_cache.update(hslowlevel.import_lib(*names))
