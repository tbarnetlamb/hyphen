from __future__ import absolute_import

import sys, os, contextlib

if sys.hexversion >> 24 < 3:
    raise ValueError("Python 3.x required")

@contextlib.contextmanager
def modified_dlopenflags(dlopenflags):
    try:
        old_dlopenflags = sys.getdlopenflags()
        sys.setdlopenflags(dlopenflags)
        yield
    finally:
        sys.setdlopenflags(old_dlopenflags)

if os.name == 'posix':
    with modified_dlopenflags(os.RTLD_NOW | os.RTLD_GLOBAL):
        import hyphen.hslowlevel as hslowlevel
else:
    import hyphen.hslowlevel     as hslowlevel
import hyphen.caches             as caches
caches.precache_modules([
    "Prelude", "Data.Complex", "Data.Maybe", "Data.Set", "Data.Map", "Data.Map.Strict",
    "Data.Hashable", "GHC.Types", "GHC.Prim", "Data.Either", "Data.Text", "Data.ByteString"])

import hyphen.utils              as utils
import hyphen.wrapping_pyfns     as wrapping_pyfns
import hyphen.hsobj              as hsobj
import hyphen.marshall_obj_to_py as marshall_obj_to_py
import hyphen.marshall_obj_to_hs as marshall_obj_to_hs
import hyphen.marshall_ctor      as marshall_ctor
import hyphen.importing          as importing

from   hyphen.hslowlevel     import HsType, TyCon, HsException, HsObjRaw
from   hyphen.hsobj          import HsObj
from   hyphen.source_loading import find_and_load_haskell_source

_USED = (hslowlevel, caches, utils, wrapping_pyfns, hsobj, marshall_obj_to_py,
         marshall_obj_to_hs, marshall_ctor, importing, HsType, TyCon, HsException)

HsFunObj = marshall_ctor.marshall_tycon(utils.hs_Func)
importing.install_hook()
import hs.Prelude, hs.Data.Text, hs.GHC.Types, hs.GHC.Prim, hs.Data.ByteString
