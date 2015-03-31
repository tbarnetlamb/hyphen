from __future__           import absolute_import

from hyphen.caches import fetch_lib_module
from hyphen        import hslowlevel

hsprimitives = fetch_lib_module('prim'        )
hs_List      = hsprimitives['[]'].hstype.head
hs_Complex   = fetch_lib_module('Data.Complex'   )[1]['Complex']
hs_Maybe     = fetch_lib_module('Data.Maybe'     )[1]['Maybe']
hs_Set       = fetch_lib_module('Data.Set'       )[1]['Set']
hs_Map       = fetch_lib_module('Data.Map'       )[1]['Map']
hs_IO        = fetch_lib_module('Prelude'        )[1]['IO']
hs_Func      = hsprimitives['(:)'].hstype.head

hs_mkComplex = fetch_lib_module('Data.Complex'   )[0][':+']
hs_imag      = fetch_lib_module('Data.Complex'   )[0]['imagPart']
hs_real      = fetch_lib_module('Data.Complex'   )[0]['realPart']
hs_mkSet     = fetch_lib_module('Data.Set'       )[0]['fromList']
hs_setElems  = fetch_lib_module('Data.Set'       )[0]['toAscList']
hs_sizeOfSet = fetch_lib_module('Data.Set'       )[0]['size']
hs_setMember = fetch_lib_module('Data.Set'       )[0]['member']
hs_mkMap     = fetch_lib_module('Data.Map.Strict')[0]['fromList']
hs_mapKeys   = fetch_lib_module('Data.Map.Strict')[0]['keys']
hs_mapPairs  = fetch_lib_module('Data.Map.Strict')[0]['assocs']
hs_sizeOfMap = fetch_lib_module('Data.Map.Strict')[0]['size']
hs_mapLookup = fetch_lib_module('Data.Map.Strict')[0]['lookup']
hs_Nothing   = fetch_lib_module('Data.Maybe'     )[0]['Nothing']
hs_mkJust    = fetch_lib_module('Data.Maybe'     )[0]['Just']
hs_isJust    = fetch_lib_module('Data.Maybe'     )[0]['isJust']
hs_fromJust  = fetch_lib_module('Data.Maybe'     )[0]['fromJust']
hs_emptyList = hsprimitives['[]']
hs_cons      = hsprimitives['(:)']
hs_null      = fetch_lib_module('Prelude'        )[0]['null']
hs_head      = fetch_lib_module('Prelude'        )[0]['head']
hs_tail      = fetch_lib_module('Prelude'        )[0]['tail']
hs_show      = fetch_lib_module('Prelude'        )[0]['show']
hs_eq        = fetch_lib_module('Prelude'        )[0]['==']
hs_lt        = fetch_lib_module('Prelude'        )[0]['<']
hs_hash      = fetch_lib_module('Data.Hashable'  )[0]['hash']

def return_type_when_saturated(hstype):
    if not isinstance(hstype.head, str) and hstype.head.name == '(->)':
        return return_type_when_saturated(hstype.tail[1])
    return hstype

def first_arg_type(hstype):
    assert hstype.head.name == '(->)'
    return hstype.tail[0]

def break_hs_fn_type(hstype, num_args=-1):
    arg_types = []
    while not isinstance(hstype.head, str) and hstype.head.name == '(->)' and num_args > 0:
        this, hstype = hstype.tail
        arg_types.append(this)
        num_args -= 1
    return arg_types, hstype

def make_hs_fn_type(arg_types, return_type):
    cur_type = return_type
    for arg_type in reversed(arg_types):
        cur_type = hs_Func(arg_type, cur_type)
    return cur_type

def datacon_tycon(dc):
    if dc is None:
        return None
    return return_type_when_saturated(dc.hstype).head

def const_fn(value):
    def inner(_):
        return value
    return inner

def hs_fn_type(*args):
    if len(args) == 0:
        raise ValueError("hs_fn_type expects at least one argument")
    ty = args[-1]
    for arg_ty in reversed(args[:-1]):
        ty = hs_Func(arg_ty, ty)
    return ty

hs_tupledacon_bylength = [hsprimitives['(' + (',' * (_l - 1)) + ')'] if _l != 1 else None
                          for _l in range(14)]
hs_tupletycs_bylength  = list(map(datacon_tycon, hs_tupledacon_bylength))
hs_tupletycs           = set(hs_tupletycs_bylength) - set([None])
hs_tupletyc_lengths    = dict((_i, _t) for (_t, _i) in enumerate(hs_tupletycs_bylength)
                              if _t is not None)
tuple_tail             = [hsprimitives['tup' + str(_l) + 'tail'] if _l > 1 else None
                          for _l in range(14)]
tuple_head             = [hsprimitives['tup' + str(_l) + 'head'] if _l > 1 else None
                          for _l in range(14)]

def make_hs_tuple_type(*components):
    if len(components) == 1:
        raise ValueError('No length-1 tuples in Haskell')
    return hs_tupletycs_bylength[len(components)](*components)

def break_haskell_tuple(tuple_):
    try:
        tlen = hs_tupletyc_lengths[tuple_.hstype.head]
    except KeyError as e:
        raise TypeError('marshall_tuple: tuple input not recognized as being of a tuple '
                        'type. (NB: we only support tuples of length <= 14.)') from e
    build = []
    for i in range(tlen-1):
        build.append(hslowlevel.apply(tuple_head[tlen], tuple_))
        tuple_ = hslowlevel.apply(tuple_tail[tlen], tuple_)
    build.append(tuple_)
    return tuple(build)

def enumerate_fresh_vars_outside(fvs):
    next_i = 0
    while True:
        next_i   += 1
        next_var  = hslowlevel.HsType('a' + str(next_i))
        if next_var not in fvs:
            yield next_var
