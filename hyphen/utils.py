"""This module provides a collection of basic utilities for working
with the hslowlevel library. (The hslowevel library provides our low
level exposure of haskell to python, but it is low level enough that
it's a pain to work with directly!)

The first section simply gives names to a bunch of Haskell objects
that are defined in the Prelude or other crucial modules like
Data.Maybe. We also create hsprimitives, which is a cached copy of
calling hslowlevel.access_basics, which provides access to various
Haskell types and objects that are literally built into the language
itself (not even in the prelude): for instance, the type '()'.

The second section defines a bunch of convenience functions for
working with HsType objects.

The third section provides convenient access to various data
constructors and type constructors related to built-in Haskell types
like tuples.

The fourth section provides other miscellaneous utility functions that
are useful to us.
"""

from __future__           import absolute_import

from hyphen.caches import fetch_lib_module
from hyphen        import hslowlevel

## This first section gives names to a bunch of Haskell objects to
## which we want easy access. Recall that
## fetch_lib_module(<module_name>) returns a pair (objs, types) where
## objs is a dictionary containing the 'object' namespace from the
## module and types is a dictionary containing the types namespace.
hsprimitives = fetch_lib_module('prim')
hs_List      = hsprimitives['[]'].hstype.head_ll
hs_Complex   = fetch_lib_module('Data.Complex'   )[1]['Complex']
hs_Maybe     = fetch_lib_module('Data.Maybe'     )[1]['Maybe']
hs_Set       = fetch_lib_module('Data.Set'       )[1]['Set']
hs_Map       = fetch_lib_module('Data.Map'       )[1]['Map']
hs_IO        = fetch_lib_module('Prelude'        )[1]['IO']
hs_Func      = hsprimitives['(:)'].hstype.head_ll

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
hs_not       = fetch_lib_module('Prelude'        )[0]['not']
hs_compose   = fetch_lib_module('Prelude'        )[0]['.']
hs_hash      = fetch_lib_module('Data.Hashable'  )[0]['hash']

## This second section defines a bunch of convenience functions for
## working with HsType objects (defined in hslowlevel; HsType is an
## abstraction of a Haskell Type).

def return_type_when_saturated(hstype):
    """Given an HsType object t (which generally should be a function
    type), we return the 'return type' of the function. In other
    words, if we started with an object of type t, and repeatedly
    apply it to (appropriately typed) arguments until the resulting
    object is NOT a function, what is the type of that final return?

    If t is not a function type, we return t itself, on the grounds
    that we think of the process just described as degenerately
    completing 'after zero steps'.

    >>> (hs_Bool, hs_Int) = (hslowlevel.hstype_Bool, hslowlevel.hstype_Int)
    >>> fn_type = hs_Func(hs_Bool, hs_Func(hs_Int, hs_Int))
    >>> str(fn_type)
    '<hyphen.HsType object representing GHC.Types.Bool -> GHC.Types.Int -> GHC.Types.Int>'
    >>> str(return_type_when_saturated(fn_type))
    '<hyphen.HsType object representing GHC.Types.Int>'
    """
    if not isinstance(hstype.head_ll, str) and hstype.head_ll.name == '(->)':
        return return_type_when_saturated(hstype.tail[1])
    return hstype

def first_arg_type(hstype):
    """Given an HsType object t (which must be a function type), return
    the type of argument that the function takes. (If t is a type of a
    curried function which takes multiple arguments, then this will be
    the type of the first of the arguments.)

    >>> (hs_Bool, hs_Int) = (hslowlevel.hstype_Bool, hslowlevel.hstype_Int)
    >>> fn_type = hs_Func(hs_Bool, hs_Func(hs_Int, hs_Int))
    >>> str(fn_type)
    '<hyphen.HsType object representing GHC.Types.Bool -> GHC.Types.Int -> GHC.Types.Int>'
    >>> str(first_arg_type(fn_type))
    '<hyphen.HsType object representing GHC.Types.Bool>'
    """
    assert hstype.head_ll.name == '(->)'
    return hstype.tail[0]

def break_hs_fn_type(hstype, num_args=-1):
    """Given a HsType object t (which generally should be a function
    type), strip off up to num_args arguments from the type, and
    return a pair (arg_types, return_type) where arg_types is a tuple
    giving the types of the arguments and return_type is the type that
    remains after those arguments have been applied. If num_args is
    negative, we strip off as many arguments as posisble and in this
    case return_type will be guaranteed not to be a function
    type. (Otherwise, it could still be a function type if num_args is
    less than the total number of arguments that objects of type t
    require.) If objects of type t require fewer than num_args
    arguments to be fully saturated, then we will still return but
    len(arg_types) will be < num_args.

    >>> (hs_Bool, hs_Int) = (hslowlevel.hstype_Bool, hslowlevel.hstype_Int)
    >>> fn_type = hs_Func(hs_Bool, hs_Func(hs_Int, hs_Int))
    >>> str(fn_type)
    '<hyphen.HsType object representing GHC.Types.Bool -> GHC.Types.Int -> GHC.Types.Int>'
    >>> display_return = (lambda x : (tuple(str(i) for i in x[0]), str(x[1])))
    >>> display_return(break_hs_fn_type(fn_type))
    (('<hyphen.HsType object representing GHC.Types.Bool>', '<hyphen.HsType object representing GHC.Types.Int>'), '<hyphen.HsType object representing GHC.Types.Int>')
    >>> display_return(break_hs_fn_type(fn_type, 1))
    (('<hyphen.HsType object representing GHC.Types.Bool>',), '<hyphen.HsType object representing GHC.Types.Int -> GHC.Types.Int>')
    >>> display_return(break_hs_fn_type(fn_type, 0))
    ((), '<hyphen.HsType object representing GHC.Types.Bool -> GHC.Types.Int -> GHC.Types.Int>')
    >>> display_return(break_hs_fn_type(fn_type, 5))
    (('<hyphen.HsType object representing GHC.Types.Bool>', '<hyphen.HsType object representing GHC.Types.Int>'), '<hyphen.HsType object representing GHC.Types.Int>')
    """
    arg_types = []
    while (not isinstance(hstype.head_ll, str)) and (
            hstype.head_ll.name == '(->)' and num_args != 0):
        this, hstype = hstype.tail
        arg_types.append(this)
        num_args -= 1
    return arg_types, hstype

def make_hs_fn_type(arg_types, return_type):
    """Given a tuple (or other iterable) of HsType objects (a1, ..., an)
    representing argument types and a HsType object r representing a
    return type, generate an HsType object represening the type of
g    functions which input arguments of types a1, ..., an in sequence
    and return with type r; that is, generate an HsType representing
    a1 -> a2 -> ... -> an -> r.

    >>> (hs_Bool, hs_Int) = (hslowlevel.hstype_Bool, hslowlevel.hstype_Int)
    >>> str(make_hs_fn_type((hs_Bool, hs_Int, hs_Bool), hs_Bool))
    '<hyphen.HsType object representing GHC.Types.Bool -> GHC.Types.Int -> GHC.Types.Bool -> GHC.Types.Bool>'
    """
    cur_type = return_type
    for arg_type in reversed(arg_types):
        cur_type = hs_Func(arg_type, cur_type)
    return cur_type

def datacon_tycon(dc):
    """Given a Haskell object which is a data constructor, return the
    type constructor for the type of the objects constructed by the
    data constructor.

    For instance, if the Haskell program has
       data X a = A Int Int | B String a
    if we apply datacon_tycon to the representation of the data
    constructor A, then we will get back the representation of the
    type constructor X.

    >>> repr(datacon_tycon(hs_Nothing)).replace('GHC.Base', 'Data.Maybe').replace('GHC.Maybe', 'Data.Maybe')
    'hs.Data.Maybe.Maybe.hs_tycon'

    (Test written in a funny way because Data.Maybe.Maybe moved to GHC.Base, and
    we want a test that passes on both versions)
    """
    if dc is None:
        return None
    return return_type_when_saturated(dc.hstype).head_ll

def const_fn(value):
    """Return a constant function which takes a single parameter and
    returns the value 'value' irrespective of input.
    """
    def inner(_):
        return value
    return inner

## The third section provides convenient access to various data
## constructors and type constructors related to built-in Haskell
## types like tuples.

# hs_tupledacon_bylength[i] is the data-constructor for tuples of
# length i. Note hs_tupledacon_bylength[1] is 'missing'. (No length 1
# tuples exist in Haskell.)
hs_tupledacon_bylength = [hsprimitives['(' + (',' * (_l - 1)) + ')'] if _l != 1 else None
                          for _l in range(14)]

# hs_tupletycs_bylength[i] is the type-constructor for tuples of
# length i. Note hs_tupletycs_bylength[1] is 'missing'.
hs_tupletycs_bylength  = list(map(datacon_tycon, hs_tupledacon_bylength))

# Set containing all tuple type constructors 
hs_tupletycs           = set(hs_tupletycs_bylength) - set([None])

# dictionary mapping a tuple type constructor to the length of the
# tuples whose types it constructs
hs_tupletyc_lengths    = dict((_i, _t) for (_t, _i) in enumerate(hs_tupletycs_bylength)
                              if _t is not None)

# tuple_tail[3] is the function (a, b, c) -> (b, c) (and so on)
# tuple_head[3] is the function (a, b, c) -> a      (and so on)
tuple_tail             = [hsprimitives['tup' + str(_l) + 'tail'] if _l > 1 else None
                          for _l in range(14)]
tuple_head             = [hsprimitives['tup' + str(_l) + 'head'] if _l > 1 else None
                          for _l in range(14)]

def make_hs_tuple_type(*components):
    """If x1, ..., xn are HsType objects representing Haskell types
    (t1, ..., tn), then make_hs_tuple_type(x1, ..., xn) is a HsType
    object representing the Haskell tuple type (t1, ..., xn).

    >>> hs_bool, hs_int = (hslowlevel.hstype_Bool, hslowlevel.hstype_Int)
    >>> tupl_type = make_hs_tuple_type(hs_bool, hs_int)
    >>> str(tupl_type)
    '<hyphen.HsType object representing (GHC.Types.Bool, GHC.Types.Int)>'
    >>> tupl_type # doctest: +ELLIPSIS
    hs.GHC.Tuple..._['(,)'](hs.GHC.Types.Bool(), hs.GHC.Types.Int())
    """
    if len(components) == 1:
        raise ValueError('No length-1 tuples in Haskell')
    return hs_tupletycs_bylength[len(components)](*components)

def break_haskell_tuple(tuple_, treat_nontuple_as_lenth_1_tuple=False):
    """If tuple_ is an HsObj representing a Haskell tuple object (o1, o2,
    ..., on) then we return a Python tuple (x1, ..., xn) where x1 is
    an HsObj wrapping x1 and so on.

    >>> (hs_zero, hs_one, hs_two) = map(hslowlevel.to_haskell_Integer, (0, 1, 2))
    >>> my_triple = hslowlevel.apply(hsprimitives['(,,)'], hs_zero, hs_one, hs_two)
    >>> my_triple                      # doctest: +ELLIPSIS
    <hsobjraw.HsObjRaw object at ...>
    >>> hslowlevel.from_haskell_String(hslowlevel.apply(hs_show, my_triple))
    '(0,1,2)'
    >>> break_haskell_tuple(my_triple) # doctest: +ELLIPSIS
    (<hsobjraw.HsObjRaw object at ...>, <hsobjraw.HsObjRaw object at ...>, <hsobjraw.HsObjRaw object at ...>)
    >>> (x1, x2, x3) = break_haskell_tuple(my_triple)
    >>> [hslowlevel.from_haskell_String(hslowlevel.apply(hs_show, x)) for x in (x1, x2, x3)]
    ['0', '1', '2']
    """
    try:
        tlen = hs_tupletyc_lengths[tuple_.hstype.head_ll]
    except KeyError as e:
        if treat_nontuple_as_lenth_1_tuple:
            return (tuple_,)
        raise TypeError('marshall_tuple: tuple input not recognized as being of a tuple '
                        'type. (NB: we only support tuples of length <= 14.)') from e
    build = []
    for i in range(tlen-1):
        build.append(hslowlevel.apply(tuple_head[tlen-i], tuple_))
        tuple_ = hslowlevel.apply(tuple_tail[tlen-i], tuple_)
    build.append(tuple_)
    return tuple(build)

## The fourth section provides other miscellaneous utility functions
## that are useful to us.

def enumerate_fresh_vars_outside(fvs):
    """Given a set fvs of free variables (represented as strings) that
    are deemed already 'used', return a Python generater which yields
    an infinite succession of legal Haskell free variables which are
    not 'used'.
    """
    next_i = 0
    while True:
        next_i   += 1
        next_var  = hslowlevel.HsType('a' + str(next_i))
        if next_var not in fvs:
            yield next_var
