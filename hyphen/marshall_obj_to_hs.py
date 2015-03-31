from __future__           import absolute_import

import collections, types

from hyphen.utils  import (
    hs_Complex, hs_Maybe, hs_List, hs_Set, hs_Map, hs_IO, hs_Func,
    hs_mkComplex, hs_mkSet, hs_mkMap, hs_Nothing, hs_mkJust, hs_emptyList, hs_cons,
    hsprimitives, return_type_when_saturated, hs_tupletycs, hs_tupletycs_bylength,
    make_hs_tuple_type, enumerate_fresh_vars_outside)
from hyphen        import hslowlevel
import hyphen.wrapping_pyfns

def isblank(hstype):
    return isinstance(hstype.head, str) and len(hstype.tail) == 0

def pyobj_hstype_hint(py_obj, known_so_far=None, fv_src=None):
    if known_so_far is None:
        known_so_far = next(fv_src)
    if isinstance(py_obj, hslowlevel.HsObjRaw):
        return py_obj.hstype
    if fv_src is None:
        fv_src = enumerate_fresh_vars_outside(known_so_far.fvs)
    if hasattr(py_obj, 'hstype'):
        return py_obj.hstype
    if hasattr(py_obj, 'hstype_fn'):
        return py_obj.hstype_fn(known_so_far, fv_src)
    for base in (type(py_obj),) + type(py_obj).__bases__:
        if base in pyobj_hstype_hint_per_type_hooks:
            return pyobj_hstype_hint_per_type_hooks[base](py_obj, known_so_far, fv_src)
    for hook in pyobj_hstype_hint_general_hooks:
        result = hook(py_obj, known_so_far, fv_src)
        if result is not None:
            return result
    return known_so_far # 'maximally vague'

maximally_vague = (lambda _a, known_so_far, _b : known_so_far)

def ifblank(to_return):
    def analyse(_a, known_so_far, _b):
        if isblank(known_so_far):
            return to_return
        else:
            return known_so_far
    return analyse

def throughmaybes_ifblank(to_return):
    def analyse(_a, known_so_far, _b):
        if isblank(known_so_far):
            return to_return
        elif known_so_far.head == hs_Maybe and isblank(known_so_far.tail[0]):
            return hs_Maybe(to_return)
        else:
            return known_so_far
    return analyse

def whenInst(type_, what_to_do):
    def inner(py_obj, known_so_far, fv_src):
        if isinstance(py_obj, type_):
            return what_to_do(py_obj, known_so_far, fv_src)
        return None
    return inner

def breakup_known_so_far(known_so_far, expected_head, fv_src):
    if known_so_far.head == expected_head:
        return known_so_far.tail
    elif isinstance(known_so_far.head, str):
        num_blanks = expected_head.arity - len(known_so_far.tail)
        return tuple(next(fv_src) for i in range(num_blanks)) + tuple(known_so_far.tail)
    return None

def complex_hstype_hint(obj, known_so_far, fv_src):
    bkup_as_complex = breakup_known_so_far(known_so_far, hs_Complex, fv_src)
    if bkup_as_complex is None:
        return known_so_far
    parttype, = bkup_as_complex
    if isblank(parttype):
        return hs_Complex(hslowlevel.hstype_Float)
    else:
        return hs_Complex(parttype)

def sequence_hstype_hint(obj, known_so_far, fv_src, allow_tuples=True):
    if allow_tuples and known_so_far.head in hs_tupletycs:
        return tuple_hstype_hint(obj, known_so_far, fv_src, False)
    breakup_as_list = breakup_known_so_far(known_so_far, hs_List, fv_src)
    if breakup_as_list is None:
        return known_so_far
    for elem in obj:
        return hs_List(pyobj_hstype_hint(elem, breakup_as_list[0], fv_src))
    return hs_List(next(fv_src))

def tuple_hstype_hint(obj, known_so_far, fv_src, allow_lists=True):
    if allow_lists and known_so_far.head == hs_List:
        return sequence_hstype_hint(obj, known_so_far, fv_src, False)
    
    if len(obj) == 1:
        return known_so_far # no length-1 tuples in Haskell

    obj_ = list(obj)
    hint_tup_parts = breakup_known_so_far(
        known_so_far, hs_tupletycs_bylength[len(obj_)], fv_src)
    if hint_tup_parts is None:
        return known_so_far
    return make_hs_tuple_type(*[
        pyobj_hstype_hint(elem, hint, fv_src) for (elem, hint) in zip(obj_, hint_tup_parts)])

def mapping_hstype_hint(obj, known_so_far, fv_src):
    breakup = breakup_known_so_far(known_so_far, hs_Map, fv_src)
    if breakup is None:
        return known_so_far
    key_hstype_sofar, val_hstype_sofar = breakup
    for key in obj:
        return hs_Map(pyobj_hstype_hint(key,      key_hstype_sofar, fv_src),
                      pyobj_hstype_hint(obj[key], val_hstype_sofar, fv_src))
    return hs_Map(key_hstype_sofar, val_hstype_sofar)

def set_hstype_hint(obj, known_so_far, fv_src):
    breakup = breakup_known_so_far(known_so_far, hs_Set, fv_src)
    if breakup is None:
        return known_so_far
    elem_hstype_sofar, = breakup
    for elem in obj:
        return hs_Set(pyobj_hstype_hint(elem,      elem_hstype_sofar, fv_src))
    return hs_Set(elem_hstype_sofar)

pyobj_hstype_hint_per_type_hooks = {
    bool       : throughmaybes_ifblank(hslowlevel.hstype_Bool),
    str        : throughmaybes_ifblank(hslowlevel.hstype_Text),
    bytes      : throughmaybes_ifblank(hslowlevel.hstype_ByteString),
    int        : throughmaybes_ifblank(hslowlevel.hstype_Integer),
    float      : throughmaybes_ifblank(hslowlevel.hstype_Float),
    complex    : complex_hstype_hint,
    type(None) : ifblank(hs_Maybe('a')),

    types.FunctionType        : maximally_vague,
    types.LambdaType          : maximally_vague,
    types.MethodType          : maximally_vague,
    types.BuiltinFunctionType : maximally_vague,
    types.BuiltinMethodType   : maximally_vague,

    tuple                     : tuple_hstype_hint,
    list                      : sequence_hstype_hint,
    dict                      : mapping_hstype_hint,
    set                       : set_hstype_hint,
}

pyobj_hstype_hint_general_hooks = [
    whenInst(collections.Sequence, sequence_hstype_hint),
    whenInst(collections.Mapping,  mapping_hstype_hint),
    whenInst(collections.Set,      set_hstype_hint),
]


#########

def to_haskell_Complex(obj, hstype):
    assert hstype.head == hs_Complex
    partsType, = hstype.tail
    re = py_to_hs(obj.real, partsType)
    im = py_to_hs(obj.imag, partsType)
    return hslowlevel.apply(hs_mkComplex, re, im)

def to_haskell_Maybe(obj, hstype):
    assert hstype.head == hs_Maybe
    justType, = hstype.tail
    if obj is None:
        return hs_Nothing
    else:
        return hs_mkJust(py_to_hs(obj, justType))

def to_haskell_IOact(obj, hstype):
    assert hstype.head == hs_IO
    wrapped_obj = hyphen.wrapping_pyfns.wrap_pyfn(obj, hstype.tail[0])
    return hslowlevel.wrap_pyfn(wrapped_obj, hstype, 0)

def to_haskell_Function(obj, hstype):
    wrapped_obj = hyphen.wrapping_pyfns.wrap_pyfn(obj, return_type_when_saturated(hstype))
    return hslowlevel.wrap_pyfn(wrapped_obj, hstype, -1)

def to_haskell_List(obj, hstype):
    assert hstype.head == hs_List
    elemType, = hstype.tail
    parts     = [py_to_hs(elem, elemType) for elem in obj]
    hslist    = hs_emptyList.narrow_type(hstype)
    for elem in reversed(parts):
        hslist = hslowlevel.apply(hs_cons, elem, hslist)
    return hslist

def to_haskell_Set(obj, hstype):
    assert hstype.head == hs_Set
    elemType, = hstype.tail
    return hslowlevel.apply(hs_mkSet, to_haskell_List(obj), hs_List(elemType))

def to_haskell_Map(obj, hstype):
    assert hstype.head == hs_Map
    try:
        my_iter = iter(obj.items())
    except:
        my_iter = ((k, obj[k]) for k in obj)
    return hslowlevel.apply(
        hs_mkMap, to_haskell_List(my_iter), hs_List(make_hs_tuple_type(*hstype.tail)))

def to_haskell_Tuple(obj, hstype):
    assert hstype.head in hs_tupletycs
    as_pytup  = tuple(obj)
    if len(as_pytup) == 1:
        raise ValueError("No length 1 tuples in Haskell.")
    if len(as_pytup) > 14:
        raise ValueError("Converting tuples of length > 14 to Haskell not supported.")
    if len(as_pytup) != len(hstype.tail):
        raise ValueError(
            "Trying to convert python object %s of length %d to Haskell tuple "
            "of type %s with length %d" % (obj, len(as_pytup), hstype, len(hstype.tail)))
    parts = [py_to_hs(*pair) for pair in zip(as_pytup, hstype.tail)]
    return hslowlevel.apply(hsprimitives['(' + ',' * (len(as_pytup) - 1) + ')'], *parts)

def skip2(fn):
    def inner(a, b):
        return fn(a)
    return inner

hs_to_py_per_tycon_hooks = {
    hslowlevel.hstype_Bool.head        : skip2(hslowlevel.to_haskell_Bool),
    hslowlevel.hstype_Text.head        : skip2(hslowlevel.to_haskell_Text),
    hslowlevel.hstype_String.head      : skip2(hslowlevel.to_haskell_String),
    hslowlevel.hstype_Char.head        : skip2(hslowlevel.to_haskell_Char),
    hslowlevel.hstype_ByteString.head  : skip2(hslowlevel.to_haskell_ByteString),
    hslowlevel.hstype_Integer.head     : skip2(hslowlevel.to_haskell_Integer),
    hslowlevel.hstype_Int.head         : skip2(hslowlevel.to_haskell_Int),
    hslowlevel.hstype_Float.head       : skip2(hslowlevel.to_haskell_Float),
    hslowlevel.hstype_Double.head      : skip2(hslowlevel.to_haskell_Double),
    hs_Complex                         : to_haskell_Complex,
    hs_Maybe                           : to_haskell_Maybe,

    hs_IO                              : to_haskell_IOact,
    hs_Func                            : to_haskell_Function,

    hs_List                            : to_haskell_List,
    hs_Set                             : to_haskell_Set,
    hs_Map                             : to_haskell_Map,
}

hs_to_py_per_tycon_hooks.update(
    dict([(tup_tyc, to_haskell_Tuple) for tup_tyc in hs_tupletycs]))

hs_to_py_per_pytype_hooks = {}

hs_to_py_general_hooks    = []

def py_to_hs(py_obj, hstype):
    if isinstance(py_obj, hslowlevel.HsObjRaw):
        return py_obj
    if len(hstype.fvs) > 0:
        raise ValueError("When converting a Python object to Haskell, target type must "
                         "be monomorphic.")
    if hasattr(py_obj, 'hs_obj'):
        return py_obj.hs_obj(hstype)
    for base in (type(py_obj),) + type(py_obj).__bases__:
        if base in hs_to_py_per_pytype_hooks:
            return hs_to_py_per_pytype_hooks[base](py_obj, hstype)
    for hook in hs_to_py_general_hooks:
        result = hook(py_obj)
        if result is not None:
            return result
    if hstype.head in hs_to_py_per_tycon_hooks:
        return hs_to_py_per_tycon_hooks[hstype.head](py_obj, hstype)
    return None


