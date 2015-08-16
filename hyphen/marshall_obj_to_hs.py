"""This module has two functions:

a) The first is to handle the automatic marshalling of Python objects
(provided by the library user) into Haskell objects that can be passed
to Haskell functions that the user wishes to invoke. A given python
object will often be able to be marshalled into *several* different
Haskell objects, of different types. Thus, the marshalling operation
takes as inputs a python object *and the desired type of the
marshalled Haskell object* and gnerates a Haskell object of the
desired type (which cannot be polymorphic).

b) The second function supports the first. If we apply a polymorphic
Haskell function (like length) to a python object like [1, 2, 3], we
would like to infer that we really want to apply the
[Integer]->Integer version, and not any other version. So we provide
functionality for 'guessing' the Haskell type that a given python
object 'most' wants to convert to. (Even though it might, as discussed
in point a, actually be able to convert itself into objects of other
Haskell types.)

The actual infrastructure for both (a) and (b) works via a series of
hooks, so users can extend the built-in handing if they so desire.

"""


from __future__           import absolute_import

import collections, types

from hyphen.utils  import (
    hs_Complex, hs_Maybe, hs_List, hs_Set, hs_Map, hs_IO, hs_Func,
    hs_mkComplex, hs_mkSet, hs_mkMap, hs_Nothing, hs_mkJust, hs_emptyList, hs_cons,
    hsprimitives, return_type_when_saturated, hs_tupletycs, hs_tupletycs_bylength,
    make_hs_tuple_type, enumerate_fresh_vars_outside)
from hyphen        import hslowlevel
import hyphen.wrapping_pyfns

def skip2(fn):
    """Utility function; given a one-argument function, return a
    two-argument function that ignores its second argument and applies
    the given function to the first argument.
    """
    def inner(a, b):
        return fn(a)
    return inner

######################################################################
## The first part of the file handles marshalling a python object to a
## Haskell object where the type of the desired Haskell object is
## specified.
##
## The basic structure, as mentioned above, works via a series of
## hooks.

## The first kind of hook works based on the python type of the object
## we're trying to marshall. Such a hook is installed by writing into
## the following dictionary; the key is the python type in question,
## the value is a function which takes arguments
## (python_object_to_marshall, haskell_type_to_which_we_are_marshalling)
## and returns an appropriately marshalled Haskell object. If you
## install a hook for type T, it is inherited by all derived classes
## (but may be overridden for derived classes).
hs_to_py_per_pytype_hooks = {}

## The second kind of hook will only be used if no hook of the first
## kind was applicable. It is simply a function which takes arguments
## (python_object_to_marshall, haskell_type_to_which_we_are_marshalling)
## and decides if it wants to be responsible for the conversion. If it
## does, it returns <marshalled_haskell_object>; if not it returns
## None. Hooks should be placed in the following list; we try them
## from begining to end, and stop as soon as a hook claims to be able
## to handle the object we're converting.
hs_to_py_general_hooks    = []

## The final type of hook (which will only be used if there was no
## match for either of the previous two kinds of hook) applies based
## on the Haskell type to which we're trying to marshall the python
## value: specifically, based on the Type Constructor at the head of
## the type. To install such a hook, insert into the following
## dictionary; the key is the hslowlevel TyCon object representing the
## type constructor in question; the value is a function which takes
## arguments (python_object_to_marshall,
## haskell_type_to_which_we_are_marshalling) and returns an
## appropriately marshalled Haskell object.

## This table will have several hooks pre-installed, and we define the
## assosciated conversion functions now... it is probably good for the
## reader to read the dictionary below before reading these functions!

def to_haskell_Complex(obj, hstype):
    assert hstype.head_ll == hs_Complex
    partsType, = hstype.tail
    re = py_to_hs(obj.real, partsType)
    im = py_to_hs(obj.imag, partsType)
    return hslowlevel.apply(hs_mkComplex, re, im)

def to_haskell_Maybe(obj, hstype):
    # Our convention is if we are asked to convert a python object to
    # Maybe X for some X, then we convert None to Nothing and
    # otherwise convert any other value we see to a Haskell object obj
    # of type X, then return Just obj. This convention is useful in
    # many ways, but dealing with Maybe (Maybe X) will give bad
    # results! (In this case, the user is recommended not to marshall
    # directly to Haskell Maybe (Maybe X) objects but to build them up
    # explicitly using Just and Nothing.)
    assert hstype.head_ll == hs_Maybe
    justType, = hstype.tail
    if obj is None:
        return hs_Nothing
    else:
        return hslowlevel.apply(hs_mkJust, py_to_hs(obj, justType))

def to_haskell_IOact(obj, hstype):
    ## Python callables may be converted to IO actions; the callable will be
    ## called with no arguments.
    assert hstype.head_ll == hs_IO and callable(obj)
    wrapped_obj = hyphen.wrapping_pyfns.wrap_pyfn(obj, hstype.tail[0])
    return hslowlevel.wrap_pyfn(wrapped_obj, hstype, 0)

def to_haskell_Function(obj, hstype):
    ## Python callables may be converted to Haskell functions too
    assert hstype.head_ll == hs_Func and callable(obj)
    wrapped_obj = hyphen.wrapping_pyfns.wrap_pyfn(obj, return_type_when_saturated(hstype))
    return hslowlevel.wrap_pyfn(wrapped_obj, hstype, -1)

def to_haskell_List(obj, hstype):
    # If obj is an iterable whose elements can be converted to Haskell
    # objects of type X, then we can convert obj to be a Haskell
    # object of type [X]; this is most commonly used when in fact obj
    # is a python list.
    assert hstype.head_ll == hs_List
    elemType, = hstype.tail
    parts     = [py_to_hs(elem, elemType) for elem in obj]
    hslist    = hs_emptyList.narrow_type(hstype)
    for elem in reversed(parts):
        hslist = hslowlevel.apply(hs_cons, elem, hslist)
    return hslist

def to_haskell_Set(obj, hstype):
    assert hstype.head_ll == hs_Set
    elemType, = hstype.tail
    return hslowlevel.apply(hs_mkSet, to_haskell_List(obj, hs_List(elemType)))

def to_haskell_Map(obj, hstype):
    assert hstype.head_ll == hs_Map
    try:
        my_iter = iter(obj.items())
    except:
        my_iter = ((k, obj[k]) for k in obj)
    return hslowlevel.apply(
        hs_mkMap, to_haskell_List(my_iter, hs_List(make_hs_tuple_type(*hstype.tail))))

def to_haskell_Tuple(obj, hstype):
    assert hstype.head_ll in hs_tupletycs
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

hs_to_py_per_tycon_hooks = {
    hslowlevel.hstype_Bool.head_ll       : skip2(hslowlevel.to_haskell_Bool),
    hslowlevel.hstype_Text.head_ll       : skip2(hslowlevel.to_haskell_Text),
    hslowlevel.hstype_String.head_ll     : skip2(hslowlevel.to_haskell_String),
    hslowlevel.hstype_Char.head_ll       : skip2(hslowlevel.to_haskell_Char),
    hslowlevel.hstype_ByteString.head_ll : skip2(hslowlevel.to_haskell_ByteString),
    hslowlevel.hstype_Integer.head_ll    : skip2(hslowlevel.to_haskell_Integer),
    hslowlevel.hstype_Int.head_ll        : skip2(hslowlevel.to_haskell_Int),
    hslowlevel.hstype_Float.head_ll      : skip2(hslowlevel.to_haskell_Float),
    hslowlevel.hstype_Double.head_ll     : skip2(hslowlevel.to_haskell_Double),
    hs_Complex                           : to_haskell_Complex,
    hs_Maybe                             : to_haskell_Maybe,

    hs_IO                                : to_haskell_IOact,
    hs_Func                              : to_haskell_Function,

    hs_List                              : to_haskell_List,
    hs_Set                               : to_haskell_Set,
    hs_Map                               : to_haskell_Map,
}

hs_to_py_per_tycon_hooks.update(
    dict([(tup_tyc, to_haskell_Tuple) for tup_tyc in hs_tupletycs]))

def py_to_hs(py_obj, hstype):
    """Marshall a python object into a Haskell object of a given type
    (which must be monomorphic).
    """
    if isinstance(py_obj, hslowlevel.HsObjRaw):
        return py_obj
    if len(hstype.fvs) > 0:
        raise ValueError("When converting a Python object to Haskell, target type must "
                         "be monomorphic.")
    if hasattr(py_obj, 'hs_obj'):
        # Allow user-defined types to control their own conversion
        return py_obj.hs_obj(hstype)
    for base in (type(py_obj),) + type(py_obj).__bases__:
        if base in hs_to_py_per_pytype_hooks:
            return hs_to_py_per_pytype_hooks[base](py_obj, hstype)
    for hook in hs_to_py_general_hooks:
        result = hook(py_obj)
        if result is not None:
            return result
    if hstype.head_ll in hs_to_py_per_tycon_hooks:
        return hs_to_py_per_tycon_hooks[hstype.head_ll](py_obj, hstype)
    return None


#################################################################
## Now we give code for guessing the 'best' Haskell type for
## converting a given python object

def isblank(hstype):
    """Convenience function to tell us whether the type hstype is
    completely uninformative (that is, unifies with anything) (that
    is, of the form 'a' for some type variable a)."""
    return isinstance(hstype.head_ll, str) and len(hstype.tail) == 0

# The basic structure of this code is that we provide a big dictionary
# where the user can put hooks to handle particular cases, and the
# main function pyobj_hstype_hint just calls hooks in turn (see the
# definition right at the bottom of this file). We wish to
# pre-populate the table, and python's rules mean we have to define
# the functions we're going to use to populate the table before we can
# write the table, so the next block of code will be defining these
# functions. Human readers of this code, however, will want to first
# read the comments before the hooks tables (which are located just
# above the definition of pyobj_hstype_hint at the bottom of this
# file), which explain the hooks system, then come back here to see
# the definitions of the concrete hooks we put in place for
# simple/built in types.

maximally_vague = (lambda _a, known_so_far, _b : known_so_far)
# ^^^ hook, suitable for insertion into the
# pyobj_hstype_hint_per_type_hooks table below, which adds no further
# information beyond what is currently known.

def ifblank(to_return):
    """Return a hook, suitable for insertion into the
    pyobj_hstype_hint_per_type_hooks table below, which returns a
    given type (provided as parameter to_return), as long as we
    currently know NOTHING about the type we're trying to guess. """
    def analyse(_a, known_so_far, _b):
        if isblank(known_so_far):
            return to_return
        else:
            return known_so_far
    return analyse

def throughmaybes_ifblank(to_return):
    """Return a hook, suitable for insertion into the
    pyobj_hstype_hint_per_type_hooks table below, which returns a
    given type (provided as parameter to_return), UNLESS what we know
    so far about the type we're trying to guess is that it's of the
    form Maybe X, in which case we return 'Maybe to_return'."""
    def analyse(_a, known_so_far, _b):
        if isblank(known_so_far):
            return to_return
        elif known_so_far.head_ll == hs_Maybe and isblank(known_so_far.tail[0]):
            return hs_Maybe(to_return)
        else:
            return known_so_far
    return analyse

def whenInst(type_, what_to_do):
    """Return a hook, suitable for insertion into the
    pyobj_hstype_hint_general_hooks table below, that handles cases
    where the python object whose 'default Haskell type' we're trying
    to guess has a type deriving from type_. Note that this is only
    really useful in the case type_ is an abstract base class:
    otherwise it would be easier to just insert into
    pyobj_hstype_hint_per_type_hooks instead, which would have the
    same effect. If we have a match, we call what_to_do with arguments
    (py_obj, known_so_far, fv_src) as per pyobj_hstype_hint and return
    the result.

    """
    def inner(py_obj, known_so_far, fv_src):
        if isinstance(py_obj, type_):
            return what_to_do(py_obj, known_so_far, fv_src)
        return None
    return inner

def breakup_known_so_far(known_so_far, expected_head, fv_src):
    """If known_so_far is a type which can be refined to a type with the
    tycon provided as expected_head as the head, then we return a
    tuple of HsTypes (of length expected_head.arity) saying what can
    be deduced about the respective arguments to which expected_head
    must have been applied in such a refinement. Otherwise, return
    None. fv_src is a source of fresh free variables: see
    pyobj_hstype_hint docstring below. """
    if known_so_far.head_ll == expected_head:
        return known_so_far.tail
    elif isinstance(known_so_far.head_ll, str):
        num_blanks = expected_head.arity - len(known_so_far.tail)
        if num_blanks < 0:
            return None
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
    if allow_tuples and known_so_far.head_ll in hs_tupletycs:
        return tuple_hstype_hint(obj, known_so_far, fv_src, False)
    breakup_as_list = breakup_known_so_far(known_so_far, hs_List, fv_src)
    if breakup_as_list is None:
        return known_so_far
    for elem in obj:
        return hs_List(pyobj_hstype_hint(elem, breakup_as_list[0], fv_src))
    return hs_List(next(fv_src))

def tuple_hstype_hint(obj, known_so_far, fv_src, allow_lists=True):
    if allow_lists and known_so_far.head_ll == hs_List:
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

# Deciding the default Haskell type to which we should convert a given
# python object is handled by a series of hooks. The first kind of
# hook operates based on the python type of the object whose default
# Haskell type we're trying to decide. Such a hook is installed by
# writing into the following dictionary; the key is the python type in
# question, the value is a function which takes arguments
# 
# (python_object, known_so_far, fv_src)
#
# with exactly the same meanings as the parameters of
# pyobj_hstype_hint (defined below; see docstring for
# description). The hook should return whatever we want
# pyobj_hstype_hint to return for objects of this type.
#
# If you install a hook for type T, it is inherited by all derived
# classes (but may be overridden for derived classes).

pyobj_hstype_hint_per_type_hooks = {
    bool       : throughmaybes_ifblank(hslowlevel.hstype_Bool),
    str        : throughmaybes_ifblank(hslowlevel.hstype_Text),
    bytes      : throughmaybes_ifblank(hslowlevel.hstype_ByteString),
    int        : throughmaybes_ifblank(hslowlevel.hstype_Integer),
    float      : throughmaybes_ifblank(hslowlevel.hstype_Float),
    complex    : complex_hstype_hint,
    type(None) : ifblank(hs_Maybe('a')),

    #  Function types etc; Could want to convert to either IO action
    #  or pure function, so have to be vague
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

# The second kind of hook will only be used if no hook of the first
# kind was applicable. It is simply a function which takes arguments
#
# (python_object, known_so_far, fv_src)
#
# again with exactly the same meanings as the parameters of
# pyobj_hstype_hint (defined below); the hook should analyze the
# arguments and decide if it wants to be responsible for guessing the
# default Haskell type to which python_object will be converted in
# this case. If it does, it should return the value we want
# pyobj_hstype_hint to return; if not, it should return None. Hooks
# should be placed in the following list; we try them from begining to
# end, and stop as soon as a hook claims to want to handle the case at
# hand

pyobj_hstype_hint_general_hooks = [
    whenInst(collections.Sequence, sequence_hstype_hint),
    whenInst(collections.Mapping,  mapping_hstype_hint),
    whenInst(collections.Set,      set_hstype_hint),
]


def pyobj_hstype_hint(py_obj, known_so_far=None, fv_src=None):
    """Code for deciding the default Haskell type to which we should
    convert a given python object. Sometimes we already know something
    about the shape of the Haskell object we're looking to create
    (e.g. we might know it's a list of something; in this case, what
    we know is provided as a HsType called known_so_far; this type
    will generally be polymorphic to express our limited knowledge
    about the type we're aiming for (if it's monomorphic, we can just
    return known_so_far!) So, for instance, if we know we're trying to
    form a list of something, then known_so_far would be an HsType
    representing "[a]", and we have to inspect py_obj to decide what a
    should be then return a HsType telling us what the type of the
    list should be; say "[Integer]" if py_obj is [1, 2, 3].

    known_so_far being None is equivalent to it being HsType('a') for
    some type variable 'a' (i.e. a Haskell type that conveys no
    information whatsoever).

    [Incidentally, the reason that a hint is supplied rather than just
    providing py_obj is the fact that a given python object might
    support conversion to many types; for instance the default
    rendering of the python tuple (1, 2) into Haskell is as a Haskell
    tuple, but it could also convert to a list if necessary; and we'd
    like to make sure that if it's used in a context where a Haskell
    list is needed, things Just Work.

    For example:
    >>> str(pyobj_hstype_hint((1, 2), None))
    '<hyphen.HsType object representing (GHC.Integer.Type.Integer, GHC.Integer.Type.Integer)>'
    >>> str(pyobj_hstype_hint((1, 2), hs_List(hslowlevel.HsType('a'))))
    '<hyphen.HsType object representing [GHC.Integer.Type.Integer]>'

    ]

    Sometimes, we cannot fully guess the type to which py_obj can best
    be converted but we can provide some information (e.g. we might
    know it's a function but not what the return type should be). In
    this case, we return a polymorphic type which is 'less
    polymorphic' than known_so_far but which is still not monomorphic.

    In doing this, we may need to insert some type variables. fv_src
    should be an iterator which yields a sequence of distinct strings
    to use as type variables which are 'safe to use' in whatever
    context we're working. If it's None, we make up free variables
    ourselves, being careful to avoid any type variables already
    mentioned in known_so_far.

    """
    # The basic structure of this function is to call hooks in turn
    # until we find someone who wants to handle this case...

    # Step one: in the totally straightforward case where py_obj is
    # already a Haskell object, return its type.
    if isinstance(py_obj, hslowlevel.HsObjRaw):
        return py_obj.hstype

    # Step two: handle fv_src/known_so_far being None
    if known_so_far is None:
        if fv_src is None:
            fv_src = iter(enumerate_fresh_vars_outside([]))
        known_so_far = next(fv_src)
    elif fv_src is None:
        fv_src = iter(enumerate_fresh_vars_outside(known_so_far.fvs))
    else:
        fv_src = iter(fv_src)

    # Step three: allow objects to control their own conversion (take
    # precedence over hooks)...
    if hasattr(py_obj, 'hstype'):
        return py_obj.hstype
    if hasattr(py_obj, 'hstype_fn'):
        return py_obj.hstype_fn(known_so_far, fv_src)

    # Step four: go though the hooks in turn.
    for base in (type(py_obj),) + type(py_obj).__bases__:
        if base in pyobj_hstype_hint_per_type_hooks:
            return pyobj_hstype_hint_per_type_hooks[base](py_obj, known_so_far, fv_src)
    for hook in pyobj_hstype_hint_general_hooks:
        result = hook(py_obj, known_so_far, fv_src)
        if result is not None:
            return result
    return known_so_far # 'maximally vague'

