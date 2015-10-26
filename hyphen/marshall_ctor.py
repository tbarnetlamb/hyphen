"""Haskell *type constructors* and *data constructors* that the user
imports to python are transformed into python <type> objects. If we
create types for several data constructors (X, Y and Z, say) for a
given type constructor (T, say), then the corresponding python <type>
objects that we create will be related by inheritance; X Y and Z will
derive from T. This allows us to do isinstance(foo, T) or
isinstance(foo, X) and get sensible answers. Continuing the example,
the Python type created to represent T will itself derive from HsObj
(defined in hsobj.py) which gives a bunch of basic-level behaviors
which we want to be common to all Python objects which wrap Haskell
objects.

(Even if the Haskell type constructor has parameters, we create a
single python type object for all Haskell types associated to the
constructor. So, for instance, we create a single python type
hs.Data.Map to represent all the Haskell types Map a b for any a and
b.)

We generally want the Python objects we build to represent Haskell
objects to be 'friendly'; we want (for instance) Set a to support
haskell 'A in B' notation, Map a b to support substripting, and
Haskell functions to give rise to callable python objects. As is
idiomatic for python, we generally achieve such behaviours by
including suitable functionality in the corresponding python
class. So, since this code generates the relevant Python type objects,
the work to put the right behaviors into them happens here. As is
usual for hyphen, we do this via a series of hooks, so users can
extend the built-in handing if they so desire. See the fifth section
below.

"""

from __future__           import absolute_import

import collections

import hyphen
from hyphen.caches import fetch_lib_module
from hyphen        import hslowlevel
from hyphen        import marshall_obj_to_py
from hyphen        import marshall_obj_to_hs
from hyphen.hsobj  import HsObj
from hyphen.utils  import (
    hs_Maybe, hs_List, hs_Set, hs_Map, hs_IO, hs_Func,
    hs_setElems, hs_sizeOfSet, hs_setMember, hs_mapKeys, hs_mapPairs, hs_sizeOfMap,
    hs_mapLookup, hs_isJust, hs_fromJust, hs_emptyList, hs_null, hs_head, hs_tail,
    break_hs_fn_type, make_hs_fn_type, first_arg_type, datacon_tycon, const_fn,
    hs_tupletycs_bylength, break_haskell_tuple, enumerate_fresh_vars_outside,
    hs_compose, hs_not)

#########################################################################
## The first part of the file defines two caches that are important in
## everything else we do.


# As we create python type objects which correspond to Haskell type
# constructors and data constructors, we cache them; we do this both
# so that each Haskell tycon or dacon has only one python type
# corresponding to it, and also for efficiency. The cache is stored in
# the following dictionary. The type corresponding to a given haskell
# tycon is marshall_cache[tycon], where 'tycon' is the Haskell Type
# Constructor object exposed by the low level Haskell bridge (such
# objects are hashable). The python type corresponding to a Haskell
# data constructor is marshall_cache[(corresponding_tycon,
# dacon_name)], where corresponding_tycon is the tycon to which the
# data-constructor belongs, and dacon_name is a string giving the name
# of the data constructor. (We can't use the low-level haskell bridge
# obejct correesponding to the data constructor, because that would be
# an HsObjRaw, which is necessarily not hashable.)
marshall_cache = {}

# See get_seen_visible_module for the purpose of this variable.
seen_visible_modules = {}

def get_seen_visible_module(tyc):
    """It is useful to know, for a given type constructor, a (Haskell)
    module in which that type constructor is exported. The low-level
    Haskell/Python bridge uses 'TyCon' objects on the Python side to
    represent type constructors; there can be several different
    objects representing the same Haskell type constructor; in this
    case, the different TyCon objects will be _equal_ but not
    _identical_. One can ask a type constructor whether it knows of a
    module which exports it; if one does this (here is the *key
    point*) equal but non-identical TyCons can give different
    answers. We want to have a 'sticky' property that once we've seen
    a module exporting a given TyCon, we always use that module. So we
    have a dictionary mapping TyCons to their 'canonical exporting
    module'. TyCons not in the dictionary are ones for which we have
    not yet seen an exporting module. (Note that the hash of a TyCon
    depends only of the TyCon-up-to-equality, not on the identity.)

    This function looks up the tycon tyc in the dictionary (which is
    called seen_visible_modules); if we find something, we return it;
    otherwise we see if tyc itself knows a good exporting module; if
    it does, we record it and return it. Otherwise, we return None.

    """
    if tyc in seen_visible_modules:
        return seen_visible_modules[tyc]
    else:
        vm = tyc.visible_module
        if vm is not None:
            seen_visible_modules[tyc] = vm
        return vm

#########################################################################
## The second part of the file defines marshall_tycon and
## get_marshalled_dacon; these are the main functions used by external
## clients of this module. They provide access to the computed python
## type obejcts corresponding to Haskell type constructors and data
## contstructors. In fact, they do very little work themselves. The
## type objects that we construct are always constructed by examining
## an entire Haskell module and converting *all* the types and data
## constructors it exports. Therefore, these functions do little more
## that (a) check if we already have a cached python type object
## available to satisfy the query (and if so return it); and (b) if no
## cached result is available, find a module that contains the type
## constructor in question, and then invoke a conversion of all the
## data/type constructors in the module. This inserts the relevant
## type constructors into the cache, and we can finally return the
## cached value.
##
## (The reason that we must convert whole modules at a time is
## essentially that to convert a data constructor we must know its
## associated type constructor and to convert a type constructor we
## must know *all* its assosciated data constructors. To find the type
## constructor from the data constructor is easy, but to find all the
## data constructors that go with a type constructor is difficult;
## there isn't a sure fire way. The beast we can do is look up all the
## data constructors defined in the 'official' module which introduces
## the type constructor, and filter for all that are data constructors
## for the type constructor in question.)

def marshall_tycon(tc, none_acceptable=True):
    """Marshall the TyCon object ty. TyCon objects are used by the low
    level Haskell/Python bridge to represent Haskell Type
    Constructors. Return a Python type object that will be used by the
    high-level Haskell/Python bridge to represent the same type
    constructor. We can only do this if we know a module from which
    the given type constructor is exported (see comment above), either
    becase tc knows such a module or we have cached one already (see
    get_seen_visible_module above for more information on this). We
    return the corresponding type object, or None if we can't because
    we don't know a module where it's visible. (If
    none_acceptable=False, we assert False instead of returning None.)

    """
    if tc not in marshall_cache:
        vm = get_seen_visible_module(tc)
        if vm is None:
            assert none_acceptable
            return None
        process_constructors_from_module(vm) # After we've done this,
                                             # it will be in the cache
    return marshall_cache[tc]

def get_marshalled_dacon(tc, dacon_name, none_acceptable=True):
    """Get the Python type object used to represent a given Haskell data
    constructor, namely the one with name given by the string
    'dacon_name' which is assosciated to the type constructor with
    TyCon object tc.

    [TyCon objects are used by the low level Haskell/Python bridge to
    represent Haskell Type Constructors. We look up this way, rather
    than with the low-level bridge's representation of the Data
    Constructor itself, because the representation of the Data
    Constructor will be a HsObjRaw that doesn't actually give enough
    information to build the python type object we want.]

    Note we can only return/create the python type object if we know a
    module from which the given type constructor is exported, either
    becase tc knows such a module or we have cached one already (see
    get_seen_visible_module above for more information on this). We
    return the corresponding type object, or None if we can't because
    we don't know a module where it's visible. (If
    none_acceptable=False, we assert False instead of returning None.)

    """
    if tc not in marshall_cache:
        vm = get_seen_visible_module(tc)
        if vm is None:
            assert none_acceptable
            return None
        process_constructors_from_module(vm)
    return marshall_cache[(tc, dacon_name)]

#########################################################################
## The third section of the file defines various utility functions
## that will be useful in setting up the features that we will add to
## various 'special' types that we create.

def applyFromPyArgs(fn, *args):
    """Given
    """
    fn_type                = fn.hstype
    arg_types, return_type = break_hs_fn_type(fn_type, len(args))
    if len(arg_types) < len(args):
        raise TypeError(
            "Haskell object %s has been applied to %d arguments, but its type %s "
            "supports at most %d." % (fn, len(args), fn.hstype, len(arg_types)))
    assert len(arg_types) == len(args)
    fv_src = enumerate_fresh_vars_outside(fn_type.fvs)
    refined_arg_types = tuple([
        marshall_obj_to_hs.pyobj_hstype_hint(args[i], arg_types[i], fv_src)
        for i in range(len(args))])

    # This narrowing operation will allow type information learned
    # from one variable to be propagated to other variables and used
    # to help guide their transformation from python to haskell
    fn_               = fn.narrow_type(make_hs_fn_type(refined_arg_types, return_type))
    arg_types_, _     = break_hs_fn_type(fn_.hstype, len(args))

    return hslowlevel.apply(fn, *[
        marshall_obj_to_hs.py_to_hs(args[i], arg_types_[i]) for i in range(len(args))])

def apply_marshalled(*full_args, **kwargs):
    """Same as applyFromPyArgs above, except that additionally we take the
    low-level HsObjRaw returned by the function call and marshall it
    back into a nice python object. (If the returned object is a basic
    type like a float, this will involve replacing the HsObjRaw with a
    python float; otherwise, we replace it with a nice HsObj which is
    derived from a nice type of the kind made by this module).

    (We also accept kwargs, but only so that we can issue a nice error
    message if they are provided; Haskell functions don't accept
    kwargs.)
    """
    if kwargs:
        raise TypeError("Cannot provide keyword arguments when calling Haskell fns")
    return hyphen.marshall_obj_to_py.hs_to_py(applyFromPyArgs(*full_args))

def doio_marshalled(io_action, *args, **kwargs):
    """Perform the Haskell IO action io_action using the low-level
    Haskell/Python bridge, then take the low-level HsObjRaw returned
    by the IO action and marshall it back into a nice python
    object. (If the returned object is a basic type like a float, this
    will involve replacing the HsObjRaw with a python float;
    otherwise, we replace it with a nice HsObj which is derived from a
    nice type of the kind made by this module).

    We accept args and kwargs, but only so we can give an informative
    error message if any is provided. (Pure IO actions don't take
    parameters.)

    """
    if args or kwargs:
        raise TypeError("Cannot provide arguments when calling Haskell IO actions")
    return hyphen.marshall_obj_to_py.hs_to_py(hslowlevel.doio(io_action))

class HsIterator(object):
    """Given an HsObjRaw which represents a Haskell list (of the basic
    list type built into the language), it is useful to be able to
    create a python-iterator-protocol-compliant iterator which
    iterates through the list. This is a class of such iterators. By
    applying to appropriately-constructed lists, this also allows us
    to iterate through Haskell Maps and so on.

    Each object returned from the iterator is marshalled to a python
    object as per the marshalling protocol in
    hyphen.marshall_obj_to_py.
    """
    __slots__ = ['cur']
    def __init__(self, hslist):
        self.cur = hslist

    def __next__(self):
        if hslowlevel.from_haskell_Bool(hslowlevel.apply(hs_null, self.cur)):
            raise StopIteration
        else:
            (this, self.cur) = (
                hslowlevel.apply(hs_head, self.cur), hslowlevel.apply(hs_tail, self.cur))
            return hyphen.marshall_obj_to_py.hs_to_py(this)

    def __iter__(self):
        return self

#########################################################################
## The fourth section of the file defines various functions that will
## end up as class member functions for the python classes that we
## will create to represent various haskell types.

# although iterate_hslist(foo) is just the same as HsIterator(foo),
# it's useful to define since iterate_hslist (being a function) has
# the magic __get__ that turns it into a bound member when pulled out
# of a class via (class instance).foo; HsIterator does not.
def iterate_hslist(the_list):
    """Given a HsObjRaw which represents a Haskell list, compute a
    python-iterator-protocol-compliant iterator which iterates through
    the list."""
    return HsIterator(the_list)

def iterate_hsmap(the_map):
    """Given a HsObjRaw which represents a Haskell map, compute a
    python-iterator-protocol-compliant iterator which iterates through
    the keys of the map (as happens when you iterate through a python
    dict)."""
    return HsIterator(hslowlevel.apply(hs_mapKeys,  the_map))

def iterate_hsset(the_set):
    """Given a HsObjRaw which represents a Haskell set, compute a
    python-iterator-protocol-compliant iterator which iterates through
    the set."""
    return HsIterator(hslowlevel.apply(hs_setElems, the_set))

def map_lookup(map_obj, key):
    """Given a Haskell Map object and a key, return the associated value
    if there is one; otherwise raise KeyError."""
    result   = applyFromPyArgs(hs_mapLookup, key, map_obj)
    if hslowlevel.from_haskell_Bool(hslowlevel.apply(hs_isJust, result)):
        return marshall_obj_to_py.hs_to_py(hslowlevel.apply(hs_fromJust, result))
    else:
        raise KeyError(key)

def set_member(set_obj, key):
    """Given a Haskell Set object and a key, return whether the key is in
    the set. """
    return hslowlevel.from_haskell_Bool(applyFromPyArgs(hs_setMember, key, set_obj))

def member_from_unary_hsfn(to_apply):
    """Given a HsObjRaw whose type looks like (X -> Y), return a function
    that can be put in the class we build for the type X which will
    invoke the function and return Y, appropriately marshalled to
    Python."""
    def fn_with_placeholder_name(self):
        return marshall_obj_to_py.hs_to_py(hslowlevel.apply(to_apply, self))
    return fn_with_placeholder_name

def member_from_hsfn(to_apply):
    """Same as member_from_unary_hsfn but the member function we create
    will accept further parameters and pass them on."""
    def fn_with_placeholder_name(self, *args):
        return apply_marshalled(to_apply, self, *args)
    return fn_with_placeholder_name

def tuple_getitem(self, idx):
    """Helper function that can be inserted into the class that will
    represent Haskell tuples in python, to get their ith element.."""
    return self._components[idx]

def make_interpreter(ways_to_interpret):
    """The python types that we make corresponding to Haskell type
    constructors all have an _interpret classmethod. This takes a
    HsObjRaw (which ought to have a Haskell type whose head matches
    the Type Constructor in question) and returns a nice python object
    of 'the best type possible' that we can use to represent the same
    Haskell object. What 'the best type possible' means is that if we
    recognize the data constructor used to build the object, we return
    a python object whose python type is the type we built to
    represent the data constructor. We might *not* recognize the
    data-constructor (it might not have been exported); in this case,
    we return a python object whose python type is the type we built
    to represent the type constructor in question.

    This function builds the _interpret classmethod. It takes a single
    parameter, ways_to_interpret. This should be a list of pairs
    (co_dacon, resulting_class). co_dacon should be a "co-data
    constructor" as returned by the low-level Haskell/Python bridge;
    see the low-evel doucmentation for details. (They allow us to
    recognize and unpack objects that were built using a particular
    data constructor.) resulting_class should be a python type object
    representing the data constructor corresponding to co_dacon.

    """
    @classmethod
    def interpret(cls, obj):
        assert obj.hstype.head_ll == cls.hs_tycon
        if len(obj.hstype.fvs) == 0:
            for co_dacon, resulting_class in ways_to_interpret:
                if not hslowlevel.from_haskell_Bool(
                        hslowlevel.apply(
                            hs_null, hslowlevel.apply(co_dacon, obj))):
                    return hslowlevel.HsObjRaw.__new__(resulting_class, obj)
        return hslowlevel.HsObjRaw.__new__(cls, obj)
    return interpret

def make_component_fetcher(co_dacon):
    """The python types that we create corresponding to Haskell data
    constructors all have a _components method. This unpacks the
    Haskell object represented by the Python object and returns a
    python tuple whose elements are the arguments provided to the
    constructor when the Haskell object was built (in order).

    This function builds such a _components method. It takes as an
    argument a "co-data constructor" as returned by the low-level
    Haskell/Python bridge corresponding to the data constructor in
    question; see the low-evel doucmentation for details.
    """
    @property
    def _components(self):
        try:
            # We memoize the result in _components_store for speed efficiency
            return self._components_store
        except AttributeError:
            self._components_store = break_haskell_tuple(
                hslowlevel.apply(hs_head, hslowlevel.apply(
                    co_dacon, self)))
            return self._components_store
    return _components

#########################################################################
## The fifth section contains the dictionaries and lists full of hooks
## which the system will use to decide how to build the types we're
## going to build.

## The first kind of hook affects the type we build for a
## tycon. Recall that the Tycon object is the low-level bridge's
## representation of Haskell type constructors. The hook repository is
## a dictionary mapping TyCons to /customization dictionaries/. The
## customization dictionary consists of a series of modifications we
## will make to the type as we build it. Most of these are just (key,
## value) pairs that we want to insert into the class's dictioary;
## there are a couple of specials. First, if we have a key-value pair
## ("NAME", foo) then we set the name of the class to be foo. Second,
## if we have a key-value pair ("BASES", foo) then we use foo as the
## bases of the new class (it should be a tuple).

tycon_specials = {
    hs_List : {
        "__iter__"    : iterate_hslist,
        "__bool__"    : member_from_unary_hsfn(hslowlevel.apply(
            hs_compose, hs_not, hs_null))
        },
    hs_Map : {
        "__getitem__" : map_lookup,
        "__iter__"    : iterate_hsmap,
        "__len__"     : member_from_unary_hsfn(hs_sizeOfMap),
        "BASES"       : (collections.Mapping,),
        },
    hs_Set : {
        "__contains__" : set_member,
        "__iter__"     : iterate_hsset,
        "__len__"      : member_from_unary_hsfn(hs_sizeOfSet),
        "BASES"        : (collections.Set,),
        },
    hs_Func : {
        "__call__"    : apply_marshalled,
        "NAME"        : "HsFunObj",
        },
    hs_IO : {
        "__call__"    : doio_marshalled,
        "NAME"        : "HsFunObj",
        },
    hs_Maybe : {
        "__bool__"    : member_from_unary_hsfn(hs_isJust),
        }
    } #Note this dict is modified by statement immediately below.
tycon_specials.update(dict([(tup_tyc, {
    "__getitem__" : tuple_getitem,
    "__len__"     : const_fn(tup_len),
    "BASES"       : (collections.Sequence,),
    }) for tup_len, tup_tyc in enumerate(hs_tupletycs_bylength)]))

# The second kind of hook affects the types we build to represent
# Haskell data constructors. It works the same as the previous
# dictionary, except that the key is a pair (tc, name) where tc is the
# TyCon associated to the type constructor associated to the data
# constructor in question, and name is a string giving the name of the
# data constructor.

dacon_specials = {}

# The third kind of hook affects the types we build to represent type
# constructors. It consists of a list of functions; each should take a
# pair (tycon, customizations_so_far) where tycon will be the Tycon
# currently being processed and customizations_so_far is what it
# sounds like; the hook can modify customizations_so_far however it
# sees fit.
tycon_hooks = []

# The fourth kind of hook affects the types we build to represent
# Haskell data constructors. It works the same as the previous list,
# except that functions will be called with parameters (tycon,
# dacon_name, dacon_itself, customizations_so_far) where tycon is the
# TyCon associated to the type constructor associated to the data
# constructor in question, dacon_name is a string giving the name of
# the data constructor, and dacon is an HsObjRaw representing the
# dacon itself.
dacon_hooks = []

#########################################################################
## The fifth section contains the actual code for building the types
## we want.

def replace_placeholder_names(d):
    """Utility function. Often we put members into the classes we build
    that are closures returned by functions in the third section
    above. Generally, we would rather the __name__ of the function we
    insert match the key under which it is inserted (e.g. if we are
    making a function which will serve as the __iter__ of a type, we
    would like it to have __name__=='__iter'). We achieve this via the
    following expedient. The closures all return functions with
    __name__='fn_with_placeholder_name'; then this function goes
    through the dictionary of things that we're going to insert into
    the class dictionary, and repalces the __name__s appropriately.
    """
    for key in d:
        if (hasattr(d[key], '__name__') and d[key].__name__ == 'fn_with_placeholder_name'
            and isinstance(key, str)):
            d[key].__name__ = key

def process_tycon(tycon, methods_by_name):
    """Build a Python class to represent a Haskell tycon. Parameter
    'tycon' should by a TyCon object (the kind of object used by the
    low-level Haskell/Python bridge to represent Type Constructors,
    representing the type constructor in question. Parameter
    methods_by_name should be a dictionary mapping strings to HsObjRaw
    objects giving members that we would like the newly-created
    function to have that were pulled over from Haskell. (Recall that
    if the haskell module M defining a type T has a function in it foo
    : T -> X -> Y -> Z, we let that give rise to a foo member on the
    python objects representing haskell objects of that type T.) """
    customizations = {'__slots__'  : [],
                      '__module__' : 'hs.' + tycon.module}
    for name, meth_fn in methods_by_name.items():
        customizations[name] = member_from_hsfn(meth_fn)
    if tycon in tycon_specials:
        customizations.update(tycon_specials[tycon])
    for hook in tycon_hooks:
        hook(tycon, customizations)
    name  = customizations.pop('NAME', tycon.name)
    bases = (HsObj,) + customizations.pop('BASES', ())
    replace_placeholder_names(customizations)
    customizations['hs_dacon'] = "BLANK_IN_TYCONS"
    customizations['hs_tycon'] = tycon
    return type(name, bases, customizations)

def process_dacon(dacon, co_dacon, dacon_name, tycon_class):
    """Build a Python class to represent a Haskell data
    constructor. Parameter dacon should by a HsObjRaw that represents
    the data constructor itself (i.e. a function from the data
    constructor arguments to constructed objects). Parameter co_dacon
    should be a "co-data constructor" as returned by the low-level
    Haskell/Python bridge corresponding to the data constructor in
    question; see the low-evel doucmentation for details. (This allows
    us to recognize and unpack objects that were built using the data
    constructor in question.) dacon_name should be a string giving the
    name of the data constructor; tycon_class is the python type
    corresponding to the type constructor associated to the data
    constructor.

    """
    customizations = {'__slots__'      : ['_components_store'],
                      '_components'    : make_component_fetcher(co_dacon),
                      '__module__'     : 'hs.' + tycon_class.hs_tycon.module}
    if (tycon_class.hs_tycon, dacon_name) in dacon_specials:
        customizations.update(dacon_specials[dacon])
    for hook in dacon_hooks:
        hook(tycon_class.hs_tycon, dacon_name, dacon, customizations)
    name  = customizations.pop('NAME', dacon_name)
    bases = (tycon_class,) + customizations.pop('BASES', ())
    customizations['hs_dacon'] = dacon
    replace_placeholder_names(customizations)
    return type(name, bases, customizations)

def process_constructor(tycon, datacons_by_name, methods_by_name):
    """Process a type constructor and all its data constructors, building
    appropriate python type objects to represent each. Store the
    resulting types in the caches. Parameter tycon should be the TyCon
    object used by the low-level bridge to represent the type
    constructor. Parameter datacons_by_name should be a dictionary
    mapping strings (names of data constructors) to pairs (data,
    co_data), where data is an HsObjRaw object representing the data
    constructor and co_data is an HsObjRaw object representing the
    co-data constructor (see the low-level Haskell-Python bridge for
    more information on these). methods_by_name should be a dictionary
    giving methods that we want the python objects to get that we
    pulled out of haskell. (Recall that if the haskell module M
    defining a type T has a function in it foo : T -> X -> Y -> Z, we
    let that give rise to a foo member on the python objects
    representing haskell objects of that type T.) The dictionary
    should map strings (names of methods) to HsObjRaw objects giving
    the Haskell functions that we want to use as methods.

    """
    tycon_class = marshall_cache[tycon] = process_tycon(tycon, methods_by_name)
    ways_to_interpret      = []
    for dacon_name, (dacon, co_dacon) in datacons_by_name.items():
        dcls = marshall_cache[(tycon, dacon_name)] = process_dacon(
            dacon, co_dacon, dacon_name, tycon_class)
        ways_to_interpret.append((co_dacon, dcls))
    tycon_class.interpret = make_interpreter(ways_to_interpret)
    tycon_class.all_constructors_known = [i[1] for i in ways_to_interpret]

def process_constructors_from_module(module_name):
    """Process all the type constructors and all data constructors from a
    given Haskell module named, building appropriate python type
    objects to represent each. Store the resulting types in the
    caches. module_name should be a string giving the module name.
    """
    data_ns, tycon_ns = fetch_lib_module(module_name)
    tycons_defined_here = {}
    # tycons_defined_here is a dictionary mappying Type constructors
    # to a pair of dictionaries. The first maps strings (names of data
    # constructors) to pairs (dacon, co_dacon) where dacon is an
    # HsObjRaw representing the data constructor and co-dacon is an
    # HsObjRaw representing the co-data-constructor. The second maps
    # strings (names of methods pulled over from Haskell) to HsObjRaws
    # representing the methods.

    if module_name == 'GHC.Types':
        tycon_ns['[]'] = hs_List # built in to language, but we want
                                 # to expose it here
    for tycon_name, tycon in tycon_ns.items():
        if not isinstance(tycon, hyphen.hslowlevel.TyCon) or (
                get_seen_visible_module(tycon) != module_name):
            continue
        assert tycon.name == tycon_name
        tycons_defined_here[tycon] = ({}, {})
    for data_name, data in data_ns.items():
        if data_name.startswith("*co-"):
            # This is a co-data-constructor; we'll process it when we
            # process the corresponding data constructor.
            assert data_name[4:] in data_ns
            continue
        if data_name[:1].isupper() or data_name[:1] == ':':
            # Data constructor
            tycon  = datacon_tycon(data)
            codata = data_ns["*co-" + data_name]
            assert codata.hstype.head_ll.name == '(->)' and (
                codata.hstype.tail[0].head_ll == tycon)
            if tycon in tycons_defined_here:
                tycons_defined_here[tycon][0][data_name] = (data, codata)
        elif data_name[:1] != '_' and not isinstance(data.hstype.head_ll, str) and (
                data.hstype.head_ll.name == '(->)'):
            # This entry in the module's data namespace is a
            # function. If the head type constructor of the first
            # argument is defined in this module, we'll use it to
            # endow the python type corresponding to that type
            # constructor with a method.
            tycon = first_arg_type(data.hstype).head_ll
            if tycon in tycons_defined_here:
                tycons_defined_here[tycon][1][data_name] = data
    for tyc, (dacons, meths) in tycons_defined_here.items():
        process_constructor(tyc, dacons, meths)
