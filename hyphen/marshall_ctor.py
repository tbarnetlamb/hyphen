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

marshall_cache = {}

# need to process lists as a special case

seen_visible_modules = {}

def get_seen_visible_module(tyc):
    if tyc in seen_visible_modules:
        return seen_visible_modules[tyc]
    else:
        vm = tyc.visible_module
        if vm is not None:
            seen_visible_modules[tyc] = vm
        return vm

def process_constructors_from_module(module_name):
    data_ns, tycon_ns = fetch_lib_module(module_name)
    tycons_defined_here = {}
    if module_name == 'GHC.Types':
        tycon_ns['[]'] = hs_List
    for tycon_name, tycon in tycon_ns.items():
        if not isinstance(tycon, hyphen.hslowlevel.TyCon) or (
                get_seen_visible_module(tycon) != module_name):
            continue
        assert tycon.name == tycon_name
        tycons_defined_here[tycon] = ({}, {})
    for data_name, data in data_ns.items():
        if data_name.startswith("*co-"):
            assert data_name[4:] in data_ns
            continue
        if data_name[:1].isupper() or data_name[:1] == ':':
            tycon  = datacon_tycon(data)
            codata = data_ns["*co-" + data_name]
            assert codata.hstype.head.name == '(->)' and codata.hstype.tail[0].head == tycon
            if tycon in tycons_defined_here:
                tycons_defined_here[tycon][0][data_name] = (data, codata)
        elif data_name[:1] != '_' and not isinstance(data.hstype.head, str) and (
                data.hstype.head.name == '(->)'):
            tycon = first_arg_type(data.hstype).head
            if tycon in tycons_defined_here:
                tycons_defined_here[tycon][1][data_name] = data
    for tyc, (dacons, meths) in tycons_defined_here.items():
        process_constructor(tyc, dacons, meths)

def marshall_tycon(tc, none_acceptable=True):
    if tc not in marshall_cache:
        vm = get_seen_visible_module(tc)
        if vm is None:
            assert none_acceptable
            return None
        process_constructors_from_module(vm)
    return marshall_cache[tc]

def get_marshalled_dacon(tc, dacon_name, none_acceptable=True):
    if tc not in marshall_cache:
        vm = get_seen_visible_module(tc)
        if vm is None:
            assert none_acceptable
            return None
        process_constructors_from_module(vm)
    return marshall_cache[(tc, dacon_name)]

def applyFromPyArgs(fn, *args):
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
    if kwargs:
        raise TypeError("Cannot provide keyword arguments when calling Haskell fns")
    return hyphen.marshall_obj_to_py.hs_to_py(applyFromPyArgs(*full_args))

def doio_marshalled(io_action, *args, **kwargs):
    if args or kwargs:
        raise TypeError("Cannot provide arguments when calling Haskell IO actions")
    return hyphen.marshall_obj_to_py.hs_to_py(hslowlevel.doio(io_action))

class HsIterator(object):
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

# although iterate_hslist(foo) is just the same as HsIterator(foo),
# it's useful to define since iterate_hslist (being a function) has
# the magic __get__ that turns it into a bound member when pulled out
# of a class via (class instance).foo; HsIterator does not.
def iterate_hslist(the_list):
    return HsIterator(the_list)

def iterate_hsmap(the_map):
    return HsIterator(hslowlevel.apply(hs_mapKeys,  the_map))

def iterate_hsset(the_set):
    return HsIterator(hslowlevel.apply(hs_setElems, the_set))

def map_lookup(map_obj, key):
    result   = applyFromPyArgs(hs_mapLookup, key, map_obj)
    if hslowlevel.from_haskell_Bool(hslowlevel.apply(hs_isJust, result)):
        return marshall_obj_to_py.hs_to_py(hslowlevel.apply(hs_fromJust, result))
    else:
        raise KeyError(key)

def set_member(set_obj, key):
    return hslowlevel.from_haskell_Bool(applyFromPyArgs(hs_setMember, key, set_obj))

def member_from_unary_hsfn(to_apply):
    def fn_with_placeholder_name(self):
        return marshall_obj_to_py.hs_to_py(hslowlevel.apply(to_apply, self))
    return fn_with_placeholder_name

def member_from_hsfn(to_apply):
    def fn_with_placeholder_name(self, *args):
        return apply_marshalled(to_apply, self, *args)
    return fn_with_placeholder_name

def tuple_getitem(self, idx):
    return self._components[idx]

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
    }

tycon_specials.update(dict([(tup_tyc, {
    "__getitem__" : tuple_getitem,
    "__len__"     : const_fn(tup_len),
    "BASES"       : (collections.Sequence,),
    }) for tup_len, tup_tyc in enumerate(hs_tupletycs_bylength)]))

dacon_specials = {}

tycon_hooks = []

dacon_hooks = []

def replace_placeholder_names(d):
    for key in d:
        if (hasattr(d[key], '__name__') and d[key].__name__ == 'fn_with_placeholder_name'
            and isinstance(key, str)):
            d[key].__name__ = key

def process_tycon(tycon, methods_by_name):
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

def make_component_fetcher(co_dacon):
    @property
    def _components(self):
        try:
            return self._components_store
        except AttributeError:
            self._components_store = break_haskell_tuple(
                hslowlevel.apply(hs_head, hslowlevel.apply(
                    co_dacon, self)))
            return self._components_store
    return _components

def process_dacon(dacon, co_dacon, dacon_name, tycon_class):
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

def make_interpreter(ways_to_interpret):
    @classmethod
    def interpret(cls, obj):
        if len(obj.hstype.fvs) == 0:
            for co_dacon, resulting_class in ways_to_interpret:
                if not hslowlevel.from_haskell_Bool(
                        hslowlevel.apply(
                            hs_null, hslowlevel.apply(co_dacon, obj))):
                    return hslowlevel.HsObjRaw.__new__(resulting_class, obj)
        return hslowlevel.HsObjRaw.__new__(cls, obj)
    return interpret

def process_constructor(tycon, datacons_by_name, methods_by_name):
    tycon_class = marshall_cache[tycon] = process_tycon(tycon, methods_by_name)
    ways_to_interpret      = []
    for dacon_name, (dacon, co_dacon) in datacons_by_name.items():
        dcls = marshall_cache[(tycon, dacon_name)] = process_dacon(
            dacon, co_dacon, dacon_name, tycon_class)
        ways_to_interpret.append((co_dacon, dcls))
    tycon_class.interpret = make_interpreter(ways_to_interpret)
    tycon_class.all_constructors_known = [i[1] for i in ways_to_interpret]
