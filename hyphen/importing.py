"""The purpose of this module is to interact with the python import
system so that, once hyphen itself has been imported and has installed
its hooks, you can write import hs.<blah> and we will import the
appropriate Haskell module for you.

There are also a couple of slight impedance mismatches between Haskell
and Python modules, which have workarounds implemented by this
module. More precisely:

a) Haskell modules have *two* namespaces [1]; a type/type constructor
namespace and a plain object namespace, and it is possible to have the
same name defined in both namespaces (although because of
capitalization rules, the object in this case will necessarily be a
Data constructor). Python has onyl a single namespace, so we must
somehow resolve this contention. (We generally do this by creating a
lightweight 'doublet' object; then you can do MyName.the_tycon or
MyName.the_dacon to specify exactly what you mean. There is one
exception to this, which is when you have a type constructor with only
one data constructor which has the same name as the type
constructor. This is a common pattern in Haskell and it's annoying to
have to do the .the_dacon stuff in this case; and furthermore, little
confusion is saved by forcing the user to spell things out in this
case. So in this case we have MyName refer to the data constructor,
which is probably what you mean, and allow you to write
MyName.the_type to get the type constructor.)

b) Haskell modules can have entries with names like '+++' which are
not legal pyhton identifiers. We still want them to be accessible from
Python, and do this by giving the improted module a special '_'
member, which is a dictionary mapping awkward names like '++' to the
appropriate object.

c) In python, when we import a module a.b.c, we necessarily also
import the packages a and a.b first. In Haskell the existence of a
module Data.Text doesn't necessarily imply the existence of a module
'Data' (and in fact no such module exists). When the user writes
'import hs.Data.Text', python will direct us to import 'hs.Data',
which will cause us to look for the Haskell module 'Data', and if we
proceeded naively we would fail when we found it didn't exist. We
solve this problem in a rudimentary way: we have a list of particular
'non-modules' X such that X is not expected to be a Haskell module
even though X.Something is. When we are told to import hs.X (probably
because the user imported hs.X.Something), we still *attempt* to load
a Haskell module X, but if we don't find one, we don't throw an
error. (This bahavior is controlled by the EXPECTED_EMPTY list below.)

[Even when a module X exists, it's not necessarily the case that every
user who imports X.Foo will want to import X. An egregious case of
this is where X=GHC; we often want to import GHC.<something> (most of
the GHC built-in types live here) but almost never want to import GHC
(which is enormous, and very slow to import, and which contains the
GHC compiler itself as a library). So we also have a list
FORCED_EMPTY; if we are asked to import some X which is on this list,
we always return an empty module rather than trying to import the
actual Haskell module with that name.

[footnote 1] In fact, Haskell officially has 6 namespaces in play:
type constructors, data constructors, type variables, plain objects,
infix operators and infix data constructors. But type variables can
never be exported, so they're out of the picture in terms of what you
might find in a module, and then the only actual *collisions* that are
possible are between type constructors and data constructors because
capitalization distinguishes the constuctors from the plain objects
that aren't data constructors, and because the infixes are
non-alphanumeric (and the two types of infixes are disambiguated based
on whether there's an initial ':'). So for our purposes we think of
there being just two namespaces: type constructors, and 'everything
else'.

"""

from __future__        import absolute_import

import sys, types, collections

from hyphen.hslowlevel import ok_python_identif
from hyphen.caches     import fetch_lib_module
from hyphen.utils      import datacon_tycon, hs_List
import hyphen

## See point (c) in module docstring for discussion of these variables

EXPECTED_EMPTY = ['Data', 'Control', 'Foreign', 'GHC']
FORCED_EMPTY   = [
    'GHC' # takes ages to import this and it's almost never what you want
    ]

## See point (a) in module docstring for why we need this class
Doublet = collections.namedtuple('Doublet', ['the_tycon', 'the_dacon'])

def _marshall_name(data_namespace_contents, type_namespace_contents):
    """See point (a) in module docstring.

    Haskell has several namespaces for each module. For our purposes,
    we group them into a namespace of type constructors and a 'data'
    namespace with everything else. If there some name exists in both
    namespaces, we need to decide what object to put in the Python
    namespace for the module. This function does that.

    The parameter data_namespace_contents (resp
    type_namespace_contents) is a python object that represent
    whatever was in the Haskell data namespace (resp type namespace),
    or None if there was nothing there.

    We return a single python object that can be stored in the
    combined python namespace.

    """
    # First handle the easy case: if the name we're processing was
    # acutally only in one of the two namespaces (i.e. one of the
    # parameters is None).
    if data_namespace_contents is None:
        return type_namespace_contents
    elif type_namespace_contents is None:
        return data_namespace_contents

    # Hard case of name collision. In general, we will return a
    # Doublet, but in one special case (see module docstring) we do
    # something else.
    if getattr(type_namespace_contents, 'all_constructors_known', None) == [
            data_namespace_contents]:
        data_namespace_contents.the_type = type_namespace_contents
        return data_namespace_contents
    return Doublet(the_tycon=type_namespace_contents, the_dacon=data_namespace_contents)

def _process_data_ns_item(name, item):
    ## The low-level importing machinery returns HsObjRaws for each
    ## item in the data namespace. We wish to marshall these into
    ## HsObjs or other nicer representations (or even, for
    ## e.g. Floats, a python-native type). We also wish to replace
    ## data constructors (which are just represented in the hslowlevel
    ## output as pure functions which construct an appropriate object
    ## with the types representing the data constructor generated in
    ## marshall_ctor.
    if name[:1].isupper() or name[:1] == ':': # data constructor?
        return hyphen.marshall_ctor.get_marshalled_dacon(
            datacon_tycon(item), name, none_acceptable=False)
    else:
        return hyphen.marshall_obj_to_py.hs_to_py(item)

def _process_type_ns_item(name, item):
    # The lowlevel importing machinery returns rather low-level
    # objects for iterms in the type namespace. Specifically, it
    # returns either TyCons (for type namespace objects that represent
    # types) or HsTypes (for objects that represent TypeDefs). We wish
    # to replace the former with the python type representing the type
    # constructor generated in marshall_ctor, and the latter with
    # functions which construct appropriate types.
    if isinstance(item, hyphen.hslowlevel.HsType):
        def inner(*args):
            assert len(args) == len(item.fvs)
            subst = dict(("arg" + str(i), arg) for (i, arg) in enumerate(args))
            return item.subst(**subst)
        inner.__name__ = name
        return inner
    else:
        return hyphen.marshall_ctor.marshall_tycon(item, none_acceptable=False)

def marshall_module(data_namespace, type_namespace, module_to_write):
    """Given the low-level contents of a Haskell module as made available
    by hslowlevel, and a (fresh) python module module_to_write that we
    would like to fill in with the contents of the Haskell module,
    this function duly fills it in with those contents, suitably
    marshalled and otherwise converted to friendly Python equivalents.

    data_namespace will be a dictionary from names to HsObjs;
    type_namespace will be a dictionary from names to either TyCons
    (for type namespace objects that represent types) or HsTypes (for
    objects that represent TypeDefs).
    """
    data_namespace = dict(
        (k, _process_data_ns_item(k, v))  for (k, v) in data_namespace.items())
    type_namespace = dict(
        (k, _process_type_ns_item(k, v))  for (k, v) in type_namespace.items())
    assert module_to_write is not None
    module_to_write._ = {}
    all_names = set(data_namespace) | set(type_namespace)
    for name in all_names:
        image = _marshall_name(data_namespace.get(name), type_namespace.get(name))
        if ok_python_identif(name) and name != '_':
            module_to_write.__dict__[name] = image
        module_to_write._[name]            = image


class HaskellFinderLoader():
    """Implement the python finder/loader protocol to allow importing of
    Haskell objects; see PEP302.
    """
    def find_module(self, module_name, path):
        if module_name.startswith('hs.'):
            return self
        elif module_name == 'hs':
            return self
        else:
            return None

    def load_module(self, module_name):
        assert module_name.startswith('hs.') or module_name == 'hs'
        if module_name in sys.modules:
            module_to_write          = sys.modules[module_name]
            erase_on_fail            = False
        else:
            module_to_write          = types.ModuleType(name=module_name)
            sys.modules[module_name] = module_to_write
            erase_on_fail            = True
        try:
            module_to_write.__dict__.pop('__file__',   None)
            module_to_write.__dict__.pop('__cached__', None)
            module_to_write.__path__ = []
            module_to_write.__loader__ = self
            #module_to_write.__path__   = '.'
            if module_name == 'hs':
                data_ns, type_ns = {}, {}
            else:
                haskell_name     = module_name[3:]
                try:
                    if haskell_name in FORCED_EMPTY:
                        assert False
                    data_ns, type_ns = fetch_lib_module(haskell_name)
                except Exception as e:
                    if haskell_name in EXPECTED_EMPTY:
                        data_ns, type_ns = {}, {}
                    else:
                        raise ImportError(
                            "Failed to import haskell module '{}'".format(
                                module_name)) from e
            try:
                marshall_module(data_ns, type_ns, module_to_write)
                if module_name == 'hs.GHC.Types':
                    module_to_write._["[]"] = hs_List
            except Exception as e:
                raise ImportError(
                    "Failed to import haskell module '{}'".format(module_name)) from e
        except:
            if erase_on_fail:
                sys.modules.pop(module_name, None)
            raise

    def module_repr(self, module):
        return "<module '{}' (from Haskell via hyphen)>".format(module.__name__)

haskell_finder_loader = HaskellFinderLoader()

def install_hook():
    """Install the hook so that we can import Haskell modules."""
    sys.meta_path.append(haskell_finder_loader)

def import_haskell_module(haskell_name):
    """Force the importing of a given Haskell module."""
    haskell_finder_loader.load_module('hs.' + haskell_name)
