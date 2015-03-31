from __future__        import absolute_import

import sys, types, collections

from hyphen.hslowlevel import ok_python_identif
from hyphen.caches     import fetch_lib_module
from hyphen.utils      import datacon_tycon, hs_List
import hyphen

Doublet = collections.namedtuple('Doublet', ['the_tycon', 'the_dacon'])

def marshall_name(data_namespace_contents, type_namespace_contents):
    if data_namespace_contents is None:
        return type_namespace_contents
    elif type_namespace_contents is None:
        return data_namespace_contents
    if type_namespace_contents.all_constructors_known == [data_namespace_contents]:
        data_namespace_contents.the_type = type_namespace_contents
        return data_namespace_contents
    return Doublet(the_tycon=type_namespace_contents, the_dacon=data_namespace_contents)

def process_data_ns_item(name, item):
    if name[:1].isupper() or name[:1] == ':':
        return hyphen.marshall_ctor.get_marshalled_dacon(datacon_tycon(item), name)
    else:
        return hyphen.marshall_obj_to_py.hs_to_py(item)

def marshall_module(data_namespace, type_namespace, module_to_write):
    data_namespace = dict(
        (k, process_data_ns_item(k, v))             for (k, v) in data_namespace.items())
    type_namespace = dict(
        (k, hyphen.marshall_ctor.marshall_tycon(v)) for (k, v) in type_namespace.items())
    assert module_to_write is not None
    module_to_write._ = {}
    all_names = set(data_namespace) | set(type_namespace)
    for name in all_names:
        image = marshall_name(data_namespace.get(name), type_namespace.get(name))
        if ok_python_identif(name) and name != '_':
            module_to_write.__dict__[name] = image
        module_to_write._[name]            = image

expected_empty = ['Data', 'Control', 'Foreign', 'GHC']

class HaskellFinderLoader():
    def find_module(self, module_name, path):
        if module_name.startswith('hs.'):
            return self
        elif module_name == 'hs':
            return self
        else:
            return False

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
                    data_ns, type_ns = fetch_lib_module(haskell_name)
                except Exception as e:
                    if haskell_name in expected_empty:
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
    sys.meta_path.append(haskell_finder_loader)

def import_haskell_module(haskell_name):
    haskell_finder_loader.load_module('hs.' + haskell_name)
