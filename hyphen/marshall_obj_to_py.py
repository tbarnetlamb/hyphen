from __future__           import absolute_import

from hyphen        import hslowlevel
from hyphen.utils  import hs_Complex, hs_imag, hs_real
import hyphen

def from_haskell_Complex(h_part_converter):
    def closure(obj):
        hs_re = hslowlevel.apply(hs_real, obj)
        hs_im = hslowlevel.apply(hs_imag, obj)
        return complex(h_part_converter(hs_re), h_part_converter(hs_im))
    return closure

per_type_hooks = {
    hslowlevel.hstype_Bool       : hslowlevel.from_haskell_Bool,
    hslowlevel.hstype_Char       : hslowlevel.from_haskell_Char,
    hslowlevel.hstype_String     : hslowlevel.from_haskell_String,
    hslowlevel.hstype_Text       : hslowlevel.from_haskell_Text,
    hslowlevel.hstype_ByteString : hslowlevel.from_haskell_ByteString,
    hslowlevel.hstype_Int        : hslowlevel.from_haskell_Int,
    hslowlevel.hstype_Integer    : hslowlevel.from_haskell_Integer,
    hslowlevel.hstype_Float      : hslowlevel.from_haskell_Float,
    hslowlevel.hstype_Double     : hslowlevel.from_haskell_Double,
    hs_Complex(hslowlevel.hstype_Float)
             : from_haskell_Complex(hslowlevel.from_haskell_Float),
    hs_Complex(hslowlevel.hstype_Double)
             : from_haskell_Complex(hslowlevel.from_haskell_Double),
}

per_tycon_hooks = {
}

general_hooks = []

def hs_to_py(obj):
    hstype = obj.hstype
    if hstype in per_type_hooks:
        return per_type_hooks[hstype](obj)
    tycon  = hstype.head
    if isinstance(tycon, str):
        return hslowlevel.HsObjRaw.__new__(hyphen.HsObj, obj)
    if isinstance(tycon, hslowlevel.TyCon) and tycon in per_tycon_hooks:
        return per_tycon_hooks[tycon](obj)
    for hook in general_hooks:
        (processed, result) = hook(obj)
        if processed:
            return result
    tyc_class = hyphen.marshall_ctor.marshall_tycon(tycon)
    if tyc_class is None:
        return hslowlevel.HsObjRaw.__new__(HsObj, obj)
    else:
        return tyc_class.interpret(obj)

