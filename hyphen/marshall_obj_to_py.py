"""This file handles the automatic marshalling of Haskell objects
(returned by calls into Haskell) back into forms that are convenient
for use from Python. In particular, Haskell objects output by
hslowlevel will be of the low-level HsObjRaw type: we will wish to
either:

a) Convert them to friendly objects of some automatically-generated
python type derived from HsObj. This python type will generally
represent the data constructor used to construct the object in
question. (In some exceptional cases we might instead return something
whose python type represents the type constructor, or return something
whose python type is simply HsObj.)

b) Convert them into a python-native object (e.g. convert an HsObjRaw
wrapping a Haskell Integer to a python int.

The general philosophy in hyphen is that we are liberal in marshalling
python objects to Haskell, but relatively strict in marshalling
objects back; so (a) is the norm and (b) only applies to objects of
some pretty basic types (like ints as mentioned). See the docs for a
justification of this choice!

(This explains why this file is shorter than its opposite number,
marshall_obj_to_hs.)

The actual conversion infrastructure works via a series of hooks, so
users can extend the built-in handing if they so desire.

"""

from __future__           import absolute_import

from hyphen        import hslowlevel
from hyphen.utils  import hs_Complex, hs_imag, hs_real
import hyphen

def from_haskell_Complex(h_part_converter):
    """Return a function for converting Haskell Complex objects to Python
    complex objects, converting the real and imaginary parts via the
    supplied function h_part_converter.
    """
    def closure(obj):
        hs_re = hslowlevel.apply(hs_real, obj)
        hs_im = hslowlevel.apply(hs_imag, obj)
        return complex(h_part_converter(hs_re), h_part_converter(hs_im))
    return closure

## As mentioned above, the conversion infrastructure works via a
## series of hooks. The most specific way of adding a hook is one that
## operates *only* on objects of a particular type. To install such a
## hook, insert into this dictionary; the key is the HsType for which
## we want to install a custom marshall-to-Python behavior; the value
## is a function which takes a HsObjRaw and marshalls it as
## desired. This hook will then *always* be used to marhsall objects
## of the type indicated.

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

## The second-most specific kind of hook that can be installed applies
## to everthing that has a particular TyCon as the head of its
## type. Again, such a hook is installed by writing to the following
## dictionary; the key is, in this case, the TyCon in question.

per_tycon_hooks = {
}

## THe most general kind of hook is simply a function which takes the
## HsObjRaw object we're trying to marshall, and decides if it can
## convert it. If it can, it should return (True, <converted value>);
## if not, it should return (False, None). Hooks should be placed in
## the following list; we try them from begining to end, and stop as
## soon as a hook claims to be able to handle the object we're
## converting.

general_hooks = []

def hs_to_py(obj):
    """Marshall an HsObjRaw object to a form which is convenient for use
    form Python.

    Haskell objects output by hslowlevel will be of the low-level
    HsObjRaw type; before providing them to the user, we will wish to
    either:

    a) Convert them to friendly objects of some
    automatically-generated python type derived from HsObj. This
    python type will generally represent the data constructor used to
    construct the object in question. (In some exceptional cases we
    might instead return something whose python type represents the
    type constructor, or return something whose python type is simply
    HsObj.)

    b) Convert them into a python-native object (e.g. convert an
    HsObjRaw wrapping a Haskell Integer to a python int.

    The general philosophy in hyphen is that we are liberal in
    marshalling python objects to Haskell, but relatively strict in
    marshalling objects back; so (a) is the norm and (b) only applies
    to objects of some pretty basic types (like ints as
    mentioned). See the docs for a justification of this choice!

    """
    hstype = obj.hstype
    if hstype in per_type_hooks:
        return per_type_hooks[hstype](obj)
    tycon  = hstype.head_ll
    if isinstance(tycon, str): #Polymorphic object with type variable for head of the type
        return hslowlevel.HsObjRaw.__new__(hyphen.HsObj, obj)
    if isinstance(tycon, hslowlevel.TyCon) and tycon in per_tycon_hooks:
        return per_tycon_hooks[tycon](obj)
    for hook in general_hooks:
        (processed, result) = hook(obj)
        if processed:
            return result

    # OK; no specific hook is implicated. This means we're definitely
    # in case (a) above (all instances of case b are handled by the
    # hook system). Try to find the python class which represents the
    # TyCon at the head of the type of obj, as generated by
    # marshall_ctor. It then takes responsibility for constructing a
    # nice python object to represent obj (in particular, one with a
    # nice python type).
    tyc_class = hyphen.marshall_ctor.marshall_tycon(tycon)
    if tyc_class is None:
        return hslowlevel.HsObjRaw.__new__(hyphen.HsObj, obj)
    else:
        return tyc_class.interpret(obj)
