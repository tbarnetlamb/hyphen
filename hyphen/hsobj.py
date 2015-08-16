"""This module defines the HsObj class. An HsObj object represents a
Haskell object; in general, hyphen will automatically marshall very
simple Haskell objects back into corresponding Python objects, but
only the very simplest objects are mashalled in this way (built in
types, basically); all other objects are represented as HsObjs.

The hslowlevel library already defines an HsObjRaw class which also
represents Haskell objects; HsObj is derived from it and just adds a
bunch of features (like definitions of __str__ and __call__) to make
the low-level version more friendly and easy to work with.

"""

from __future__   import absolute_import

from functools    import total_ordering
from hyphen       import hslowlevel
from hyphen.utils import make_hs_fn_type, hs_eq, hs_hash, hs_show, hs_lt, hs_Func
import hyphen

@total_ordering
class HsObj(hslowlevel.HsObjRaw):
    """An HsObj object represents a Haskell object. The hslowlevel
    library already defines an HsObjRaw class which also represents
    Haskell objects; HsObj is derived from it and just adds a bunch of
    features (like definitions of __str__ and __hash__) to make it
    more friendly and easy to work with.
    """
    def __repr__(self):
        try:
            foo = hslowlevel.apply(hs_show, self)
            shown = ', containing %r' % (
                hslowlevel.from_haskell_String(hslowlevel.apply(hs_show, self)),)
        except:
            shown = ''
        return '<%s.%s object of Haskell type %s%s>' % (
            type(self).__module__, type(self).__name__, self.hstype.name, shown)

    def __str__(self):
        return repr(self)

    def __hash__(self):
        try:
            return hslowlevel.from_haskell_Int(hslowlevel.apply(hs_hash, self))
        except Exception as e:
            raise ValueError(
                "Unhashable Haskell object of type %s" % self.hstype.name) from e

    def __eq__(self, other):
        if type(self) != type(other) or self.hstype != other.hstype:
            return False
        try:
            hs_eq_ = hs_eq.narrow_type(make_hs_fn_type(
                (self.hstype, self.hstype), hslowlevel.hstype_Bool))
        except Exception:
            return False
        return hslowlevel.from_haskell_Bool(hslowlevel.apply(hs_eq_, self, other))

    def __lt__(self, other):
        if type(self) != type(other) or self.hstype != other.hstype:
            return (type(self), self.hstype) < (type(other), other.hstype)
        try:
            hs_lt_ = hs_lt.narrow_type(make_hs_fn_type(
                (self.hstype, self.hstype), hslowlevel.hstype_Bool))
        except Exception as e:
            raise ValueError(
                "Incomparable Haskell object of type %s" % self.hstype.name) from e
        return hslowlevel.from_haskell_Int(hslowlevel.apply(hs_lt_, self, other))

    def __new__(cls, *args):
        if cls.hs_dacon == 'BLANK_IN_HSOBJ':
            raise TypeError("Cannot construct HsObj's directly")
        elif cls.hs_dacon == 'BLANK_IN_TYCONS':
            # In a somewhat abusive notation that is nonetheless
            # clearly (I claim) appropriate for the situation,
            # calling a type object that represents a Haskell Type
            # constructor does not create an HsObj object at all, but
            # rather an HsType object...
            return cls.hs_tycon(*args)
        # OK; we're trying to construct something of a class derived
        # from HsObj which is assosciated to a data constructor. We
        # will call the appropriate data constructor to build a
        # Haskell object which we then wrap as a HsObj. The only
        # wrinkle is that some data constructors (like Nothing) are
        # not functions: they have no arguments in such cases, the
        # data constructor doesn't so much construct an object;
        # rather, it already *is* the (necessarily unique) object
        # that comes form that data constructor. In this case we
        # don't call the data constructor, but just return it.
        if cls.hs_dacon.hstype.head_ll == hs_Func:
            return hslowlevel.HsObjRaw.__new__(cls, hyphen.marshall_ctor.applyFromPyArgs(
                cls.hs_dacon, *args))
        else:
            return hslowlevel.HsObjRaw.__new__(cls, cls.hs_dacon)

    def _is_polymorphic(self):
        """Is this a polymorphic Haskell object?"""
        return len(self.fvs) == 0

    ## One final thing to do. HsObjRaw defines two functions
    ## (narrow_type and subst) which return HsObjRaw objects. We want
    ## to wrap them in HsObj so that when called on HsObjs the return
    ## is promoted to an HsObj.
    
    def narrow_type(self, new_type):
        """To be called upon a polymorphic HsObj; we check that our type can
        be specialized to new_type; (which may itself be polymorphic,
        but obviously must be less polymorphic than our current
        type). If it is, we return an appropriately specialized
        version of ourselves with type new_type.
        """
        return hyphen.marshall_obj_to_py.hs_to_py(super(HsObj, self).narrow_type(new_type))

    def subst(self, *args, **kwa):
        """If obj is a polymorphic HsObj and ty1 and ty2 are HsType objects,
        then obj.subst(a=ty1, b=ty2) represents the (less polymorphic)
        refinement of obj where the type variable 'a' in the type of
        obj has been specialized to the type represented by ty1, and
        the type variable 'b' has been specialized to the type
        represented by ty2. No positional arguments may be
        provided. Not all the type variables in the type of obj need
        to be specialized (but every type variable that you try to
        specialize must genuinely exist in the type of obj).
        """
        return hyphen.marshall_obj_to_py.hs_to_py(super(HsObj, self).subst(*args, **kwa))

    hs_dacon = 'BLANK_IN_HSOBJ'
