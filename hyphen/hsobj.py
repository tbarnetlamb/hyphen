from __future__   import absolute_import

from functools    import total_ordering
from hyphen       import hslowlevel
from hyphen.utils import hs_fn_type, hs_eq, hs_hash, hs_show, hs_lt, hs_Func
import hyphen

@total_ordering
class HsObj(hslowlevel.HsObjRaw):
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
            hs_eq_ = hs_eq.narrow_type(hs_fn_type(
                self.hstype, self.hstype, hslowlevel.hstype_Bool))
        except Exception:
            return False
        return hslowlevel.from_haskell_Bool(hslowlevel.apply(hs_eq_, self, other))

    def __lt__(self, other):
        if type(self) != type(other) or self.hstype != other.hstype:
            return (type(self), self.hstype) < (type(other), other.hstype)
        try:
            hs_lt_ = hs_lt.narrow_type(hs_fn_type(
                self.hstype, self.hstype, hslowlevel.hstype_Bool))
        except Exception as e:
            raise ValueError(
                "Incomparable Haskell object of type %s" % self.hstype.name) from e
        return hslowlevel.from_haskell_Int(hslowlevel.apply(hs_lt_, self, other))

    def __new__(cls, *args):
        if cls.hs_dacon == 'BLANK_IN_HSOBJ':
            raise TypeError("Cannot construct HsObj's directly")
        elif cls.hs_dacon == 'BLANK_IN_TYCONS':
            return cls.hs_tycon(*args)
        if cls.hs_dacon.hstype.head == hs_Func:
            return hslowlevel.HsObjRaw.__new__(cls, hyphen.marshall_ctor.applyFromPyArgs(
                cls.hs_dacon, *args))
        else:
            return hslowlevel.HsObjRaw.__new__(cls, cls.hs_dacon)

    def _is_polymorphic(self):
        return len(self.fvs) == 0

    def narrow_type(self, new_type):
        return hyphen.marshall_obj_to_py.hs_to_py(super(HsObj, self).narrow_type(new_type))

    def subst(self, *args, **kwa):
        return hyphen.marshall_obj_to_py.hs_to_py(super(HsObj, self).subst(*args, **kwa))

    hs_dacon = 'BLANK_IN_HSOBJ'
