"""
Roundtrips
==========

>>> import hyphen, hs.Prelude
>>> hs.Prelude.id(3)
3

>>> hs.Prelude.id(3.0)
3.0

>>> hs.Prelude.id(True)
True

>>> hs.Prelude.id(False)
False

>>> hs.Prelude.id(1+2j)
(1+2j)

>>> hs.Prelude.id('foo')
'foo'

>>> hs.Prelude.id(b'foo')
b'foo'

>>> hs.Prelude.id([1, 2, 3])
<hs.GHC.Types.[] object of Haskell type [GHC.Integer.Integer], containing '[1,2,3]'>

>>> hs.Prelude.id([1, '2', 3])
Traceback (most recent call last):
    ...
TypeError: must be int, not str

>>> hs.Prelude.id([1.0, 2, 3])
<hs.GHC.Types.[] object of Haskell type [GHC.Types.Float], containing '[1.0,2.0,3.0]'>

>>> hs.Prelude.id([1, 2.0, 3])
Traceback (most recent call last):
    ...
TypeError: must be int, not float

>>> hs.Prelude.id((1, 2.0, 'foo'))
<hs.GHC.Tuple.(,,) object of Haskell type (GHC.Integer.Integer, GHC.Types.Float, Data.Text.Internal.Text), containing '(1,2.0,"foo")'>

>>> hs.Prelude.id((True, 1+2.j))
<hs.GHC.Tuple.(,) object of Haskell type (GHC.Types.Bool, Data.Complex.Complex GHC.Types.Float), containing '(True,1.0 :+ 2.0)'>

>>> hs.Prelude.id({1 : True, 2 : False})
<hs.Data.Map.Base.Map object of Haskell type Data.Map.Base.Map GHC.Integer.Integer GHC.Types.Bool, containing 'fromList [(1,True),(2,False)]'>

>>> hs.Prelude.id({1, 2})
<hs.Data.Set.Base.Set object of Haskell type Data.Set.Base.Set GHC.Integer.Integer, containing 'fromList [1,2]'>

>>> hs.Prelude.id.subst(a=hyphen.utils.hs_Maybe('a'))(1)
<hs.Data.Maybe.Just object of Haskell type Data.Maybe.Maybe GHC.Integer.Integer, containing 'Just 1'>

>>> hs.Prelude.id.subst(a=hyphen.utils.hs_Maybe(hs.Prelude.Integer()))(None)
<hs.Data.Maybe.Nothing object of Haskell type Data.Maybe.Maybe GHC.Integer.Type.Integer, containing 'Nothing'>

>>> list(hs.Prelude.id([1, 2, 3]))
[1, 2, 3]

>>> mymap = hs.Prelude.id({1 : True, 2 : False})
>>> mymap[1]
True
>>> mymap[2]
False
>>> mymap[3]
Traceback (most recent call last):
    ...
KeyError: 3
>>> mymap.get(1, 'nada')
True
>>> mymap.get(3, 'nada')
'nada'

>>> sorted(list(mymap))
[1, 2]

>>> myset = hs.Prelude.id({1, 2})
>>> sorted(list(myset))
[1, 2]
>>> 1 in myset
True
>>> 2 in myset
True
>>> 3 in myset
False

"""

if __name__ == "__main__":
    import doctest
    doctest.testmod()
