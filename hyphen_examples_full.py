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
<hs.GHC.Types.[] object of Haskell type [GHC.Integer.Type.Integer], containing '[1,2,3]'>

>>> hs.Prelude.id([1, '2', 3]) # doctest: +ELLIPSIS
Traceback (most recent call last):
    ...
TypeError: ...must be int, not str

>>> hs.Prelude.id([1.0, 2, 3])
<hs.GHC.Types.[] object of Haskell type [GHC.Types.Float], containing '[1.0,2.0,3.0]'>

>>> hs.Prelude.id([1, 2.0, 3]) # doctest: +ELLIPSIS
Traceback (most recent call last):
    ...
TypeError: ...must be int, not float

>>> hs.Prelude.id((1, 2.0, 'foo'))
<hs.GHC.Tuple.(,,) object of Haskell type (GHC.Integer.Type.Integer, GHC.Types.Float, Data.Text.Internal.Text), containing '(1,2.0,"foo")'>

>>> hs.Prelude.id((True, 1+2.j))
<hs.GHC.Tuple.(,) object of Haskell type (GHC.Types.Bool, Data.Complex.Complex GHC.Types.Float), containing '(True,1.0 :+ 2.0)'>

>>> hs.Prelude.id({1 : True, 2 : False}) # doctest: +ELLIPSIS
<hs.Data.Map... object of Haskell type Data.Map... GHC.Integer.Type.Integer GHC.Types.Bool, containing 'fromList [(1,True),(2,False)]'>

>>> hs.Prelude.id({1, 2}) # doctest: +ELLIPSIS
<hs.Data.Set... object of Haskell type Data.Set... GHC.Integer.Type.Integer, containing 'fromList [1,2]'>

(Next 2 tests written in a funny way because Data.Maybe.Maybe moved to GHC.Base,
and we want a test that passes both before and after the move)

>>> repr(hs.Prelude.id.subst(a=hyphen.utils.hs_Maybe('a'))(1)).replace('GHC.Base', 'Data.Maybe')
"<hs.Data.Maybe.Just object of Haskell type Data.Maybe.Maybe GHC.Integer.Type.Integer, containing 'Just 1'>"

>>> repr(hs.Prelude.id.subst(a=hyphen.utils.hs_Maybe(hs.Prelude.Integer()))(None)).replace('GHC.Base', 'Data.Maybe')
"<hs.Data.Maybe.Nothing object of Haskell type Data.Maybe.Maybe GHC.Integer.Type.Integer, containing 'Nothing'>"

Checking that (e.g.) Haskell lists give rise to iterable Python objects
==========

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

>>> hyphen.find_and_load_haskell_source()
>>> import hs.Test
>>> hs.Test.Test
<class 'hs.Test.Test'>
>>> hs.Test.foo(3)
4
"""

import sys

def runtest():
    import doctest, sys
    (fails1, _) = doctest.testmod()
    import hyphen.utils, hyphen.marshall_obj_to_hs, hyphen_examples
         # add other modules here if they have docstrings with tests
    (fails2, _) = doctest.testmod(hyphen.utils)
    (fails3, _) = doctest.testmod(hyphen.marshall_obj_to_hs)
    (fails4, _) = doctest.testmod(hyphen_examples)
    if fails1 + fails2 + fails3 + fails4 > 0:
        print("Some failures")
        sys.exit(1)

if __name__ == "__main__":
    import hyphen
    for GIL in 'GIL_mode_lazy', 'GIL_mode_fancy':
        for sig in 'signal_mode_lazy', 'signal_mode_haskell', 'signal_mode_python':
            print("**********************************************")
            print("* TEST BATCH : " + GIL + " " + sig )
            print("**********************************************")
            getattr(hyphen.hslowlevel, 'set_' + GIL)()
            getattr(hyphen.hslowlevel, 'set_' + sig)()
            runtest()
