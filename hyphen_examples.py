"""
This file contains all the examples from README.md as doctests.

>>> import hyphen, hs.Prelude
>>> hs.Prelude.drop(1, [1,2,3]) # doctest: +ELLIPSIS
<hs.GHC.Types.[] object of Haskell type [GHC...Integer], containing '[2,3]'>
>>> list(hs.Prelude.drop(1, [1,2,3]))   # Convert back to Python list
[2, 3]
>>> hs.Prelude.id(3)
3

>>> import hyphen
>>> import hs.Prelude
>>> import hs.Data.Text

>>> hs.Prelude._["+"] (1, 2)
3
>>> hs.Prelude.sum([1,2,3]) # list converted to Haskell list
6
>>> hs.Prelude.drop(5, "Hello, world")
', world'

>>> hs.Prelude.drop(1, [1,2,3])  # doctest: +ELLIPSIS
<hs.GHC.Types.[] object of Haskell type [GHC...Integer], containing '[2,3]'>

>>> my_list = hs.Prelude.drop(1, [1,2,3])
>>> hs.Prelude.sum(my_list)
5

>>> for x in my_list:
...     print(x)
...
2
3

>>> import hs.Data.Map
>>> my_map = hs.Data.Map.fromList([(1, 'Hello'), (2, 'World')])
>>> my_map[1]
'Hello'
>>> print(sorted([key for key in my_map]))
[1, 2]

>>> my_func = hs.Prelude.const(4)
>>> my_func  # doctest: +ELLIPSIS
<hyphen.HsFunObj object of Haskell type b_0 -> GHC...Integer>
>>> my_func('Hello')
4

>>> hs.Prelude.putStrLn("Test") # Construct IO action, but don't perform it
<hs.GHC.Types.IO object of Haskell type GHC.Types.IO ()>

>>> import hyphen
>>> hyphen.find_and_load_haskell_source()
>>> from hs.Test import Test
>>> my_test_obj = Test(3)
>>> my_test_obj
<hs.Test.Test object of Haskell type Test.Test, containing 'Test 3'>
>>> my_test_obj.extract_number # doctest: +ELLIPSIS
<bound method ... of <hs.Test.Test object of Haskell type Test.Test, containing 'Test 3'>>
>>> my_test_obj.extract_number()
3
>>> my_test_obj.make_sum(4)
7
>>> my_test_obj[5]
8

>>> import hyphen, hs.Prelude, hs.Data.Text
>>> hs.Prelude.drop(6, "Hello world")   # Python string -> Haskell String
'world'
>>> hs.Data.Text.drop(6, "Hello world") # Python string -> Haskell Text
'world'
>>> hs.Prelude.drop(1, (1, 2))          # Python tuple  -> Haskell list  # doctest: +ELLIPSIS
<hs.GHC.Types.[] object of Haskell type [GHC...Integer], containing '[2]'>
>>> hs.Prelude.snd((1, 2))              # Python tuple  -> Haskell tuple
2

>>> hs.Prelude._['+'] (1, 2)            # Select Integer version
3
>>> hs.Prelude._['+'] (1+0j, 2+3j)      # Select Complex Float version
(3+3j)
>>> hs.Prelude.id([1, 2, 3])            # Invoke version of id for lists of integers   # doctest: +ELLIPSIS
<hs.GHC.Types.[] object of Haskell type [GHC...Integer], containing '[1,2,3]'>

>>> hs.Prelude.id((1, 2))  # Prefer to convert Python tuples to Haskell tuples, not lists  # doctest: +ELLIPSIS
<hs.GHC.Tuple.(,) object of Haskell type (GHC...Integer, GHC...Integer), containing '(1,2)'>
>>> hs.Prelude.id((1, "Test")) # Prefer to convert Python strings to Haskell Text  # doctest: +ELLIPSIS
<hs.GHC.Tuple.(,) object of Haskell type (GHC...Integer, Data.Text.Internal.Text), containing '(1,"Test")'>

>>> try:
...    hs.Prelude._['+'] (1, 2+3j) # doctest: +ELLIPSIS
... except TypeError as e:
...    print(str(e).replace('\\t','    '))
Incompatible types: cannot resolve object of type
    a -> a -> a
to type
    GHC...Integer -> Data.Complex.Complex GHC.Types.Float -> a

>>> hs.Prelude.foldr((lambda x, y: x + y), 0, [1, 2, 3])
6

>>> hs.Prelude.foldr((lambda x, y: x + y), 0, range(60))
1770

>>> import sys
>>> old_recursion_limit = sys.getrecursionlimit()
>>> sys.setrecursionlimit(300)
>>> try:
...     hs.Prelude.foldr((lambda x, y: x + y), 0, range(10000))
... except RuntimeError as e:
...     print('maximum recursion depth exceeded' in str(e))
True
>>> sys.setrecursionlimit(old_recursion_limit)

>>> hs.Prelude.replicate
<hyphen.HsFunObj object of Haskell type GHC.Types.Int -> a -> [a]>
>>> specialized_repl = hs.Prelude.replicate.subst(a=hs.Prelude.IO(hs.Data.Text.Text()))
>>> specialized_repl
<hyphen.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.IO Data.Text.Internal.Text -> [GHC.Types.IO Data.Text.Internal.Text]>
>>> specialized_repl(4, (lambda : input()))
<hs.GHC.Types.[] object of Haskell type [GHC.Types.IO Data.Text.Internal.Text]>
>>> hs.Prelude.sequence(specialized_repl(4, (lambda : input())))
<hs.GHC.Types.IO object of Haskell type GHC.Types.IO [Data.Text.Internal.Text]>

>>> import hyphen, hs.Prelude, hs.Data.Maybe, hs.Data.Text
>>> identity_on_Maybe_Text = hs.Prelude.id.subst(a=hs.Data.Maybe.Maybe(hs.Data.Text.Text()))
>>> identity_on_Maybe_Text           # doctest: +ELLIPSIS
<hyphen.HsFunObj object of Haskell type GHC...Maybe Data.Text.Internal.Text -> GHC...Maybe Data.Text.Internal.Text>
>>> identity_on_Maybe_Text("Hello")  # doctest: +ELLIPSIS
<hs.GHC...Just object of Haskell type GHC...Maybe Data.Text.Internal.Text, containing 'Just "Hello"'>
>>> identity_on_Maybe_Text(None)     # doctest: +ELLIPSIS
<hs.GHC...Nothing object of Haskell type GHC...Maybe Data.Text.Internal.Text, containing 'Nothing'>

>>> hs.Test.Example
<class 'hs.Test.Example'>
>>> type(hs.Test.Example)
<class 'type'>
>>> hs.Test.ExampleWithInt
<class 'hs.Test.ExampleWithInt'>
>>> hs.Test.ExampleWithString
<class 'hs.Test.ExampleWithString'>
>>> hs.Test.ExampleWithString.__bases__
(<class 'hs.Test.Example'>,)
>>> hs.Test.ExampleWithInt.__bases__
(<class 'hs.Test.Example'>,)


>>> hs.Test.ExampleWithInt(1)
<hs.Test.ExampleWithInt object of Haskell type Test.Example, containing 'ExampleWithInt 1'>
>>> hs.Test.ExampleWithString("hello")
<hs.Test.ExampleWithString object of Haskell type Test.Example, containing 'ExampleWithString "hello"'>

>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.ExampleWithInt)
True
>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.ExampleWithString)
False
>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.Example)
True
>>> type(hs.Test.ExampleWithInt(1))
<class 'hs.Test.ExampleWithInt'>

>>> hs.Test.ExampleWithInt(1)._components
(1,)

>>> hs.Prelude.LT
<class 'hs.GHC.Types.LT'>
>>> hs.Prelude.LT()
<hs.GHC.Types.LT object of Haskell type GHC.Types.Ordering, containing 'LT'>
>>> type(hs.Prelude.LT)
<class 'type'>
>>> type(hs.Prelude.LT())
<class 'hs.GHC.Types.LT'>

>>> map1 = hs.Prelude.id({1 :1, 2:2})
>>> map1 # doctest: +ELLIPSIS
<hs.Data.Map... object of Haskell type Data.Map...Map GHC...Integer GHC...Integer, containing 'fromList [(1,1),(2,2)]'>
>>> map2 = hs.Prelude.id({1:'one', 2:'two'})
>>> map2 # doctest: +ELLIPSIS
<hs.Data.Map... object of Haskell type Data.Map...Map GHC...Integer Data.Text.Internal.Text, containing 'fromList [(1,"one"),(2,"two")]'>
>>> type(map1) == type(map2)
True

>>> map1.hstype # doctest: +ELLIPSIS
hs.Data...Map(hs.GHC...Integer(), hs.GHC...Integer())
>>> str(map1.hstype) # NB: str(...) representation easier to read than repr, more closely matches Haskell notation  # doctest: +ELLIPSIS
'<hyphen.HsType object representing Data.Map...Map GHC...Integer GHC...Integer>'
>>> str(map2.hstype)  # doctest: +ELLIPSIS
'<hyphen.HsType object representing Data.Map...Map GHC...Integer Data.Text.Internal.Text>'
>>> map1.hstype == map2.hstype
False

>>> map1.hstype.head # doctest: +ELLIPSIS
('Map', 'Data.Map...', ...)
>>> map1.hstype.tail # doctest: +ELLIPSIS
(hs.GHC...Integer(), hs.GHC...Integer())
>>> import hs.Data.Map
>>> hs.Data.Map.Map('a', 'b') # doctest: +ELLIPSIS
hs.Data.Map....Map(hyphen.HsType("a"), hyphen.HsType("b"))
>>> hyphen.HsType("a")
hyphen.HsType("a")
>>> hyphen.HsType("a", kind="* -> *")
hyphen.HsType("a", kind="* -> *")
>>> hyphen.HsType("a").head
'a'
>>> hyphen.HsType('a', hs.Prelude.Integer(), kind="*->*")
hyphen.HsType("a", hs.Prelude.Integer(), kind="* -> *")

>>> hs.Prelude.Integer  # doctest: +ELLIPSIS
<class 'hs.GHC...Integer'>
>>> type(hs.Prelude.Integer)
<class 'type'>
>>> hs.Prelude.Integer()
hs.Prelude.Integer()
>>> str(hs.Prelude.Integer())  # doctest: +ELLIPSIS
'<hyphen.HsType object representing GHC...Integer>'
>>> type(hs.Prelude.Integer())
<class 'hyphen.HsType'>

>>> my_hstype = hs.Prelude.id.hstype
>>> my_hstype
hs.GHC.Prim._['(->)'](hyphen.HsType("a"), hyphen.HsType("a"))
>>> str(my_hstype)
'<hyphen.HsType object representing a -> a>'
>>> my_hstype.fvs
{'a': '*'}
>>> my_hstype.kind
'*'
>>> my_hstype2 = my_hstype.subst(a=map1.hstype)
>>> str(my_hstype2) # doctest: +ELLIPSIS
'<hyphen.HsType object representing Data.Map...Map GHC...Integer GHC...Integer -> Data.Map...Map GHC...Integer GHC...Integer>'

>>> int_identity = hs.Prelude.id.subst(a=hs.Prelude.Int())
>>> int_identity
<hyphen.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.Int>
>>> int_identity('Foo') # doctest: +ELLIPSIS
Traceback (most recent call last):
...
TypeError: ...
>>> int_identity(1)
1
>>> hs.Prelude.id.narrow_type(int_identity.hstype)
<hyphen.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.Int>

>>> hs.Prelude.quot(1, 0)
Traceback (most recent call last):
...
ZeroDivisionError: divide by zero

"""
