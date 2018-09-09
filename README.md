[![Travis build Status](https://travis-ci.com/tbarnetlamb/hyphen.svg?branch=master)](https://travis-ci.com/tbarnetlamb/hyphen)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/414bc62yo05vetel?svg=true)](https://ci.appveyor.com/project/tbarnetlamb/hyphen)

Hyphen
======

Hyphen allows one to access Haskell modules from Python (3 or
better). (More precisely, it allows one to access Haskell modules
compiled with GHC from CPython.) It is in some sense the dual of the
cpython package on Hackage, which allows Haskell code to access Python
modules.

For instance:

    >>> import hyphen, hs.Prelude
    >>> hs.Prelude.drop(1, [1,2,3])
    <hs.GHC.Types.[] object of Haskell type [GHC.Integer.Integer], containing '[2,3]'>
    >>> list(hs.Prelude.drop(1, [1,2,3]))   # Convert back to Python list
    [2, 3]
    >>> hs.Prelude.id(3)
    3


Why the name?
-------------

The other obvious portmanteau is 'Pascal', which is taken.

Building
--------

For a guide to building Hyphen, see BUILDING.md. Hyphen has been
successfully built and used on Mac OS X, Ubuntu, Windows (32 bit and
64 bit) and Windows+Cygwin (32 bit and 64 bit).

Basic usage
-----------

Once you have imported hyphen, you can import Haskell modules as though they
were Python modules with an hs prefix, so for example

    >>> import hyphen
    >>> import hs.Prelude
    >>> import hs.Data.Text

The functions defined in these modules can now be called from
Python. For instance, the Haskell function `Prelude.drop` can be
referred to from python as `hs.Prelude.drop`. The only slightly fiddly
thing is that some Haskell functions can have names that Python won't
accept. (So for instance there's a function `Prelude.(+)`, but Python
won't let you refer to `hs.Prelude.(+)`.) In such cases, you can find
these symbols as, for example, `hs.Prelude._["+"]`. (Note that
`hs.Prelude._["drop"]` works too.)

(Quick note: so far we're just talking about how to access Haskell
code from installed modules. In this usage pattern, the Haskell code
that you want to access from Python should have Cabal install it to
the (system or user) library, and you'll be able to import it from
there. Hyphen also supports just having a file `Whatever.hs` in the
same directory as your python script and importing functions from
there into python: see 'non-library modules' below for more.)

When you call a Haskell function, you can call it with arguments that
are themselves Haskell objects, but you can also call it with python
objects as arguments, in which case we will try to *marshall* those
arguments into Haskell objects before calling the Haskell
function. For instance

    >>> hs.Prelude._["+"] (1, 2)
    3
    >>> hs.Prelude.sum([1,2,3]) # list converted to Haskell list
    6
	>>> hs.Prelude.drop(5, "Hello, world")
	", world"

In these three cases, the return value has been marshalled back from
Haskell to Python. Our philosophy is to aggressively try to marshall
parameters *to* Haskell types, but only to convert the very simplest
types (String, Text, Int, Integer, Float, Complex) *back* to Python
types on return. More complicated objects stay as Haskell
objects. For instance:

    >>> hs.Prelude.drop(1, [1,2,3])
    <hs.GHC.Types.[] object of Haskell type [GHC.Integer.Type.Integer], containing '[2,3]'>

These Haskell objects can be used from python in many ways. The basic
way of using them is to pass them to further Haskell functions; and
this certainly works, for example:

    >>> my_list = hs.Prelude.drop(1, [1,2,3])
    >>> hs.Prelude.sum(my_list)
	5

But we can also do other things. For instance, if the return value is
a Haskell list, its python representation will be a python iterable:

    >>> for x in my_list:
    ...     print(x)
	...
	2
	3

...and if the return value is a Haskell Map or HashMap, the resulting
Haskell object will behave like a python dict...

    >>> import hs.Data.Map
    >>> my_map = hs.Data.Map.fromList([(1, 'Hello'), (2, 'World')])
    >>> my_map[1]
	'Hello'
	>>> print(sorted([key for key in my_map]))
	[1, 2]

...and if the return value is a function object, we can call
it. (Indeed, the Haskell function objects we get returned by calling
Haskell-functions-that-return-functions are the exact same kind of
thing as the Haskell function objects that get imported into modules
when we import hs.something; in other words, they're the same as the
objects we've been calling so far.)

    >>> my_func = hs.Prelude.const(4)
    >>> my_func
	<hs.GHC.Prim.HsFunObj object of Haskell type b_0 -> GHC.Integer.Type.Integer>
	>>> my_func('Hello')
	4

In a similar vein, if the Haskell object is in `Cmp` or `Eq` then the
corresponding python object will support comparisons or equality
tests, and if the Haskell object is hashable, the python object will
be too.

As you'd expect, you can partially apply any function just by calling
it with fewer than the full number of arugments, and you'll get a
function which accepts the remaining arguments later.

In addition, Haskell objects that represent IO actions can be induced to
actually perform the action by calling .act on them. This returns
whatever the return type of the action might be.

    >>> hs.Prelude.putStrLn("Test") # Construct IO action, but don't perform it
	<hs.GHC.Types.HsFunObj object of Haskell type GHC.Types.IO ()>
	>>> hs.Prelude.putStrLn("Test").act()
	Test
	<hs.GHC.Tuple.() object of Haskell type (), containing '()'>

It goes without saying that it is important to remember to call .act:
if your code doesn't seem to be doing what it's meant to be doing,
it's possible that you're constructing an IO action but then
discarding it without performing it!

Finally, if we find a a Haskell type `T` and a function `f :: T ->
<something>` which is *defined in the same module*, then this gives
rise to a member function on the Python representation of objects of
type `T`, such that `foo.f(*args)` means the same as `f(foo,
*args)`. For example, if we have Haskell code in the module `Test` as
follows:

    data Test = Test Integer deriving (Typeable, Show)
    
    extract_number :: Test -> Integer
    extract_number (Test i) = i

    make_sum :: Test -> Integer -> Integer
	make_sum (Test i) j = i + j


Then from python we can do:

	>>> import hyphen
	>>> hyphen.find_and_load_haskell_source()
	>>> from hs.Test import Test
	>>> my_test_obj = Test(3)
	>>> my_test_obj
	<hs.Test.Test object of Haskell type Test.Test, containing 'Test 3'>
	>>> my_test_obj.extract_number
	<bound method Test.extract_number of <hs.Test.Test object of Haskell type Test.Test, containing 'Test 3'>>
	>>> my_test_obj.extract_number()
	3
	>>> my_test_obj.make_sum(4)
	7

Names of Haskell functions that begin with an underscore are exempt
from this rule because we don't want users to accidentally end up
defining python members like `__getitem` which can change the behavior
of an object quite dramatically. If you want to create such members
deliberately (which you might be: perhaps you're trying to use Haskell
to build a python type that has interesting non-standard behaviors),
then you can escape the exemption as follows: if you define a member
is Haskell with a name like `hy__<type-name>__<something>__`, then
this will be used to create a python member called `__<something>__`.

For instance, continuing the example above, if the Haskell code continues

    hy__Test__getitem__ :: Test -> Integer -> Integer
    hy__Test__getitem__ (Test i) j = i + j

Then the python example could have continued

    >>> my_test_obj[5]
    8


More detail about marshalling to Haskell
----------------------------------------

We have already seen some examples of how objects are marshalled from
Haskell to Python. A key point about this process is that a single
Python object could be used to construct Haskell objects of various
different types, depending on type expected by the Haskell function
which we're applying. For instance:

	>>> import hyphen, hs.Prelude, hs.Data.Text
	>>> hs.Prelude.drop(6, "Hello world")   # Python string -> Haskell String
	'world'
	>>> hs.Data.Text.drop(6, "Hello world") # Python string -> Haskell Text
	'world'
	>>> hs.Prelude.drop(1, (1, 2))          # Python tuple  -> Haskell list
	<hs.GHC.Types.[] object of Haskell type [GHC.Integer.Type.Integer], containing '[2]'>
	>>> hs.Prelude.snd((1, 2))              # Python tuple  -> Haskell tuple
	2

On the other hand, you can apply polymorphic Haskell functions to
Python objects, and the type of the Python object to which we apply
the function will be used to determine what Haskell type should be
used for the polymorphic arguments; for instance:

	>>> hs.Prelude._['+'] (1, 2)            # Select Integer version
	3
	>>> hs.Prelude._['+'] (1+0j, 2+3j)      # Select Complex Float version
	(3+3j)
	>>> hs.Prelude.id([1, 2, 3])            # Invoke version of id for lists of integers
	<hs.GHC.Types.[] object of Haskell type [GHC.Integer.Type.Integer], containing '[1,2,3]'>

When a Python object could have been conerted into multiple Haskell
types, we will 'break the tie' and convert it to some preferred type:

	>>> hs.Prelude.id((1, 2))  # Prefer to convert Python tuples to Haskell tuples, not lists
	<hs.GHC.Tuple.(,) object of Haskell type (GHC.Integer.Type.Integer, GHC.Integer.Type.Integer), containing '(1,2)'>
	>>> hs.Prelude.id((1, "Test")) # Prefer to convert Python strings to Haskell Text
	<hs.GHC.Tuple.(,) object of Haskell type (GHC.Integer.Type.Integer, Data.Text.Internal.Text), containing '(1,"Test")'>

This is not foolproof however; for instance, we get an error in the following case:

	>>> hs.Prelude._['+'] (1, 2+3j)
	Traceback (most recent call last):
	...
	TypeError: Incompatible types: cannot resolve object of type
	a -> a -> a
	to type
	GHC.Integer.Type.Integer -> Data.Complex.Complex GHC.Types.Float -> a

Before we close this section, we'll cover two other behaviors of the
marshalling code that are important or useful.

One key point is that Python functions can be marshalled into Haskell
functions. For example:

    >>> hs.Prelude.foldr((lambda x, y: x + y), 0, [1, 2, 3])
    6

Although you should be careful when doing this; Haskell idioms make full use of Haskell's ability to have infinite stack depth, whereas Python has a finite stack depth. This can cause problems in examples like:

    >>> hs.Prelude.foldr((lambda x, y: x + y), 0, range(10000))
    ...
    RuntimeError: maximum recursion depth exceeded while calling a Python object

Similarly, Python functions can be marshalled into IO actions. In this
case the Python function will be called with no arguments. For
example, we can take the usual 'replicate' function in the Prelude,
force its type to be `Int -> IO Text -> IO [Text]` (see the section on
types below for more on this), then play with it as follows:

	>>> hs.Prelude.replicate
	<hs.GHC.Prim.HsFunObj object of Haskell type GHC.Types.Int -> a -> [a]>
	>>> specialized_repl = hs.Prelude.replicate.subst(a=hs.Prelude.IO(hs.Data.Text.Text()))
	>>> specialized_repl
	<hs.GHC.Prim.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.IO Data.Text.Internal.Text -> [GHC.Types.IO Data.Text.Internal.Text]>
	>>> specialized_repl(4, (lambda : input()))
	<hs.GHC.Types.[] object of Haskell type [GHC.Types.IO Data.Text.Internal.Text]>
	>>> hs.Prelude.sequence(specialized_repl(4, (lambda : input())))
	<hs.GHC.Types.HsFunObj object of Haskell type GHC.Types.IO [Data.Text.Internal.Text]>
	>>> hs.Prelude.sequence(specialized_repl(4, (lambda : input()))).act()
	Fee
	Fi
	Fo
	Fum
	<hs.GHC.Types.[] object of Haskell type [Data.Text.Internal.Text], containing '["Fee","Fi","Fo","Fum"]'>

(The reason we have to force the type is that otherwise we have no way
of knowing that we should marshall the Python function into an IO
action, nor what that IO action should return.)

The other useful behavior is that if we can marshall a python object
to a Haskell object of type `T`, then we can generally also marshall
it to Haskell type `Maybe T` (in which case we insert an implicit
`Just`); we can also marshall python `None` to any Haskell type `Maybe
x`, in which case we render it as `Nothing`.

For example:

	>>> import hyphen, hs.Prelude, hs.Data.Maybe, hs.Data.Text
	>>> identity_on_Maybe_Text = hs.Prelude.id.subst(a=hs.Data.Maybe.Maybe(hs.Data.Text.Text()))
	>>> identity_on_Maybe_Text
	<hs.GHC.Prim.HsFunObj object of Haskell type GHC.Base.Maybe Data.Text.Internal.Text -> GHC.Base.Maybe Data.Text.Internal.Text>
	>>> identity_on_Maybe_Text("Hello")
	<hs.GHC.Base.Just object of Haskell type GHC.Base.Maybe Data.Text.Internal.Text, containing 'Just "Hello"'>
	>>> identity_on_Maybe_Text(None)
	<hs.GHC.Base.Nothing object of Haskell type GHC.Base.Maybe Data.Text.Internal.Text, containing 'Nothing'>


Don't cross the streams
-----------------------

In general, the goal with hyphen is that things Just Work (TM). I'll
leave it to the reader to decide whether that goal is met in general,
but there's one really nasty case where it seems pretty impossible to
make things Just Work in the best possible way. So now I'll give the
single most important warning about using Hyphen: you *must not
construct reference loops that consist of Haskell and python
objects*. If you do this, then neither the Haskell garbage collection
nor the Python garbage collection will be able to collect the objects
in the loop, because neither will be able to see the 'whole loop' and
recognize that it can be collected.

To be clear, it's perfectly fine to have Python objects refer to
Haskell objects (whenever you get a return value from a Haskell
function which is not marshalled back to Python, and you store that
result somewhere in a Python object, you're building Python objects
which refer to Haskell objects. It's perfectly fine to have Haskell
objects refer to Python objects. (When you marshall a python closure
over to Haskell for use as a Haskell function), then that closure (and
hence, indirectly, any python objects which that closure refers to)
will now be referred to from Haskell. It's perfectly fine, even, to
have Haskell objects refer to Python objects which themselves refer to
Haskell objects which refer to Python objects which refer to Haskell
objects and so on. What you can't do is have (say) a Haskell object
refer to a Python object which refers to the *original* Haskell
object.

Importing non-library modules
-----------------------------

In general, the preferred way of accessing Haskell code from Python
via hyphen is to install the Haskell code as a ghc-pkg visible
library; such libraries can always be directly imported into Python
with hyphen (as we have been doing so far).

But you might want to have some Haskell source code in the same
directory as your python source, and then magically import Haskell
functions from that source. This is possible, but there are some
limitations. The basic issue is that to ensure type-safety, the
hyphen system must enforce that we only ever compile and import from
source *once* per program run. (Reason: Haskell will recompile the
source if it has changed, which might lead us to have
binary-incompatible objects floating around which the type system
thinks can be freely inter-substituted, leading to Problems.) So we
have to have a system for deciding which Haskell modules we want
compiled as part of this 'one shot' compilation.

We provide two options for this.

Option one is enabled by calling
`hyphen.find_and_load_haskell_source()`. This looks in the directory of
the running script for Haskell source, and (if any is found) we will
also check subdirectories for more source, and (if any is found in a
subdirectory) we check sub-sub directories recursively. All the source
we find, we try to compile; if there are .hs files lying around which
are not valid source, we will get errors. Once compiled, we can import
the contents of these files from the `hs.*` namespace as usual.

This is meant to cover the case when you're writing a little script
and you want to import a little bespoke Haskell routine. It assumes
that you can control the contents of the directory where your script
lives; not an unreasonable assumption.

Option two is not really recommended. It covers the case where you're
writing a python library which is being imported from somewhere on the
python path and which (in turn) wants to import a little Haskell piece
of code that lives in the same directory. (As we've said, the
recommended way of handling this is to install the Haskell code as a
Haskell library visible form ghc-pkg, then to import it from
python... but we'll assume this isn't possible for you.) To enable
option two, call `find_and_load_haskell_source(check_full_path=True)`
as your python library module is imported. This will check the *entire
python path* for Haskell files (using the same rules as were used to
check the script directory above, including recursively reading
subdirectories). We then compile them and again they may be
imported. This is (a) somewhat slow, and (b) runs the risk that we'll
come across an .hs file somewhere in the path which isn't valid
Haskell and die.

What happens to Haskell type constructors and data constructors?
----------------------------------------------------------------

So far, we've talked a lot about what happens to objects imported from
Haskell modules. But Haskell modules can also contain type
constructors and data constructors. What happens to them? The answer
is that they are both transformed into python `type` objects. So if we
have a Haskell type constructor with a couple of data constructors, we
will end up with a little class hierarchy on the Python side. For
example, if the Haskell module `Test` has:

    data Example = ExampleWithInt    Int
                 | ExampleWithString String deriving (Typeable, Show)

we'll end up with python classes arranged as follows

    hyphen.HsObj (base class of all Haskell objects viewed from python via hyphen)
	|
	\--- hs.Test.Example
	        |
			\---- hs.Test.ExampleWithInt
			|
			\---- hs.Test.ExampleWithString

As we can see from Python:

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

We can call the data constructors to make Haskell objects of the
relevant types:

    >>> hs.Test.ExampleWithInt(1)
	<hs.Test.ExampleWithInt object of Haskell type Test.Example, containing 'ExampleWithInt 1'>
	>>> hs.Test.ExampleWithString("hello")
	<hs.Test.ExampleWithString object of Haskell type Test.Example, containing 'ExampleWithString "hello"'>

Things behave as you'd expect

	>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.ExampleWithInt)
	True
	>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.ExampleWithString)
	False
	>>> isinstance(hs.Test.ExampleWithInt(1), hs.Test.Example)
	True
	>>> type(hs.Test.ExampleWithInt(1))
	<class 'hs.Test.ExampleWithInt'>


It's also worth remarking that you can take apart a Haskell object to
see what arguments were applied to its constructor as follows:

	>>> hs.Test.ExampleWithInt(1)._components
	(1,)

One potentially confusing thing: you need to realize that, for data
constructors which take no parameters, there's a big difference
between the python representation of the data constructor itself and
the value you get when you call it. (Whereas in Haskell they're
basically the same thing.) For instance:

    >>> hs.Prelude.LT
	<class 'hs.GHC.Types.LT'>
	>>> hs.Prelude.LT()
	<hs.GHC.Types.LT object of Haskell type GHC.Types.Ordering, containing 'LT'>
	>>> type(hs.Prelude.LT)
	<class 'type'>
	>>> type(hs.Prelude.LT())
	<class 'hs.GHC.Types.LT'>

Another thing to bear in mind is that since a Haskell module's
type/type constructor namespace and object namespace are completely
separate things, it is possible to have the same name defined in both
namespaces (in such cases, because of capitalization rules, the object
in this case will necessarily be a Data constructor). Python has onyl
a single namespace, so we must somehow resolve this contention. We
generally do this by creating a lightweight 'doublet' object; then you
can do `hs.Foo.Bar.MyName.the_tycon` or `hs.Foo.Bar.MyName.the_dacon`
to specify exactly what you mean. There is one exception to this,
which is when you have a type constructor with only one data
constructor, and which has the same name as the type constructor. This
is a common pattern in Haskell and it's annoying to have to do the
`.the_dacon` stuff in this case. So in this case alone we have
`MyName` refer to the data constructor, which is probably what you
mean, and allow you to write `MyName.the_type` to get the type
constructor.)

The difference between Python types and Haskell types
-----------------------------------------------------

We've just described how hyphen creates python types to represent
Haskell type constructors and data constructors. *Every* Haskell
object viewed from python via hyphen will be given a python type using
these types.  (Exception: if the type constructor is invisible because
it's not exported from anywhere, then the type will just be `HsObj`.)

If follows from all this that the Python type of a Haskell object
viewed through hyphen can be rather different to its Haskell type. On
the one hand, the Python type will depend on exactly which data
constructor was used. On the other hand, if the Haskell type is
something like `Map Int Int`, the python type will not pick up the
arguments (`Int` and `Int`) provided to the type constructor; the
python type will just be `hs.Data.Map.Map`, and so:

    >>> map1 = hs.Prelude.id({1 :1, 2:2})
	>>> map1
	<hs.Data.Map.Base.Map object of Haskell type Data.Map.Base.Map GHC.Integer.Type.Integer GHC.Integer.Type.Integer, containing 'fromList [(1,1),(2,2)]'>
	>>> map2 = hs.Prelude.id({1:'one', 2:'two'})
	>>> map2
	<hs.Data.Map.Base.Map object of Haskell type Data.Map.Base.Map GHC.Integer.Type.Integer Data.Text.Internal.Text, containing 'fromList [(1,"one"),(2,"two")]'>
	>>> type(map1) == type(map2)
	True

If you care to know the full Haskell type of a Haskell object viewed
from hyphen, we can certainly do that; it's visible as the `.hstype`
member. This will be a python object of python type `hyphen.HsType`:

    >>> map1.hstype
	hs.Data.Map.Map(hs.GHC.Integer.Integer(), hs.GHC.Integer.Integer())
	>>> str(map1.hstype) # NB: str(...) representation easier to read than repr, more closely matches Haskell notation
	'<hyphen.HsType object representing Data.Map.Base.Map GHC.Integer.Type.Integer GHC.Integer.Type.Integer>'
	>>> str(map2.hstype)
	'<hyphen.HsType object representing Data.Map.Base.Map GHC.Integer.Type.Integer Data.Text.Internal.Text>'
	>>> map1.hstype == map2.hstype
	False

`HsType` objects can be manipulated in a few ways. You can break them
up by calling the `head` and `tail` members. (`head` will return a
triple giving the name/module/package key of a type constructor, or
just a string in the case that the head of the type is a type
variable). You can build HsTypes by invoking type constructors (pass
other HsTypes as the parameters, and strings for type variables). To
build HsTypes representng type variables do `HsType('foo')` (or
`HsType('foo', kind='* -> *')` etc. for variables of more complicated
kinds), and if you want to create an odd HsType with a type variable
as its head and a nontrivial tail (legal Haskell, but very odd), you
can do something like `HsType('a', hs.Prelude.Integer(),
kind="*->*")`).

	>>> map1.hstype.head
	('Map', 'Data.Map.Base', 'conta_LKCPrTJwOTOLk4OU37YmeN')
	>>> map1.hstype.tail
	(hs.GHC.Integer.Integer(), hs.GHC.Integer.Integer())
	>>> Data.Map.Map('a', 'b')
	Traceback (most recent call last):
	File "<stdin>", line 1, in <module>
	NameError: name 'Data' is not defined
	>>> hs.Data.Map.Map('a', 'b')
	Traceback (most recent call last):
	File "<stdin>", line 1, in <module>
	AttributeError: 'module' object has no attribute 'Map'
	>>> import hs.Data.Map
	>>> hs.Data.Map.Map('a', 'b')
	hs.Data.Map.Map(hyphen.HsType("a"), hyphen.HsType("b"))
	>>> hyphen.HsType("a")
	hyphen.HsType("a")
	>>> hyphen.HsType("a", kind="* -> *")
	hyphen.HsType("a", kind="* -> *")
	>>> hyphen.HsType("a").head
	'a'
	>>> hyphen.HsType('a', hs.Prelude.Integer(), kind="*->*")
	hyphen.HsType("a", hs.Prelude.Integer(), kind="* -> *")

In this connexion, it's important when there are nullary type
constructors in play to be mindful of the difference between the
python object we create to represent the type constructor and the HsType
you get by invoking it:

    >>> hs.Prelude.Integer
	<class 'hs.GHC.Integer.Type.Integer'>
	>>> type(hs.Prelude.Integer)
	<class 'type'>
	>>> hs.Prelude.Integer()
	hs.Prelude.Integer()
	>>> str(hs.Prelude.Integer())
	'<hyphen.HsType object representing GHC.Integer.Type.Integer>'
	>>> type(hs.Prelude.Integer())
	<class 'hyphen.HsType'>

There are a few other interesting things to mention. One is that you
can call `subst` on HsTypes to provide substitutions for type
variables therein. You can call `kind` to get the kind of the
type. And you can call `fvs` to get the free variables (as a
dictionary mapping variables to strings representing their kinds).

    >>> my_hstype = hs.Prelude.id.hstype
	>>> my_hstype
	hs.GHC.Prim._['(->)'] (hyphen.HsType("a"), hyphen.HsType("a"))
	>>> str(my_hstype)
	'<hyphen.HsType object representing a -> a>'
	>>> my_hstype.fvs
	{'a': '*'}
	>>> my_hstype.kind
	'*'
	>>> my_hstype2 = my_hstype.subst(a=map1.hstype)
	>>> str(my_hstype2)
	'<hyphen.HsType object representing Data.Map.Base.Map GHC.Integer.Type.Integer GHC.Integer.Type.Integer -> Data.Map.Base.Map GHC.Integer.Type.Integer GHC.Integer.Type.Integer>'

Once you know how to construct and play with `HsType` objects, this
also allows you to do some new things with `HsObj`. You can also call
`subst` on HsObjs, in order to substitute for the type variables that
occur in their types, and you can call `narrow_type` to narrow an
object to a specific type.

    >>> int_identity = hs.Prelude.id.subst(a=hs.Prelude.Int())
	>>> int_identity
	<hs.GHC.Prim.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.Int>
	>>> int_identity('Foo')
	Traceback (most recent call last):
	...
	TypeError: an integer is required (got type str)
	>>> int_identity(1)
	1
	>>> hs.Prelude.id.narrow_type(int_identity.hstype)
	<hs.GHC.Prim.HsFunObj object of Haskell type GHC.Types.Int -> GHC.Types.Int>
	
What about exceptions?
----------------------

We try to be sensible about converting Haskell exceptions to Python
exceptions whenever they escape from Haskell code. For instance:

	>>> hs.Prelude.quot(1, 0)
	Traceback (most recent call last):
	...
	ZeroDivisionError: divide by zero

(here the Haskell `DivideByZero` was converted to a python
`ZeroDivisionError`). Python exceptions raised by python functions
that were marshalled into Haskell functions will propagate through
Haskell code and finally come out to the python code on the other
side.

Notes on customizability and the low-level layer
------------------------------------------------

A brief point about the implementation of hyphen. Basically, hyphen
consists of two rather separate componnents. The first component is a
fairly low-level bridge between Haskell and Python. This low-level
bridge is written mostly in Haskell with a little bit of C. The second
component is built on top of this and gives the high-level bridge
we've just described. This second part is in fact implemented in
Python. There are two nice results of this. First, if something goes
wrong in the high-level layer, you'll get a nice traceback (at least
until the problem hits the low-level layer), which may help you to
diagnose what's up and what can be done to fix it. Second, the high
level layer is full of hooks that can be manipulated in Python: so if
you, the Python user, want to add functionality (say) to marshall
python objects to some particular Haskell type in some particular way,
that's totally something you can do, from pure python, by just
customizing the high-level layer via its hooks.

If this is something that interests you, the best way to start is by
reading the sources of the high level layer, especially
`marshall_ctor.py`, `marshall_obj_to_hs.py` and
`marshall_obj_to_py,py`. There are comments in each of those files
which provide a guide for customizing behavor.

For more documentation on the low-level layer, see LOWLEVEL.md.

Notes on efficiency
-------------------

The previous section describes the implementation of hyphen in terms
of a low-level layer sitting on a high level layer. This allows us to
segue into the question of efficiency. Obviously, if you call a
Haskell function from Python via hyphen then once we actually get
through to running Haskell code, things will be just as efficient as
running any other Haskell code anywhere else. So as long as you don't
have context switches between Haskell and python inside your inner
loop (the intended case for using hyphen is that you should not have
such things!),then you're probably fine.

If you *do* want to do lots of context switches in an inner or
nearly-inner loop, then you might have a problem. The python
high-level layer which handles marshalling of objects between the two
langauges is not designed to be super fast, so your code may be
slow. If it's not possible to avoid having context switches close to
your inner loop, you can try bypassing the high-level layer and using
the low-level layer directly. If you do that, then things will be
pretty efficient: if you invoke a Haskell function from Python using
the low-level layer, then it's basically a type check followed by a
direct jump from python to a C function and from that C function to
the Haskell machine code. So it should be reasonably fast.

One final point in this connection is that, even going via the
low-level layer, resolving polymorphic Haskell objects down to
monomorphic ones is reasonably expensive. So you should resolve any
polymorphism once and for all, *outside* your inner loop, then call
the monomorphic functions from inside the inner loop.

Notes on the GIL
----------------

Python notoriously has a locking structure, called the Global
Interpreter Lock or GIL, which must be held whenever the python
interpreter is doing anything. Hyphen allows you to optionally release
this lock before calling in to Haskell code, and re-acquire it on
returning from the Haskell code. This is often a good idea (it allows
other python threads to make progress while the Haskell code is doing
its work); but it might not be (releasing and re-aquiring the GIL is a
little expensive and if all your Haskell calls return quickly and/or
your python code isn't multithreaded anyway, then there's no benefit
to releasing the GIL).

If you want to have hyphen release the GIL while running Haskell code,
do `hyphen.hslowlevel.set_GIL_mode_fancy()`. If you want it not to
bother to release the GIL, do `hyphen.hslowlevel.set_GIL_mode_lazy`.
If you want to know the current state, do
`hyphen.hslowlevel.get_GIL_mode()` (which will return `'fancy'` or
`'lazy'` as appropriate).

Notes on signals and Keyboard Interrupts
----------------------------------------

This section only applies on unix-like OSs.

Sometimes, the operating system will send a *signal* to a running
program, telling it (say) to terminate, or to interrupt what it's
doing. For instance, when you press ctrl+C in the console, the
runnning program recieves a interrupt signal. Python allows you to
install a *Python signal handler* for any signal, which is basically
some python code which will get executed when the signal is sent by
the operating system. A default handler is installed for the interrupt
signal, which basically raises a `KeyboardInterrupt` exception which
then propagates outward through your Python code, causing the code to
stop unless the exception is caught.

Basically the same story is true in Haskell: Haskell allows you to
install *Haskell signal handler* for any signal, which is basically
some Haskell code which will get executed when the signal is sent by
the operating system. Again, a default handler is installed for the
interrupt signal, which basically raises a `UserInterrupt` exception
which then propagates outward as before.

The wrinkle in all this is the following. It turns out that the way
Python implements python signal handlers goes like this. The OS
provides a low-level primtive that lets you install some C code to run
when a signal is delivered (we'll call that an *OS signal
handler*). Unfortunately, what you're allowed to do inside an OS
signal handler is extremely limited. So when you ask python to install
a python signal handler, python will (in turn) install an OS signal
handler, but *all that handler does is to set a flag somewhere*. Then
python constantly polls that flag as it interprets code, and if it's
set, Python suspends whatever execution is currently in progress and
rushes to call the (python) signal handler code.

(The same is true for Haskell.)

What does this have to do with Hyphen? Well, when you use hyphen to
jump into Haskell code, then while that Haskell code is running,
Python's OS signal handler is still installed, but no one is checking
the flag. So if you (say) ctrl-C your program, the flag will get set,
but the program will not stop running because no one will check the
flag, and so `KeyboardInterrupt` will not be raised, so the program
will not stop. (Until, that is, the Haskell routine returns and Python
finally checks the flag.) This is Not Ideal.

There are two ways around this. One option is to have Haskell
periodically check *python's* 'has a signal been recieved' flags, and
(if a signal has been recieved), go service the python signal
handler. If the signal handler (in turn) raises an exception (like
`KeyboardInterrupt`), then we propagate that exception to the running
Haskell code, which will pass it up the stack, and the exception will
eventually escape to the python code that called the Haskell code,
which is what we want. This is generally the best option, but it does
require some multithreadying overhead on the Haskell side (we have to
spawn another Haskell thread just to service the Python signal
handlers), and while multithreading in Haskell is cheap compared to
python, it's not free. (Another proviso is that Haskell will only
check the flag every 50ms or so, which is not nearly as often as
Python would check it normally.)

An alternative is the following. While Haskell code is running, we can
install *Haskell's* OS signal handler together with a Haskell signal
handler that processes ctrl-C. (We then restore python's OS signal
handler at the end of the Haskell code.) Then, if a ctrl-C is received
during the executing of some Haskell code, *Haskell's* flag will be
set to say that there's a signal waiting, and (since Haskell is
constantly polling that flag while Haskell code is running), Haskell
will notice that ctrl-C was pressed, which will invoke the Hasell
signal handler, which will raise `UserInterrupt`, which will propagate
up through the Haskell code and finally be translated to
`KeyboardInterrupt` when it crosses over into python. This again will
have the effect that our Haskell code is properly interrupted, and we
will be polling the flag much more frequently than once every
100ms. The disadvantage is that which signal handlers run depends on
whether the signal arrives while Python or Haskell code is
running. This may be quite confusing! But if all the signal handlers
actually end up doing is raising `UserInterrupt` or
`KeyboardInterrupt`, it's probably managable.

(A final alternative is to do nothing, and just live with the fact
that signals that are recieved while hyphen is executing Haskell code
will not be processed by python until the Haskell code returns. This
is obviously the lowest-overhead option.)

If you want to have hyphen service python signal handlers while
Haskell code is running, do
`hyphen.hslowlevel.set_signal_mode_python()`. If you want to have
hyphen replace the OS signal handler while Haskell code is running so
that Haskell signal handlers will process any signals received while
Haskell code is running, do
`hyphen.hslowlevel.set_signal_mode_haskell()`. If you want to be lazy
and not check for signals at all until Haskell returns control to
python, do `hyphen.hslowlevel.set_signal_mode_lazy()`.  If you want to
know the current state, do `hyphen.hslowlevel.get_signal_mode()`
(which will return `'python'`, `'haskell'` or `'lazy'` as
appropriate).
