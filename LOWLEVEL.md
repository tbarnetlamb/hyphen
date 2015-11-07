The low-level layer
===================

The low level layer lives in `hyphen.hslowlevel`. It defines three
types: `HsObjRaw`, `HsType`, and `TyCon`, which represent haskell
objects, haskell types, and haskell type constructors respectively. It
also defines a bunch of functions for doing various things. Many of
these functions are for tasks like converting a HsObjRaws that
represents an `Int` into a native python `int` (and the opposite).

This document is arranged in six parts. The first three describe the
methods of `HsObjRaw`, `HsType`, and `TyCon` in turn. The next
describes the functions for converting between (say) python `int`s and
Haskell `Int`s. The next describes three very important functions,
which provide the actual way we can import stuff from Haskell into
python. The last section describes the remaining functions defined in
`hslowlevel`.

The HsObjRaw type
-----------------

An `HsObjRaw` object represents a Haskell object. The members are as follows:

 * `hstype`: get the Haskell type of the `HsObjRaw` object. Returns a
   `HsType` object---see below.
 * `narrow_type(to_type)`: return a modified version of the `HsObjRaw`
   whose type is `to_type`. Obviously, the current type of the
   `HsObjRaw` in question must be at least as general as
   `to_type`. Moreover, narrowing might require certain instances to
   exist in order to work. (So you can't narrow `Prelude.show` to have
   type `Foo -> String` unless `Foo` is in the `Show` class...)
 * `subst(var1=hstype1,var2=hstype2)`: substitute for the type
   variables in the type of the `HsObjRaw`, returning a new `HsObjRaw`
   with a more specific type.

You can construct new `HsObjRaw` objects by doing
`HsObjRaw(existing_obj)` where `existing_obj` is an existing
`HsObjRaw` object. The newly created `HsObjRaw` will represent the
same underlying Haskell object as the `HsObjRaw` you started with. As
such this option is of limited usefulness!

The HsType type
---------------

An `HsType` object represents a Haskell type. `HsType` objects are
hashable, and comparable and support a `str` operation which gives a
nice string representation of the `HsType` which includes the type it
represents spelt out in usual Haskell notation, and a `repr` object
which gives python code that can be used to recreate the `HsType`
(assuming `hyphen` is in play).

The members are as follows:

 * `fvs`: return the free variables of the type, as a dictionary
   mapping variable name to kind.
 * `head_ll`: gives the head of the type; either a string (if the head
   is a type variable) or a `TyCon` object (see below) if the head is
   a type constructor. (There's also a `head` member, which low-level
   users don't really want to call. It exists because `HsType`s are
   also part of the high-level interface, and we don't want
   high-level-interface users to end up seeing `TyCon` objects,
   because they are not part of the high-level interface.)
 * `kind`: returns the kind of the type, as a string.
 * `name`: returns the name of the type in usual Haskell notation.
 * `subst(var1=hstype1, var2=hstype2)`: substitute for the free
   variables in the HsType.
 * `tail`: returns a tuple giving the arguments to which the head of
   the type has been applied.

You can construct new `HsType` objects by calling `HsType(head, tail1,
tail2, ..., tailn)`. Here `head` should be either a `TyCon` (to create
an `HsType` whose head is an explicit type constructor) or a string
(to create an `HsType` whose head is a type variable). The `tailX`
parameters give the types to which the head is applied: they should be
other `HsType` objects. If the head is a string (a rare case!), you
can provide a `kind` keyword argumnet to specify the kind of the
overall type.

The TyCon type
--------------

A `TyCon` object represents a Haskell type constructor. `TyCon`
objects are hashable, comparable and support `str` and `repr`
operations giving human-readable and python-executable string
representations in the usual python way.

You can call a `TyCon` object to construct a new `HsType` object:
`my_tycon(*args)` is exactly equivalent to `HsType(my_tycon, *args)`.

The members are as follows:

 * `arity`: The arity of the type constructor
 * `kind`: The kind of the type constructor
 * `module`: The module in which the type constructor was originally
   defined.
 * `name`: The (Haskell) name of the type constructor.
 * `package`: The package (or package key of the package) in which the
   type constructor was defined.
 * `visible_module`: An importable module from which one can import
   this type constructor, or `None` if no such module is known. May
   differ from `module`: if a type constructor was originally defined
   in a hidden module, but is later imported into a visible module and
   re-exported, then `module` will be the original (hidden) module of
   definition and `visible_module` will be the visible module which
   re-exports it.

It is not possible to construct new `TyCon` objects directly from python.

The conversion functions
------------------------

holowlevel exports functions `to_haskell_X` and `from_haskell_X` for
each of the following `X`:

 * `Bool`, `Char`, `String`, `Text`, `ByteString`, `Int`, `Integer`, `Float`, `Double`

In each case, the functions are used as follows. (We illustrate for
the `Bool` functions.) `to_haskell_Bool` takes as an argument a python
`bool` object and returns an `HsObjRaw` containing a Haskell `Bool`
with the same value. `from_haskell_Bool` takes as an argument an
`HsObjRaw` containing a Haskell `Bool` and returns a python `bool`
object with the same value.

The importing functions
-----------------------

There are three very important functions defined in hslowlevel which
allow you to import modules from Haskell.

### The `hslowlevel.import_lib` function ###

One calls `hslowlevel.import_lib(module_name1, module_name2, ...)` to
import modules form library packages visible to `hs_pkg`. The return
is a dictionary, mapping module names to module contents. The keys
will match the set of module names provided as arguments to
`import_lib`. The 'module contents' assosciated to a given key will be
a pair (`object_namespace`, `type_namespace`). `object_namespace` will
be a dictionary mapping strings (names of things in the object
namespace) to `HsObjRaw`s. `type_namespace` will be a dictionary whose
keys are strings (names of things in the type namespace). The value
assosciated to a given key will either be a `TyCon` object (if that
name is the name of a data type constructor) or a `HsType` object (if
the name is the name of a type synonym); in the latter case, the
`HsType` shows what the type synonym expands to, with the free
variables in the `HsType` being the parameters of the type synonym.

If there are data-constructors in the module in question, then as well
as creating functions (like say `Just`) in the data namespace that
correspond to the data constructors in question and can be used to
construct objects, we also create functions (with names like
`*co-Just`) which can be used to see if an object was made using the
data constructor in question, and if so, break it up. We call these
'co-dataconstructors'. Specifically, the co-dataconstructor transforms
an object constructed using the data constructor in question into a
singleton list containing a tuple containing the arguments that were
given to the Data Constructor to construct the object. (If we apply
the 'co-data constructor' to something that was constructed using a
different Data constructor for the same type, we get back an empty
list.)

### The `hslowlevel.import_src` function ###

One calls `hslowlevel.import_src(source_path1, source_path2, ...)` to
compile and import Haskell source. The return is exactly the same as
for `import_lib`, although you should note that the keys in the
dictionary returned will again be the names of the modules (as with
`import_lib`, not the source paths that those modules came from).

Note that you can only call this function once. (Reason: if we didn't
have this rule, Haskell will recompile the source if it has changed,
which might lead us to have binary-incompatible objects floating
around which the type system thinks can be freely inter-substituted,
leading to Problems.)

### The `hslowlevel.access_basics`function ###

There are a few functions/objects (like the empty list) which are
basically trivial to define in Haskell but which are not actually
importable. We need to be able to access them. So
`hslowlevel.access_basics()` returns a dictionary mapping strings to
HsObjRaws, which define the following objects

* `[]`
* `()`
* `(:)`
* `(,)`, `(,,)`, `(,,,)`, etc. (up to 15-tuples)
* Functions like `tup3head :: (a, b, c) -> a` for 2 through 15 tuples
* Functions like `tup3tail :: (a, b, c) -> (b, c)` for 2 through 15
  tuples. Note that `tup2tail :: (a, b) -> b`.

The other functions
-------------------

Aside from the `to_haskell_X`, `from_haskell_X` , `access_basics`,
`import_lib` and `import_src` functions described in the previous two
sections, we have the following other functions in the hslowlevel
module:

 * `hslowlevel.apply(fn, *args)`. Given `fn` (an `HsObjRaw` object of
   some function type), apply to it to the arguments `args`, which
   should also be `HsObjRaw`s.
 * `hslowlevel.doio(action)`. Given `action` (an `HsObjRaw` object
   which encodes an `IO` action), perform the action, and return an
   `HsObjRaw` giving the return value of the action. 
 * `hslowlevel.ok_python_identif(identifier_string)`. Convenience
   function; is `identifier_string` an acceptable python identifier?
 * `hslowlevel.wrap_pyfn(python_fn, haskell_type, arity)`. Given a
   python function `python_fn` of arity `arity`, which expects all its
   arguments to be `HsObjRaw`s and which returns an `HsObjRaw`, wrap
   that function as a Haskell function of type
   `haskell_type`. Obviously, the `haskell_type` must be consistent
   with it being a function of arity `arity`. Note that if `arity=-1`
   then that's equivalent to choosing the maxiumum arity consistent
   with `haskell_type`. Also note that if `arity=0` we assume
   `python_fn` takes no arguments and that `haskell_type` is the type
   of an `IO` action; we create a Haskell IO action which calls the
   python function.
 * `set_GIL_mode_lazy()`: instructs hyphen not to bother to release
   the python GIL when performing long-running Haskell
   calculations. See README.md for more information.
 * `set_GIL_mode_fancy()`: instructs hyphen to release the python GIL
   when performing long-running Haskell calculations. See README.md
   for more information.
 * `get_GIL_mode()`: Return `'lazy'` or `'fancy'` according to whether
   hyphen is currently configured to release the python GIL when
   performing long-running Haskell calculations or not. See README.md
   for more information.
 * `set_sig_mode_python()`: instructs hyphen to service python signal
   handlers while performing long-running Haskell calculations. See
   README.md for more information.
 * `set_sig_mode_haskell()`: instructs hyphen to install a Haskell
   signal handler to process ctrl-C while performing long-running
   Haskell calculations. See README.md for more information.
 * `set_sig_mode_lazy()`: instructs hyphen not to bother to do
   anything to ensure that we will respond to OS signals in a timely
   manner while performing long-running Haskell calculations. See
   README.md for more information.
 * `get_sig_mode()`: Return `'python'`, `'haskell'` or `'lazy'`
   according to our currently-configured behavor for managing signals
   received while performing long-running Haskell calculations. See
   README.md for more information.
 
