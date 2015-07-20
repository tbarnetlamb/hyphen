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

Basic usage
-------------

Once you have imported hyphen, you can import Haskell modules as though they
were Python modules with an hs prefix, so for example

    >>> import hyphen
    >>> import hs.Prelude
    >>> import hs.Data.Text
    >>> import hs.Control.Monad

Objects defined