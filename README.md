Hyphen
====

Hyphen allows one to access Haskell modules from Python (3 or
better). It is in some sense the dual of the cpython package on
Hackage, which allows Haskell code to access Python modules.

For instance:

  >>> import hyphen, hs.Prelude
  >>> hs.Prelude.drop(1, [1,2,3])
  <hs.GHC.Types.[] object of Haskell type [GHC.Integer.Integer], containing '[2,3]'>
  >>> list(hs.Prelude.drop(1, [1,2,3]))
  [2, 3]
  >>> hs.Prelude.id(3)
  3
