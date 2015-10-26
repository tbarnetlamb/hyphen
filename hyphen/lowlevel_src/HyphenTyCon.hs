{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenTyCon where

import Control.Arrow
import Control.DeepSeq
import Data.Typeable                  (Typeable, typeRepTyCon, typeOf)
import Data.Hashable
import Data.Text                      (Text)
import Data.Map.Strict                (Map)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Typeable

import HyphenBase
import HyphenKinds

-- | This module defines the TyCon type, which is a Haskell
-- representation of a Haskell Type constructor located somewhere in
-- the accesible Haskell codebase. It is similar, in this respect, to
-- the TyCon type defined in Data.Typeable. The present representation
-- is different from Data.Typeable.TyCon in that we record (as well as
-- the Package/Module/Name), two additional things: the kind of the
-- type constructor, and also a 'location', which tells us some way
-- that we can write some code which 'finds' the type constructor in
-- importable modules. (That is a little vague; see the TyCLocation
-- type below for more precision!)

-- | A TyCLocation tells us how we can 'get access' to a given TyCon
-- from importable modules. One possible way is that the TyCon is
-- visible, by name, in a given importable module. In this case its
-- TyCLocation is InExplicitModuleNamed module_name. But it's possible
-- for a tycon to be *used* by some importable module, while never
-- actually exported by any module. In this case, we are still
-- interested in how one can import *something* which mentions the
-- TyCon. In this case the TyCLocation uses the ImplicitlyVia
-- constructor. implicitTycLocFullName is a fully qualified name
-- (module included) of something we can import. If
-- implicitTycLocIsViaType then it's the name of something in the type
-- namespace, otherwise it's in the data namespace. In any case, we
-- can *form* a type by either taking a type-namespace-thing directly
-- or taking the type of a data-namespace-thing. Then
-- implicitTycLocPath tells us where in the parse tree of this type we
-- can find the type constructor in question. This is perhaps made
-- clearer by examinining the code for tyConRepr below, which shows
-- how to use the TyCLocation to name the TyCon from inside python.

data TyCLocation
  = InExplicitModuleNamed Text
  | ImplicitlyVia {
    implicitTycLocFullName  :: Text,
    implicitTycLocIsViaType :: Bool,
    implicitTycLocPath      :: [Int]
    } deriving (Eq, Ord, Show)

instance Hashable TyCLocation where
  hashWithSalt s (InExplicitModuleNamed name) = hashWithSalt s ('1', name)
  hashWithSalt s (ImplicitlyVia m o p)        = hashWithSalt s ('2', m, o, p)

instance NFData TyCLocation where
  rnf (InExplicitModuleNamed t) = rnf t
  rnf (ImplicitlyVia m o p)     = rnf (m, o, p)

-- | This module defines the TyCon type, which is a Haskell
-- representation of a Haskell Type constructor located somewhere in
-- the accesible Haskell codebase. Includes a Hash for rapid
-- comparison. See first comment in this file for details on how this
-- differs from the type in Data.Typeable of the same name.

data TyCon = TyCon {
     tyConHash      :: {-# UNPACK #-} !Int,
     tyConPackage   :: Text,
     tyConModule    :: Text,
     tyConName      :: Text,
     tyConLocation  :: TyCLocation,
     tyConKind      :: Kind,
     tyConIsCls     :: Bool
     } deriving (Show)

instance Eq (TyCon) where
  a == b = tyConHash a == tyConHash b

instance Hashable (TyCon) where
  hash = tyConHash
  hashWithSalt salt (TyCon _ p m n _ k _ ) = hashWithSalt salt (p, m, n, k)

instance Ord (TyCon) where
  compare (TyCon h p m n _ k i) (TyCon h' p' m' n' _ k' i')
   = compare (h, p, m, n, k, i) (h', p', m', n', k', i')

instance NFData (TyCon) where
  rnf (TyCon h p m l n k ic) = rnf (h, p, m, l, n, k, ic)

-- | The approved way of making a TyCon. Just pre-computes the hash.

mkTyCon :: Text -> Text  -> Text -> TyCLocation -> Kind -> Bool -> TyCon
mkTyCon p m n l k i = TyCon (hash (p, m, n, k)) p m n l k i

-- | Get the 'full name' of the TyCon; a combination of its name and
-- module of definition.

tyConFullName :: TyCon -> Text
tyConFullName tc = T.concat [tyConModule tc, T.pack ".", tyConName tc]

-- | Get a string representation of the tycon that is suitable to use
-- as its Python @repr@ (that is a string that, if executed in
-- python+hyphen with appropriate imports, will evaluate to a python
-- object which represents the TyCon). Note that this depends on the
-- high-level haskell/python bridge, so there's a bit of something
-- like leaky abstraction here...

tyConRepr     :: TyCon -> Text
tyConRepr     tc@(TyCon {tyConLocation=InExplicitModuleNamed mname})
  = let name  = tyConName tc
        ok    = name /= (T.pack "_") && okPythonIdentif name
        adjN  = if ok then name else T.concat [T.pack "_['", name, T.pack "']"]
    in T.concat [T.pack "hs.", mname, T.pack ".", adjN]
tyConRepr     tc@(TyCon {tyConLocation=ivloc@(ImplicitlyVia {})})
  = let transformLocPath :: [Int] -> [Text]
        transformLocPath []      = [T.pack ".head_ll"]
        transformLocPath (i0:is)
          = T.pack (".tail[" ++ show (-i0-1) ++ "]") : transformLocPath is
    in case implicitTycLocIsViaType ivloc of
      False -> T.concat (
        T.pack "hs." : implicitTycLocFullName ivloc :
        transformLocPath (implicitTycLocPath ivloc))
      True  -> T.concat (
        T.pack "hs." : implicitTycLocFullName ivloc : T.pack ".hstype" :
        transformLocPath (implicitTycLocPath ivloc))

-- | Get the arity of a tycon

tyConArity :: TyCon -> Int
tyConArity = length . kindArgKinds . tyConKind

-- | Construct a TyCon from a Data.Typeable.TyCon and a Kind. This
-- generally assumes that the module that defines the TyCon in
-- question acutally exports it, which is not actually a safe
-- assumption in all cases. The only exceptions to that rule are
-- listed in the table exceptionalLookups below (this Map sends
-- package/module pairs to package/module pairs; if a rtype defined in
-- a given package purports to be accessible in the module listed in
-- the key, we replace it with the new module as listed in the value.

tyConFromTypeableTyCon :: Kind -> Data.Typeable.TyCon  -> TyCon
tyConFromTypeableTyCon knd ghcTyc
  = let [p0, m0, n]  = map T.pack [
          Data.Typeable.tyConPackage ghcTyc, Data.Typeable.tyConModule ghcTyc,
          Data.Typeable.tyConName ghcTyc]
        (p, m) = Map.findWithDefault (p0, m0) (p0, m0) exceptionalLookups
    in mkTyCon p m0 n (InExplicitModuleNamed m) knd False

-- | See description of tyConFromTypeableTyCon above

exceptionalLookups :: Map (Text, Text) (Text,Text)
exceptionalLookups = Map.fromList . map ((T.pack *** T.pack) *** (T.pack *** T.pack)) $ [
  (("integer-gmp", "GHC.Integer.Type"), ("integer-gmp", "GHC.Integer"))
  ]

-- | TyCon object representing the Haskell function type constructor @(->)@

fnTyCon :: TyCon
fnTyCon = mkTyCon (T.pack "ghc-prim") (T.pack "GHC.Prim") (T.pack "(->)")
          (InExplicitModuleNamed $ T.pack "GHC.Prim") (simplKnd 2) False

-- | TyCon object representing the Haskell type constructor @IO@ for the IO monad

ioTyCon   :: TyCon
ioTyCon   = tyConFromTypeableTyCon (simplKnd 1) . typeRepTyCon . typeOf $ putChar undefined

-- | TyCon object representing the Haskell type constructor @[]@ for lists

listTyCon :: TyCon
listTyCon = tyConFromTypeableTyCon (simplKnd 1) . typeRepTyCon . typeOf $ [""]

-- | Is the TyCon provided a representation of the built in type
-- constructor for lists?

isListTyCon :: TyCon -> Bool
isListTyCon tyc = tyConName tyc == T.pack "[]"

-- | Is the TyCon provided a representation of the built in type
-- constructor for some kind of tuple?

isTupTyCon :: TyCon -> Bool
isTupTyCon tyc = case T.group (tyConName tyc) of
  [op, middle, cl]   -> (op == T.pack "(") && (cl == T.pack ")") && (T.head middle == ',')
  [op, cl]           -> (op == T.pack "(") && (cl == T.pack ")")
  _                  -> False

-- | Given an integer, return a TyCon object representing the Haskell
-- type constructor @(,,, ... ,,,)@ for tuples of that length. (Nota
-- bene that length 1 tuples do not exist in Haskell, but length 0
-- ones do.)

tupleTyCon :: Int -> TyCon
tupleTyCon i
  = mkTyCon (T.pack $ Data.Typeable.tyConPackage ghcTyc)
    (T.pack $ Data.Typeable.tyConModule ghcTyc) (T.pack $ '(' : replicate i' ',' ++ ")")
    (InExplicitModuleNamed . T.pack $ Data.Typeable.tyConModule ghcTyc)
    (simplKnd i) False
  where ghcTyc = typeRepTyCon $ typeOf ("","")
        i'     = case i of
          0 -> 0
          1 -> error "tupleTyCon: no length 1 tuples"
          _ -> i - 1

-- | Given two different TyCon objects that represent the same type
-- constructor but perhaps have different TyCLocation information
-- about where it can be imported from or otherwise accessed, pick the
-- 'best' one (i.e. the one with the most terse and readable method of
-- access.

pickBestTyc :: TyCon -> TyCon -> TyCon
pickBestTyc t1 t2 = case compare (tyconQual t1) (tyconQual t2) of
  LT -> t2
  EQ -> t2
  GT -> t1
  where tyconQual :: TyCon -> (Int, Int)
        tyconQual tyc@(TyCon {tyConLocation = (InExplicitModuleNamed name)})
          | name == tyConModule tyc        = (2, 0)
          | otherwise                      = (1, T.count (T.pack ".") name)
        tyconQual _                       = (0, 0)
