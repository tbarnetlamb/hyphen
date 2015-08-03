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

mkTyCon :: Text -> Text  -> Text -> TyCLocation -> Kind -> Bool -> TyCon
mkTyCon p m n l k i = TyCon (hash (p, m, n, k)) p m n l k i

tyConFullName :: TyCon -> Text
tyConFullName tc = T.concat [tyConModule tc, T.pack ".", tyConName tc]

tyConRepr     :: TyCon -> Text
tyConRepr     tc@(TyCon {tyConLocation=InExplicitModuleNamed mname})
  = let name  = tyConName tc
        ok    = name /= (T.pack "_") && okPythonIdentif name
        adjN  = if ok then name else T.concat [T.pack "_['", name, T.pack "']"]
    in T.concat [T.pack "hs.", mname, T.pack ".", adjN]
tyConRepr     tc@(TyCon {tyConLocation=ivloc@(ImplicitlyVia {})})
  = let transformLocPath :: [Int] -> [Text]
        transformLocPath []      = [T.pack ".head"]
        transformLocPath (i0:is)
          = T.pack (".tail[" ++ show (-i0-1) ++ "]") : transformLocPath is
    in case implicitTycLocIsViaType ivloc of
      False -> T.concat (
        T.pack "hs." : implicitTycLocFullName ivloc :
        transformLocPath (implicitTycLocPath ivloc))
      True  -> T.concat (
        T.pack "hs." : implicitTycLocFullName ivloc : T.pack ".hstype" :
        transformLocPath (implicitTycLocPath ivloc))

tyConArity :: TyCon -> Int
tyConArity = length . kindArgKinds . tyConKind

exceptionalLookups :: Map (Text, Text) (Text,Text)
exceptionalLookups = Map.fromList . map ((T.pack *** T.pack) *** (T.pack *** T.pack)) $ [
  (("integer-gmp", "GHC.Integer.Type"), ("integer-gmp", "GHC.Integer"))
  ]

tyConFromTypeableTyCon :: Kind -> Data.Typeable.TyCon  -> TyCon
tyConFromTypeableTyCon knd ghcTyc
  = let [p0, m0, n]  = map T.pack [
          Data.Typeable.tyConPackage ghcTyc, Data.Typeable.tyConModule ghcTyc,
          Data.Typeable.tyConName ghcTyc]
        (p, m) = Map.findWithDefault (p0, m0) (p0, m0) exceptionalLookups
    in mkTyCon p m n (InExplicitModuleNamed m) knd False

fnTyCon :: TyCon
fnTyCon = mkTyCon (T.pack "ghc-prim") (T.pack "GHC.Prim") (T.pack "(->)")
          (InExplicitModuleNamed $ T.pack "GHC.Prim") (simplKnd 2) False

ioTyCon   :: TyCon
ioTyCon   = tyConFromTypeableTyCon (simplKnd 1) . typeRepTyCon . typeOf $ putChar undefined

listTyCon :: TyCon
listTyCon = tyConFromTypeableTyCon (simplKnd 1) . typeRepTyCon . typeOf $ [""]

isListTyCon :: TyCon -> Bool
isListTyCon tyc = tyConName tyc == T.pack "[]"

isTupTyCon :: TyCon -> Bool
isTupTyCon tyc = case T.group (tyConName tyc) of
  [op, middle, cl]   -> (op == T.pack "(") && (cl == T.pack ")") && (T.head middle == ',')
  [op, cl]           -> (op == T.pack "(") && (cl == T.pack ")")
  _                  -> False

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


tyconQual :: TyCon -> (Int, Int)
tyconQual tyc@(TyCon {tyConLocation = (InExplicitModuleNamed name)})
 | name == tyConModule tyc        = (2, 0)
 | otherwise                      = (1, T.count (T.pack ".") name)
tyconQual _                       = (0, 0)

pickBestTyc :: TyCon -> TyCon -> TyCon
pickBestTyc t1 t2 = case compare (tyconQual t1) (tyconQual t2) of
  LT -> t2
  EQ -> t2
  GT -> t1
