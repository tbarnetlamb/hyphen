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

instance NFData (TyCon) where
  rnf (TyCon h p m n k ic) = rnf (h, p, m, n, k, ic)

data TyCon = TyCon {
     tyConHash      :: {-# UNPACK #-} !Int,
     tyConPackage   :: Text,
     tyConModule    :: Text,
     tyConName      :: Text,
     tyConKind      :: Kind,
     tyConIsCls     :: Bool
     } deriving (Ord, Show)

exceptionalLookups :: Map (Text, Text) (Text,Text)
exceptionalLookups = Map.fromList . map ((T.pack *** T.pack) *** (T.pack *** T.pack)) $ [
  (("integer-gmp", "GHC.Integer.Type"), ("integer-gmp", "GHC.Integer")),
  (("containers-0.5.0.0", "Data.Set.Base"), ("containers-0.5.0.0", "Data.Set")),
  (("containers-0.5.0.0", "Data.Map.Base"), ("containers-0.5.0.0", "Data.Map"))
  ]

mkTyCon :: Text -> Text -> Text -> Kind -> Bool -> TyCon
mkTyCon p m n a i = TyCon (hash (p', m', n, a)) p' m' n a i
  where (p', m') = Map.findWithDefault (p, m) (p, m) exceptionalLookups

tyConFullName :: TyCon -> Text
tyConFullName tc = T.concat [tyConModule tc, T.pack ".", tyConName tc]

tyConRepr     :: TyCon -> Text
tyConRepr     tc = T.concat [T.pack "hs.", tyConModule tc, T.pack ".", adjN]
  where name  = tyConName tc
        ok    = name /= (T.pack "_") && okPythonIdentif name
        adjN  = if ok then name else T.concat [T.pack "_['", name, T.pack "']"]

tyConArity :: TyCon -> Int
tyConArity = length . kindArgKinds . tyConKind

instance Eq (TyCon) where
  a == b = tyConHash a == tyConHash b

instance Hashable (TyCon) where
  hash = tyConHash

tyConFromTypeableTyCon :: Kind -> Data.Typeable.TyCon  -> TyCon
tyConFromTypeableTyCon knd ghcTyc
  = mkTyCon 
    (T.pack $ Data.Typeable.tyConPackage ghcTyc) 
    (T.pack $ Data.Typeable.tyConModule ghcTyc) 
    (T.pack $ Data.Typeable.tyConName ghcTyc) knd False

fnTyCon :: TyCon
fnTyCon = mkTyCon (T.pack "ghc-prim") (T.pack "GHC.Prim") (T.pack "(->)") (simplKnd 2) False

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
    (T.pack $ Data.Typeable.tyConModule ghcTyc) (T.pack $ '(' : replicate i ',' ++ ")")
    (simplKnd i) False
  where ghcTyc = typeRepTyCon $ typeOf ("","")


