{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenBase where

import Control.Arrow
import Control.Monad.Trans.Error
import Data.Char
import Data.Text                     (Text)
import Data.Map.Strict               (Map)
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Foreign.StablePtr

keyOf :: Ord k => k -> Map k a -> Bool
keyOf = Map.member

newtype WStPtr = WStPtr {unWStPtr :: Ptr ()} deriving (Storable)

castPtrToStablePtr :: WStPtr -> Foreign.StablePtr.StablePtr a
castPtrToStablePtr = Foreign.StablePtr.castPtrToStablePtr . unWStPtr

castStablePtrToPtr :: Foreign.StablePtr.StablePtr a -> WStPtr
castStablePtrToPtr = WStPtr . Foreign.StablePtr.castStablePtrToPtr

tbd = undefined

cleaveMap    :: (Ord k, Ord k1, Ord k2) => (k -> (k1, k2)) -> Map k v -> Map k1 (Map k2 v)
cleaveMap fn =
  (fmap Map.fromList) . Map.fromListWith (++) . (map $ shuffle . first fn) . Map.toList
  where shuffle ((k1, k2), v) = (k1, [(k2, v)])

newtype ErrMsg = ErrMsg {getErrMsg :: Text} deriving (Eq, Ord, Show)

instance Error (ErrMsg) where
  strMsg = ErrMsg . T.pack

report = Left . ErrMsg . T.pack :: String -> Either ErrMsg a

(>>.) :: (Monad m) => m a -> m b -> m a
(>>.) a1 a2 = do res <- a1
                 a2
                 return res

okPythonIdentif :: Text -> Bool
okPythonIdentif name
  = (not $ T.null name) && okHead (T.head name) && T.all okCont (T.drop 1 name)
  where
    okHead ch = ch == '_' || case generalCategory ch of
      UppercaseLetter -> True
      LowercaseLetter -> True
      TitlecaseLetter -> True
      ModifierLetter  -> True
      OtherLetter     -> True
      LetterNumber    -> True
      _               -> False
    okCont ch = case generalCategory ch of
      UppercaseLetter      -> True
      LowercaseLetter      -> True
      TitlecaseLetter      -> True
      ModifierLetter       -> True
      OtherLetter          -> True
      LetterNumber         -> True
      DecimalNumber        -> True
      NonSpacingMark       -> True
      SpacingCombiningMark -> True
      ConnectorPunctuation -> True
      _                    -> False