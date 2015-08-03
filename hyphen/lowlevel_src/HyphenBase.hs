{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : HyphenBase
Description : Some simple utility functions and definitions used by Hyphen
-}

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

-- |For interoperability with C libraries, Haskell provides
-- functionality to wrap stable pointers as Ptr ()'s. It is helpful
-- for us to keep track of Ptr ()s which represent stable pointers in
-- this manner, and so we create a newtype.

newtype WStPtr = WStPtr {unWStPtr :: Ptr ()} deriving (Storable)

castPtrToStablePtr :: WStPtr -> Foreign.StablePtr.StablePtr a
castPtrToStablePtr = Foreign.StablePtr.castPtrToStablePtr . unWStPtr

castStablePtrToPtr :: Foreign.StablePtr.StablePtr a -> WStPtr
castStablePtrToPtr = WStPtr . Foreign.StablePtr.castStablePtrToPtr

-- |Alias for undefined to represent a function which *should*
-- eventually be defined; equivalent of a TODO, but allows code to
-- compile in interim.

tbd = undefined

-- |Utility function which does the obvious thing one would expect
-- from the type signature.

cleaveMap    :: (Ord k, Ord k1, Ord k2) => (k -> (k1, k2)) -> Map k v -> Map k1 (Map k2 v)
cleaveMap fn =
  (fmap Map.fromList) . Map.fromListWith (++) . (map $ shuffle . first fn) . Map.toList
  where shuffle ((k1, k2), v) = (k1, [(k2, v)])

-- |It is useful to us to have a newtype ErrMsg repersenting a Text
-- that is to be thought of as an error message

newtype ErrMsg = ErrMsg {getErrMsg :: Text} deriving (Eq, Ord, Show)

-- Arrange that Either ErrMsg is a monad by creating an Error (ErrMsg)
-- instance.  We also define a 'report' function which raises an error
-- in this monad.

instance Error (ErrMsg) where
  strMsg = ErrMsg . T.pack

report = Left . ErrMsg . T.pack :: String -> Either ErrMsg a

-- |Convenience function for working with Monads; do two actions in
-- the monad; discard the result of the second and return the result
-- of the first.

(>>.) :: (Monad m) => m a -> m b -> m a
(>>.) a1 a2 = do res <- a1
                 a2
                 return res

-- |Convenience function to determine if a Text is a legal Python
-- identifier, following the official rules from the Python 3 grammar
-- reference.
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