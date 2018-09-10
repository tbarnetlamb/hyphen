{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenKinds (Kind(..), simplKnd, kindString, kindFromText) where

import Control.DeepSeq
import Data.Hashable
import Data.List
import Data.Text (Text)
import Text.Parsec.Text
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.Text            as T

import HyphenBase

-- | This module defines the Kind type, which can be sued to label
-- Haskell kinds. It's exstremely compact: a Kind is just a list of
-- Kinds; if the kind X is associated to a list of other kinds [A, B,
-- C] this means that a type of kind X must be applied to arguments of
-- kinds A, B and C in turn before we get something of kind *. (So if
-- A is (* -> *), B is * and C is (* -> * -> *), then X would be (* ->
-- *) -> * -> (* -> * -> *) -> *. Thus the empty list corresponds to *
-- itself.

newtype Kind   = Kind {kindArgKinds :: [Kind]} deriving (Eq, Ord, Show)

-- | Convenience function; simplKnd n returns the kind * -> * -> * ->
-- * where there are n arguments of kind * (and so (n+1) *s in the
-- "* -> * -> * -> *" in total)

simplKnd :: Int -> Kind
simplKnd = Kind . flip replicate (Kind [])

-- | Convert a Kind to a String using the usual Haskell notation for kinds

kindString :: Kind -> String
kindString (Kind ks) = intercalate " -> " $ map kindString' ks ++ ["*"]
  where kindString' (Kind []) = "*"
        kindString' k         = "(" ++ kindString k ++ ")"

-- | Parser for kinds that either look like (k_1 -> k_2 -> k_3 ->
-- .. -> k_n) (where the k_i are arbitrary kinds, but there must be
-- brackets around) or else are just *.

atomicKindParser :: Parser Kind
atomicKindParser = (char '(' *> spaces *> kindParser <* char ')' <* spaces)
                   <|> (char '*' *> return (Kind []))

-- | Parser for kinds

kindParser :: Parser Kind
kindParser = do first <- atomicKindParser
                let continuation = spaces *> string "->" *> spaces *> kindParser
                rest  <- (Just <$> continuation) <|> (return Nothing)
                return $ case rest of
                  Nothing        -> first
                  Just (Kind ks) -> Kind (first:ks)

-- | Extract a Kind from a string, or else give a nice error message if we can't

kindFromText :: Text -> Either ErrMsg Kind
kindFromText txt = case parse (spaces *> kindParser <* eof) "" txt of
  Right kind -> return kind
  Left  err  -> (Left . ErrMsg . T.pack .
                 (("Failed to interpret kind string "++T.unpack txt++")")++) . show) err

instance Hashable (Kind) where
  hashWithSalt salt (Kind ks) = hashWithSalt salt ks

instance NFData (Kind) where
  rnf (Kind ks) = rnf ks
