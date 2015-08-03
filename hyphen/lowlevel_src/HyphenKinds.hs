{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenKinds (Kind(..), simplKnd, kindString, kindFromText) where

import Control.Applicative hiding ((<|>))
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

newtype Kind   = Kind {kindArgKinds :: [Kind]} deriving (Eq, Ord, Show)

simplKnd :: Int -> Kind
simplKnd = Kind . flip replicate (Kind [])

kindString :: Kind -> String
kindString (Kind ks) = intercalate " -> " $ map kindString' ks ++ ["*"]
  where kindString' (Kind []) = "*"
        kindString' k         = "(" ++ kindString k ++ ")"


atomicKindParser :: Parser Kind
atomicKindParser = (char '(' *> spaces *> kindParser <* char ')' <* spaces)
                   <|> (char '*' *> return (Kind []))

kindParser :: Parser Kind
kindParser = do first <- atomicKindParser
                let continuation = spaces *> string "->" *> spaces *> kindParser
                rest  <- (Just <$> continuation) <|> (return Nothing)
                return $ case rest of
                  Nothing        -> first
                  Just (Kind ks) -> Kind (first:ks)

kindFromText :: Text -> Either ErrMsg Kind
kindFromText txt = case parse (spaces *> kindParser <* eof) "" txt of
  Right kind -> return kind
  Left  err  -> (Left . ErrMsg . T.pack .
                 (("Failed to interpret kind string "++T.unpack txt++")")++) . show) err

instance Hashable (Kind) where
  hashWithSalt salt (Kind ks) = hashWithSalt salt ks

instance NFData (Kind) where
  rnf (Kind ks) = rnf ks
