{-# LANGUAGE DeriveDataTypeable #-}

module Test where

import Data.Maybe
import Data.Typeable

foo :: Integer -> Integer
foo = (+ 1)

data Test = Test Integer deriving (Typeable, Show)