{-# LANGUAGE DeriveDataTypeable #-}

module Test where

import Data.Maybe
import Data.Typeable

foo :: Integer -> Integer
foo = (+ 1)

data Test = Test Integer deriving (Typeable, Show)

extract_number :: Test -> Integer
extract_number (Test i) = i

make_sum :: Test -> Integer -> Integer
make_sum (Test i) j = i + j

hy__Test__getitem__ :: Test -> Integer -> Integer
hy__Test__getitem__ (Test i) j = i + j

data Example = ExampleWithInt    Int
             | ExampleWithString String deriving (Typeable, Show)
