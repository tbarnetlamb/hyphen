{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Pythonate where

import Control.Monad.State.Strict
import Data.Word
import Data.Text           (Text)
import Data.ByteString     (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict     (Map)
import Foreign.Ptr
import Foreign.C.String    (CString)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Text.Foreign
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import qualified Data.ByteString.Unsafe

import PythonBase

-- | These pythonate functions convert Haskell values to pointers to
-- Python objects representing the same value. Some are imported
-- directly from C functions that do the same thing, for types where
-- the FFI can interconvert directly between corresponding C and
-- Haskell types. Others are slighltly higher level, taking a Haskell
-- value that couldn't be directly set to C and massaging it so that
-- its value can be 'pythonated' using the simpler functions already
-- defined.
--
-- Many of these functions are just in the IO monad, but they still
-- might set a Python exception condition. If they do set an
-- exception, they return a null pointer. So they can be wrapped into
-- proper PythonM operations via (treatingAsErr nullPtr). The reason
-- we by default provide them as pre-wrapped is that in most cases
-- calling one of these functions is the last thing we do before
-- transferring control to Python, which will automatically interpret
-- the null Ptr as meaning it has to check the exception condition; so
-- checking for a null ptr return ourselves is in many cases totally
-- superfluous and putting it in PythonM would make extra work for
-- ourselves.

foreign import ccall unsafe pythonateInt        :: Int        -> IO PyObj
foreign import ccall unsafe pythonateFloat      :: Float      -> IO PyObj
foreign import ccall unsafe pythonateDouble     :: Double     -> IO PyObj
foreign import ccall unsafe pythonateUTF16Ptr   :: Ptr Word16 -> Int -> IO PyObj
foreign import ccall unsafe pythonateBytePtr    :: CString    -> Int -> IO PyObj
foreign import ccall unsafe pythonateTrue       :: IO PyObj
foreign import ccall unsafe pythonateFalse      :: IO PyObj
foreign import ccall unsafe pythonateIntegerFromStr :: Ptr Word16 -> Int -> IO PyObj

pythonateBool         :: Bool    -> IO PyObj
pythonateBool b       = if b then pythonateTrue else pythonateFalse

pythonateChar         :: Char    -> IO PyObj
pythonateChar         = pythonateString . (:[])

pythonateString       :: String  -> IO PyObj
pythonateString       = pythonateText . T.pack

pythonateText         :: Text    -> IO PyObj
pythonateText t       = Data.Text.Foreign.useAsPtr t pythonateUTF16Ptr'
  where pythonateUTF16Ptr' ptr i16 = pythonateUTF16Ptr ptr (fromInteger $ toInteger i16)

pythonateByteString   :: ByteString    -> IO PyObj
pythonateByteString t = Data.ByteString.Unsafe.unsafeUseAsCStringLen t
                        $ uncurry pythonateBytePtr


pythonateInteger      :: Integer -> IO PyObj
pythonateInteger      i
  = if i >= toInteger (minBound :: Int) && i <= toInteger (maxBound :: Int)
    then pythonateInt (fromInteger i)
    else let text = Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
                    $ Data.Text.Lazy.Builder.Int.hexadecimal i
         in Data.Text.Foreign.useAsPtr text pythonateIntegerFromStr'
  where pythonateIntegerFromStr' ptr i16
          = pythonateIntegerFromStr ptr (fromInteger $ toInteger i16)

-- | Pythonate a general dictionary-like type. Requires a function to
-- enumerate key-value pairs from the dictionary, a function to
-- pythonate keys, and a function to pythonate values (and, of course,
-- the dictionary we want to pythonate). In the PythonM monad, not
-- just IO (in other words we correctly handle exceptions, stopping
-- what we're doing and discarding python objects we already
-- constructed that now do not have an owner); the pythonation
-- functions provided by the user must similarly be in the PythonM
-- monad (so the functions above cannot be used without a
-- treatingAsErr nullPtr).

pythonateDictCore :: (d a b -> [(a, b)])
                     -> (a -> PythonM PyObj) -> (b -> PythonM PyObj) -> d a b
                     -> PythonM PyObj
pythonateDictCore enumfn kfn vfn map = do
  let addKVPair dict (key, val) = do
        key_pyo <- kfn key
        releasingOnFail key_pyo $ do
          val_pyo <- vfn val
          releasingOnFail key_pyo $ do
            treatingAsErr (-1) $ pyDict_SetItem dict key_pyo val_pyo

  newDict <- treatingAsErr nullPyObj pyDict_New
  releasingOnFail newDict ((mapM (addKVPair newDict) $ enumfn map) >> return newDict)

-- | Pythonate a Haskell Map. Requires a function to pythonate keys,
-- and a function to pythonate values (and, of course, the dictionary
-- we want to pythonate). In the PythonM monad, not just IO (in other
-- words we correctly handle exceptions, stopping what we're doing and
-- discarding python objects we already constructed that now do not
-- have an owner) the pythonation functions provided by the user must
-- similarly be in the PythonM monad (so the functions above cannot be
-- used without a treatingAsErr nullPtr).

pythonateDict :: (a -> PythonM PyObj) -> (b -> PythonM PyObj) -> Map a b
                 -> PythonM PyObj
pythonateDict = pythonateDictCore Map.toList

-- | Pythonate a Haskell HashMap. Requires a function to pythonate
-- keys, and a function to pythonate values (and, of course, the
-- dictionary we want to pythonate). In the PythonM monad, not just IO
-- (in other words we correctly handle exceptions, stopping what we're
-- doing and discarding python objects we already constructed that now
-- do not have an owner) the pythonation functions provided by the
-- user must similarly be in the PythonM monad (so the functions above
-- cannot be used without a treatingAsErr nullPtr).

pythonateHDict :: (a -> PythonM PyObj) -> (b -> PythonM PyObj) -> HashMap a b
                  -> PythonM PyObj
pythonateHDict = pythonateDictCore HashMap.toList

-- | Build a Python tuple, givne a list of functions (in the PythonM
-- monad) that build the elements of the tuple.

pythonateTuple :: [PythonM PyObj] -> PythonM PyObj
pythonateTuple list = do
  let nElems = length list
  pytup <- treatingAsErr nullPyObj $ pyTuple_New nElems
  releasingOnFail pytup $ do
    let initialize atIndex withValueFactory = do
          value <- withValueFactory
          lift $ pyTuple_SET_ITEM pytup atIndex value
    sequence_ $ zipWith initialize [0..nElems-1] list
    return pytup
