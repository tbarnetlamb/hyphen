{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Pythonate where

import Control.Monad.State.Strict
import Data.Word
import Data.Text        (Text)
import Data.ByteString  (ByteString)
import Data.Map.Strict  (Map)
import Foreign.Ptr
import Foreign.C.String (CString)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Foreign
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import qualified Data.ByteString.Unsafe

import PythonBase

foreign import ccall pythonateInt        :: Int        -> IO PyObj
foreign import ccall pythonateFloat      :: Float      -> IO PyObj
foreign import ccall pythonateDouble     :: Double     -> IO PyObj
foreign import ccall pythonateUTF16Ptr   :: Ptr Word16 -> Int -> IO PyObj
foreign import ccall pythonateBytePtr    :: CString    -> Int -> IO PyObj
foreign import ccall pythonateTrue       :: IO PyObj
foreign import ccall pythonateFalse      :: IO PyObj
foreign import ccall pythonateIntegerFromStr :: Ptr Word16 -> Int -> IO PyObj

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

pythonateDict :: (a -> PythonM PyObj) -> (b -> PythonM PyObj) -> Map a b
                 -> PythonM PyObj
pythonateDict kfn vfn map = do
  let addKVPair dict (key, val) = do
        key_pyo <- kfn key
        releasingOnFail key_pyo $ do
          val_pyo <- vfn val
          releasingOnFail key_pyo $ do
            treatingAsErr (-1) $ pyDict_SetItem dict key_pyo val_pyo
  
  newDict <- treatingAsErr nullPyObj pyDict_New
  releasingOnFail newDict ((mapM (addKVPair newDict) $ Map.toList map) >> return newDict)

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

