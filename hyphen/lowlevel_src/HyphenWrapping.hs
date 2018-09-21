{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenWrapping where

import Control.Monad
import Data.Typeable     (Typeable)
import Foreign.StablePtr (deRefStablePtr, newStablePtr)

import HyphenBase
import PythonBase
import HyphenTyCon
import HsType
import HsObjRaw

-- | A key element of the low-level haskell-python bridge is that it
-- creates three important new Python types: TyCon, HsType and
-- HsObjRaw. Each of these is a thin wrapper over a stableptr to a
-- Haskell object (of types TyCon, HsType and HsObj respectively). It
-- is useful to have functions from a Haskell TyCon to a PyObj (the
-- Haskell type that represents a pointer to some Python object) which
-- represents a (pointer to a) Python TyCon, and vice versa, and
-- similarly for HsType and HsObjRaw. We define those functions here,
-- using as a basis C functions which pull out the stableptr from a
-- Python TyCon (or HsType, or HsObjRaw) and other C functions which
-- build the wrapping Python TyCon (or HsType, or HsObjRaw) from the
-- stableptr we want inside.
-- 
-- Note that the functions we define here that take PyObjs as their
-- arguments and 'unwrap' them do no checking that the PyObj they are
-- provided with are of the Python types that are expected. (I.e., for
-- example, unwrapPythonTyCon doesn't check that it's applied to a
-- TyCon.) If these functions are applied to objects of the wrong
-- (python) type, undefined behavior will ensue...

-- | We also take the oppotunity to import a few related functions,
-- which check that PyObjs are of type TyCon (or HsType, or HsObjRaw)
-- and which take a Pyhton tuple and make sure that it is a singleton
-- and its only element is a HsObjRaw (say), then pulling out the
-- HsObjRaw (this is useful for parsing argument tuples...)

foreign import ccall unsafe pyHsObjRaw_Check           :: PyObj -> IO Bool
foreign import ccall unsafe pyHsType_Check             :: PyObj -> IO Bool
foreign import ccall unsafe pyTyCon_Check              :: PyObj -> IO Bool
foreign import ccall unsafe parseTupleToPythonHsObjRaw :: PyObj -> IO PyObj
foreign import ccall unsafe parseTupleToPythonHsType   :: PyObj -> IO PyObj

formSimpleHsObjRaw :: (Typeable a) => a -> IO PyObj
formSimpleHsObjRaw = wrapPythonHsObjRaw <=< formObjSimple

foreign import ccall unsafe c_unwrapPythonTyCon  :: PyObj -> WStPtr
unwrapPythonTyCon  :: PyObj -> IO TyCon
unwrapPythonTyCon  = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonTyCon

foreign import ccall unsafe c_wrapPythonTyCon  :: WStPtr -> IO PyObj
wrapPythonTyCon    :: TyCon  -> IO PyObj
wrapPythonTyCon    =  c_wrapPythonTyCon . castStablePtrToPtr <=< newStablePtr

foreign import ccall unsafe c_unwrapPythonHsType :: PyObj -> WStPtr
unwrapPythonHsType :: PyObj -> IO HsType
unwrapPythonHsType = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonHsType

foreign import ccall unsafe c_wrapPythonHsType  :: WStPtr -> IO PyObj
wrapPythonHsType   :: HsType  -> IO PyObj
wrapPythonHsType   =  c_wrapPythonHsType . castStablePtrToPtr <=< newStablePtr

foreign import ccall unsafe c_unwrapPythonHsObjRaw :: PyObj -> WStPtr
unwrapPythonHsObjRaw :: PyObj -> IO HsObj
unwrapPythonHsObjRaw = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonHsObjRaw

foreign import ccall unsafe c_wrapPythonHsObjRaw  :: WStPtr -> IO PyObj
wrapPythonHsObjRaw   :: HsObj -> IO PyObj
wrapPythonHsObjRaw = c_wrapPythonHsObjRaw . castStablePtrToPtr <=< newStablePtr

