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

foreign import ccall pyHsObjRaw_Check           :: PyObj -> IO Bool
foreign import ccall pyHsType_Check             :: PyObj -> IO Bool
foreign import ccall pyTyCon_Check              :: PyObj -> IO Bool
foreign import ccall parseTupleToPythonHsObjRaw :: PyObj -> IO PyObj
foreign import ccall parseTupleToPythonHsType   :: PyObj -> IO PyObj

formSimpleHsObjRaw :: (Typeable a) => a -> IO PyObj
formSimpleHsObjRaw = wrapPythonHsObjRaw <=< formObjSimple

foreign import ccall c_unwrapPythonTyCon  :: PyObj -> WStPtr
unwrapPythonTyCon  :: PyObj -> IO TyCon
unwrapPythonTyCon  = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonTyCon

foreign import ccall c_wrapPythonTyCon  :: WStPtr -> IO PyObj
wrapPythonTyCon    :: TyCon  -> IO PyObj
wrapPythonTyCon    =  c_wrapPythonTyCon . castStablePtrToPtr <=< newStablePtr

foreign import ccall c_unwrapPythonHsType :: PyObj -> WStPtr
unwrapPythonHsType :: PyObj -> IO HsType
unwrapPythonHsType = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonHsType

foreign import ccall c_wrapPythonHsType  :: WStPtr -> IO PyObj
wrapPythonHsType   :: HsType  -> IO PyObj
wrapPythonHsType   =  c_wrapPythonHsType . castStablePtrToPtr <=< newStablePtr

foreign import ccall c_wrapPythonHsObjRaw  :: WStPtr -> IO PyObj
wrapPythonHsObjRaw   :: Obj -> IO PyObj
wrapPythonHsObjRaw = c_wrapPythonHsObjRaw . castStablePtrToPtr <=< newStablePtr

foreign import ccall c_unwrapPythonHsObjRaw :: PyObj -> WStPtr
unwrapPythonHsObjRaw :: PyObj -> IO Obj
unwrapPythonHsObjRaw = deRefStablePtr . castPtrToStablePtr . c_unwrapPythonHsObjRaw

