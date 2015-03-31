{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PythonBase where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Text        (Text)
import Foreign.Ptr
import Foreign.C.String (CString, withCString)
import qualified Data.Text            as T

import HyphenBase

type PythonM = MaybeT IO

--------

newtype PyObj_Contents = PyObj_Contents PyObj_Contents
type    PyObj  = Ptr PyObj_Contents
nullPyObj = (nullPtr :: PyObj)

foreign import ccall c_pyTypeErr          :: CString -> IO PyObj
foreign import ccall c_pyValueErr         :: CString -> IO PyObj
foreign import ccall c_getHsExceptionAttr :: PyObj -> IO PyObj
foreign import ccall c_installHaskellCtrlCHandler  :: IO Int
foreign import ccall c_reinstallPythonCtrlCHandler :: IO Int
foreign import ccall pyErr_NoMemory       :: IO PyObj
foreign import ccall pyErr_Fetch          :: Ptr PyObj -> Ptr PyObj -> Ptr PyObj -> IO ()
foreign import ccall pyErr_NormalizeException
                                          :: Ptr PyObj -> Ptr PyObj -> Ptr PyObj -> IO ()
foreign import ccall pyErr_Restore        :: PyObj -> PyObj -> PyObj -> IO ()
foreign import ccall pyErr_SetObject      :: PyObj -> PyObj -> IO ()
foreign import ccall pyErr_CheckSignals   :: IO Int
foreign import ccall pyTuple_New          :: Int -> IO PyObj
foreign import ccall pyTuple_GET_ITEM     :: PyObj -> Int -> IO PyObj
foreign import ccall pyTuple_SET_ITEM     :: PyObj -> Int -> PyObj -> IO ()
foreign import ccall pyTuple_Size         :: PyObj -> IO Int
foreign import ccall pyUnicode_Check      :: PyObj -> IO Bool
foreign import ccall pyCallable_Check     :: PyObj -> IO Bool
foreign import ccall pyObject_Str         :: PyObj -> IO PyObj
foreign import ccall pyObject_SetAttr     :: PyObj -> PyObj -> PyObj -> IO Int
foreign import ccall pyObject_Call        :: PyObj -> PyObj -> PyObj -> IO PyObj
foreign import ccall pyDict_New           :: IO PyObj
foreign import ccall pyDict_Next  :: PyObj -> Ptr Int -> Ptr PyObj -> Ptr PyObj -> IO Int
foreign import ccall pyDict_SetItem       :: PyObj -> PyObj -> PyObj -> IO Int
foreign import ccall "&py_DECREF" addr_py_DECREF :: FunPtr (PyObj -> IO ())
foreign import ccall py_DECREF            :: PyObj -> IO ()
foreign import ccall py_INCREF            :: PyObj -> IO ()
foreign import ccall pyModule_AddObject   :: PyObj -> CString -> PyObj -> IO Int
foreign import ccall pyGILState_Ensure    :: IO (Ptr ())
foreign import ccall pyGILState_Release   :: Ptr () -> IO ()
foreign import ccall pyEval_SaveThread    :: IO (Ptr ())
foreign import ccall pyEval_RestoreThread :: Ptr () -> IO ()

foreign import ccall exHsException        :: IO PyObj
foreign import ccall exKeyboardInterrupt  :: IO PyObj
foreign import ccall exOverflowError      :: IO PyObj
foreign import ccall exZeroDivisionError  :: IO PyObj
foreign import ccall exFloatingPointError :: IO PyObj
foreign import ccall exAttributeError     :: IO PyObj
foreign import ccall exSystemExit         :: IO PyObj
foreign import ccall exEOFError           :: IO PyObj

foreign import ccall py_NotImplemented    :: IO PyObj

pyTypeErr :: Text -> IO PyObj
pyTypeErr str = withCString (T.unpack str) c_pyTypeErr

pyValueErr :: Text -> IO PyObj
pyValueErr str = withCString (T.unpack str) c_pyValueErr

pyTypeErr' :: String -> PythonM a
pyTypeErr' = (>> mzero) . lift . pyTypeErr . T.pack

check  :: Bool    -> String -> PythonM () {- check cond on pain of TypeError -}
check cond = unless cond . pyTypeErr'
      
checkM :: IO Bool -> String -> PythonM () {- check cond on pain of TypeError -}
checkM cond str = do cond' <- lift cond
                     check cond' str

treatingAsErr :: (Eq a) => a -> IO a -> PythonM a
treatingAsErr errval action = do ans <- lift action
                                 if ans == errval then mzero else return ans

promoteErr           :: Either ErrMsg a -> PythonM a
promoteErr           =   either ((>> mzero) . lift . pyTypeErr  . getErrMsg) return

promoteErrAsValueErr :: Either ErrMsg a -> PythonM a
promoteErrAsValueErr =   either ((>> mzero) . lift . pyValueErr . getErrMsg) return

releasingOnFail :: PyObj -> PythonM a -> PythonM a
releasingOnFail to_release act = MaybeT (releaseOnNothing =<< runMaybeT act)
  where releaseOnNothing Nothing = py_DECREF to_release >> return Nothing
        releaseOnNothing other   = return other

