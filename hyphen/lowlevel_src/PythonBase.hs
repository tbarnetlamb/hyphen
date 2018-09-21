{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : PythonBase
Description : Low-level Haskell bindings to the Python C API
-}

module PythonBase where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Data.Text        (Text)
import Foreign.Ptr
import Foreign.C.String (CString, withCString)
import qualified Data.Text            as T

import HyphenBase

--------

-- | The type PyObj represents a python object; it's defined as a
-- pointer type and the type to which it points is (via some newtype
-- trickery) something that Haskell can never create or pull
-- apart. Therefore, only functions that come from the C side can
-- create or unpack PyObjs.

type    PyObj  = Ptr PyObj_Contents
newtype PyObj_Contents = PyObj_Contents PyObj_Contents
nullPyObj = (nullPtr :: PyObj)

-- Many functions pulled over from the C side; many of them simply
-- wrap over Python API functions. See hyphen_c.c. (The reason we wrap
-- the Python API functions here rather than just foreign import
-- directly from the C side is so that the C compiler will check that
-- (say) HsPtr and whatever the Python library is using are compatible
-- types.)

foreign import ccall unsafe c_pyTypeErr          :: CString -> IO PyObj
foreign import ccall unsafe c_pyValueErr         :: CString -> IO PyObj
foreign import ccall safe   c_getHsExceptionAttr :: PyObj -> IO PyObj
foreign import ccall unsafe c_installHaskellCtrlCHandler  :: IO Int
foreign import ccall unsafe c_reinstallPythonCtrlCHandler :: IO Int
foreign import ccall safe   c_makeHaskellText    :: PyObj -> IO PyObj
foreign import ccall unsafe c_isThisTheMainPythonThread :: IO Bool
foreign import ccall unsafe pyErr_NoMemory       :: IO PyObj
foreign import ccall unsafe pyErr_Fetch          :: Ptr PyObj -> Ptr PyObj -> Ptr PyObj -> IO ()
foreign import ccall safe   pyErr_NormalizeException
                                          :: Ptr PyObj -> Ptr PyObj -> Ptr PyObj -> IO ()
foreign import ccall unsafe pyErr_Restore        :: PyObj -> PyObj -> PyObj -> IO ()
foreign import ccall safe   pyErr_SetObject      :: PyObj -> PyObj -> IO ()
foreign import ccall safe   pyErr_CheckSignals   :: IO Int
foreign import ccall unsafe pyTuple_New          :: Int -> IO PyObj
foreign import ccall unsafe pyTuple_GET_ITEM     :: PyObj -> Int -> IO PyObj
foreign import ccall unsafe pyTuple_SET_ITEM     :: PyObj -> Int -> PyObj -> IO ()
foreign import ccall unsafe pyTuple_Size         :: PyObj -> IO Int
foreign import ccall unsafe pyUnicode_Check      :: PyObj -> IO Bool
foreign import ccall unsafe pyCallable_Check     :: PyObj -> IO Bool
foreign import ccall safe   pyObject_Str         :: PyObj -> IO PyObj
foreign import ccall safe   pyObject_SetAttr     :: PyObj -> PyObj -> PyObj -> IO Int
foreign import ccall safe   pyObject_Call        :: PyObj -> PyObj -> PyObj -> IO PyObj
foreign import ccall unsafe pyDict_New           :: IO PyObj
foreign import ccall unsafe pyDict_Next  :: PyObj -> Ptr Int -> Ptr PyObj -> Ptr PyObj -> IO Int
foreign import ccall safe   pyDict_SetItem       :: PyObj -> PyObj -> PyObj -> IO Int -- safe because we need to compute a hash
foreign import ccall safe   "&py_DECREF_with_GIL_acq" addr_py_DECREF :: FunPtr (PyObj -> IO ())
foreign import ccall safe   py_DECREF            :: PyObj -> IO ()
foreign import ccall unsafe py_INCREF            :: PyObj -> IO ()
foreign import ccall safe   pyModule_AddObject   :: PyObj -> CString -> PyObj -> IO Int
foreign import ccall unsafe pyGILState_Ensure    :: IO (Ptr ())
foreign import ccall unsafe pyGILState_Release   :: Ptr () -> IO ()
foreign import ccall unsafe pyEval_SaveThread    :: IO (Ptr ())
foreign import ccall unsafe pyEval_RestoreThread :: Ptr () -> IO ()

foreign import ccall unsafe exHsException        :: IO PyObj
foreign import ccall unsafe exKeyboardInterrupt  :: IO PyObj
foreign import ccall unsafe exOverflowError      :: IO PyObj
foreign import ccall unsafe exZeroDivisionError  :: IO PyObj
foreign import ccall unsafe exFloatingPointError :: IO PyObj
foreign import ccall unsafe exAttributeError     :: IO PyObj
foreign import ccall unsafe exSystemExit         :: IO PyObj
foreign import ccall unsafe exEOFError           :: IO PyObj

foreign import ccall unsafe py_NotImplemented    :: IO PyObj
foreign import ccall unsafe py_None              :: IO PyObj

-- | Set a python TypeError Exception with the provided Text

pyTypeErr :: Text -> IO PyObj
pyTypeErr str = withCString (T.unpack str) c_pyTypeErr

-- | Set a python ValueError Exception with the provided Text

pyValueErr :: Text -> IO PyObj
pyValueErr str = withCString (T.unpack str) c_pyValueErr

-- | Monad for conveniently working with IO actions which may cause an
-- exception state to be set within the python interpreter. In general
-- in python, actions can raise exceptions; when they do this they
-- both set an exception state in the python interpreter and
-- (generally) give a return value indicating abnormal
-- completion. When a called function indicates abnormal completion,
-- it is the caller's responsibility to stop what it's doing and to
-- *itself* return an abnormal completion. This continues until some
-- caller wishes to catch the exception.
--
-- We use PythonM = MaybeT IO as a monad to keep track of the
-- bookeeping for us. More precisely, a well-behaved PythonM IO action
-- should return Nothing in the MaybeT wrapper if, and only if, an
-- exception has been set in the python interpreter. Then the
-- automatic short-circuiting in the MaybeT IO monad will
-- automatically take care of the semantic convention outlined in the
-- previous paragraph for us.

type PythonM = MaybeT IO

-- | Convenience function for building PythonM values. Often one has
-- an IO action which has the python interpreter do something; this
-- action may cause the python interpreter to raise an exception state
-- and if so the result is signalled by a particular return
-- value. @treatingAsErr errval action@ performs the IO action
-- @action@; if the return value is anything except @errval@ we assume
-- successful completion; otherwise we assume an exception has been
-- raised.

treatingAsErr :: (Eq a) => a -> IO a -> PythonM a
treatingAsErr errval action = do ans <- lift action
                                 if ans == errval then mzero else return ans

-- | Convenience function. Given an Either ErrMsg a which represents
-- either an anomalous result (encoded via the error message ErrMsg)
-- or a succesful computation of a result a, transform it into a
-- PythonM a value =, where errors are raised as TypeErrors in
-- python.

promoteErr           :: Either ErrMsg a -> PythonM a
promoteErr           =   either ((>> mzero) . lift . pyTypeErr  . getErrMsg) return

-- | Convenience function. Given an Either ErrMsg a which represents
-- either an anomalous result (encoded via the error message ErrMsg)
-- or a succesful computation of a result a, transform it into a
-- PythonM a value =, where errors are raised as ValueErrors in
-- python.

promoteErrAsValueErr :: Either ErrMsg a -> PythonM a
promoteErrAsValueErr =   either ((>> mzero) . lift . pyValueErr . getErrMsg) return

-- | Convenience function. Perform the action @act@ in the PythonM
-- monad. If it fails, then release the python object @to_release@
-- before passing the exception upstream. Otherwise just return the
-- succesfully-computed value.

releasingOnFail :: PyObj -> PythonM a -> PythonM a
releasingOnFail to_release act = MaybeT (releaseOnNothing =<< runMaybeT act)
  where releaseOnNothing Nothing = py_DECREF to_release >> return Nothing
        releaseOnNothing other   = return other

-- | Version of pyTypeError which (a) takes a String not a Text and
-- (b) returns in the PythonM monad. (It will always fail.)

pyTypeErr' :: String -> PythonM a
pyTypeErr' = (>> mzero) . lift . pyTypeErr . T.pack

-- | Check a condition on pain of TypeError

check  :: Bool    -> String -> PythonM ()
check cond = unless cond . pyTypeErr'

-- | Check a condition (in the IO monad) on pain of TypeError

checkM :: IO Bool -> String -> PythonM ()
checkM cond str = do cond' <- lift cond
                     check cond' str
