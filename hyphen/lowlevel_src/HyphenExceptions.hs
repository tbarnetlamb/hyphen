{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenExceptions (translatePyException, translatePyExceptionIfAny,
                         translatingHsExcepts) where

import System.IO.Error (isEOFError)
import System.Exit
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Control.Exception (
  SomeException, toException, Exception, Handler(..), catches, AsyncException(..),
  ArithException(..), NoMethodError(..), RecConError(..), RecSelError(..),
  RecUpdError(..))
import Data.Typeable (Typeable, typeOf)
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Control.Exception as Exception
import qualified Unsafe.Coerce

import HyphenBase
import PythonBase
import HsType
import HsObjRaw
import Pythonate
import HyphenWrapping

data PyException = PyException PyObj PyObj PyObj
                 deriving (Show, Typeable)

instance Exception PyException

someExceptionHsType :: HsType
someExceptionHsType = hsTypeFromSimpleTypeRep $ typeOf $ (undefined :: SomeException)

translatePyException :: IO a
translatePyException = alloca (
  \store_type -> alloca (
    \store_value -> alloca (
      \store_trace -> do
        pyErr_Fetch              store_type store_value store_trace
        pyErr_NormalizeException store_type store_value store_trace
        hsExceptionPyObj <- c_getHsExceptionAttr =<< peek store_value
        when (hsExceptionPyObj /= nullPyObj) $ do
          pyTypeChecksOut <- pyHsObjRaw_Check hsExceptionPyObj
          when (pyTypeChecksOut) $ do
            hsExceptionObj <- unwrapPythonHsObjRaw hsExceptionPyObj
            case hsExceptionObj of
              (MonoObj ty' ptr) -> when (ty' == someExceptionHsType) $ do
                let ex = Unsafe.Coerce.unsafeCoerce ptr :: SomeException
                Exception.throwIO ex
              _                            -> return ()
        Exception.throwIO =<< (
          return PyException `ap` (peek store_type) `ap` (peek store_value)
                             `ap` (peek store_trace))
        )))

translatePyExceptionIfAny :: PythonM a -> IO a
translatePyExceptionIfAny = maybe translatePyException return <=< runMaybeT

translatingHsExcepts :: PythonM a -> PythonM a
translatingHsExcepts = MaybeT . flip catches handlers . runMaybeT
  where
    handlers = [
      Handler (\ (PyException ty val tr) -> pyErr_Restore ty val tr >> return Nothing),
      Handler (\ e -> case e of
                  StackOverflow -> setPyExc exHsException        e
                  HeapOverflow  -> pyErr_NoMemory >> return Nothing
                  ThreadKilled  -> setPyExc exHsException        e
                  UserInterrupt -> setPyExc exKeyboardInterrupt  e
                  ),
      Handler (\ e -> case e of
                  Overflow      -> setPyExc exOverflowError      e
                  DivideByZero  -> setPyExc exZeroDivisionError  e
                  _             -> setPyExc exFloatingPointError e
                  ),
      Handler (\ ex -> setPyExc exAttributeError (ex :: NoMethodError)),
      Handler (\ ex -> setPyExc exAttributeError (ex :: RecConError  )),
      Handler (\ ex -> setPyExc exAttributeError (ex :: RecSelError  )),
      Handler (\ ex -> setPyExc exAttributeError (ex :: RecUpdError  )),
      Handler (\ e -> if isEOFError e then setPyExc exEOFError e else
                        setPyExc exHsException e),
      Handler (\ ex -> setSystemExit             (ex :: ExitCode) ),
      Handler (\ ex -> setPyExc exHsException    (ex :: SomeException))]

setPyExc :: (Exception e) => IO PyObj -> e -> IO (Maybe x)
setPyExc exTyFn e = setPyExcRaw exTyFn [treatingAsErr nullPyObj . pythonateString $ show e] e

setPyExcRaw :: (Exception e) => IO PyObj -> [PythonM PyObj] -> e -> IO (Maybe x)
setPyExcRaw exTyFn pyExArgs e = (>> return Nothing) . runMaybeT $ do
  exTy   <- lift exTyFn
  args   <- pythonateTuple pyExArgs
  exc    <- treatingAsErr nullPyObj $ pyObject_Call exTy args nullPyObj >>. py_DECREF args
  treatingAsErr () $ do
    str    <- pythonateString "hs_exception"
    unless (str == nullPyObj) $ do
      hsExc  <- (formSimpleHsObjRaw :: SomeException -> IO PyObj) $ toException e
      unless (hsExc == nullPyObj) $ do
        retval <- pyObject_SetAttr exc str hsExc
        unless (retval == (-1)) $
          pyErr_SetObject exTy exc
        py_DECREF hsExc
      py_DECREF str
    py_DECREF exc

setSystemExit :: ExitCode -> IO (Maybe x)
setSystemExit ecode =
  setPyExcRaw exSystemExit [treatingAsErr nullPyObj . pythonateInt $ codeOf ecode] ecode
  where codeOf (ExitFailure i) = i
        codeOf (ExitSuccess)   = 0
