{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module HyphenExceptions (translatePyException, translatePyExceptionIfAny,
                         translatingHsExcepts, captureAsyncExceptions) where

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
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import qualified Control.Exception as Exception
import qualified Unsafe.Coerce

import HyphenBase
import PythonBase
import HsType
import HsObjRaw
import Pythonate
import HyphenWrapping

-- | This module provides for translating exceptions from Python to
-- Haskell and back again.
--
-- The basic plan for doing so is as follows. When Python exceptions
-- cross into Haskell code (as may happen if we wrap a python function
-- and pass it over to the Haskell side), they are wrapped as using
-- the PyException Exception type (which we create); we don't try to
-- 'translate' them into meaningful Haskell exceptions because in this
-- case the Right Thing is almost always for the python exception to
-- run up the stack until it hits Python code again. This basically
-- befits our role as a library for Python code to call Haskell
-- code---it's only in rare cases (wrapping function objects) that we
-- call in the other direction. (Proviso: while we don't translate
-- Python exceptions, if the python exception itself came from a
-- translated Haskell exception---as seen by the presence of an
-- hs_exception member which contains an HsObjRaw which contains a
-- Haskell SomeException---then as it crosses back into Haskell we
-- just pull out the original Haskell exception again.)
--
-- When the reverse crossing takes place (Haskell -> Python), we try
-- to convert the Haskell exception into a Python exception that will
-- be meaningful to the calling Python code. We keep a copy of the
-- originating Haskell exception in the hs_exception member of the
-- Python exception we create, so that Python code can look at it, if
-- it is so minded...

-- | A PyException represents a Python exception; it consists of three
-- PyObjs (type, value, traceback).

data PyException = PyException (ForeignPtr PyObj_Contents) (ForeignPtr PyObj_Contents) (ForeignPtr PyObj_Contents)
                 deriving (Show, Typeable)

instance Exception PyException

-- | It's helpful to have a name for an HsType which refers to the
-- Haskell type SomeException.

someExceptionHsType :: HsType
someExceptionHsType = hsTypeFromSimpleTypeRep $ typeOf $ (undefined :: SomeException)

-- | This function should only be called when the Python exception
-- condition is set. It fetches the python exception (clearing the
-- python exception condition) and transforms the fetched exception
-- into a Haskell exception, which it throws. (Normally 'transforming'
-- just means wrapping in a PyException; the special case is that
-- python exceptions that came from a translated Haskell
-- exception---as seen by the presence of an hs_exception member which
-- contains an HsObjRaw which contains a Haskell SomeException---are
-- 'transformed' by pulling out the original Haskell exception.)

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
                    py_DECREF_whenNotNull pyobj 
                      = when (pyobj /= nullPyObj) $ py_DECREF pyobj
                py_DECREF =<< peek store_type
                py_DECREF_whenNotNull =<< peek store_value
                py_DECREF_whenNotNull =<< peek store_trace
                Exception.throwIO ex
              _                            -> return ()
        type_fp  <- newForeignPtr addr_py_DECREF =<< peek store_type
        value_fp <- newForeignPtr addr_py_DECREF =<< peek store_value
        trace_fp <- newForeignPtr addr_py_DECREF =<< peek store_trace
        Exception.throwIO $ PyException type_fp value_fp trace_fp
        )))


pyErr_RestoreWrapped :: PyException -> IO ()
pyErr_RestoreWrapped (PyException type_fp value_fp trace_fp) 
  = withForeignPtr type_fp (
    \ type_obj -> withForeignPtr value_fp (
      \ value_obj -> withForeignPtr trace_fp (
        \ trace_obj -> do
          py_INCREF type_obj
          when (value_obj /= nullPyObj) $ py_INCREF value_obj
          when (trace_obj /= nullPyObj) $ py_INCREF trace_obj
          pyErr_Restore type_obj value_obj trace_obj
        )))


-- | Convert an action in the PythonM monad to an IO action which (a)
-- carries out the PythonM action, and (b) if it succeeds sucessfully,
-- returns the result, but (c) if the monadic return is Nothing,
-- indicating that the computation raised a python exception at some
-- point, transforms that to a Haskell exception and throws
-- it. (Normally 'transforming' just means wrapping in a PyException;
-- the special case is that python exceptions that came from a
-- translated Haskell exception---as seen by the presence of an
-- hs_exception member which contains an HsObjRaw which contains a
-- Haskell SomeException---are 'transformed' by pulling out the
-- original Haskell exception.)

translatePyExceptionIfAny :: PythonM a -> IO a
translatePyExceptionIfAny = maybe translatePyException return <=< runMaybeT

-- | Given an operation in the PythonM monad which may additionally
-- raise Haskell exceptions, catch all Haskell exceptions and
-- translate them into appropriate Python exceptions, which are raised
-- in the usual way (by setting the exception state within the Pyhton
-- interpreter, and setting the PythonM state to 'Nothing' to indicate
-- that an exception was returned).

translatingHsExcepts :: PythonM a -> PythonM a
translatingHsExcepts = MaybeT . flip catches handlers . runMaybeT
  where
    handlers = [
      Handler (\ e@(PyException _ _ _) -> pyErr_RestoreWrapped e >> return Nothing),
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

-- | Generic code for setting a python exception which came from a
-- Haskell exception. We take a function (in the IO monad) which
-- constructs the (python) type of the exception we wish to raise, a
-- list of functions in the PythonM monad which construct the various
-- arguments of the exception (if one of these functions itself raises
-- an exception, then that exception will be raised instead of the
-- exception were in the process of trying to raise), and the original
-- Haskell exception that was translated into the python
-- exception. (The latter will be wrapped as an HsObjRaw and placed in
-- teh hs_exception member of the newly created Python exception, so
-- that the original Haskell exception is not lost.)

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

-- | Set an exception from a Haskell exception. Same as setPyExcRaw,
-- but assumes that the python exception which we're raising will
-- take, as its arguments, the 'show' of the original Haskell
-- exception.

setPyExc :: (Exception e) => IO PyObj -> e -> IO (Maybe x)
setPyExc exTyFn e = setPyExcRaw exTyFn [treatingAsErr nullPyObj . pythonateString $ show e] e

-- | Raise a python system exit exception, with the error code as
-- given by the (Haskell) ErrorCode provided.

setSystemExit :: ExitCode -> IO (Maybe x)
setSystemExit ecode =
  setPyExcRaw exSystemExit [treatingAsErr nullPyObj . pythonateInt $ codeOf ecode] ecode
  where codeOf (ExitFailure i) = i
        codeOf (ExitSuccess)   = 0


-- | It is imperative that every function that we foreign export must
-- handle any exception raised within it before it hits C, since
-- trying to propagate exceptions out of a foreign exported function
-- is a fatal error. None of the hyphen-proper code that we run (as
-- opposed to user haskell code that we call into) will raise
-- synchronous exceptions, and any exceptions from user Haskell code
-- are converted to python exceptions well before they hit C. But it
-- is always possible that an asynchronous exception might be raised
-- while we're running our hyphen code, so we must handle these (even
-- though our code runs for only a very short amount of time, so
-- exceptions are pretty unlikely to hit while it is
-- running). Moreover, an exception *might* arise even after we've
-- constructed a python object to return, in which case we will need
-- to dispose of that python object (so as not to leak memory) and
-- provide a return value consistent with the fact that we have set
-- the error indicator.

captureAsyncExceptions :: forall a b. (b -> IO b)
                          -> ((IO a -> IO a) -> IO b)
                          -> IO b
captureAsyncExceptions dispose action = Exception.mask $ \restore -> do
  result    <- action restore
  interrupt <- Exception.try Exception.allowInterrupt
  let setPyExcAndEatFurtherExceptions :: SomeException -> IO ()
      setPyExcAndEatFurtherExceptions ex = do setPyExc exHsException ex
                                              eatFurtherExceptions
      -- Once we have been hit by one async exception, we eat any
      -- further exceptions that arise between that point and where we
      -- exit into python. (Raising one async exception in Python is
      -- good enough!)
      eatFurtherExceptions :: IO ()
      eatFurtherExceptions 
        = do interrupt' <- Exception.try Exception.allowInterrupt
             case (interrupt' :: Either Exception.SomeException ())  of
               Left ex   -> eatFurtherExceptions
               Right ()  -> return ()
                                  
  case interrupt of
    Left ex  -> dispose result >>. setPyExcAndEatFurtherExceptions ex
    Right () -> return result
