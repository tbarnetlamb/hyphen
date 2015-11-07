{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Hyphen () where

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Control.Exception (
  SomeException, toException, Exception, assert, AsyncException(..))
import Control.Concurrent
import Control.DeepSeq
import Data.IORef
import Data.Monoid
import Data.Word
import Data.Maybe
import Data.Hashable
import Data.Char
import Data.Typeable       (Typeable, typeOf)
import Data.Text           (Text)
import Data.ByteString     (ByteString)
import Data.Map.Strict     (Map)
import Data.HashMap.Strict (HashMap)
import Foreign             (newForeignPtr, withForeignPtr)
import Foreign.StablePtr   (deRefStablePtr, newStablePtr, StablePtr)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String    (CString, withCString)
import System.IO.Unsafe    (unsafePerformIO)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Foreign
import qualified Data.Text.Read
import qualified Data.Text.Encoding
import qualified Data.ByteString
import qualified Data.Traversable
import qualified Control.Exception as Exception
import qualified Unsafe.Coerce
import qualified GHC
#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Signals
#endif
import qualified System.Mem.Weak
import qualified GhcMonad

import HyphenBase
import PythonBase
import HyphenKinds
import HyphenTyCon
import HsType
import HsObjRaw
import Pythonate
import HyphenGHC
import HyphenWrapping
import HyphenExceptions

------------------------------------

-- | Like unfoldr, but for a Monadic action

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM fn seed = do res0 <- fn seed
                      case res0 of
                        Nothing          -> return []
                        Just (r, seed')  -> liftM (r :) (unfoldrM fn seed')

-- | Version of pyTypeErr which also formats the type error for you
-- (takes a parameter the object that was actually recieved, and a
-- string describing the type that was expected).

pyTypeErr'' :: String -> PyObj -> PythonM a
pyTypeErr'' exp pyObj = do
  py_str    <- treatingAsErr nullPyObj $ pyObject_Str pyObj
  got       <- textFromPythonObj py_str
  pyTypeErr' $ "Expected " ++ exp ++ ", not " ++ T.unpack got

-- | Convert a python dict, whose keys are python strings, to a Map
-- Txt PyObjin the obvious way. Useful for processing kwargs.

unPythonateKwargs :: PyObj -> PythonM (Map Text PyObj)
unPythonateKwargs dict = if dict == nullPyObj then return Map.empty else do
  liftM Map.fromList . MaybeT $ alloca (
    \store_key -> alloca (
      \store_val -> alloca (
        \store_pos -> runMaybeT $ do
          lift $ poke store_pos 0
          flip unfoldrM () . const $ do
            ret <- lift $ pyDict_Next dict store_pos store_key store_val
            if ret == (0 :: Int) then return Nothing else do
              key   <- lift $ peek store_key
              val   <- lift $ peek store_val
              keyOK <- lift $ pyUnicode_Check key
              unless keyOK $ pyTypeErr'' "string for parameter name" key
              key'  <- textFromPythonObj key
              return $ Just ((key', val), ())
        )))

-- | Convenience function to parse keyword arguments for a python
-- function. Takes a list of strings (the names of allowed keyword
-- arguments), a string (the name of the python function whose kwargs
-- we're parsing; just used for error messages); a Map Text PyObj
-- containing the kwargs as provided by the user (which the user will
-- usually have created by caling unPythonateKwargs on the raw python
-- dict provided by python), we return a [Maybe PyObj]. This list will
-- always have the same length as the list of expected keyword
-- arguments; in each place, it will either provide the PyObj that was
-- provided as the value for the kwarg named in the corresponding
-- position in the list of expected arguments, or Nothing if that
-- argument was not provided.

readKwargs :: [String] -> String -> Map Text PyObj -> PythonM [Maybe PyObj]
readKwargs expected errstr kwargs =
  let expected'  = map T.pack expected
      unexpected = Map.difference kwargs (Map.fromList $ map (\x -> (x,())) expected')
  in case Map.toList unexpected of
    ((name, _):_)  -> pyTypeErr' (
        errstr ++ ": unexpected keyword argument " ++ T.unpack name)
    _              -> return $ map (flip Map.lookup kwargs) expected'


------------------------------

-- | Acquire the GIL and check python signals, raising and translating
-- to Haskell any exceptions that are seen to be necessary in the
-- light of the signal state.

checkPythonSignals :: IO ()
checkPythonSignals = do
  acquiringGIL . translatePyExceptionIfAny . treatingAsErr (-1) $ pyErr_CheckSignals
  return ()

-- | Perform the given IO action, while simultaneously (in a separate
-- thread) keep checking whether any python signal handlers need to
-- run (and if so, ensuring that they do run).
servicingPySignalHandlers :: forall a. IO a -> IO a
servicingPySignalHandlers act = do
  resultMVar <- (newEmptyMVar :: IO (MVar (Maybe (Either SomeException a))))
  endSync    <- (newEmptyMVar :: IO (MVar ()))
  workThrd   <- forkFinally (act >>. putMVar endSync ()) (putMVar resultMVar . Just)
  let loop :: IO (Either SomeException a)
      loop = do forkIO $ (threadDelay 50 >> tryPutMVar resultMVar Nothing >> return ())
                mresult <- takeMVar resultMVar
                case mresult of
                  Just result -> return result
                  Nothing     ->
                    do Exception.catch checkPythonSignals processException
                       loop

      processException :: SomeException -> IO ()
      processException ex =
        do otherStopped <- tryPutMVar endSync () -- try to stop the other thread from ending
           case otherStopped of
             True  -> throwTo workThrd ex -- succeeded; other thread will wait for us to
                                          -- interrupt it
             False -> Exception.throwIO ex
  result <- loop
  case result of
    Left  ex  -> Exception.throwIO ex
    Right ans -> return ans

#if !defined(mingw32_HOST_OS)
-- | Carry out the provided IO action, switching from the currently
-- installed ctrl-C handler (which will usually be the Python one) to
-- the Haskell one, so that any ctrl-C presses while the operation is
-- being performed will result in proper KeyboardInterrupt exceptions
-- being raised. We don't directly install a Haskell handler (as per
-- setupHaskellCtrlCHandler below), but get the C layer to install a
-- special 'compound' Haskell ctrl-C handler. The reason we do this is
-- that the way Haskell actually handles signals is a bit
-- indirect. When we install a Haskell handler, Haskell in turn
-- installs a C handler to get control of the signal. But running the
-- C signal handler that was installed doesn't immediately and
-- directly cause the Haskell handler that was installed to
-- run. Instead it sets a flag, and the Haskell handler will run
-- 'soon', when the RTS hits a context switch (reason for doing things
-- this way is mostly that you're very limited in what you can do in a
-- signal handler). So there's a possibility that, after we re-install
-- the Python handler, we're in a situation where Haskell's C signal
-- handler has seen an exception and set a flag saying that the
-- Haskell handler should run; but we haven't run it yet. We want to
-- make sure that the Haskell handler runs before we quit. This is
-- where the 'compound' handler comes in. It lets us know whether the
-- Haskell-installed C handler was ever triggered, and if it is we
-- wait 1 second before continuing, which gives Haskell time to make
-- sure the Handler is triggered. (Probably waiting 1 second is
-- overkill; anything which invokes the scheduler is enough.)
--
-- (Note that there is a corresponding worry when we switch from the
-- python signal handler to the Haskell one, since the
-- python-installed C signal handler *also* jsut sets a flag and then
-- later processes it. But it's easier this way because Python exposes
-- the functionality to explicitly check the flag, so we just do so
-- immediately after we've installed the Haskell signal handler.)

catchingCtrlC :: IO a -> IO a
catchingCtrlC = Exception.bracket before after . const
  where before       = do main_thread_id <- myThreadId
                          weak_tid <- mkWeakThreadId main_thread_id
                          atomicModifyIORef threadToInterruptStack $
                            \lst -> ((weak_tid:lst), ())
                          c_installHaskellCtrlCHandler >>. checkPythonSignals
        after count1 = do count2 <- c_reinstallPythonCtrlCHandler
                          when (count1 /= count2) $ threadDelay 1000
                          atomicModifyIORef threadToInterruptStack $
                            \lst -> (drop 1 lst, ())

threadToInterruptStack :: IORef [System.Mem.Weak.Weak ThreadId]
threadToInterruptStack = unsafePerformIO (newIORef [])

-- | Set up a Haskell ctrl-C handler, and install it. Used by the C
-- side, which takes a copy of this exception handler and uses it to
-- build a 'compound exception handler'.
foreign export ccall setupHaskellCtrlCHandler :: IO ()
setupHaskellCtrlCHandler = do
  let handler = System.Posix.Signals.Catch $ do
        tti <- readIORef threadToInterruptStack
        case tti of
          []     -> return ()
          (wk:_) -> do
            m <- System.Mem.Weak.deRefWeak wk
            case m of
              Nothing  -> return ()
              Just tid -> Exception.throwTo tid (toException UserInterrupt)
  System.Posix.Signals.installHandler System.Posix.Signals.sigINT handler Nothing
  return ()
#else
catchingCtrlC :: IO a -> IO a
catchingCtrlC = id
#endif

------------------------------

-- | Perform the given IO action, bracketing the action with
-- operations to release and re-acquire the Python Global Interpreter
-- Lock

releasingGIL :: IO a -> IO a
releasingGIL = Exception.bracket pyEval_SaveThread pyEval_RestoreThread . const

-- | Perform the given IO action, bracketing the action with
-- operations to acquire (if not already held) and release (if
-- acquired) the Python Global Interpreter Lock

acquiringGIL :: IO a -> IO a
acquiringGIL = Exception.bracket pyGILState_Ensure' pyGILState_Release . const
  where pyGILState_Ensure' = do st <- pyGILState_Ensure
                                if (st == nullPtr) then Exception.throwIO HeapOverflow
                                  else return st

------------------------------

data GILRule    = GILRuleLazy    | GILRuleFancy                          deriving Enum
data SignalRule = SignalRuleLazy | SignalRuleHaskell | SignalRulePython  deriving Enum

-- | Long running operations in Haskell ought sometimes to be
-- 'protected' in one or both of two ways. Firstly, we might want to
-- take some steps to release the python GIL while we're doing the
-- long-running Haskell operation (since, in pure Haskell code, we're
-- not using it, so other Python threads ought to be able to
-- progress). Secondly, we might want to take some action such that
-- signals (e.g. and interrupt coming from ctrl-C) will be correctly
-- processed while we're working. (See the documentation for why this
-- is not a foregone conclusion). We have two ways of doing this;
-- either we can make sure that Python signal handlers continue to run
-- even while we're running Haskell code, or we can enable the Haskell
-- signal handler while the Haskell code is running.
--
-- A choice of whether we want to release the GIL is encoded as a
-- GILRule ('lazy' means we don't bother to release the GIL; 'fancy'
-- means that we do). A choice of whether we want to take steps to
-- ensure signals are processed, and if so whether we want to do it
-- the 'Haskell way' or the 'Python way', is encoded as a SignalRule
-- (again, 'lazy' means don't bother to take any of these
-- steps). protectLongRunningOperation then takes an IO action and
-- replaces it with a new IO action that is protected in one or both
-- of these ways.

protectLongRunningOperation :: GILRule -> SignalRule -> IO a -> IO a
protectLongRunningOperation gilRule sigRule
  = gilProtection gilRule . sigConversion sigRule
  where gilProtection GILRuleLazy       = id
        gilProtection GILRuleFancy      = releasingGIL
        sigConversion SignalRuleLazy    = id
        sigConversion SignalRuleHaskell = catchingCtrlC
        sigConversion SignalRulePython  = servicingPySignalHandlers

------------------------------

-- | Convenience function; given a function of one argument, make it
-- into a function of two arguments which ignores its second argument
-- in the obvious way.

ignoring2nd :: (a -> c) -> (a -> b -> c)
ignoring2nd fn a _ = fn a

-- | Some standard constants from Python

py_LT = 0 :: Int
py_LE = 1 :: Int
py_EQ = 2 :: Int
py_NE = 3 :: Int
py_GT = 4 :: Int
py_GE = 5 :: Int

-- | To expose a comparison operation to python, you define a
-- 'richcmpfunc' which basically takes the type of comparison test we
-- desire to perform (e.g. py_LT if we're trying to decide whether a
-- <= b) and which then returns a bool saying whether the test comes
-- back positive or negative. The following function builds such a
-- comparison function (of type @PyObj -> PyObj -> Int -> IO PyObj@)
-- for pyobjs that works by converting the given pyobj to a haskell
-- type @a@ that supports @Ord(a)@, and then using compare to do the
-- comparison. We also take a function @ok :: PyObj -> IO Bool@ to
-- decide whether we want to claim to know whether a given object can
-- be compared. (It's important in Python only to claim comparisons
-- that you actually can do...)

compareVia :: Ord a => (PyObj -> IO Bool) -> (PyObj -> IO a)
              -> PyObj -> PyObj -> Int -> IO PyObj
compareVia ok unwrap obj1 obj2 cmp_desired = do
  ok1 <- ok obj1
  ok2 <- ok obj2
  if not (ok1 && ok2) then py_NotImplemented else
    do a1 <- unwrap obj1
       a2 <- unwrap obj2
       pythonateBool $ case cmp_desired of
         0 {-py_LT-} -> case compare a1 a2 of {LT -> True;  EQ -> False; GT -> False;}
         1 {-py_LE-} -> case compare a1 a2 of {LT -> True;  EQ -> True;  GT -> False;}
         2 {-py_EQ-} -> case compare a1 a2 of {LT -> False; EQ -> True;  GT -> False;}
         3 {-py_NE-} -> case compare a1 a2 of {LT -> True;  EQ -> False; GT -> True;}
         4 {-py_GT-} -> case compare a1 a2 of {LT -> False; EQ -> False; GT -> True;}
         5 {-py_GE-} -> case compare a1 a2 of {LT -> False; EQ -> True;  GT -> True;}

-- We now define various operations on tycons. These are generally
-- sent directly via the C layer into the method tables (etc.) that we
-- expose to python. Therefore, the documentation of what these
-- functions do is found in the documentation of the low-level library
-- itself.

foreign export ccall tycon_hash               :: PyObj -> IO Int
tycon_hash       = return . hash                             <=< unwrapPythonTyCon

foreign export ccall tycon_richcmp            :: PyObj -> PyObj -> Int -> IO PyObj
tycon_richcmp    = compareVia pyTyCon_Check unwrapPythonTyCon

foreign export ccall tycon_str                :: PyObj -> IO PyObj
tycon_str        = pythonateText . aug . tyConFullName       <=< unwrapPythonTyCon
  where aug txt = T.concat [T.pack "<hyphen.TyCon object representing ", txt, T.pack ">"]

foreign export ccall tycon_repr               :: PyObj -> IO PyObj
tycon_repr       = pythonateText . tyConRepr                 <=< unwrapPythonTyCon

foreign export ccall tycon_getname            :: PyObj -> Ptr () -> IO PyObj
tycon_getname    = ignoring2nd (pythonateText . tyConName    <=< unwrapPythonTyCon)

foreign export ccall tycon_getmodule          :: PyObj -> Ptr () -> IO PyObj
tycon_getmodule  = ignoring2nd (pythonateText . tyConModule  <=< unwrapPythonTyCon)

foreign export ccall tycon_get_visible_module :: PyObj -> Ptr () -> IO PyObj
tycon_get_visible_module =
                   ignoring2nd (pythonateLoc . tyConLocation <=< unwrapPythonTyCon)
  where pythonateLoc :: TyCLocation -> IO PyObj
        pythonateLoc (InExplicitModuleNamed t) = pythonateText t
        pythonateLoc _                         = py_None

foreign export ccall tycon_getpackage         :: PyObj -> Ptr () -> IO PyObj
tycon_getpackage = ignoring2nd (pythonateText . tyConPackage <=< unwrapPythonTyCon)

foreign export ccall tycon_getarity           :: PyObj -> Ptr () -> IO PyObj
tycon_getarity   = ignoring2nd (pythonateInt  . tyConArity   <=< unwrapPythonTyCon)

foreign export ccall tycon_get_is_cls         :: PyObj -> Ptr () -> IO PyObj
tycon_get_is_cls = ignoring2nd (pythonateBool . tyConIsCls   <=< unwrapPythonTyCon)

foreign export ccall tycon_getkind            :: PyObj -> Ptr () -> IO PyObj
tycon_getkind    = ignoring2nd (
                   pythonateString . kindString . tyConKind  <=< unwrapPythonTyCon)

-- We now define various operations on hstypes. These are generally
-- sent directly via the C layer into the method tables (etc.) that we
-- expose to python. Therefore, the documentation of what these
-- functions do is found in the documentation of the low-level library
-- itself.

foreign export ccall hstype_hash              :: PyObj -> IO Int
hstype_hash      = return . hash               <=< unwrapPythonHsType

foreign export ccall hstype_richcmp           :: PyObj -> PyObj -> Int -> IO PyObj
hstype_richcmp   = compareVia pyHsType_Check unwrapPythonHsType

foreign export ccall hstype_str               :: PyObj -> IO PyObj
hstype_str       = pythonateText . aug . typeName             <=< unwrapPythonHsType
  where aug txt = T.concat [T.pack "<hyphen.HsType object representing ", txt, T.pack ">"]

foreign export ccall hstype_repr              :: PyObj -> IO PyObj
hstype_repr      = pythonateText . hsTypeRepr                 <=< unwrapPythonHsType

foreign export ccall hstype_gettail           :: PyObj -> Ptr () -> IO PyObj
hstype_gettail   = ignoring2nd (
  liftM (fromMaybe nullPyObj) . runMaybeT . pythonateTuple
  . map (treatingAsErr nullPyObj . wrapPythonHsType) . typeTail <=< unwrapPythonHsType)

foreign export ccall hstype_getname           :: PyObj -> Ptr () -> IO PyObj
hstype_getname   = ignoring2nd (pythonateText . typeName        <=< unwrapPythonHsType)

foreign export ccall hstype_getkind           :: PyObj -> Ptr () -> IO PyObj
hstype_getkind   = ignoring2nd (
                      pythonateString . kindString . typeKind   <=< unwrapPythonHsType)

foreign export ccall hstype_getfvs            :: PyObj -> Ptr () -> IO PyObj
hstype_getfvs    = ignoring2nd (
  liftM (fromMaybe nullPyObj) . runMaybeT . pythonateDict
  (treatingAsErr nullPyObj . pythonateText . getVar)
  (treatingAsErr nullPyObj . pythonateString . kindString)
  . typeFreeVars <=< unwrapPythonHsType)

-- | Version of unwrapHsType which (unlike unwrapHsType itself) checks
-- that the python object to which it is applied is really an HsType
-- (unwrapHsType itself just has undefined behavor in that case...)

unwrapHsTypeChecked :: String -> PyObj -> PythonM HsType
unwrapHsTypeChecked errContextMsg val = do
  valOK <- lift $ pyHsType_Check  val
  unless valOK $ pyTypeErr'' ("HsType for " ++ errContextMsg) val
  lift $ unwrapPythonHsType val

-- We continue to define various operations on hstypes...
  
foreign export ccall hstype_subst           :: PyObj -> PyObj -> PyObj -> IO PyObj
hstype_subst self_pyobj args kwargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size args
  check (nArgs == 0) "subst: all arguments must be keyword arguments"
  substs <- Data.Traversable.mapM (unwrapHsTypeChecked "parameter value")
            . Map.mapKeys Var =<< unPythonateKwargs kwargs
  self   <- lift $ unwrapPythonHsType self_pyobj
  promoteErr $ transformTypeAllowedCheck substs self
  lift . wrapPythonHsType $ transformType substs self

foreign export ccall hstype_gethead         :: PyObj -> Ptr () -> IO PyObj
hstype_gethead p _ = do
  hstype <- unwrapPythonHsType p
  case typeHead hstype of
    Left tyc         -> liftM (fromMaybe nullPyObj) . runMaybeT $ do
      pythonateTuple [treatingAsErr nullPyObj . pythonateText . f $ tyc
                     | f <- [tyConName, tyConModule, tyConPackage]]
    Right (Var v, k) -> pythonateText  v

foreign export ccall hstype_gethead_ll      :: PyObj -> Ptr () -> IO PyObj
hstype_gethead_ll p _ = do hstype <- unwrapPythonHsType p
                           case typeHead hstype of
                             Left tyc         -> wrapPythonTyCon tyc
                             Right (Var v, k) -> pythonateText  v

-- We now define various operations on hsobjraws. These are generally
-- sent directly via the C layer into the method tables (etc.) that we
-- expose to python. Therefore, the documentation of what these
-- functions do is found in the documentation of the low-level library
-- itself.

foreign export ccall hsobjraw_gethstype     :: PyObj -> Ptr () -> IO PyObj
hsobjraw_gethstype = ignoring2nd (wrapPythonHsType . objType <=< unwrapPythonHsObjRaw)

foreign export ccall hsobjraw_narrow        :: PyObj -> PyObj -> IO PyObj
hsobjraw_narrow pyobj pyargtuple = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  pyhstype <- treatingAsErr nullPyObj $ parseTupleToPythonHsType pyargtuple
  ty       <- lift $ unwrapPythonHsType pyhstype
  obj      <- lift $ unwrapPythonHsObjRaw pyobj
  obj'     <- resolveToType ty obj
  lift $ wrapPythonHsObjRaw obj'

foreign export ccall hsobjraw_subst         :: PyObj -> PyObj -> PyObj -> IO PyObj
hsobjraw_subst self_pyobj args kwargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size args
  check (nArgs == 0) "subst: all arguments must be keyword arguments"
  substs <- Data.Traversable.mapM (unwrapHsTypeChecked "parameter value")
            . Map.mapKeys Var =<< unPythonateKwargs kwargs
  obj    <- lift $ unwrapPythonHsObjRaw self_pyobj
  promoteErr $ transformTypeAllowedCheck substs (objType obj)
  obj'   <- transformObjTypes substs obj
  lift $ wrapPythonHsObjRaw obj'

foreign export ccall hsobjraw_new           :: PyObj -> PyObj -> (Ptr WStPtr) -> IO Int
hsobjraw_new args kwds sptr_loc = liftM (fromMaybe (-1)) . runMaybeT $ do
  kwargs          <- unPythonateKwargs kwds
  nArgs           <- treatingAsErr (-1) $ pyTuple_Size args
  check (nArgs == 1) $ "HsObjRaw.__new__: must have at exactly one argument."
  argpyobj        <- lift $ pyTuple_GET_ITEM args 0
  typeRight       <- lift $ pyHsObjRaw_Check argpyobj
  check (typeRight)  $ "HsObjRaw.__new__: parameter must be a HsObjRaw."
  lift (
    poke sptr_loc . castStablePtrToPtr =<< newStablePtr =<< unwrapPythonHsObjRaw argpyobj)
  return 0

foreign import ccall c_makeHaskellText :: PyObj -> IO PyObj

-- | It's convenient to have a Haskell function that takes a PyObj
-- which wraps a python string, and pulls out a Haskell Text
-- representing the same string. We do this in a somewhat inefficient
-- manner (which doesn't really matter, since we only use this in
-- making error messages and other far-from-inner-loop operations). In
-- particular, we build a tuple containing the python string in
-- question, then call to_haskell_Text (which builds an HsObjRaw
-- wrapping a Text representing the stirng in question) and finally
-- unpack and discard the HsObjRaw to get the Text we want. The
-- c_makeHaskellText helper function imported above from C does
-- everything except unwrap the HsObjRaw.

textFromPythonObj :: PyObj -> PythonM Text
textFromPythonObj pyobj =
  do pyobj_hsstr  <- treatingAsErr nullPyObj $ c_makeHaskellText pyobj
     lift $ do
       MonoObj ty ptr <- unwrapPythonHsObjRaw pyobj_hsstr
       py_DECREF pyobj_hsstr
       return (Unsafe.Coerce.unsafeCoerce ptr :: Text)

-- | Given a string which is NOT a valid Haskell type variable, return
-- Just s, where s is a string containing an explanation of why
-- not. Otherwise, return Nothing.

invalidTyVarMsg :: String -> Maybe String
invalidTyVarMsg varString
  | not (all isAlphaNum varString)       = Just "contains illegal characters."
  | varString == ""                      = Just "is empty."
  | not (all isLower $ take 1 varString) = Just "does not begin with a lower-case letter."
  | otherwise                            = Nothing

-- | Given a string which might or might not be a valid Haskell type
-- variable, determine whether it is or it isn't. If it is, do
-- nothing. If it isn't, create a nice exception with a good error
-- message explaining why not.

throwOnInvalidTyVar :: Text -> PythonM ()
throwOnInvalidTyVar varString = case invalidTyVarMsg (T.unpack varString) of
  Just errMsg -> lift (
    pyValueErr . T.pack $ "Type variable '" ++ T.unpack varString ++ "'" ++ errMsg
    ) >> mzero
  _           -> return ()

-- | Interpret an argument given to a TyCon which has been used as a
-- function. (Using Tycons as functions is one of the ways we can
-- construct types in Hyphen; the other is by calling the HsType
-- object itself.) Each Python argument should either be an HsType
-- python object (which represents a Haskell type in the obvious way)
-- or a string which is a valid name for a type variable (which
-- represents the HsType consisting of that type variable, with
-- whatever kind that argument is required to have by the TyCon which
-- is being called. This function interprets an argument PyObj as an
-- HsType in this way, raising a nice type error if the argument is
-- not legal according to the above rules. (It doesn't check that the
-- kind of an HsType provided is correct; that will be done
-- elsewhere.) The Kind parameter is the expected kind of the
-- argument, only used if the arugment is a string.

interpretTyConCallArgument :: Kind -> PyObj -> PythonM HsType
interpretTyConCallArgument expectedKind pyObj
  = do is_hsType <- lift $ pyHsType_Check  pyObj
       is_str    <- lift $ pyUnicode_Check pyObj
       unless (is_hsType || is_str) $ pyTypeErr'' "HsType or str" pyObj
       if is_hsType then lift (unwrapPythonHsType pyObj) else do
         varString <- textFromPythonObj pyObj
         throwOnInvalidTyVar varString
         return $ mkHsType (Right $ (Var varString, expectedKind)) []

-- | Given a PyObj representing a TyCon and a list of PyObjs
-- representing parameters with which the TyCon has been called, carry
-- out the call to construct a new type. See
-- interpretTyConCallArgument docstring above for what's a valid
-- argument and how it is interpreted.

constructHsTypeFromTycon :: PyObj -> [PyObj] -> PythonM HsType
constructHsTypeFromTycon head_pyobj tail_list_pyobj = do
  head            <- lift $ unwrapPythonTyCon head_pyobj
  check (length tail_list_pyobj <= length (kindArgKinds $ tyConKind head)) (
    "Too many type arguments provided.")
  tail_list       <- sequence $ zipWith interpretTyConCallArgument
                         (kindArgKinds $ tyConKind head) tail_list_pyobj
  promoteErrAsValueErr $ mkHsTypeSafe (Left head) tail_list

-- | Code for handling a user's attempt to call a TyCon
-- object. Basically just call constructHsTypeFromTycon above.

foreign export ccall tycon_call             :: PyObj -> PyObj -> PyObj -> IO PyObj
tycon_call self_pyobj args kwargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  check (kwargs == nullPyObj)
    "When calling a type constructor, keyword arguments not supported"
  nArgs           <- treatingAsErr (-1) $ pyTuple_Size args
  tail_list_pyobj <- mapM (lift . pyTuple_GET_ITEM args) [0..nArgs-1]
  lift . wrapPythonHsType =<< constructHsTypeFromTycon self_pyobj tail_list_pyobj

-- | Code to implement callign the HsType type directly to construct a
-- new HsType. The first parameter must either be a TyCon or a string
-- (which is a valid type variable). If the first parameter is a
-- TyCon, we essentially simulate the effect of calling the TyCon with
-- the remaining arguments (by invoking constructHsTypeFromTycon). If
-- the first parameter is a string, then we will construct an HsType
-- whose head is a type variable (unusual in Haskell, but certainly
-- within the rules). In this case, we support a 'kind' keyword
-- argument, whereby the user can specify the kind of the final
-- returned type (and hence, implicitly, the kind of the type variable
-- used as the head, which would otherwise not be completely
-- determined). In this case, all the arguments must be genuine
-- HsTypes since if a string is used (to represent a type variable) we
-- will not be able to determine its kind from context. So a user
-- should write @HsType('a', HsType('b', kind='*'), kind-='*')@, not
-- @HsType('a', 'b', kind='*')@.

foreign export ccall hstype_new             :: PyObj -> PyObj -> (Ptr WStPtr) -> IO Int
hstype_new args kwds sptr_loc = liftM (fromMaybe (-1)) . runMaybeT $ do
  kwargs          <- unPythonateKwargs kwds
  nArgs           <- treatingAsErr (-1) $ pyTuple_Size args
  check (nArgs > 0) $ "HsType.__new__: must have at least one argument."
  head_pyobj      <- lift $ pyTuple_GET_ITEM args 0
  tail_list_pyobj <- mapM (lift . pyTuple_GET_ITEM args) [1..nArgs-1]
  headIsTyCon     <- lift $ pyTyCon_Check head_pyobj
  lift . poke sptr_loc . castStablePtrToPtr =<< lift . newStablePtr =<< if headIsTyCon
    then do check (kwds == nullPyObj) (
              "When constructing an HsType with head an explicit TyCon, no " ++
              "keyword arguments are allowed")
            constructHsTypeFromTycon head_pyobj tail_list_pyobj
    else do headVar     <- textFromPythonObj head_pyobj
            [kindPyStr] <- readKwargs ["kind"] "HsType.__new__" kwargs
            throwOnInvalidTyVar headVar
            let processTailArg :: PyObj -> PythonM HsType
                processTailArg pyObj = do
                  is_hstype <- lift $ pyHsType_Check pyObj
                  if is_hstype then lift (unwrapPythonHsType pyObj) else
                    pyTypeErr'' "HsType" pyObj
            tail_list <- mapM processTailArg tail_list_pyobj
            let interp   :: PyObj -> PythonM Kind
                interp   = (promoteErr . kindFromText) <=< textFromPythonObj <=<
                           treatingAsErr nullPyObj . pyObject_Str
            (Kind ks)   <- maybe (return $ Kind []) interp kindPyStr
            let headKind = Kind (map typeKind tail_list ++ ks)
            promoteErrAsValueErr $
              mkHsTypeSafe (Right (Var headVar, headKind)) tail_list
  return 0

------------------------------------------------------------------

-- | As part of initializing the hslowlevel module, we want to add to
-- it HsType objects representing all the basic Haskell types like
-- Bool and Char. The following function is used to add a single such
-- function. It is polymorphic, and by providing arguments of
-- appropriate types we can force the type variable @a@ to match the
-- type for which we wish to construct an HsType to insert into the
-- module. This must be a 'simple type' (that is, all the Type
-- Constructors used in it are fully saturated). The first parameter
-- is the module we wish to insert the HsType into; the String is the
-- name under which we should insert it.

addSimpleHsTypeObjToModule :: Typeable a => PyObj -> String -> (a -> a) -> PythonM ()
addSimpleHsTypeObjToModule module_ name ifn = do
  let hsType = hsTypeFromSimpleTypeRep $ typeOf (ifn undefined)
  pyHsType <- treatingAsErr nullPyObj $ wrapPythonHsType hsType
  releasingOnFail pyHsType $ do
    treatingAsErr (-1) . withCString name $ \cStrName ->
      pyModule_AddObject module_ cStrName pyHsType
  return ()

-- | As part of initializing the hslowlevel module, we want to add to
-- it HsType objects representing all the basic Haskell types like
-- Bool and Char. We do this with the following function.

foreign export ccall addSimpleHsTypeObjsToModule :: PyObj -> IO Int
addSimpleHsTypeObjsToModule module_ = liftM (fromMaybe (-1)) . runMaybeT $ do
  addSimpleHsTypeObjToModule module_ "hstype_Bool"       (id :: Bool       -> Bool)
  addSimpleHsTypeObjToModule module_ "hstype_Char"       (id :: Char       -> Char)
  addSimpleHsTypeObjToModule module_ "hstype_String"     (id :: String     -> String)
  addSimpleHsTypeObjToModule module_ "hstype_Text"       (id :: Text       -> Text)
  addSimpleHsTypeObjToModule module_ "hstype_ByteString" (id :: ByteString -> ByteString)
  addSimpleHsTypeObjToModule module_ "hstype_Int"        (id :: Int        -> Int)
  addSimpleHsTypeObjToModule module_ "hstype_Integer"    (id :: Integer    -> Integer)
  addSimpleHsTypeObjToModule module_ "hstype_Float"      (id :: Float      -> Float)
  addSimpleHsTypeObjToModule module_ "hstype_Double"     (id :: Double     -> Double)
  return 0

-- | Given an HsObjRaw, check that the underlying Haskell object is of
-- some particular type and then extract the object and perform some
-- Haskell operation on it. Before the operation is performed, we
-- reduce the HsObjRaw to normal form (i.e. de-lazy-fy it): this is
-- appropriate for the intended use cases, because the intention is
-- that the operation we're about to perform is to convert it to a
-- Python object, which is necessarily non-lazy. While forcing the
-- object (which may involve a lot of Haskell computation) we may
-- release the GIL and/or switch on the Haskell event handler or
-- service Python events. The configuration for which fo these to do
-- is given by providing a GILRule and SignalRule, although these are
-- provided encoded as Ints for easier interoperating with C.

withHsObjRawOfType :: (NFData a) =>
                      HsType      -- ^ The expected type of the HsObjRaw;
                                  -- must match/encode type @a@ in type of
                                  -- next argument
                      -> (a -> IO PyObj) -- ^ The function to apply, returning a PyObj
                      -> Int      -- ^ The GILRule    desired (in Int form via fromEnum)
                      -> Int      -- ^ The SignalRule desired (in Int form via fromEnum)
                      -> PyObj    -- ^ Python object which contians
                                  -- the HsObjRaw to which we do the
                                  -- work.
                      -> IO PyObj
withHsObjRawOfType ty f gilCode sigCode pyargtuple
  = liftM (fromMaybe nullPyObj) . runMaybeT $ do
      pyobj <- treatingAsErr nullPyObj $ parseTupleToPythonHsObjRaw pyargtuple
      obj   <- lift $ unwrapPythonHsObjRaw pyobj
      let protectLro = protectLongRunningOperation (toEnum gilCode) (toEnum sigCode)
          processObj obj = do protectLro (Exception.evaluate $ force obj)
                              f obj
      translatingHsExcepts  $ case obj of
        MonoObj ty' ptr ->
          if ty == ty'
          then (treatingAsErr nullPyObj . processObj $ Unsafe.Coerce.unsafeCoerce ptr)
          else pyTypeErr' $ "Expected type " ++ T.unpack (typeName ty)
            ++ "; got " ++ T.unpack (typeName ty')
        _ ->   pyTypeErr' $ "Only monomorphic objects supported."


-- | As withHsObjRawOfType except the HsType is automatically inferred
-- from the type variable @a@; note that this requires that @a@ have a
-- 'simple type' (all the Type Constructors used in the type are fully
-- saturated).

withHsObjRawSimp :: (Typeable a, NFData a) =>
                    (a -> IO PyObj) -> Int -> Int -> PyObj -> IO PyObj
withHsObjRawSimp fn = let
  typeConstrainer :: (a -> b) -> a
  typeConstrainer = const undefined
  in withHsObjRawOfType (hsTypeFromSimpleTypeRep $ typeOf $ typeConstrainer fn) fn

-- Now provide the core of the implementation of the from_haskell_X
-- functions that will be exposed to Python. In the C layer, the
-- from_haskell_X function (for whatever X) will be defined as a thin
-- wrapper around the from_haskell_X_impl function that we will
-- define. The wrapper simply discards the un-needed PyObj which
-- contains the module itself, and provides two Ints, which are
-- encoded versions of the GILRule and SignalRule in force.

foreign export ccall from_haskell_Bool_impl       :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Bool_impl        = withHsObjRawSimp pythonateBool

foreign export ccall from_haskell_Char_impl       :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Char_impl        = withHsObjRawSimp pythonateChar

foreign export ccall from_haskell_String_impl     :: Int -> Int -> PyObj -> IO PyObj
from_haskell_String_impl      = withHsObjRawSimp pythonateString

foreign export ccall  from_haskell_Text_impl      :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Text_impl        = withHsObjRawSimp pythonateText

foreign export ccall from_haskell_ByteString_impl :: Int -> Int -> PyObj -> IO PyObj
from_haskell_ByteString_impl  = withHsObjRawSimp pythonateByteString

foreign export ccall from_haskell_Int_impl        :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Int_impl         = withHsObjRawSimp pythonateInt

foreign export ccall from_haskell_Integer_impl    :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Integer_impl     = withHsObjRawSimp pythonateInteger

foreign export ccall from_haskell_Float_impl      :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Float_impl       = withHsObjRawSimp pythonateFloat

foreign export ccall from_haskell_Double_impl     :: Int -> Int -> PyObj -> IO PyObj
from_haskell_Double_impl      = withHsObjRawSimp pythonateDouble

-- Now implement the buildHaskellX functions, which are used to create
-- Python HsObjRaw objects containing particular Haskell values, where
-- the value is provided in some C-compatile way.

foreign export ccall buildHaskellBool       :: Bool   -> IO PyObj
buildHaskellBool       = formSimpleHsObjRaw

foreign export ccall buildHaskellChar       :: Char   -> IO PyObj
buildHaskellChar       = formSimpleHsObjRaw

-- String is provided by C caller as Ptr to UTF16 and a length (in double-byte words).

foreign export ccall buildHaskellString     :: Ptr Word16 -> Int -> IO PyObj
buildHaskellString     = curry (
  formSimpleHsObjRaw . T.unpack
  <=< uncurry Data.Text.Foreign.fromPtr . second (fromInteger . toInteger))

-- String is provided by C caller as Ptr to UTF16 and a length (in double-byte words).

foreign export ccall buildHaskellText       :: Ptr Word16 -> Int -> IO PyObj
buildHaskellText       = curry (
  formSimpleHsObjRaw
  <=< uncurry Data.Text.Foreign.fromPtr . second (fromInteger . toInteger))

-- String is provided by C caller as Ptr to bytes and a length (in bytes).

foreign export ccall buildHaskellByteString :: CString -> Int -> IO PyObj
buildHaskellByteString = curry (formSimpleHsObjRaw <=< Data.ByteString.packCStringLen)

foreign export ccall buildHaskellInt        :: Int     -> IO PyObj
buildHaskellInt        = formSimpleHsObjRaw

foreign export ccall buildHaskellInteger    :: Int     -> IO PyObj
buildHaskellInteger    = formSimpleHsObjRaw . toInteger

-- Build Integer from a String containing an ASCII hex encoding of the number.

foreign export ccall buildHaskellIntegerStr :: CString -> Int -> IO PyObj
buildHaskellIntegerStr = curry (
  formSimpleHsObjRaw . readHex . Data.Text.Encoding.decodeUtf8
  <=< Data.ByteString.packCStringLen)
  where readHex :: Text -> Integer
        readHex = (\ (Right (a, t)) -> a) . Data.Text.Read.hexadecimal

foreign export ccall buildHaskellFloat      :: Float   -> IO PyObj
buildHaskellFloat    = formSimpleHsObjRaw

foreign export ccall buildHaskellDouble     :: Double  -> IO PyObj
buildHaskellDouble   = formSimpleHsObjRaw

------------------------------------------------------------------

-- | When we import a Haskell module, we end up with a pair (HashMap
-- Text HsObj, HashMap Text TyNSElt) where the first hashmap is the
-- object namespace and the second is the type namespace; in this, a
-- TyNSElt is simply Either TyCon HsType, where a TyCon is what you
-- might expect and an HsType represents a type synonym/typedef (with
-- the free variables of the HsType corresponding to the parameters of
-- the typedef). We want to be able to convert such a thing to a
-- python object, which is what pythonateModuleRet does.

pythonateModuleRet :: (HashMap Text HsObj, HashMap Text TyNSElt) -> PythonM PyObj
pythonateModuleRet (objs, tycelts) = pythonateTuple [
  prepareDict (treatingAsErr nullPyObj . wrapPythonHsObjRaw) objs,
  prepareDict (treatingAsErr nullPyObj . either wrapPythonTyCon wrapPythonHsType) tycelts]
  where prepareDict = pythonateHDict (treatingAsErr nullPyObj . pythonateText)

-- | The function prepare_GHC_state is called by the C layer at module
-- startup to set-up some GHC to allow us to import modules. It
-- basically just calls createGHCSession then wraps the resulting
-- Session in a StablePtr so that the C layer can take care of it for
-- us.

foreign export ccall prepare_GHC_state :: Ptr WStPtr -> IO Int
prepare_GHC_state addr = liftM (fromMaybe (-1)) . runMaybeT $ do
  session  <- createGHCSession
  stable   <- lift $ newStablePtr session
  lift $ poke addr (castStablePtrToPtr (stable :: StablePtr GhcMonad.Session))
  return 0

-- | The function close_GHC_state is called by the C layer at module
-- teardown to clear up the GHC state.

foreign export ccall close_GHC_state :: WStPtr -> IO Int
close_GHC_state stableSessionPtr = liftM (fromMaybe (-1)) . runMaybeT $ do
  sess  <- lift $ deRefStablePtr (
    castPtrToStablePtr stableSessionPtr :: StablePtr GhcMonad.Session)
  translatingHsExcepts . lift $ flip GhcMonad.reflectGhc sess $ do
    flags <- GHC.getSessionDynFlags
    GHC.defaultCleanupHandler flags $ return 0

-- | Fetch the integer which means GILRuleLazy

foreign export ccall get_GIL_mode_lazy          :: IO Int
get_GIL_mode_lazy       = return $ fromEnum GILRuleLazy

-- | Fetch the integer which means GILRuleFancy

foreign export ccall get_GIL_mode_fancy         :: IO Int
get_GIL_mode_fancy      = return $ fromEnum GILRuleFancy

-- | Convert a GILRule encoded as an integer to a nice Python string
-- describing the GILRule

foreign export ccall stringify_GIL_mode         :: Int -> IO PyObj
stringify_GIL_mode      = pythonateString . displayFn . toEnum
  where displayFn GILRuleLazy  = "lazy"
        displayFn GILRuleFancy = "fancy"

-- | Fetch the integer which means SignalRuleLazy

foreign export ccall get_signal_mode_lazy    :: IO Int
get_signal_mode_lazy    = return $ fromEnum SignalRuleLazy

-- | Fetch the integer which means SignalRuleHaskell

foreign export ccall get_signal_mode_haskell :: IO Int
get_signal_mode_haskell = return $ fromEnum SignalRuleHaskell

-- | Fetch the integer which means SignalRulePython

foreign export ccall get_signal_mode_python  :: IO Int
get_signal_mode_python  = return $ fromEnum SignalRulePython

-- | Convert a SignalRule encoded as an integer to a nice Python
-- string describing the SignalRule

foreign export ccall stringify_signal_mode   :: Int -> IO PyObj
stringify_signal_mode   = pythonateString . displayFn . toEnum
  where displayFn SignalRuleLazy    = "lazy"
        displayFn SignalRuleHaskell = "haskell"
        displayFn SignalRulePython  = "python"

-- Now we provide the main implementations of several functions that
-- are going to be exposed as part of the hslowlevel module. For
-- documentation of what the functions do, see the documenation of
-- hslowlevel itself. In some cases, the function as defined here is
-- in exactly the form needed to stick it in the Python module
-- table. In others (indicated by a '_core' or an '_impl' in the name)
-- there is a thin C wrapper around the function defined here which is
-- actually inserted into the Python module table. In the latter case,
-- there will be a brief comment saying what the wrapper does.

-- The next three functions are all wrapped in C. The C wrapper
-- discards the PyObj which encodes the module (we never need it) and
-- provides the WStPtr giving the GHC state.

foreign export ccall hyphen_import_lib_core :: WStPtr -> PyObj -> IO PyObj
hyphen_import_lib_core = wrapImport importLibModules

foreign export ccall hyphen_import_src_core :: WStPtr -> PyObj -> IO PyObj
hyphen_import_src_core = wrapImport importSrcModules

foreign export ccall hyphen_access_basics_core :: WStPtr -> PyObj -> IO PyObj
hyphen_access_basics_core stableSessionPtr pyargsTuple =
  liftM (fromMaybe nullPyObj) . runMaybeT $ do
    sess  <- lift $ deRefStablePtr (
      castPtrToStablePtr stableSessionPtr :: StablePtr GhcMonad.Session)
    nArgs <- treatingAsErr (-1) $ pyTuple_Size pyargsTuple
    check (nArgs == 0) $ "hyphen.access_basics: no arguments expected."
    objs <- accessBasics sess
    pythonateHDict (treatingAsErr nullPyObj . pythonateText)
      (treatingAsErr nullPyObj . wrapPythonHsObjRaw) objs

-- | This function captures the commonality between
-- hyphen_import_lib_core and hyphen_import_src_core defined above.

wrapImport :: (GhcMonad.Session -> [Text] ->
               MaybeT IO (HashMap Text (HashMap Text HsObj, HashMap Text TyNSElt)))
              -> WStPtr -> PyObj -> IO PyObj
wrapImport i_fn stableSessionPtr pyargsTuple = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  sess  <- lift $ deRefStablePtr (
    castPtrToStablePtr stableSessionPtr :: StablePtr GhcMonad.Session)
  nArgs <- treatingAsErr (-1) $ pyTuple_Size pyargsTuple
  check (nArgs > 0) $ "hyphen.hyphen_import_src: must have at least one argument."
  let getArgChecked :: Int -> PythonM Text
      getArgChecked idx = do
        pyobj <- lift $ pyTuple_GET_ITEM pyargsTuple (idx - 1)
        ok    <- lift $ pyUnicode_Check pyobj
        unless ok $ pyTypeErr'' ("string as argument " ++ show idx) pyobj
        textFromPythonObj pyobj
  allSrcsList <- mapM getArgChecked [1..nArgs]
  pythonateHDict (treatingAsErr nullPyObj . pythonateText) pythonateModuleRet
    =<< (i_fn sess allSrcsList)

foreign export ccall ok_python_identif          :: PyObj -> PyObj -> IO PyObj
ok_python_identif _ pyargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size pyargs
  check (nArgs == 1) $ "hyphen.ok_python_identif: expected exactly 1 argument."
  pyobj <- lift $ pyTuple_GET_ITEM pyargs 0
  ok    <- lift $ pyUnicode_Check pyobj
  check ok $ "hyphen.ok_python_identif: expected string as parameter."
  treatingAsErr nullPyObj . pythonateBool . okPythonIdentif =<< textFromPythonObj pyobj

foreign export ccall hyphen_apply               :: PyObj -> PyObj -> IO PyObj
hyphen_apply _ pyargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size pyargs
  check (nArgs > 1 ) $ "hyphen.apply: must have at least 2 arguments."
  check (nArgs <= 6) $ "hyphen.apply: must have at most 6 arguments."
  let getArgChecked :: Int -> PythonM HsObj
      getArgChecked idx = do
        pyobj <- lift $ pyTuple_GET_ITEM pyargs (idx - 1)
        ok    <- lift $ pyHsObjRaw_Check pyobj
        unless ok $ pyTypeErr'' ("Haskell object for parameter " ++ show idx) pyobj
        lift $ unwrapPythonHsObjRaw pyobj
  allArgsList <- mapM getArgChecked [1..nArgs]
  let (fnToApply : trueArgsList) = allArgsList
  -- Do not release GIL here. Apply just creates a thunk anyhow (so we
  -- don't spend much time in this call) and the apply call might
  -- raise Python exceptions.
  obj' <- translatingHsExcepts  $ apply fnToApply trueArgsList
  lift $ wrapPythonHsObjRaw obj'

-- The next function is wrapped in C. The C wrapper discards the PyObj
-- which encodes the module (we never need it) and provides Ints
-- encoding the GILRule and the SignalRule which are in force

foreign export ccall hyphen_doio_impl           :: Int -> Int -> PyObj -> IO PyObj
hyphen_doio_impl gilCode sigCode pyargtuple = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  pyobj <- treatingAsErr nullPyObj $ parseTupleToPythonHsObjRaw pyargtuple
  obj   <- lift $ unwrapPythonHsObjRaw pyobj
  act   <- promoteErr $ doIO obj
  let protectLro = protectLongRunningOperation (toEnum gilCode) (toEnum sigCode)
  obj'  <- translatingHsExcepts . lift . protectLro $ act
  lift $ wrapPythonHsObjRaw obj'

-- The next function is wrapped in C. The C layer parses the argument
-- tuple and validates teh types, then gives the arguments to the
-- function below (so the arguments below exactly match the arguments
-- of the Python function we're exposing...)

foreign export ccall hyphen_wrap_pyfn_impl      :: PyObj -> PyObj -> Int -> IO PyObj
hyphen_wrap_pyfn_impl fn tyPyo arityReq = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  checkM (pyCallable_Check fn) "wrap_pyfn(): first parameter must be callable"
  ty <- lift $ unwrapPythonHsType tyPyo
  check (isMonoType ty)        "wrap_pyfn(): type provided cannot be polymorphic."
  check (arityReq == -1 || arityReq >= 0) $
    "wrap_pyfn(): third parameter (arity) must either be a non-negative integer, or -1 ("
    ++ "meaning 'as large as possible'"
  let (argTypes, fnResultType) = peelArgTypes ty arityReq
      arity                    = length argTypes
                                 -- smaller then arityReq if we ran out of arguments
      isIOAction             = typeHead ty == Left ioTyCon
      resultType             = if isIOAction then head (typeTail ty) else fnResultType
  check (arityReq == arity || arityReq == -1) $
    "wrap_pyfn(): you have requested that we wrap a python function of arity "
    ++ show arityReq ++ ", but have provided a type " ++ (T.unpack $ typeName ty)
    ++ " which supports a maximum arity of " ++ show arity
  if arityReq /= (-1) then check (arity > 0 || isIOAction) (
    "wrap_pyfn(): you have requested that we wrap an IO action (by requesting "
    ++ "arity 0; but have provided a type " ++ (T.unpack $ typeName ty)
    ++ "which is not the type of an IO action.") else check (arity > 0 || isIOAction) (
    "wrap_pyfn(): the type provided must either be a function type or "
    ++ "the type of an IO action, not " ++ (T.unpack $ typeName ty))
  assert (not isIOAction || arity == 0) (return ())
  check (arity <= 5) $
    "wrap_pyfn(): can only wrap python functions of arity <=5"
  lift $ py_INCREF fn
  --lift $ py_INCREF fn
  fn' <- lift $ newForeignPtr addr_py_DECREF fn
  let callTheFn :: [IO HsObj] -> IO Any
      callTheFn objs = acquiringGIL . translatePyExceptionIfAny $ do
        paramTup <- pythonateTuple $ map (
          treatingAsErr nullPyObj . (wrapPythonHsObjRaw =<<)) objs
        pyRes <- treatingAsErr nullPyObj . withForeignPtr fn' $
                 (\ fn'' -> pyObject_Call fn'' paramTup nullPyObj >>. py_DECREF paramTup)
        checkM (pyHsObjRaw_Check pyRes) $
          "Return value of wrapped Python function must be a Haskell object"
        objRet <- lift $ unwrapPythonHsObjRaw pyRes
        check (objType objRet == resultType) $
          "Return value of wrapped Python function should have type " ++
          (T.unpack $ typeName resultType) ++ " not " ++
          (T.unpack . typeName $ objType objRet)
        let (MonoObj _ raw) = objRet
        return $ raw
  lift $ wrapPythonHsObjRaw =<< case argTypes of
    []                        -> -- special case; wrap IO action
      let fnToWrap :: IO Any
          fnToWrap           = callTheFn []
      in formObjOfType ty fnToWrap
    [t_a]                     -> -- wrap unary fn
      let fnToWrap :: Any -> Any
          fnToWrap a         = unsafePerformIO $ callTheFn [
            formObjOfType t_a a]
      in formObjOfType ty fnToWrap
    [t_a, t_b]                -> -- wrap binary fn
      let fnToWrap :: Any -> Any -> Any
          fnToWrap a b       = unsafePerformIO $ callTheFn [
            formObjOfType t_a a, formObjOfType t_b b]
      in formObjOfType ty fnToWrap
    [t_a, t_b, t_c]           -> -- wrap ternary fn
      let fnToWrap :: Any -> Any -> Any -> Any
          fnToWrap a b c     = unsafePerformIO $ callTheFn [
            formObjOfType t_a a, formObjOfType t_b b, formObjOfType t_c c]
      in formObjOfType ty fnToWrap
    [t_a, t_b, t_c, t_d]      -> -- wrap 4ary fn
      let fnToWrap :: Any -> Any -> Any -> Any -> Any
          fnToWrap a b c d   = unsafePerformIO $ callTheFn [
            formObjOfType t_a a, formObjOfType t_b b, formObjOfType t_c c,
            formObjOfType t_d d]
      in formObjOfType ty fnToWrap
    [t_a, t_b, t_c, t_d, t_e] -> -- wrap 5ary fn
      let fnToWrap :: Any -> Any -> Any -> Any -> Any -> Any
          fnToWrap a b c d e = unsafePerformIO $ callTheFn [
            formObjOfType t_a a, formObjOfType t_b b, formObjOfType t_c c,
            formObjOfType t_d d, formObjOfType t_e e]
      in formObjOfType ty fnToWrap
