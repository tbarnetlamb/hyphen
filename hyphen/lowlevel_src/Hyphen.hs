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
import Data.IORef
import Data.Monoid
import Data.Word
import Data.Maybe
import Data.Hashable
import Data.Char
import Data.Typeable     (Typeable, typeOf)
import Data.Text         (Text)
import Data.ByteString   (ByteString)
import Data.Map.Strict   (Map)
import Foreign           (newForeignPtr, withForeignPtr)
import Foreign.StablePtr (deRefStablePtr, newStablePtr, StablePtr)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.String  (CString, withCString)
import System.IO.Unsafe  (unsafePerformIO)
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
import qualified System.Posix.Signals
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

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM fn seed = do res0 <- fn seed
                      case res0 of
                        Nothing          -> return []
                        Just (r, seed')  -> liftM (r :) (unfoldrM fn seed')

pyTypeErr'' :: String -> PyObj -> PythonM a
pyTypeErr'' exp pyObj = do 
  py_str    <- treatingAsErr nullPyObj $ pyObject_Str pyObj
  got       <- textFromPythonObj py_str
  pyTypeErr' $ "Expected " ++ exp ++ ", not " ++ T.unpack got

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
           
readKwargs :: [String] -> String -> Map Text PyObj -> PythonM [Maybe PyObj]
readKwargs expected errstr kwargs =
  let expected'  = map T.pack expected
      unexpected = Map.difference kwargs (Map.fromList $ map (\x -> (x,())) expected')
  in case Map.toList unexpected of 
    ((name, _):_)  -> pyTypeErr' (
        errstr ++ ": unexpected keyword argument " ++ T.unpack name)
    _              -> return $ map (flip Map.lookup kwargs) expected'


------------------------------

checkPythonSignals :: IO ()
checkPythonSignals = do
  acquiringGIL . translatePyExceptionIfAny . treatingAsErr (-1) $ pyErr_CheckSignals
  return ()

servicingPySignalHandlers :: forall a. IO a -> IO a
servicingPySignalHandlers act = do
  resultMVar <- (newEmptyMVar :: IO (MVar (Either SomeException a)))
  workThrd   <- forkFinally act (putMVar resultMVar)
  let loop :: IO (Either SomeException a)
      loop = do threadDelay 50
                Exception.catch checkPythonSignals (
                  throwTo workThrd :: SomeException -> IO ())
                threadDelay 50
                mresult <- tryTakeMVar resultMVar
                case mresult of 
                  Just result -> return $ result
                  Nothing     -> loop
  result <- loop
  case result of
    Left  ex  -> Exception.throwIO ex
    Right ans -> return ans

threadToInterruptStack :: IORef [System.Mem.Weak.Weak ThreadId]
threadToInterruptStack = unsafePerformIO (newIORef [])

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

------------------------------

releasingGIL :: PythonM a -> PythonM a
releasingGIL =
  MaybeT . Exception.bracket pyEval_SaveThread pyEval_RestoreThread . f . runMaybeT
  where f action = (>> action) . guard . (/= nullPtr)

acquiringGIL :: IO a -> IO a
acquiringGIL = Exception.bracket pyGILState_Ensure pyGILState_Release . const

------------------------------

ignoring2nd :: (a -> c) -> (a -> b -> c)
ignoring2nd fn a _ = fn a

py_LT = 0 :: Int
py_LE = 1 :: Int
py_EQ = 2 :: Int
py_NE = 3 :: Int
py_GT = 4 :: Int
py_GE = 5 :: Int

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

foreign export ccall tycon_hash             :: PyObj -> IO Int
tycon_hash       = return . hash                             <=< unwrapPythonTyCon

foreign export ccall tycon_richcmp          :: PyObj -> PyObj -> Int -> IO PyObj
tycon_richcmp    = compareVia pyTyCon_Check unwrapPythonTyCon

foreign export ccall tycon_str              :: PyObj -> IO PyObj
tycon_str        = pythonateText . aug . tyConFullName       <=< unwrapPythonTyCon
  where aug txt = T.concat [T.pack "<hyphen.TyCon object representing ", txt, T.pack ">"]

foreign export ccall tycon_repr             :: PyObj -> IO PyObj
tycon_repr       = pythonateText . tyConRepr                 <=< unwrapPythonTyCon

foreign export ccall tycon_getname          :: PyObj -> Ptr () -> IO PyObj
tycon_getname    = ignoring2nd (pythonateText . tyConName    <=< unwrapPythonTyCon)

foreign export ccall tycon_getmodule        :: PyObj -> Ptr () -> IO PyObj
tycon_getmodule  = ignoring2nd (pythonateText . tyConModule  <=< unwrapPythonTyCon)

foreign export ccall tycon_getpackage       :: PyObj -> Ptr () -> IO PyObj
tycon_getpackage = ignoring2nd (pythonateText . tyConPackage <=< unwrapPythonTyCon)

foreign export ccall tycon_getarity         :: PyObj -> Ptr () -> IO PyObj
tycon_getarity   = ignoring2nd (pythonateInt  . tyConArity   <=< unwrapPythonTyCon)

foreign export ccall tycon_get_is_cls       :: PyObj -> Ptr () -> IO PyObj
tycon_get_is_cls = ignoring2nd (pythonateBool . tyConIsCls   <=< unwrapPythonTyCon)

foreign export ccall tycon_getkind          :: PyObj -> Ptr () -> IO PyObj
tycon_getkind    = ignoring2nd (
                   pythonateString . kindString . tyConKind  <=< unwrapPythonTyCon)

foreign export ccall hstype_hash            :: PyObj -> IO Int
hstype_hash      = return . hash               <=< unwrapPythonHsType

foreign export ccall hstype_richcmp          :: PyObj -> PyObj -> Int -> IO PyObj
hstype_richcmp   = compareVia pyHsType_Check unwrapPythonHsType

foreign export ccall hstype_str              :: PyObj -> IO PyObj
hstype_str       = pythonateText . aug . typeName             <=< unwrapPythonHsType
  where aug txt = T.concat [T.pack "<hyphen.HsType object representing ", txt, T.pack ">"]

foreign export ccall hstype_repr             :: PyObj -> IO PyObj
hstype_repr      = pythonateText . hsTypeRepr                 <=< unwrapPythonHsType

foreign export ccall hstype_gettail         :: PyObj -> Ptr () -> IO PyObj
hstype_gettail   = ignoring2nd (
  liftM (fromMaybe nullPyObj) . runMaybeT . pythonateTuple 
  . map (treatingAsErr nullPyObj . wrapPythonHsType) . typeTail <=< unwrapPythonHsType)

foreign export ccall hstype_getname         :: PyObj -> Ptr () -> IO PyObj
hstype_getname   = ignoring2nd (pythonateText . typeName        <=< unwrapPythonHsType)

foreign export ccall hstype_getkind         :: PyObj -> Ptr () -> IO PyObj
hstype_getkind   = ignoring2nd (
                      pythonateString . kindString . typeKind   <=< unwrapPythonHsType)

foreign export ccall hstype_getfvs          :: PyObj -> Ptr () -> IO PyObj
hstype_getfvs    = ignoring2nd (
  liftM (fromMaybe nullPyObj) . runMaybeT . pythonateDict 
  (treatingAsErr nullPyObj . pythonateText . getVar) 
  (treatingAsErr nullPyObj . pythonateString . kindString)
  . typeFreeVars <=< unwrapPythonHsType)

unwrapHsTypeChecked :: String -> PyObj -> PythonM HsType
unwrapHsTypeChecked errContextMsg val = do
  valOK <- lift $ pyHsType_Check  val
  unless valOK $ pyTypeErr'' ("HsType for " ++ errContextMsg) val
  lift $ unwrapPythonHsType val

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
hstype_gethead p _ = do hstype <- unwrapPythonHsType p
                        case typeHead hstype of
                          Left tyc         -> wrapPythonTyCon tyc
                          Right (Var v, k) -> pythonateText  v

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

textFromPythonObj :: PyObj -> PythonM Text
textFromPythonObj pyobj = 
  do pyobj_hsstr  <- treatingAsErr nullPyObj $ c_makeHaskellText pyobj
     lift $ do
       MonoObj ty ptr <- unwrapPythonHsObjRaw pyobj_hsstr
       py_DECREF pyobj_hsstr
       return (Unsafe.Coerce.unsafeCoerce ptr :: Text)

invalidTyVarMsg :: String -> Maybe String
invalidTyVarMsg varString
  | not (all isAlphaNum varString)       = Just "contains illegal characters."
  | varString == ""                      = Just "is empty."
  | not (all isLower $ take 1 varString) = Just "does not begin with a lower-case letter."
  | otherwise                            = Nothing

throwOnInvalidTyVar :: Text -> PythonM ()
throwOnInvalidTyVar varString = case invalidTyVarMsg (T.unpack varString) of
  Just errMsg -> lift (
    pyValueErr . T.pack $ "Type variable '" ++ T.unpack varString ++ "'" ++ errMsg
    ) >> mzero
  _           -> return ()

interpretTyConCallArgument :: Kind -> PyObj -> PythonM HsType
interpretTyConCallArgument expectedKind pyObj 
  = do is_hsType <- lift $ pyHsType_Check  pyObj
       is_str    <- lift $ pyUnicode_Check pyObj
       unless (is_hsType || is_str) $ pyTypeErr'' "HsType or str" pyObj
       if is_hsType then lift (unwrapPythonHsType pyObj) else do
         varString <- textFromPythonObj pyObj
         throwOnInvalidTyVar varString
         return $ mkHsType (Right $ (Var varString, expectedKind)) []

constructHsTypeFromTycon :: PyObj -> [PyObj] -> PythonM HsType
constructHsTypeFromTycon head_pyobj tail_list_pyobj = do
  head            <- lift $ unwrapPythonTyCon head_pyobj
  check (length tail_list_pyobj <= length (kindArgKinds $ tyConKind head)) (
    "Too many type arguments provided.")
  tail_list       <- sequence $ zipWith interpretTyConCallArgument
                         (kindArgKinds $ tyConKind head) tail_list_pyobj
  promoteErrAsValueErr $ mkHsTypeSafe (Left head) tail_list

foreign export ccall tycon_call             :: PyObj -> PyObj -> PyObj -> IO PyObj
tycon_call self_pyobj args kwargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  check (kwargs == nullPyObj) 
    "When calling a type constructor, keyword arguments not supported"
  nArgs           <- treatingAsErr (-1) $ pyTuple_Size args
  tail_list_pyobj <- mapM (lift . pyTuple_GET_ITEM args) [0..nArgs-1]
  lift . wrapPythonHsType =<< constructHsTypeFromTycon self_pyobj tail_list_pyobj

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

addSimpleHsTypeObjToModule :: Typeable a => PyObj -> String -> (a -> a) -> PythonM ()
addSimpleHsTypeObjToModule module_ name ifn = do
  let hsType = hsTypeFromSimpleTypeRep $ typeOf (ifn undefined)
  pyHsType <- treatingAsErr nullPyObj $ wrapPythonHsType hsType
  releasingOnFail pyHsType $ do
    treatingAsErr (-1) . withCString name $ \cStrName -> 
      pyModule_AddObject module_ cStrName pyHsType
  return ()

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

withHsObjRawOfType :: HsType -> (a -> IO PyObj) -> PyObj -> PyObj -> IO PyObj
withHsObjRawOfType ty f _ pyargtuple = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  pyobj <- treatingAsErr nullPyObj $ parseTupleToPythonHsObjRaw pyargtuple
  obj   <- lift $ unwrapPythonHsObjRaw pyobj
  translatingHsExcepts  . releasingGIL $ case obj of
    MonoObj ty' ptr -> 
      if ty == ty' then lift (f $ Unsafe.Coerce.unsafeCoerce ptr) else
        pyTypeErr' $ "Expected type " ++ T.unpack (typeName ty) 
        ++ "; got " ++ T.unpack (typeName ty')
    _ ->   pyTypeErr' $ "Only monomorphic objects supported."


withHsObjRawSimp :: (Typeable a) => (a -> IO PyObj) -> PyObj -> PyObj -> IO PyObj
withHsObjRawSimp fn = let 
  typeConstrainer :: (a -> b) -> a
  typeConstrainer = const undefined
  in withHsObjRawOfType (hsTypeFromSimpleTypeRep $ typeOf $ typeConstrainer fn) fn

foreign export ccall from_haskell_Bool        :: PyObj -> PyObj -> IO PyObj
from_haskell_Bool        = withHsObjRawSimp pythonateBool

foreign export ccall from_haskell_Char        :: PyObj -> PyObj -> IO PyObj
from_haskell_Char        = withHsObjRawSimp pythonateChar

foreign export ccall from_haskell_String      :: PyObj -> PyObj -> IO PyObj
from_haskell_String      = withHsObjRawSimp pythonateString

foreign export ccall  from_haskell_Text       :: PyObj -> PyObj -> IO PyObj
from_haskell_Text        = withHsObjRawSimp pythonateText

foreign export ccall from_haskell_ByteString  :: PyObj -> PyObj -> IO PyObj
from_haskell_ByteString  = withHsObjRawSimp pythonateByteString

foreign export ccall from_haskell_Int         :: PyObj -> PyObj -> IO PyObj
from_haskell_Int         = withHsObjRawSimp pythonateInt

foreign export ccall from_haskell_Integer     :: PyObj -> PyObj -> IO PyObj
from_haskell_Integer     = withHsObjRawSimp pythonateInteger

foreign export ccall from_haskell_Float       :: PyObj -> PyObj -> IO PyObj
from_haskell_Float       = withHsObjRawSimp pythonateFloat

foreign export ccall from_haskell_Double      :: PyObj -> PyObj -> IO PyObj
from_haskell_Double      = withHsObjRawSimp pythonateDouble

foreign export ccall buildHaskellBool       :: Bool   -> IO PyObj
buildHaskellBool       = formSimpleHsObjRaw

foreign export ccall buildHaskellChar       :: Char   -> IO PyObj
buildHaskellChar       = formSimpleHsObjRaw

foreign export ccall buildHaskellString     :: Ptr Word16 -> Int -> IO PyObj
buildHaskellString     = curry (
  formSimpleHsObjRaw . T.unpack
  <=< uncurry Data.Text.Foreign.fromPtr . second (fromInteger . toInteger))

foreign export ccall buildHaskellText       :: Ptr Word16 -> Int -> IO PyObj
buildHaskellText       = curry (
  formSimpleHsObjRaw 
  <=< uncurry Data.Text.Foreign.fromPtr . second (fromInteger . toInteger))

foreign export ccall buildHaskellByteString :: CString -> Int -> IO PyObj
buildHaskellByteString = curry (formSimpleHsObjRaw <=< Data.ByteString.packCStringLen)

foreign export ccall buildHaskellInt        :: Int     -> IO PyObj
buildHaskellInt        = formSimpleHsObjRaw

foreign export ccall buildHaskellInteger    :: Int     -> IO PyObj
buildHaskellInteger    = formSimpleHsObjRaw . toInteger

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

pythonateModuleRet :: (Map Text Obj, Map Text TyCon) -> PythonM PyObj
pythonateModuleRet (objs, typs) = pythonateTuple [
  prepareDict (treatingAsErr nullPyObj . wrapPythonHsObjRaw) objs, 
  prepareDict (treatingAsErr nullPyObj . wrapPythonTyCon)   typs]
  where prepareDict = pythonateDict (treatingAsErr nullPyObj . pythonateText)

wrapImport :: (GhcMonad.Session -> [String] -> 
               MaybeT IO (Map Text (Map Text Obj, Map Text TyCon)))
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
  pythonateDict (treatingAsErr nullPyObj . pythonateText) pythonateModuleRet
    =<< (i_fn sess $ map T.unpack allSrcsList)

foreign export ccall prepare_GHC_state :: Ptr WStPtr -> IO Int
prepare_GHC_state addr = liftM (fromMaybe (-1)) . runMaybeT $ do
  session  <- createGHCSession
  stable   <- lift $ newStablePtr session
  lift $ poke addr (castStablePtrToPtr (stable :: StablePtr GhcMonad.Session))
  return 0

foreign export ccall close_GHC_state :: WStPtr -> IO Int
close_GHC_state stableSessionPtr = liftM (fromMaybe (-1)) . runMaybeT $ do
  sess  <- lift $ deRefStablePtr (
    castPtrToStablePtr stableSessionPtr :: StablePtr GhcMonad.Session)
  translatingHsExcepts . lift $ flip GhcMonad.reflectGhc sess $ do
    flags <- GHC.getSessionDynFlags
    GHC.defaultCleanupHandler flags $ return 0

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
    check (nArgs == 0) $ "hyphen.hyphen_import_src: no arguments expected."
    objs <- accessBasics sess
    pythonateDict (treatingAsErr nullPyObj . pythonateText)
      (treatingAsErr nullPyObj . wrapPythonHsObjRaw) objs

foreign export ccall ok_python_identif      :: PyObj -> PyObj -> IO PyObj
ok_python_identif _ pyargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size pyargs
  check (nArgs == 1) $ "hyphen.ok_python_identif: expected exactly 1 argument."
  pyobj <- lift $ pyTuple_GET_ITEM pyargs 0
  ok    <- lift $ pyUnicode_Check pyobj
  check ok $ "hyphen.ok_python_identif: expected string as parameter."
  treatingAsErr nullPyObj . pythonateBool . okPythonIdentif =<< textFromPythonObj pyobj

foreign export ccall hyphen_apply           :: PyObj -> PyObj -> IO PyObj
hyphen_apply _ pyargs = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  nArgs  <- treatingAsErr (-1) $ pyTuple_Size pyargs
  check (nArgs > 1 ) $ "hyphen.apply: must have at least 2 arguments."
  check (nArgs <= 6) $ "hyphen.apply: must have at most 6 arguments."
  let getArgChecked :: Int -> PythonM Obj
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

foreign export ccall hyphen_doio            :: PyObj -> PyObj -> IO PyObj
hyphen_doio _ pyargtuple = liftM (fromMaybe nullPyObj) . runMaybeT $ do
  pyobj <- treatingAsErr nullPyObj $ parseTupleToPythonHsObjRaw pyargtuple
  obj   <- lift $ unwrapPythonHsObjRaw pyobj
  act   <- promoteErr $ doIO obj
  obj'  <- translatingHsExcepts . releasingGIL $ lift act
  lift $ wrapPythonHsObjRaw obj'

foreign export ccall hyphen_wrap_pyfn_impl   :: PyObj -> PyObj -> Int -> IO PyObj
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
  let callTheFn :: [IO Obj] -> IO Any
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


