{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenGHC (createGHCSession, importLibModules, importSrcModules, accessBasics,
                  TyNSElt) where

--import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import Data.Monoid ()
import Data.Maybe
import Data.Either
import Data.List
import Data.Text                      (Text)
import Data.Set                       (Set)
import Data.HashMap.Strict            (HashMap)
import qualified Control.Exception
import qualified Data.Text            as T
import qualified Data.Set             as Set
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Traversable
import qualified Unsafe.Coerce
import qualified GHC
import qualified GHC.Paths
#if __GLASGOW_HASKELL__ >= 906
import qualified GHC.Builtin.Types.Prim
#endif
#if __GLASGOW_HASKELL__ >= 902
import qualified GHC.Builtin.Types    as GHCTysWiredIn
#elif __GLASGOW_HASKELL__ >= 900
import qualified GHC.Builtin.Names    as GHCPrelNames
import qualified GHC.Builtin.Types    as GHCTysWiredIn
#elif __GLASGOW_HASKELL__ >= 802
import qualified PrelNames   as GHCPrelNames
import qualified TysWiredIn  as GHCTysWiredIn
#else
import qualified StaticFlags as GHCStaticFlags
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.SysTools as GHCSysTools
#else
import qualified SysTools    as GHCSysTools
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.SysTools.BaseDir as GHCSysToolsBaseDir
#elif __GLASGOW_HASKELL__ >= 808
import qualified SysTools.BaseDir as GHCSysToolsBaseDir
#endif
#if __GLASGOW_HASKELL__ >= 906
import qualified Language.Haskell.Syntax.Module.Name as GHCModuleName
#elif __GLASGOW_HASKELL__ >= 900
import qualified GHC.Unit.Module.Name as GHCModuleName
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Core.TyCon       as GHCTyCon
import qualified GHC.Types.Name.Occurrence     as GHCOccName
import qualified GHC.Unit.Types      as GHCModule
#else
import qualified TyCon       as GHCTyCon
import qualified OccName     as GHCOccName
import qualified Module      as GHCModule
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Core.Type as GHCKind
#elif __GLASGOW_HASKELL__ >= 810
import qualified Type        as GHCKind
#elif __GLASGOW_HASKELL__ >= 800
import qualified Kind        as GHCKind
#endif
#if __GLASGOW_HASKELL__ >= 902
import qualified GHC.Types.SourceError
import qualified GHC.Types.Target
import qualified GHC.Unit.Module.Graph
import qualified GHC.Unit.Module.ModSummary
import qualified GHC.Types.TyThing         as GHCType
import qualified GHC.Core.Type
#elif __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Types      as GHCHscTypes
import qualified GHC.Driver.Types
#else
import qualified HscTypes    as GHCHscTypes
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Core.Type         as GHCType
import qualified GHC.Driver.Session    as GHCDynFlags
import qualified GHC.Driver.Main       as GHCHscMain
import qualified GHC.Utils.Monad       as GHCMonadUtils
import qualified GHC.Types.Var         as GHCVar
import qualified Control.Monad.Catch as MC
#else
import qualified Type        as GHCType
import qualified DynFlags    as GHCDynFlags
import qualified HscMain     as GHCHscMain
import qualified MonadUtils  as GHCMonadUtils
import qualified Var         as GHCVar
import qualified Outputable  as GHCOutputable
import qualified ErrUtils    as GHCErrUtils
import qualified Exception   as GHCException
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Core.ConLike     as GHCConLike
#elif __GLASGOW_HASKELL__ >= 708
import qualified ConLike     as GHCConLike
#endif
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Monad as GhcMonad
#else
import qualified GhcMonad
#endif

import HyphenBase
import PythonBase
import HyphenKinds
import HyphenTyCon
import HsType
import HsObjRaw
import HyphenExceptions

-- initGHCMonad, as defined in the GHC API, annoyingly sets several
-- totally inappropriate signal handlers, which will completely mess
-- with Python's signal handlers. So we have to have our own
-- version. This function, alas, must be kept in sync with the GHC
-- version of which it is mostly a clone

-- | Like initGHCMonad, as defined in the GHC API: but don't set
-- signal handlers.

ourInitGhcMonad :: (GHC.GhcMonad m) => Maybe FilePath -> m ()
ourInitGhcMonad mb_top_dir = do
#if __GLASGOW_HASKELL__ >= 802
#else
  GHCMonadUtils.liftIO $ GHCStaticFlags.initStaticOpts
#endif

#if __GLASGOW_HASKELL__ >= 906
  top_dir    <- GHCMonadUtils.liftIO $ GHCSysToolsBaseDir.findTopDir mb_top_dir
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools top_dir
  dflags     <- GHCMonadUtils.liftIO (GHCDynFlags.initDynFlags (
                  GHCDynFlags.defaultDynFlags mySettings))
#elif __GLASGOW_HASKELL__ >= 810
  top_dir    <- GHCMonadUtils.liftIO $ GHCSysToolsBaseDir.findTopDir mb_top_dir
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools top_dir
  llvmc      <- GHCMonadUtils.liftIO $ GHCSysTools.lazyInitLlvmConfig top_dir
  dflags     <- GHCMonadUtils.liftIO (GHCDynFlags.initDynFlags (
                  GHCDynFlags.defaultDynFlags mySettings llvmc))
#elif __GLASGOW_HASKELL__ >= 808
  top_dir    <- GHCMonadUtils.liftIO $ GHCSysToolsBaseDir.findTopDir mb_top_dir
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools top_dir
  llvmc      <- GHCMonadUtils.liftIO $ GHCSysTools.initLlvmConfig top_dir
  dflags     <- GHCMonadUtils.liftIO (GHCDynFlags.initDynFlags (
                  GHCDynFlags.defaultDynFlags mySettings llvmc))
#elif __GLASGOW_HASKELL__ >= 806
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools mb_top_dir
  llvmc      <- GHCMonadUtils.liftIO $ GHCSysTools.initLlvmConfig mb_top_dir
  dflags     <- GHCMonadUtils.liftIO (GHCDynFlags.initDynFlags (
                  GHCDynFlags.defaultDynFlags mySettings llvmc))
#elif __GLASGOW_HASKELL__ >= 804
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools mb_top_dir
  llvmt      <- GHCMonadUtils.liftIO $ GHCSysTools.initLlvmTargets mb_top_dir
  --let llvmt = GHCOutputable.panic "tbl: v_unsafeGlobalDynFlags: llvmTargets not initialised"
  dflags     <- GHCMonadUtils.liftIO (GHCDynFlags.initDynFlags (
                  GHCDynFlags.defaultDynFlags mySettings llvmt))
#else
  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools mb_top_dir
  dflags     <- GHCMonadUtils.liftIO
                $ GHCDynFlags.initDynFlags (GHCDynFlags.defaultDynFlags mySettings)
#endif
#if __GLASGOW_HASKELL__ >= 802
  liftIO $ GHCDynFlags.setUnsafeGlobalDynFlags dflags
#else
#endif
#if __GLASGOW_HASKELL__ >= 906
  env <- GHCMonadUtils.liftIO $ GHCHscMain.newHscEnv top_dir (
    GHCDynFlags.wopt_unset dflags GHCDynFlags.Opt_WarnWarningsDeprecations)
#elif __GLASGOW_HASKELL__ >= 900
  env <- GHCMonadUtils.liftIO $ GHCHscMain.newHscEnv (
    GHCDynFlags.wopt_unset dflags GHCDynFlags.Opt_WarnWarningsDeprecations)
#else
  env <- GHCMonadUtils.liftIO $ GHCHscMain.newHscEnv dflags
#endif
  GHC.setSession env

-- | The essential function of the following function is to take a
-- action in the GhcMonad.Ghc monad and turn it into an action in the
-- PythonM monad. It takes the appropriate actions to ensure that GHC
-- Errors turn into Haskell exceptions, and that those Haskell
-- exceptions (in turn) turn into nice Python exceptions, as indeed do
-- any other Haskell exceptions we raise. We optionally take a string
-- that can be used to provide some context to Ghc errors, should they
-- end up needing to be propagated out. We also take a
-- GhcMonad.Session, because to do any GhcMonad.Ghc you need a session
-- in place.

performGHCOps  :: (Maybe String) -> GhcMonad.Session -> GhcMonad.Ghc a -> PythonM a
performGHCOps msgHint sess ghcAct = promoteErr =<< (
  translatingHsExcepts . lift $ flip GhcMonad.reflectGhc sess
  $ reportingGHCErrors msgHint ghcAct)

-- | Various incantations need to be performed to get nice error
-- messages from Ghc operations. The following function does those
-- incantations, and turns a @GhcMonad.Ghc a@ action into a modified
-- action that returns @Either ErrMsg a@. We take an optional String
-- which will be used to annotate error messages if they arise. (It
-- can be helpful to have some context when dealing with certain Ghc
-- errors...!)

reportingGHCErrors :: (Maybe String) -> GhcMonad.Ghc a -> GhcMonad.Ghc (Either ErrMsg a)
reportingGHCErrors msgHint action = do
  logref <- GHCMonadUtils.liftIO $ newIORef ""
  dflags <- GhcMonad.getSessionDynFlags
#if __GLASGOW_HASKELL__ >= 900
  let prep  = GHC.setSessionDynFlags $ dflags
#else
  let prep  = GHC.setSessionDynFlags $ dflags { GHCDynFlags.log_action = logHandler logref }
#endif
      clean = GHC.setSessionDynFlags dflags
#if __GLASGOW_HASKELL__ >= 902
      handler :: GHC.Types.SourceError.SourceError -> GhcMonad.Ghc a
#elif __GLASGOW_HASKELL__ >= 900
      handler :: GHC.Driver.Types.SourceError -> GhcMonad.Ghc a
#else
      handler :: GHCHscTypes.SourceError -> GhcMonad.Ghc a
#endif
      handler ex = GHCMonadUtils.liftIO $ do
        case msgHint of
          Just h  -> Control.Exception.throwIO . Control.Exception.AssertionFailed $
                     h ++ show ex
          Nothing -> Control.Exception.throwIO ex

#if __GLASGOW_HASKELL__ >= 900
  result <- MC.handle handler $ do
    MC.bracket prep (const clean) . const $ do
#else
  result <- GHCException.ghandle handler $ do
    GHCException.gbracket prep (const clean) . const $ do
#endif
      action
  problems <- GHCMonadUtils.liftIO $ readIORef logref

  return $ if (problems == "") then Right result else report $ hintedWith msgHint problems
 where hintedWith :: (Maybe String) -> String -> String
       hintedWith Nothing  s = s
       hintedWith (Just h) s = concat [s, "\n(while trying to ", h, ")"]

#if __GLASGOW_HASKELL__ >= 900
#else
       logHandler :: IORef String -> GHCDynFlags.LogAction
#if __GLASGOW_HASKELL__ >= 800
       logHandler ref dflags warnreason severity srcSpan style msg =
#else
       logHandler ref dflags severity srcSpan style msg =
#endif
           case severity of
             GHCErrUtils.SevError ->  modifyIORef' ref (++ printDoc)
             GHCErrUtils.SevFatal ->  modifyIORef' ref (++ printDoc)
             _                    ->  return () -- ignore the rest
         where cntx = GHCOutputable.initSDocContext dflags style
               locMsg = GHCErrUtils.mkLocMessage severity srcSpan msg
               printDoc = show (GHCOutputable.runSDoc locMsg cntx)
#endif

-- | Make a new GHC session and bring the Prelude into scope. This
-- function translates any errors that arise into nice Python
-- exceptions, and is in PythonM for that reason.

createGHCSession :: PythonM GhcMonad.Session
createGHCSession = do
  ref <- lift $ newIORef (error "Empty session")
  let session = GhcMonad.Session ref
  translatingHsExcepts . lift . flip GhcMonad.reflectGhc session $
    ourInitGhcMonad (Just GHC.Paths.libdir)
  performGHCOps Nothing session (
    ensureModulesInContext $ Set.fromList [T.pack "Prelude"])
  return session

#if __GLASGOW_HASKELL__ >= 902
isLiftedRuntimeRep = GHC.Core.Type.isLiftedRuntimeRep
#elif __GLASGOW_HASKELL__ >= 802
isLiftedRuntimeRep arg
  | Just (tc, []) <- GHCType.splitTyConApp_maybe arg
  , Just dc       <- GHCTyCon.isPromotedDataCon_maybe tc
     = dc `GHCPrelNames.hasKey` GHCPrelNames.liftedRepDataConKey
  | otherwise = False
#endif

makeDePolyGHCKindChecker :: GHC.TyVar -> Maybe (GHC.Type -> Bool, GHC.Type)
makeDePolyGHCKindChecker v
#if __GLASGOW_HASKELL__ >= 802
  | GHCType.isRuntimeRepVar v
               = Just (isLiftedRuntimeRep, GHCTysWiredIn.liftedRepTy)
#endif
#if __GLASGOW_HASKELL__ >= 800
  | GHCKind.isLiftedTypeKind (GHCType.tyVarKind v)
               = Just (GHCKind.isLiftedTypeKind, GHCType.liftedTypeKind)
#endif
  | otherwise  = Nothing

dePolyGHCKind :: GHC.Kind -> Maybe (GHC.Kind, [GHC.Type -> Bool])
dePolyGHCKind k =
#if __GLASGOW_HASKELL__ >= 902
  do let (vars, rest)  = GHC.splitForAllTyCoVars k
#else
  do let (vars, rest)  = GHC.splitForAllTys k
#endif
     checkers <- mapM makeDePolyGHCKindChecker vars
     return (GHCType.substTyWith vars (map snd checkers) rest,
             map fst checkers)

-- | Convert a GHC.Kind into one of our Kinds. This isn't guaranteed
-- to work (so is wrapped in Maybe monad) because there are some Kinds
-- known to GHC that are not part of general standard Haskell (unboxed
-- types and so on) and so not supported by our Kind
-- datatype. (Functions whose types have Kinds that are nonstandard in
-- this way will simply not be visible through hyphen.)

unpackSimpleGHCKind :: GHC.Kind -> Maybe Kind
unpackSimpleGHCKind k
  | GHCType.isFunTy k   = do (Kind ks') <- unpackSimpleGHCKind $ GHCType.funResultTy k
                             (Kind . (: ks')) <$> unpackSimpleGHCKind (GHCType.funArgTy k)
#if __GLASGOW_HASKELL__ >= 800
  | otherwise           = do guard (GHCKind.isLiftedTypeKind k) >> (Just $ Kind [])
#else
  | otherwise           = do (kcon, _) <- GHCType.splitTyConApp_maybe k
                             guard (kcon == GHCType.liftedTypeKindTyCon) >> (Just $ Kind [])
#endif

-- The basic process of importing Haskell names splits into three
-- steps. In the first step, we invoke GHCI to read the module and we
-- get its contents in a GHCI-y format: as a bunch of TyThings. This
-- is not pure; it takes place in the Ghc monad. The second step
-- consists of converting these into 'PreObjs' and 'TyNSElts'
-- (representing things in the object and type namespace
-- respectively). This second step is pure. It basically consists of
-- discarding everything that GHCI knows about the TyThing that we
-- *don't* need to know. The third step, in the PythonM monad (and
-- needs access to the GHCI session), consists of turning those
-- PreObjs into HsObjs (the TyNSElts are already in a usable state.

-- | A PreObj represents an object in the data namespace. It's simply
-- a pair (fully qualifie name, type)

type PreObj    = (Text, HsType)

-- | A TyNSElt represents an object in the type namespace; it's simply
-- Either TyCon HsType, where a TyCon is what you might expect and an
-- HsType represents a type synonym/typedef (with the free variables
-- of the HsType corresponding to the parameters of the typedef).

type TyNSElt   = Either TyCon HsType

--------------------------

-- | Given the (fully qualified) name of a module, get GHC to read it
-- and tell us what is in it (as a list of @GHC.TyThing@
-- objects). This is done in the GHC monad. This is the first step of
-- reading a module.

readGHCModule :: Text -> GhcMonad.Ghc [GHC.TyThing]
readGHCModule name = do
  mod        <- GHC.findModule (GHC.mkModuleName $ T.unpack name) Nothing
  mi         <- GHC.getModuleInfo mod
#if __GLASGOW_HASKELL__ >= 804
  xinfos      <- mapM (GHC.getInfo False) $ maybe [] GHC.modInfoExports mi
  return [a |  Just (a, _, _, _, _) <- xinfos]
#elif __GLASGOW_HASKELL__ >= 708
  xinfos      <- mapM (GHC.getInfo False) $ maybe [] GHC.modInfoExports mi
  return [a |  Just (a, _, _, _) <- xinfos]
#else
  xinfos      <- mapM GHC.getInfo $ maybe [] GHC.modInfoExports mi
  return [a |  Just (a, _, _) <- xinfos]
#endif

-----------------------------

-- The following functions handle the second stage of importing a
-- module: converting TyThings into PreObjs and TyNSElts

-- | Convert a Data constructor into a TyCon, if we can. (We can't if,
-- say, it uses unboxed types or something like that.) We usually
-- assume that the TyCon can be imported from which it is defined; if
-- that is not true and it should be imported/accessed from elsewhere,
-- information to that effect should be provided in the form of a
-- Maybe TyCLocation.

transformGHCTyc :: Maybe TyCLocation -> GHC.TyCon -> Maybe (TyCon, [GHC.Type -> Bool])
#if __GLASGOW_HASKELL__ >= 906
transformGHCTyc _ tyc | GHC.Builtin.Types.Prim.isArrowTyCon tyc
                                            = Just (fnTyCon, [const True, isLiftedRuntimeRep, isLiftedRuntimeRep])
#elif __GLASGOW_HASKELL__ >= 900
transformGHCTyc _ tyc | GHC.isFunTyCon tyc  = Just (fnTyCon, [const True, isLiftedRuntimeRep, isLiftedRuntimeRep])
#endif
transformGHCTyc loc tyc = do
  let ghcName = GHC.getName tyc
      modl    = GHC.nameModule ghcName
#if __GLASGOW_HASKELL__ >= 900
      pckg    = T.pack . GHCModule.unitString $ GHC.moduleUnit modl
#elif __GLASGOW_HASKELL__ >= 800
      pckg    = T.pack . GHCModule.unitIdString $ GHC.moduleUnitId modl
#elif __GLASGOW_HASKELL__ >= 710
      pckg    = T.pack . GHCModule.packageKeyString $ GHC.modulePackageKey modl
#else
      pckg    = T.pack . GHCModule.packageIdString $ GHC.modulePackageId modl
#endif
      mname   = T.pack . GHC.moduleNameString $ GHC.moduleName modl
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
      loc'    = fromMaybe (InExplicitModuleNamed mname) loc
  (kind, chks) <- dePolyGHCKind $ GHCTyCon.tyConKind tyc
  kind'        <- unpackSimpleGHCKind kind
  let rawTyCon = mkTyCon pckg mname oname loc' kind' (GHC.isClassTyCon tyc)
  return $ (normalizeTyCon rawTyCon, chks)

-- | Convert something from the Type Construct namespace into a
-- (name, TyNSElt) pair. We need to know the module it was imported
-- from. We might fail to convert if (say) the kind of a type alias is
-- too complicated for us. (e.g. involves unboxed types).

transformGHCTyNSElt :: Text -> GHC.TyCon -> Maybe (Text, TyNSElt)
transformGHCTyNSElt import_module tyc = let
  ghcName      = GHC.getName tyc
  oname        = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
  fullname     = T.concat [import_module, T.pack ".", oname]
  args         = [ Var . (T.append $ T.pack "arg_") . T.pack . show $ i
                 | i <- [1..GHCTyCon.tyConArity tyc]] :: [Var]
#if __GLASGOW_HASKELL__ >= 800
  in case GHCTyCon.expandSynTyCon_maybe tyc args of
#else
  in case GHCTyCon.tcExpandTyCon_maybe tyc args of
#endif
#if __GLASGOW_HASKELL__ >= 906
    GHCTyCon.NoExpansion -> do
#else
    Nothing -> do
#endif
      (tyc', _) <- transformGHCTyc (Just $ InExplicitModuleNamed import_module) tyc
      return (if tyc' == fnTyCon then T.pack "(->)" else oname, Left tyc')
#if __GLASGOW_HASKELL__ >= 906
    GHCTyCon.ExpandsSyn assigs expansion leftoverVars -> case leftoverVars of
#else
    Just (assigs, expansion, leftoverVars) -> case leftoverVars of
#endif
      (_:_) -> error "transformGHCTyNSElt: unexpected leftover vars"
      []    -> do let doAssig (tyv, var) = do ((tyv', k), _) <- transformGHCTyVar tyv
                                              return (tyv', mkHsType (Right (var, k)) [])
                  assigs'      <- mapM doAssig assigs
                  let substMap  = Map.fromList assigs'
                  hst          <- transformGHCType fullname True expansion
                  return (oname, Right $ transformType substMap hst)


-- | When we transform Ghc types into HsTypes, we need to annotate
-- every TyCon that gets mentioned in the output with a way of
-- importing it. This we do by keeping track of how one could have
-- imported/accessed the type of the original HsType being converted,
-- and then memorizing how to traverse the parse tree of that type to
-- get down to the TyCon in question. We keep track of this
-- information, as we go along, in the form of a PreTycLoc. The 'Text'
-- is the fully qualified name of some type constructor or object
-- constructor that implicates the original HsType we're trying to
-- convert. The Bool records whether it is a type constructor or an
-- object constructor. The [Int] tells us how to traverse the parse
-- tree.
--
-- Note that the 'ways of importing' we construct at this stage might
-- be superseded by 'better ways' down the line (basically, if the
-- TyCon itself can be easily imported form somewhere); see
-- canonicalizePreModuleTycs below.

data PreTycLoc = PreTycLoc Text Bool [Int]

-- | Convert a PreTycLoc, and an Int which tells us a final move we
-- need to take in our traversal of the patse tree, make a
-- TyCLocation.

finalizePreTycLoc :: Int -> PreTycLoc -> Maybe TyCLocation
finalizePreTycLoc i (PreTycLoc n ivt locs)
  | n /= T.pack "" = Just $ ImplicitlyVia n ivt (reverse $ i:locs)
  | otherwise      = Nothing

-- | Transform a GHC.Type into an HsType, or fail if it's kind is too
-- complicated (say, if it involves the kind of unboxed types). We
-- need to know where we can import/access the original type being
-- converted (so that we will know how to access the TyCons
-- therein). This is provided as a Text (fully qualified name of some
-- type constructor or object constructor that has or is this type)
-- and an Int (to let us know whether the name is a name of a type
-- constructor or object constructor).

applyChecksAndStripChecked :: [a -> Bool] -> [a] -> Maybe [a]
applyChecksAndStripChecked (_:_)  [] = Nothing
applyChecksAndStripChecked [] args   = Just args
applyChecksAndStripChecked (chk:chks) (arg:args) = case chk arg of
  True  -> applyChecksAndStripChecked chks args
  False -> Nothing

transformGHCType :: Text -> Bool -> GHC.Type -> Maybe HsType
transformGHCType n ivt = transformGHCType' (PreTycLoc n ivt []). GHCType.expandTypeSynonyms

transformGHCType' :: PreTycLoc -> GHC.Type -> Maybe HsType
transformGHCType' locSoFar typ
#if __GLASGOW_HASKELL__ >= 902
  = let (vars,   rest)  = GHC.splitForAllTyCoVars typ
#else
  = let (vars,   rest)  = GHC.splitForAllTys typ
#endif
    in case splitConstraint rest of
      (Just c, rest') -> transformGHCType' locSoFar rest'
      (Nothing, _)    -> case GHCType.splitTyConApp_maybe rest of
        Just (tyc, args) -> do
          (kind', _)      <- dePolyGHCKind $ GHCType.typeKind rest
          unpackSimpleGHCKind kind'  -- check result type is of a kind we support
                                     -- (lifted, etc.)
          (tyc', checks)  <- transformGHCTyc (finalizePreTycLoc (length args) locSoFar) tyc
          args_remaining  <- applyChecksAndStripChecked checks args
          args'           <- transformGHCTypes locSoFar args_remaining
          return $ mkHsType (Left tyc') args'
        Nothing          ->
          let (head, args) = GHCType.splitAppTys rest in case GHCType.getTyVar_maybe head of
            Just tyv        -> do
              (tyv', checks)  <- transformGHCTyVar tyv
              args_remaining  <- applyChecksAndStripChecked checks args
              args'           <- transformGHCTypes locSoFar args_remaining
              return $ mkHsType (Right tyv') args'
            Nothing         -> Nothing

-- | Utility function, used in the recursion of transformGHCType' to
-- construct HsType objects corresponding to all the child nodes of
-- the parse tree of some GHC.Type.

transformGHCTypes :: PreTycLoc -> [GHC.Type] -> Maybe [HsType]
transformGHCTypes (PreTycLoc mn on sf) tys = let
  subLocs = map ((PreTycLoc mn on) . (:sf)) [0..]
  in sequence . reverse $ zipWith transformGHCType' subLocs (reverse tys)

-- | Convert a @GHC.TyVar@ to a @(Var, Kind)@ (our preferred
-- representation of a TyVar). Will fail, returning @Nothing@, if the
-- @Kind@ is too complicated (e.g. if it involves the kind of unboxed
-- types).

transformGHCTyVar :: GHC.TyVar -> Maybe ((Var, Kind), [GHC.Type -> Bool])
transformGHCTyVar tyv = do
  (k', argChecks) <- dePolyGHCKind $ GHCVar.tyVarKind tyv
  k''             <- unpackSimpleGHCKind k'
  let n  = Var . T.pack . GHCOccName.occNameString . GHC.getOccName $ GHCVar.tyVarName tyv
  return ((n, k''), argChecks)

-- | Split constraints off of a GHC Type.

splitConstraint :: GHC.Type -> (Maybe GHC.Type, GHC.Type)
splitConstraint ty = case GHCType.splitFunTy_maybe ty of
  Nothing          -> (Nothing, ty)
#if __GLASGOW_HASKELL__ >= 906
  Just (_, _, src, dest) -> if GHCType.isConstraintKind (GHCType.typeKind src)
                               then (Just src, dest) else (Nothing, ty)
#elif __GLASGOW_HASKELL__ >= 900
  Just (_, src, dest) -> if GHCType.tcIsConstraintKind (GHCType.typeKind src)
                            then (Just src, dest) else (Nothing, ty)
#elif __GLASGOW_HASKELL__ >= 806
  Just (src, dest) -> if GHCType.tcIsConstraintKind (GHCType.typeKind src)
                         then (Just src, dest) else (Nothing, ty)
#elif __GLASGOW_HASKELL__ >= 800
  Just (src, dest) -> if GHCKind.isConstraintKind (GHCType.typeKind src)
                         then (Just src, dest) else (Nothing, ty)
#else
  Just (src, dest) -> case GHCType.splitTyConApp_maybe (GHCType.typeKind src) of
    Nothing -> (Nothing, ty)
    Just (tyc, _) -> if tyc == GHCType.constraintKindTyCon
                     then (Just src, dest) else (Nothing, ty)
#endif

-- | Transform a GHC 'id' (basically, an object namespace thing that
-- isn't a data constructor) into a (<name>, PreObj) pair.

transformGHCId :: Text -> GHC.Id -> Maybe (Text, PreObj)
transformGHCId import_module i = do
  let ghcName = GHC.getName i
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
      fullN   = T.concat [import_module, T.pack ".", oname]
  hst <- transformGHCType fullN False $ GHC.idType i
  return (oname, (fullN, hst))

-- | Certain data constructors (all tuples, at the moment) need to
-- have their constructors and co-constructors built in a special way
-- (basically, because GHC thinks tells us the constructor is called
-- something like GHC.Tuple.(,,), but to use it we actually ought to
-- write just (,,)). Here is a list of overrides. It's a map data
-- constructor name -> (code to give data constructor, code to give
-- co-constructor).

dataConSpecials :: HashMap Text (Text, Text)
dataConSpecials = HashMap.fromList $ map (T.pack *** (T.pack *** T.pack)) $ [
  ("GHC.Tuple.()"                ,("()",                "(:[])")),
  ("GHC.Tuple.(,)"               ,("(,)",               "(:[])")),
  ("GHC.Tuple.(,,)"              ,("(,,)",              "(:[])")),
  ("GHC.Tuple.(,,,)"             ,("(,,,)",             "(:[])")),
  ("GHC.Tuple.(,,,,)"            ,("(,,,,)",            "(:[])")),
  ("GHC.Tuple.(,,,,,)"           ,("(,,,,,)",           "(:[])")),
  ("GHC.Tuple.(,,,,,,)"          ,("(,,,,,,)",          "(:[])")),
  ("GHC.Tuple.(,,,,,,,)"         ,("(,,,,,,,)",         "(:[])")),
  ("GHC.Tuple.(,,,,,,,,)"        ,("(,,,,,,,,)",        "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,)"       ,("(,,,,,,,,,)",       "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,)"      ,("(,,,,,,,,,,)",      "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,,)"     ,("(,,,,,,,,,,,)",     "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,,,)"    ,("(,,,,,,,,,,,,)",    "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,,,,)"   ,("(,,,,,,,,,,,,,)",   "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,,,,,)"  ,("(,,,,,,,,,,,,,,)",  "(:[])")),
  ("GHC.Tuple.(,,,,,,,,,,,,,,,)" ,("(,,,,,,,,,,,,,,,)", "(:[])")),
  ("GHC.Tuple.Prim.()"                ,("()",                "(:[])")),
  ("GHC.Tuple.Prim.(,)"               ,("(,)",               "(:[])")),
  ("GHC.Tuple.Prim.(,,)"              ,("(,,)",              "(:[])")),
  ("GHC.Tuple.Prim.(,,,)"             ,("(,,,)",             "(:[])")),
  ("GHC.Tuple.Prim.(,,,,)"            ,("(,,,,)",            "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,)"           ,("(,,,,,)",           "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,)"          ,("(,,,,,,)",          "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,)"         ,("(,,,,,,,)",         "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,)"        ,("(,,,,,,,,)",        "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,)"       ,("(,,,,,,,,,)",       "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,)"      ,("(,,,,,,,,,,)",      "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,,)"     ,("(,,,,,,,,,,,)",     "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,,,)"    ,("(,,,,,,,,,,,,)",    "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,,,,)"   ,("(,,,,,,,,,,,,,)",   "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,,,,,)"  ,("(,,,,,,,,,,,,,,)",  "(:[])")),
  ("GHC.Tuple.Prim.(,,,,,,,,,,,,,,,)" ,("(,,,,,,,,,,,,,,,)", "(:[])"))
  ]

-- | Convert a GHC Data constructor into two @(<name>, PreObj)@ pairs;
-- one for the Data constructor itself (just thought of as a function)
-- and one for the 'co-data constructor', a function we build on the
-- fly which transforms an object constructed using the Data
-- Constructor into a singleton list containing a tuple containing the
-- arguments that were given to the Data Constructor to construct the
-- object. (If we apply the 'co-data constructor' to something that
-- was constructed using a different Data constructor for the same
-- type, we get back an empty list.)

transformGHCDataCon :: Text -> GHC.DataCon -> [] (Text, PreObj)
transformGHCDataCon import_module dc = fromMaybe [] $ do
  let ghcName = GHC.getName dc
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
      fullnam = T.concat [import_module, T.pack ".", oname]
  dctyp       <- transformGHCType fullnam False $ GHC.dataConType dc
  let (types, resultType) = breakFnTypeRec dctyp
      ntypes  = length types
      ntypes' = min 50 ntypes
      tupTyp  = if ntypes == 1 then head types
                else mkHsType (Left . tupleTyCon $ ntypes') (take ntypes' types)
      codctyp = mkHsType (Left fnTyCon) [
        resultType, mkHsType (Left listTyCon) [tupTyp]]
      vars    = [T.pack ("a" ++ show i) | i <- [0..ntypes-1]]
      commaVars = T.intercalate (T.pack ", ") $ take ntypes' vars
      spaceVars = T.unwords vars
  return $ case HashMap.lookup fullnam dataConSpecials of
    Nothing -> [
      (oname, (fullnam, dctyp)),
      (T.concat [T.pack "*co-", oname], (T.concat [
         T.pack "(\\ x -> [(", commaVars, T.pack ") | ", fullnam, T.pack " ", spaceVars,
         T.pack " <- [x]])"] , codctyp))]
    Just (code, cocode) -> [(oname, (code, dctyp)),
                            (T.concat [T.pack "*co-", oname], (cocode, codctyp))]

-- | Convert a GHC TyThing into zero or more (Text, PreObj) pairs
-- (actually, always at most one) and into zero, one or two (Text,
-- TyNSElt) pairs.

transformGHCTyThing :: Text -> GHC.TyThing -> ([(Text, PreObj)], [(Text, TyNSElt)])
transformGHCTyThing im (GHCType.AnId     id) = (maybeToList $ transformGHCId      im id, [])
#if __GLASGOW_HASKELL__ >= 708
transformGHCTyThing im (GHCType.AConLike (GHCConLike.RealDataCon dc))
                                             = (transformGHCDataCon im dc,               [])
#else
transformGHCTyThing im (GHCType.ADataCon dc) = (transformGHCDataCon im dc,               [])
#endif
transformGHCTyThing im (GHCType.ATyCon   tc) = ([], maybeToList (transformGHCTyNSElt im tc))
transformGHCTyThing _  _                     = ([],                                      [])

-- | Turn a list of GHC.TyThings into a 'Pre module'. This is like our
-- representation of a module except that we have PreObjs instead of
-- HsObjs. In particular, it's a pair (HashMap Text PreObj, HashMap
-- Text TyNSElt) where the first hashmap is the object namespace and
-- the second is the type namespace; in this, a TyNSElt is simply
-- Either TyCon HsType, where a TyCon is what you might expect and an
-- HsType represents a type synonym/typedef (with the free variables
-- of the HsType corresponding to the parameters of the typedef). We
-- want to be able to convert such a thing to a python object, which
-- is what pythonateModuleRet does.

makePreModule :: Text -> [GHC.TyThing] -> (HashMap Text PreObj, HashMap Text TyNSElt)
makePreModule im tyths = let
  (objs, tynselts) = mconcat (map (transformGHCTyThing im) tyths)
  in (HashMap.fromList objs, HashMap.fromList tynselts)

---------------------

-- The third step of importing stuff from Haskell; convert PreObjs to
-- HsObjs. This is done in the PythonM monad (and needs access to the
-- GHCI session).

-- | Turn a PreObj into an HsObj. In the PythonM monad.

createObj :: GhcMonad.Session -> PreObj -> PythonM HsObj
createObj sess (code, orig_type) = do
  memoTable <- lift $ newIORef HashMap.empty
  let core :: HsType -> PythonM HsObj
      core hst = do
        curMemoTable <- lift $ readIORef memoTable
        hobj <- case HashMap.lookup hst curMemoTable of
          Just found -> return found
          Nothing    -> do
            let to_eval = T.unpack . T.concat $ [
                  T.pack "(", makeTypeForcer hst, T.pack " (", code, T.pack "))"]
            --let to_eval = T.unpack . T.concat $ [
            --      T.pack "((", code, T.pack ") :: ", typeName hst, T.pack ")"]
            made <- performGHCOps (Just $ "evaluate " ++ to_eval) sess (
              GHC.compileExpr to_eval)
            lift . atomicModifyIORef memoTable $ \oldTable ->
              (HashMap.insert hst made oldTable, ())
            return made
        return $ MonoObj hst (Unsafe.Coerce.unsafeCoerce hobj)
  tryMakeMonomorphic $ PolyObj orig_type orig_type (PolyObjCore core) []

-- | Convert a whole PreModule (as returned from makePreModule) into
-- our representation of a final module, by transforming the PreObjs
-- to HsObjs.

createModule :: GhcMonad.Session -> (HashMap Text PreObj, HashMap Text TyNSElt) ->
              PythonM (HashMap Text HsObj, HashMap Text TyNSElt)
createModule sess (preobjs, tynselts) = do
  objs <- Data.Traversable.mapM (createObj sess) preobjs
  return (objs, tynselts)

----------------------

-- In the code above, which is the main code which imports modules
-- using GHCI and gets them ready to pass to Python, we construct a
-- lot of TyCons, and when we do so we are always careful to mention a
-- way that the TyCon can be accessed from importable names. It's
-- always quite a complicated and convoluted way of access, involving
-- importing some type alias or object, and then spelunking thorough
-- its parse tree. In most cases, this is much too complicated---the
-- TyCon in question will just be importable from some easy-to-find
-- module, and we should just import it.

-- This next section of code handles coming up with a good list of
-- modules where we think we might be able to find a given TyCon as
-- immediately importable (and the combined list for all the TyCons
-- mentioned in a given HsType, or a given module); getting GHC to
-- read those modules to see if, in fact, they do export the TyCon in
-- question, and finally modifying TyCons (or HsTypes, or whole
-- PreModules) by replacing the TyCons' TyCLocations with better ones,
-- if we found them.

-- | Given a tycon, determine a good list of modules where we think we
-- might be able to find it as immediately importable. Basically, it's
-- the module of definition of the tycon and all it's ancestor
-- modules, EXCEPT that if we're dealing with a GHC.blah module we
-- don't search GHC.

tyConOtherModsOfInterest :: TyCon -> Set Text
tyConOtherModsOfInterest tyc = Set.fromList . modAncestry . tyConModule $ tyc

-- | Utility function, used by tyConOtherModsOfInterest above.

modAncestry :: Text -> [Text]
modAncestry mod = map (T.intercalate dot) . drop n . inits . T.splitOn dot $ mod
  where dot       = T.singleton '.'
        n | T.take 4 mod == T.pack "GHC." = 2
          | otherwise                     = 1

-- | Given an HsType, determine a good list of modules where we think
-- we might be able to find TyCons mentioned it it as immediately
-- importable.

hsTypeOtherModsOfInterest :: HsType -> Set Text
hsTypeOtherModsOfInterest hst = let
  headPart = either tyConOtherModsOfInterest (const Set.empty) (typeHead hst)
  in mconcat (headPart : map hsTypeOtherModsOfInterest (typeTail hst))

-- | Given an Pre-module, determine a good list of modules where we think
-- we might be able to find TyCons mentioned it it as immediately
-- importable.

preModuleOtherModsOfInterest :: (HashMap Text PreObj, HashMap Text TyNSElt) -> Set Text
preModuleOtherModsOfInterest (preobjs, tynselts)
  = mconcat . map hsTypeOtherModsOfInterest $ (
    (map snd $ HashMap.elems preobjs) ++ (rights $ HashMap.elems tynselts))

-- | Given the name of a module, scan it for TyCons that it
-- exports. If we find any, provide a map which sends a given TyCon to
-- a 'better' version in which the TyCLocation is set to import it
-- from the module in question.

readGHCModuleTycCanon :: Text -> GhcMonad.Ghc (HashMap TyCon TyCon, Maybe Text)
#if __GLASGOW_HASKELL__ >= 900
readGHCModuleTycCanon mname = MC.handle handler $ do
#else
readGHCModuleTycCanon mname = GHCException.ghandle handler $ do
#endif
  --GHCMonadUtils.liftIO $ print mname
  result <- reportingGHCErrors Nothing $ do
    tyths      <- readGHCModule mname
    let nselts  = mapMaybe (transformGHCTyNSElt mname) [tc | GHCType.ATyCon tc <- tyths]
        tycons  = lefts $ map snd nselts
    return . HashMap.fromList $ zip tycons tycons
  return $ case result of
    Left errmsg  -> (HashMap.empty, Nothing)
    Right answer -> (answer,    Just mname)
  where handler :: (Monad m) => Control.Exception.SomeException -> m (
          HashMap TyCon TyCon, Maybe Text)
        handler _ = return (HashMap.empty, Nothing)

-- | Given a map from TyCons to 'better' versions of the same TyCons
-- where placeholder TyCLocations have been replaced by something
-- nicer, convert a Pre-module into a new version of that pre-module
-- by replacing all TyCons with better versions, if available.

canonicalizePreModuleTycs :: HashMap TyCon TyCon -> (HashMap Text PreObj, HashMap Text TyNSElt)
                             -> (HashMap Text PreObj, HashMap Text TyNSElt)
canonicalizePreModuleTycs canonMap (preobjs, tynselts)
  = let canonicalizePreObj (code, hst) = (code, canonicalizeHsType hst)
        canonicalizeTyNSElt = either Left (Right . canonicalizeHsType)

        canonicalizeHsType :: HsType -> HsType
        canonicalizeHsType (HsType _ (Left tyc) tail _ _ _)
         = mkHsType (Left $ canonicalizeTyc tyc) (map canonicalizeHsType tail)
        canonicalizeHsType (HsType _ (Right vk) tail _ _ _)
         = mkHsType (Right vk)                   (map canonicalizeHsType tail)
        canonicalizeTyc tyc = HashMap.lookupDefault tyc tyc canonMap
    in (HashMap.map canonicalizePreObj preobjs, HashMap.map canonicalizeTyNSElt tynselts)

----

-- | Ensure that a set of modules (provided as a Set Text giving the
-- module names) is in scope, *qualified*, in the ghci session.

ensureModulesInContext :: Set Text -> GhcMonad.Ghc ()
ensureModulesInContext toEnsure = do
  curiis <- GHC.getContext
  let curImps = Set.fromList [
        T.pack . GHC.moduleNameString . GHC.unLoc $ GHC.ideclName decl
        | GHC.IIDecl decl <- curiis]
      newImps  = toEnsure `Set.difference` curImps
  GHC.setContext $ [
    (GHC.IIDecl $ (GHC.simpleImportDecl . GHC.mkModuleName . T.unpack $ modName)  {
#if __GLASGOW_HASKELL__ >= 810
        GHC.ideclQualified = GHC.QualifiedPre})
#else
        GHC.ideclQualified = True})
#endif
    | modName <- Set.toList newImps] ++ curiis

-- | Make a string like (x1, x2, x3, ..., xn), where the xs are all
-- spelt out and numbered from low to high.

mkTupStr :: Int -> Int -> String
mkTupStr low high = "(" ++ intercalate "," (map (('x':) . show) [low..high]) ++ ")"

-- There are certain functions that are needed to be able to construct
-- lists and to construct and break apart tuples that are built in to
-- the Haskell language and therefore not importable form anywhere,
-- even the Prelude. We need to expose them to python though. This is
-- done through 'accessBasics', which provides a pseudo-module
-- containing what we need. The things we need are named in
-- basicsByName, which is a list of pairs (<name under which thing
-- should be exposed to python>, <haskell code to make that thing>)

basicsByName = (
  [("[]", "[]"), ("()", "()"), ("(:)", "(:)")] ++
  [(a,a) | a <- ["(" ++ replicate n ',' ++ ")" | n<-[1..15]]] ++
  [("tup" ++ show n ++ "head", "\\ " ++ mkTupStr 1 n ++ " -> x1") | n<-[2..15]] ++
  [("tup" ++ show n ++ "tail", "\\ " ++ mkTupStr 1 n ++ " -> " ++ mkTupStr 2 n)
  | n<-[2..15]])

accessBasics :: GhcMonad.Session -> PythonM (HashMap Text HsObj)
accessBasics sess = do
  let evalToPreObj :: (String, String) -> GhcMonad.Ghc (Maybe (Text, PreObj))
      evalToPreObj (name, expr) = do
#if __GLASGOW_HASKELL__ >= 802
        exprTy <- GHC.exprType GHC.TM_Inst expr
#else
        exprTy <- GHC.exprType expr
#endif
        return $ do
          hst <- transformGHCType (T.pack "") False exprTy
          return (T.pack name, (T.pack expr, hst))
  preobjs <- performGHCOps (Just "access primitive operations") sess $ do
    maybePreobjs <- mapM evalToPreObj basicsByName
    return . HashMap.fromList . catMaybes $ maybePreobjs
  Data.Traversable.mapM (createObj sess) preobjs

-- | Import a list of library modules. Given a Ghc session (needed to
-- import anything), and a list of modules to import, we return a map
-- @<module name> -> <imported module contents>@ where the imported
-- module contents is a pair @(<object namespace>, <type constructor
-- namespace>)@, where @<object namespace>@ is a map from name to
-- @HsObj@, and where @<type constructor namespace>@ is a map from
-- name to TyNSElt, which represents an element of the type
-- (constructor) namespace. Specifically a TyNSElt is simply Either
-- TyCon HsType, where a TyCon is what you might expect and an HsType
-- represents a type synonym/typedef (with the free variables of the
-- HsType corresponding to the parameters of the typedef).


importLibModules :: GhcMonad.Session -> [Text] -> PythonM (
  HashMap Text (HashMap Text HsObj, HashMap Text TyNSElt))
importLibModules sess names = do
  preModules <- performGHCOps Nothing sess $ do
    modContents        <- mapM readGHCModule names
    let preModules0     = zipWith makePreModule names modContents
        additionalMods  = mconcat (map preModuleOtherModsOfInterest preModules0)
    (canonMaps, okMds) <- unzip <$> mapM readGHCModuleTycCanon (Set.toList additionalMods)
    ensureModulesInContext (Set.fromList $ names ++ catMaybes okMds)
    let canonMap0       = foldr (HashMap.unionWith pickBestTyc) HashMap.empty canonMaps
        canonMap        = HashMap.insert listTyCon listTyCon canonMap0
    return $ fmap (canonicalizePreModuleTycs canonMap) preModules0
  modules     <- mapM (createModule sess) preModules
  return . HashMap.fromList $ zip names modules

-- | Import a list of source modules. Given a Ghc session (needed to
-- import anything), and a list of FILENAMES OF source files
-- containing modules to import, we return a map @<module name> ->
-- <imported module contents>@ where the imported module contents is a
-- pair @(<object namespace>, <type constructor namespace>)@, where
-- @<object namespace>@ is a map from name to @HsObj@, and where
-- @<type constructor namespace>@ is a map from name to TyNSElt, which
-- represents an element of the type (constructor)
-- namespace. Specifically a TyNSElt is simply Either TyCon HsType,
-- where a TyCon is what you might expect and an HsType represents a
-- type synonym/typedef (with the free variables of the HsType
-- corresponding to the parameters of the typedef).

importSrcModules :: GhcMonad.Session -> [Text] -> PythonM (
  HashMap Text (HashMap Text HsObj, HashMap Text TyNSElt))
importSrcModules sess paths = do
  srcModuleNames <- performGHCOps Nothing sess $ do
#if __GLASGOW_HASKELL__ >= 904
    dflags <- GhcMonad.getSessionDynFlags
    GHC.setTargets [GHC.Types.Target.Target {
                       GHC.Types.Target.targetId = GHC.Types.Target.TargetFile (T.unpack path) Nothing,
                       GHC.Types.Target.targetAllowObjCode = True,
                       GHC.Types.Target.targetUnitId       = GHCDynFlags.homeUnitId_ dflags,
                       GHC.Types.Target.targetContents     = Nothing}             | path <- paths]
#elif __GLASGOW_HASKELL__ >= 902
    GHC.setTargets [GHC.Types.Target.Target {
                       GHC.Types.Target.targetId = GHC.Types.Target.TargetFile (T.unpack path) Nothing,
                       GHC.Types.Target.targetAllowObjCode = True,
                       GHC.Types.Target.targetContents     = Nothing}             | path <- paths]
#else
    GHC.setTargets [GHCHscTypes.Target {
                       GHCHscTypes.targetId = GHCHscTypes.TargetFile (T.unpack path) Nothing,
                       GHCHscTypes.targetAllowObjCode = True,
                       GHCHscTypes.targetContents     = Nothing}             | path <- paths]
#endif
#if __GLASGOW_HASKELL__ >= 902
    moduleGraph <- liftM GHC.Unit.Module.Graph.mgModSummaries $ GHC.depanal [] True
#elif __GLASGOW_HASKELL__ >= 804
    moduleGraph <- liftM GHCHscTypes.mgModSummaries $ GHC.depanal [] True
#else
    moduleGraph <- GHC.depanal [] True
#endif
    curiis      <- GHC.getContext
    loadOK      <- GHC.load GHC.LoadAllTargets
    GHC.setContext curiis
    case loadOK of
      GHC.Succeeded ->
#if __GLASGOW_HASKELL__ >= 902
        return [T.pack . GHCModuleName.moduleNameString
                . GHCModule.moduleName . GHC.Unit.Module.ModSummary.ms_mod $ ms | ms <- moduleGraph]
#elif __GLASGOW_HASKELL__ >= 900
        return [T.pack . GHCModuleName.moduleNameString
                . GHCModule.moduleName . GHCHscTypes.ms_mod $ ms | ms <- moduleGraph]
#else
        return [T.pack . GHCModule.moduleNameString
                . GHCModule.moduleName . GHCHscTypes.ms_mod $ ms | ms <- moduleGraph]
#endif
      GHC.Failed    -> return []
  importLibModules sess srcModuleNames
