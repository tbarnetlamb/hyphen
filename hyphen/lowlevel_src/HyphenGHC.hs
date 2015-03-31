{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenGHC (createGHCSession, importLibModules, importSrcModules, accessBasics) where

--import Debug.Trace
import Control.Arrow
import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Text                      (Text)
import Data.Set                       (Set)
import Data.Map.Strict                (Map)
import qualified Data.Text            as T
import qualified Data.Set             as Set
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Traversable
import qualified Unsafe.Coerce
import qualified GHC
import qualified GHC.Paths
import qualified StaticFlags as GHCStaticFlags
import qualified SysTools    as GHCSysTools
import qualified TyCon       as GHCTyCon
import qualified OccName     as GHCOccName
import qualified Module      as GHCModule
import qualified Type        as GHCType
import qualified DynFlags    as GHCDynFlags
import qualified HscMain     as GHCHscMain
import qualified MonadUtils  as GHCMonadUtils
import qualified Var         as GHCVar
import qualified Outputable  as GHCOutputable
import qualified ErrUtils    as GHCErrUtils
import qualified GhcMonad

import HyphenBase
import PythonBase
import HyphenKinds
import HyphenTyCon
import HsType
import HsObjRaw
import HyphenExceptions


ourInitGhcMonad :: (GHC.GhcMonad m) => Maybe FilePath -> m ()
ourInitGhcMonad mb_top_dir = do
  -- initGHCMonad, as defined in the GHC API, annoyingly sets several
  -- totally inappropriate signal handlers, which will completely mess
  -- with Python's signal handlers. So we have to have our own
  -- version. This function, alas, must be kept in sync with the GHC
  -- version of which it is mostly a clone
  GHCMonadUtils.liftIO $ GHCStaticFlags.initStaticOpts

  mySettings <- GHCMonadUtils.liftIO $ GHCSysTools.initSysTools mb_top_dir
  dflags <- GHCMonadUtils.liftIO 
            $ GHCDynFlags.initDynFlags (GHCDynFlags.defaultDynFlags mySettings)
  env <- GHCMonadUtils.liftIO $ GHCHscMain.newHscEnv dflags
  GHC.setSession env

performGHCOps  :: GhcMonad.Session -> GhcMonad.Ghc a -> PythonM a
performGHCOps sess ghcAct = promoteErr =<< (
  translatingHsExcepts . lift $ flip GhcMonad.reflectGhc sess $ reportingGHCErrors ghcAct)

reportingGHCErrors :: GhcMonad.Ghc a -> GhcMonad.Ghc (Either ErrMsg a)
reportingGHCErrors action = do
  logref <- GHCMonadUtils.liftIO $ newIORef ""
  dflags <- GhcMonad.getSessionDynFlags
  GHC.setSessionDynFlags $ dflags { GHCDynFlags.log_action = logHandler logref }
  result <- action
  problems <- GHCMonadUtils.liftIO $ readIORef logref
  GHC.setSessionDynFlags dflags
  return $ if (problems == "") then Right result else report problems
 where logHandler :: IORef String -> GHCDynFlags.LogAction
       logHandler ref dflags severity srcSpan style msg =
           case severity of
             GHCErrUtils.SevError ->  modifyIORef' ref (++ printDoc)
             GHCErrUtils.SevFatal ->  modifyIORef' ref (++ printDoc)
             _                    ->  return () -- ignore the rest
         where cntx = GHCOutputable.initSDocContext dflags style
               locMsg = GHCErrUtils.mkLocMessage severity srcSpan msg
               printDoc = show (GHCOutputable.runSDoc locMsg cntx)

createGHCSession :: PythonM GhcMonad.Session
createGHCSession = do
  ref <- lift $ newIORef (error "Empty session")
  let session = GhcMonad.Session ref
  translatingHsExcepts . lift . flip GhcMonad.reflectGhc session $ (
    ourInitGhcMonad (Just GHC.Paths.libdir))
  return session

unpackGHCKind :: GHC.Kind -> Maybe Kind
unpackGHCKind k 
  | GHCType.isFunTy k   = do (Kind ks') <- unpackGHCKind $ GHCType.funResultTy k
                             (Kind . (: ks')) <$> unpackGHCKind (GHCType.funArgTy k)
  | otherwise           = do (kcon, _) <- GHCType.splitTyConApp_maybe k
                             guard (kcon == GHCType.liftedTypeKindTyCon) >> (Just $ Kind [])

transformGHCTyc :: GHC.TyCon -> Maybe (Text, TyCon)
transformGHCTyc tyc = do
  let ghcName = GHC.getName tyc
      modl    = GHC.nameModule ghcName
      pckg    = T.pack . GHCModule.packageIdString $ GHC.modulePackageId modl
      mname   = T.pack . GHC.moduleNameString $ GHC.moduleName modl
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
  kind <- unpackGHCKind $ GHCTyCon.tyConKind tyc
  return $ (oname, mkTyCon pckg mname oname kind (GHC.isClassTyCon tyc))

transformGHCType :: GHC.Type -> Maybe HsType
transformGHCType = transformGHCType' . GHCType.expandTypeSynonyms

transformGHCType' :: GHC.Type -> Maybe HsType
transformGHCType' typ 
  = let (vars,   rest)  = GHC.splitForAllTys typ
    in case splitConstraint rest of
      (Just c, rest') -> transformGHCType' rest'
      (Nothing, _)    -> case GHCType.splitTyConApp_maybe rest of
        Just (tyc, args) -> do 
          unpackGHCKind $ GHCType.typeKind rest -- check result type is of a kind we support 
                                                -- (lifted, etc.)
          args'     <- mapM transformGHCType' args
          (_, tyc') <- transformGHCTyc tyc
          return $ mkHsType (Left tyc') args'
        Nothing          -> 
          let (head, args) = GHCType.splitAppTys rest in case GHCType.getTyVar_maybe head of
            Just tyv        -> do
              tyvKind     <- unpackGHCKind $ GHCVar.tyVarKind tyv
              let tyvName = Var . T.pack . GHCOccName.occNameString . GHC.getOccName $
                            GHCVar.tyVarName tyv
              args' <- mapM transformGHCType' args
              return $ mkHsType (Right (tyvName, tyvKind)) args'
            Nothing         ->  Nothing

splitConstraint :: GHC.Type -> (Maybe GHC.Type, GHC.Type)
splitConstraint ty = case GHCType.splitFunTy_maybe ty of
  Nothing          -> (Nothing, ty)
  Just (src, dest) -> case GHCType.splitTyConApp_maybe (GHCType.typeKind src) of
    Nothing -> (Nothing, ty)
    Just (tyc, _) -> if tyc == GHCType.constraintKindTyCon
                     then (Just src, dest) else (Nothing, ty)

type PreObj = (Text, HsType)

createObj :: GhcMonad.Session -> PreObj -> PythonM Obj
createObj sess (code, orig_type) = do
  memoTable <- lift $ newIORef HashMap.empty
  let core :: HsType -> PythonM Obj
      core hst = do
        curMemoTable <- lift $ readIORef memoTable
        hobj <- case HashMap.lookup hst curMemoTable of
          Just found -> return found
          Nothing    -> do
            -- lift . traceIO . T.unpack . T.concat $ [ T.pack "about to eval: ", 
            --   T.pack "((", code, T.pack ") :: ", typeName hst, T.pack ")"]
            made <- performGHCOps sess . GHC.compileExpr . T.unpack . T.concat $ [
              T.pack "((", code, T.pack ") :: ", typeName hst, T.pack ")"]
            lift . atomicModifyIORef memoTable $ \oldTable ->
              (HashMap.insert hst made oldTable, ())
            return made
        return $ MonoObj hst (Unsafe.Coerce.unsafeCoerce hobj)
  tryMakeMonomorphic $ PolyObj orig_type orig_type (PolyObjCore core) []

transformGHCId :: Text -> GHC.Id -> Maybe (Text, PreObj)
transformGHCId import_module i = do
  let ghcName = GHC.getName i
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
  hst <- transformGHCType $ GHC.idType i
  return (oname, (T.concat [import_module, T.pack ".", oname], hst))

dataConSpecials :: Map Text (Text, Text)
dataConSpecials = Map.fromList $ map (T.pack *** (T.pack *** T.pack)) $ [
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
  ("GHC.Tuple.(,,,,,,,,,,,,,,,)" ,("(,,,,,,,,,,,,,,,)", "(:[])"))
  ]

transformGHCDataCon :: Text -> GHC.DataCon -> [] (Text, PreObj)
transformGHCDataCon import_module dc = fromMaybe [] $ do
  dctyp       <- transformGHCType $ GHC.dataConType dc
  let ghcName = GHC.getName dc
      oname   = T.pack . GHCOccName.occNameString $ GHC.getOccName ghcName
      (_, _, types, resultType) = GHC.dataConSig dc
  types'      <- mapM transformGHCType types
  resultType' <- transformGHCType resultType
  let ntypes  = length types
      codctyp = mkHsType (Left fnTyCon) [
        resultType',
        mkHsType (Left listTyCon) [
          mkHsType (Left . tupleTyCon $ ntypes) types']]
      vars    = [T.pack ("a" ++ show i) | i <- [0..ntypes-1]]
      commaVars = T.intercalate (T.pack ", ") vars
      spaceVars = T.unwords vars
      fullnam = T.concat [import_module, T.pack ".", oname]
  return $ case Map.lookup fullnam dataConSpecials of 
    Nothing -> [
      (oname, (fullnam, dctyp)), 
      (T.concat [T.pack "*co-", oname], (T.concat [
         T.pack "(\\ x -> [(", commaVars, T.pack ") | ", fullnam, T.pack " ", spaceVars, 
         T.pack " <- [x]])"] , codctyp))]
    Just (code, cocode) -> [(oname, (code, dctyp)), 
                            (T.concat [T.pack "*co-", oname], (cocode, codctyp))]

transformGHCTyThing :: Text -> GHC.TyThing -> ([(Text, PreObj)], [(Text, TyCon)])
transformGHCTyThing im (GHCType.AnId     id) = (maybeToList $ transformGHCId      im id, [])
transformGHCTyThing im (GHCType.ADataCon dc) = (transformGHCDataCon im dc,               [])
transformGHCTyThing _  (GHCType.ATyCon   tc) = ([],        maybeToList (transformGHCTyc tc))
transformGHCTyThing _  _                     = ([],                                      [])

makePreModule :: String -> [GHC.TyThing] -> (Map Text PreObj, Map Text TyCon)
makePreModule im tyths = let
  im'          = T.pack im
  (objs, tycs) = mconcat (map (transformGHCTyThing im') tyths)
  in (Map.fromList objs, Map.fromList tycs)

createModule :: GhcMonad.Session -> (Map Text PreObj, Map Text TyCon) -> 
              PythonM (Map Text Obj, Map Text TyCon)
createModule sess (preobjs, tycs) = do
  objs <- Data.Traversable.mapM (createObj sess) preobjs
  return (objs, tycs)

readGHCModule :: String -> GhcMonad.Ghc [GHC.TyThing]
readGHCModule name = do
  mod        <- GHC.findModule (GHC.mkModuleName name) Nothing
  mi         <- GHC.getModuleInfo mod
  infos      <- mapM GHC.getInfo $ maybe [] GHC.modInfoExports mi
  return [a |  Just (a, _, _) <- infos]

readGHCModuleTypExpNames :: Text -> GhcMonad.Ghc (Text, Set Text)
readGHCModuleTypExpNames name = do
  mod        <- GHC.findModule (GHC.mkModuleName $ T.unpack name) Nothing
  mi         <- GHC.getModuleInfo mod
  let s = Set.fromList [T.pack . GHCOccName.occNameString $ occname
                       | occname <- map GHC.getOccName $ maybe [] GHC.modInfoExports mi
                       , GHCOccName.occNameSpace occname == GHCOccName.tcName]
  return (name, s)

preModuleAdditionalNames :: (Map Text PreObj, Map Text TyCon) -> Set Text
preModuleAdditionalNames (preobjs, _) 
  = mconcat . map (Map.keysSet . preObjAdditionalNames) . Map.elems $ preobjs

filterPreModule :: Map Text (Set Text) -> (Map Text PreObj, Map Text TyCon)
                   -> (Map Text PreObj, Map Text TyCon)
filterPreModule allowed (preobjs, tycs)
  = (Map.filter (preObjUsesTypesFrom allowed) preobjs, tycs)

preObjAdditionalNames :: PreObj -> Map Text (Set Text)
preObjAdditionalNames (_, hst) = hsTypeAdditionalNames hst

preObjUsesTypesFrom :: Map Text (Set Text) -> PreObj -> Bool
preObjUsesTypesFrom allowed obj = preObjAdditionalNames obj `mapSetSubset` allowed

mapSetSubset :: (Ord a, Ord b) => Map a (Set b) -> Map a (Set b) -> Bool
mapSetSubset = Map.isSubmapOfBy (Set.isSubsetOf)

hsTypeAdditionalNames :: HsType -> Map Text (Set Text)
hsTypeAdditionalNames hst = let
  headPart = either tyConAdditionalNames (const Map.empty) (typeHead hst)
  in Map.unionsWith mappend (headPart : map hsTypeAdditionalNames (typeTail hst))

tyConAdditionalNames :: TyCon -> Map Text (Set Text)
tyConAdditionalNames tyc = Map.fromList [(tyConModule tyc, Set.fromList [tyConName tyc])]

ensureModulesInContext :: Set Text -> GhcMonad.Ghc ()
ensureModulesInContext toEnsure = do
  curiis <- GHC.getContext
  let curImps = Set.fromList [
        T.pack . GHC.moduleNameString . GHC.unLoc $ GHC.ideclName decl
        | GHC.IIDecl decl <- curiis]
      newImps  = toEnsure `Set.difference` curImps
  GHC.setContext $ [
    (GHC.IIDecl $ (GHC.simpleImportDecl . GHC.mkModuleName . T.unpack $ modName)  {
        GHC.ideclQualified = True})
    | modName <- Set.toList newImps] ++ curiis 

wiredInAvailable = map (T.pack *** (Set.fromList . map T.pack)) $ [
  --("GHC.Prim", ["(->)"]),
  ("GHC.Types", ["[]"])
  ]

mkTupStr :: Int -> Int -> String
mkTupStr low high = "(" ++ intercalate "," (map (('x':) . show) [low..high]) ++ ")"

basicsByName = (
  [("[]", "[]"), ("()", "()"), ("(:)", "(:)")] ++ 
  [(a,a) | a <- ["(" ++ replicate n ',' ++ ")" | n<-[1..15]]] ++
  [("tup" ++ show n ++ "head", "\\ " ++ mkTupStr 1 n ++ " -> x1") | n<-[2..15]] ++
  [("tup" ++ show n ++ "tail", "\\ " ++ mkTupStr 1 n ++ " -> " ++ mkTupStr 2 n)
  | n<-[2..15]])

accessBasics :: GhcMonad.Session -> PythonM (Map Text Obj)
accessBasics sess = do
  let evalToPreObj :: (String, String) -> GhcMonad.Ghc (Maybe (Text, PreObj))
      evalToPreObj (name, expr) = do
        exprTy <- GHC.exprType expr
        return $ do
          hst <- transformGHCType exprTy
          return (T.pack name, (T.pack expr, hst))
  preobjs <- performGHCOps sess $ do
    maybePreobjs <- mapM evalToPreObj basicsByName
    return . Map.fromList . catMaybes $ maybePreobjs
  Data.Traversable.mapM (createObj sess) preobjs

importLibModules :: GhcMonad.Session -> [String] -> PythonM (
  Map Text (Map Text Obj, Map Text TyCon))
importLibModules sess names = do
  preModules <- performGHCOps sess $ do
    modContents <- mapM readGHCModule names
    let preModules0     = zipWith makePreModule names modContents
        additionalNames = mconcat (map preModuleAdditionalNames preModules0)
    availableNames     <- mapM readGHCModuleTypExpNames $ Set.toList additionalNames
    let availableNames' = Map.fromListWith (Set.union) (availableNames ++ wiredInAvailable)
    let preModules      = fmap (filterPreModule $ availableNames') preModules0
        names'          = map T.pack names
    ensureModulesInContext (additionalNames `mappend` Set.fromList names')
    return preModules
  modules     <- mapM (createModule sess) preModules
  return . Map.fromList $ zip (map T.pack names) modules

importSrcModules :: GhcMonad.Session -> [String] -> PythonM (
  Map Text (Map Text Obj, Map Text TyCon))
importSrcModules names = tbd{-do
  logref <- liftIO $ newIORef ""
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory
                              , log_action = logHandler logref
                              }
  defaultCleanupHandler dflags $ do
    setTargets =<< sequence [guessTarget "test.hs" Nothing]
    load LoadAllTargets
    -- Bringing the module into the context
    setContext [IIModule $ mkModuleName "Test"]
    GHC.setContext [ 
      GHC.IIDecl $ (
         (GHC.simpleImportDecl . GHC.mkModuleName $ name) {GHC.ideclQualified = True}) 
      | name <- names]
    
    mapM GHC.getInfo =<< GHC.getNamesInScope
    
    GHC.compileExpr "Prelude.filter"
    
 where logHandler :: IORef String -> LogAction
       logHandler ref dflags severity srcSpan style msg =
           case severity of
             SevError ->  modifyIORef' ref (++ printDoc)
             SevFatal ->  modifyIORef' ref (++ printDoc)
             _        ->  return () -- ignore the rest
         where cntx = initSDocContext dflags style
               locMsg = mkLocMessage severity srcSpan msg
               printDoc = show (runSDoc locMsg cntx)
       onlyTycs :: [Maybe (TyThing, Fixity, [ClsInst])] -> [TyCon]
       onlyTycs infos = [ t | Just (ATyCon t, _, _) <- infos]

       onlyIDs :: [Maybe (TyThing, Fixity, [ClsInst])] -> [Id]
       onlyIDs infos = [ i | Just (AnId i, _, _) <- infos ]

       onlyDaCons :: [Maybe (TyThing, Fixity, [ClsInst])] -> [DataCon]
       onlyDaCons infos = [ i | Just (ADataCon i, _, _) <- infos ]-}

