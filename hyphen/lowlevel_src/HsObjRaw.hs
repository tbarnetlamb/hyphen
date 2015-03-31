{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HsObjRaw (PolyObjCore(..), Obj(..), objType, formObjOfType, formObjSimple,
                 transformObjTypes, transformObjTypes', resolveToType, alignTypes,
                 applyMono, apply, makeMonomorphic, tryMakeMonomorphic, doIO) where

import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.Monad
import Data.Typeable    (Typeable, typeOf)
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Text        (Text)
import Data.Map.Strict  (Map)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Read
import qualified Unsafe.Coerce

import HyphenBase
import HyphenKinds
import HyphenTyCon
import HsType
import HyphenUnify
import PythonBase

data PolyObjCore = PolyObjCore {resolvePolyObjCoreToType :: HsType -> PythonM Obj}

data Obj = 
  MonoObj HsType Any
  | PolyObj {
    polyObjFinalType     :: HsType,
    polyObjCoreType      :: HsType,
    polyObjCore          :: PolyObjCore,
    polyObjAppliedTo     :: [Obj]
    }

objType :: Obj -> HsType
objType (MonoObj t _)                  = t
objType (PolyObj {polyObjFinalType=t}) = t

formObjOfType :: HsType -> a -> IO Obj
formObjOfType ty = return . MonoObj ty . Unsafe.Coerce.unsafeCoerce

formObjSimple :: (Typeable a) => a -> IO Obj
formObjSimple ob = formObjOfType (hsTypeFromSimpleTypeRep $ typeOf ob) ob

transformObjTypes' :: Map Var HsType -> Obj -> Obj
transformObjTypes' d = go
  where go (PolyObj ft ct c ato) = PolyObj (f ft) (f ct) c (map go ato)
        go mo@(MonoObj _ _)      = mo
        f = transformType d

transformObjTypes :: Map Var HsType -> Obj -> PythonM Obj
transformObjTypes d = tryMakeMonomorphic . transformObjTypes' d

resolveToType :: HsType -> Obj -> PythonM Obj
resolveToType ty obj = do
  let unify_result = unify [mapVars (T.append $ T.pack "o_") $ objType obj, 
                            mapVars (T.append $ T.pack "r_") ty]
  (_, substRaw) <- maybe (
    pyTypeErr' $ "Incompatible types: cannot resolve object of type\n\t"
    ++ T.unpack (typeName $ objType obj) ++ "\nto type\n\t"
    ++ T.unpack (typeName ty)) return unify_result
  let subst = Map.mapKeys (Var . T.drop 2 . getVar) . fst
              . Map.split (Var $ T.pack "q") $ substRaw
  transformObjTypes subst obj
      
ellipsisVar :: Var
ellipsisVar = Var $ T.pack $ "..."

-------------

simplifyFVs :: Map Var Kind -> Map Text Text
simplifyFVs =  Map.fromList . concatMap finalize . Map.toList
               . fmap rationalize . Map.fromListWith (++) . map process . Map.keys
  where process :: Var -> (Text, [(Int, Var)])
        process orig = let (prefix, rest) = separateVarPrefix orig
                           (core, number) = separateVarNumber rest
                       in (core, [(number, orig)])
        rationalize :: [(Int, Var)] -> [(Int, Var)]
        rationalize = rationalizeFrom 0 [] . sort
        rationalizeFrom _        []      []       = []
        rationalizeFrom nextFree (ov:vs) []
          = (nextFree, ov) : rationalizeFrom (nextFree+1) vs []
        rationalizeFrom nextFree []            ((i, v):ivs) 
          | i >= nextFree    = (i, v)         : rationalizeFrom (i+1)        []  ivs
          | otherwise        =                  rationalizeFrom nextFree     [v] ivs
        rationalizeFrom nextFree allvs@(ov:vs) allivs@((i, v):ivs) 
          | i > nextFree     = (nextFree, ov) : rationalizeFrom (nextFree+1) vs     allivs
          | otherwise        = (i, v)         : rationalizeFrom (nextFree+1) allvs  ivs
        finalize    :: (Text, [(Int, Var)]) -> [(Text, Text)]
        finalize (core, lst) = map doOne lst
          where doOne (i, Var orig) = (orig, T.concat [core, T.pack $ "_" ++ show i])

separateVarNumber :: Var -> (Text, Int)
separateVarNumber v | v == ellipsisVar = (T.pack "a", 0) --special case
                    | T.null prePart   = (getVar v, 0)
                    | otherwise        = case Data.Text.Read.decimal endPart of
    Right (num', remainder) -> if T.null remainder then (prePart, num') else (getVar v, 0)
    _                       -> (getVar v, 0)
  where (prePart_, endPart) = T.breakOnEnd (T.pack "_") (getVar v)
        prePart             = if T.null prePart_ then (T.pack "X*X") else T.init prePart_

separateVarPrefix   :: Var -> (Text, Var)
separateVarPrefix t  = case T.findIndex (=='_') (getVar t) of 
  Nothing  -> (T.empty, t)
  Just pos -> second Var $ T.splitAt (pos+1) (getVar t)

alignTypes :: (Obj, [Obj]) -> Either ErrMsg (Obj, [Obj], HsType)
alignTypes (o1, os) = do
  let (t1, ts) = (objType o1, map objType os)
  _                 <- breakFnType t1
  let argPrefixes    = [T.pack $ "a" ++ show i ++ "_" | i <- [1..]]
      ts'            = zipWith (mapVars . T.append) argPrefixes ts
      wantedFnType   = foldr fnHsType (mkHsType (Right (ellipsisVar, Kind [])) []) ts'
      unify_result   = unify [mapVars (T.append $ T.pack "res_") t1, wantedFnType]
  (totalTypeRaw, substRaw) <-
      maybe (report $ "Type mismatch in application. Based on types of arguments, " ++
             "function applied should have a type like\n\t" ++
             T.unpack (typeName wantedFnType) ++
             "\nBut actual function applied has type\n\t" ++
             T.unpack (typeName t1)) Right unify_result
  let simplifySubst  = simplifyFVs $ typeFreeVars totalTypeRaw
      totalType      = mapVars (simplifySubst Map.!) totalTypeRaw
      subst          = mapVars (simplifySubst Map.!) <$> substRaw
      resultType     = fromMaybe (error "alignTypes: internal err") 
                       $ Map.lookup ellipsisVar subst
      substsByPref   = cleaveMap separateVarPrefix subst
      substsFor x    = Map.findWithDefault Map.empty x substsByPref
      o1'            = transformObjTypes' (substsFor $ T.pack "res_") o1
      os'            = zipWith transformObjTypes' (map substsFor argPrefixes) os
  when (typeFreeVars resultType /= typeFreeVars totalType) (
    report $ "Ambiguous application: You tried to apply an object of type \n\t"
    ++ T.unpack (typeName t1) ++ "\nto objects of type \n\t["
    ++ intercalate ", " (map (T.unpack . typeName) ts)
    ++ "].\nAfter resolving type variables, this "
    ++ "amounts to applying an object of type\n\t" 
    ++ T.unpack (typeName $ objType o1') ++ "\n to objects of type\n\t["
    ++ intercalate ", " (map (T.unpack . typeName . objType) os') 
    ++ "].\nThe variables "
    ++ show (map (getVar . fst) . Map.toAscList $ Map.difference 
             (typeFreeVars totalType) (typeFreeVars resultType))
    ++ " are present in the input but not the result and cannot be resolved.\n")
    -- ++ (show . typeName $ totalTypeRaw) ++ "\n"
    -- ++ show (fmap typeName $ substRaw))
  return (o1', os', resultType)

-------------

applyMono' :: (HsType, Any) -> (HsType, Any) -> (HsType, Any)
applyMono' (fn_type, ptr_fn) (arg_type, ptr_arg) =
  let (ty_fr, ty_to) = breakFnTypeUnsafe fn_type
      in if ty_fr /= arg_type 
         then error ("applyMono: expected arg type\n\t" ++ T.unpack (typeName ty_fr)
                     ++ "\ngot:\n\t" ++ T.unpack (typeName arg_type))
         else (ty_to, (Unsafe.Coerce.unsafeCoerce ptr_fn) ptr_arg)

applyMono :: Obj -> Obj -> Obj
applyMono (MonoObj ty1 obj1) (MonoObj ty2 obj2) 
  = uncurry MonoObj $ applyMono' (ty1, obj1) (ty2, obj2)


apply :: Obj -> [Obj] -> PythonM Obj
apply fn_orig args_orig = do 
  (fn, args, resultType) <- promoteErr $ alignTypes (fn_orig, args_orig)
  case fn of
    PolyObj {} -> tryMakeMonomorphic $
      (fn {polyObjFinalType  =resultType,
           polyObjAppliedTo  =polyObjAppliedTo fn ++ args})
    MonoObj _ _ -> do
      args'  <- mapM makeMonomorphicUnsafe args
      return $ foldl applyMono fn args'

makeMonomorphic :: Obj -> PythonM (Maybe Obj)
makeMonomorphic m@(MonoObj _ _) = return $ return m
makeMonomorphic obj@(PolyObj {})
  | isMonoType (polyObjFinalType obj)
  = do appliedToMono <- mapM makeMonomorphicUnsafe (polyObjAppliedTo obj)
       coreMono      <- resolvePolyObjCoreToType (polyObjCore obj) (polyObjCoreType obj)
       return . Just $ foldl applyMono coreMono appliedToMono
makeMonomorphic _ = return Nothing

makeMonomorphicUnsafe obj = fromMaybe (error "makeMonomorphicUnsafe:not monomorphic")
                               <$> makeMonomorphic obj

tryMakeMonomorphic :: Obj -> PythonM Obj
tryMakeMonomorphic o = fromMaybe o <$> makeMonomorphic o

doIO :: Obj -> Either ErrMsg (IO Obj)
doIO (MonoObj ty ptr) = do
  unless (typeHead ty == Left ioTyCon) (
    report $ "Attempt to perform IO action, but instead of an action an object " ++
    "of type " ++ (T.unpack $ typeName ty) ++ " was supplied.")
  let [ioRetType] = typeTail ty
  return $ do
    ioRet <- (Unsafe.Coerce.unsafeCoerce ptr :: IO Any)
    return $ MonoObj ioRetType ioRet
doIO obj@(PolyObj {}) = 
  report $ "IO action to perform must have monomorphic type, not " ++ (
    T.unpack . typeName $ objType obj)
