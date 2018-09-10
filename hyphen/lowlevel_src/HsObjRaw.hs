{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HsObjRaw (PolyObjCore(..), HsObj(..), objType, formObjOfType, formObjSimple,
                 transformObjTypes, transformObjTypes', resolveToType, alignTypes,
                 applyMono, apply, makeMonomorphic, tryMakeMonomorphic, doIO) where

--import Debug.Trace
import Control.Arrow
import Control.Monad
--import Control.Monad.Trans.Class
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

-- | This module defines the HsObj type, which in a run-time
-- representation of a Haskell object of some (possibly polymorphic)
-- type. It is different to Data.Dynamic mostly in that it supports a
-- representation of polymorphic objects; it is also different in that
-- it integrates with HsType (our preferred representation of Haskell
-- types, which can represent polymorphic types and which stores more
-- information about the type constructors therein contained) rather
-- than the standard library representation of Haskell types (which
-- cannot represent polymorphism, and which stores less information
-- about type constructors, for example loosing information about
-- their kinds).
--
-- When we say that we 'represent' polymorphic objects, this is a
-- little misleading. We simply represent them in terms of how they
-- can be built out of 'fundamental' polymorphic objects (names that
-- can be imported from modules) using the fundamental operation of
-- function application; for 'fundamental' polymorphic objects, we
-- just store some code that can be passed to ghci to refer to the
-- object in question, which allows us to generate code to pick up a
-- monomorphic realization. For this reason, what we do is quite
-- inefficient if you work at length with polymorphic objects; this is
-- why the documentation for hyphen explains that for anything where
-- efficiency matters, you should make all your objects monomorphic as
-- early as possible.
--
-- A monomorphic HsObj is simple: it just consists of its monomorphic
-- type and an Any which points to the object in question.  A
-- Polymorphic object is more complicated. It consists of a *core*,
-- which represents a 'fundamental' polymorphic object (i.e. one
-- importable from somewhere) and which is itself concretely
-- represented as an abstract function which takes an HsType (which
-- had better be both monomorphic and a specialization of the
-- polymorphic type of the underlying polymorphic HsObj) and returns
-- an HsObj (a specialization of the Polymorphic HsObj to the given
-- type) in the PythonM monad. (This, in turn, is internally
-- implemented by using the 'stored piece of code' mentioned above.)
-- In simple cases, the HsObj consists only of the core; in more
-- complicated cases, there may be a sequence of other HsObjs that we
-- need to function-apply the core to before we get the encoded
-- object. We keep track of the type of the core and the type of the
-- final HsObj.
--
-- Occasionally, we pass around HsObjs which claim to be polymorphic
-- but whose type si monomorphic. These are allowed, but only as
-- intermediates; the actual HsObjs that we wrap and expose as Python
-- objects should never be like that...

data PolyObjCore = PolyObjCore {resolvePolyObjCoreToType :: HsType -> PythonM HsObj}

data HsObj =
  MonoObj HsType Any
  | PolyObj {
    polyObjFinalType     :: HsType,
    polyObjCoreType      :: HsType,
    polyObjCore          :: PolyObjCore,
    polyObjAppliedTo     :: [HsObj]
    }

-- | Debug representation of HsObjs

debugDumpHsObj :: HsObj -> Text
debugDumpHsObj (MonoObj hst _) = bracket $ T.unwords [T.pack "MonoObj", typeName hst]
debugDumpHsObj (PolyObj ft ct _ ato) = bracket $ T.unwords [
  T.pack "PolyObj", bracket $ typeName ft, bracket $ typeName ct, T.concat (
     T.pack "[" : intersperse (T.pack ", ") (map debugDumpHsObj ato) ++ [T.pack "]"])]

-- | Fetch the type of an HsObj

objType :: HsObj -> HsType
objType (MonoObj t _)                  = t
objType (PolyObj {polyObjFinalType=t}) = t

-- | form an HsObj given the desired type; *takes on trust that it will
-- be applied to something of the type specified; in other words, not
-- type safe*!

formObjOfType :: HsType -> a -> IO HsObj
formObjOfType ty = return . MonoObj ty . Unsafe.Coerce.unsafeCoerce

-- | form an HsObj; only correct if the type of the object in question
-- is simple (a single type constructor of arity 0)

formObjSimple :: (Typeable a) => a -> IO HsObj
formObjSimple ob = formObjOfType (hsTypeFromSimpleTypeRep $ typeOf ob) ob

-- | Same as transformObjTypes below, but skip the final step (which
-- is replacing a polymorphic representation with a monomorphic one if
-- the type has become monomorphic).

transformObjTypes' :: Map Var HsType -> HsObj -> HsObj
transformObjTypes' d = go
  where go (PolyObj ft ct c ato) = PolyObj (f ft) (f ct) c (map go ato)
        go mo@(MonoObj _ _)      = mo
        f = transformType d

-- | Specialize the type of an HsObj by substituting the given types for
-- the type-variables it contains.

transformObjTypes :: Map Var HsType -> HsObj -> PythonM HsObj
transformObjTypes d = tryMakeMonomorphic . transformObjTypes' d

-- | Resolve the HsObj given to the Type given; raises python TypeError
-- if this is not possible.

resolveToType :: HsType -> HsObj -> PythonM HsObj
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

-- | The pseudo-variable '...', which, while it is only a
-- pseudo-variable (it wouldn't be legal in Haskell) is useful in
-- various operations below.

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

-- | Given an HsObj which we'd like to apply to a list of other HsObjs in
-- turn, see if it's possible to specialize the types of all the HsObjs
-- concerned to make the application type check. If so, return HsObjs
-- with types suitably specialized (but without reducing them to
-- monomorphic objs if their type has become monomorphic). Otherwise,
-- return a friendly error message.

alignTypes :: (HsObj, [HsObj]) -> Either ErrMsg (HsObj, [HsObj], HsType)
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
      fullSubstRaw   = Map.union substRaw $ Map.fromList [
        (v, mkHsType (Right (v, k)) []) | (v, k) <- Map.toList (typeFreeVars totalTypeRaw)]
      subst          = mapVars (simplifySubst Map.!) <$> fullSubstRaw
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

-- Helper function, the 'real word' of applyMono

applyMono' :: (HsType, Any) -> (HsType, Any) -> (HsType, Any)
applyMono' (fn_type, ptr_fn) (arg_type, ptr_arg) =
  let (ty_fr, ty_to) = breakFnTypeUnsafe fn_type
      in if ty_fr /= arg_type
         then error ("applyMono: expected arg type\n\t" ++ T.unpack (typeName ty_fr)
                     ++ "\ngot:\n\t" ++ T.unpack (typeName arg_type))
         else (ty_to, (Unsafe.Coerce.unsafeCoerce ptr_fn) ptr_arg)

-- | Apply a monomorphic object to another monomorphic object. Checks
-- types. Barfs if the objects are, in fact, not monomorphic.

applyMono :: HsObj -> HsObj -> HsObj
applyMono (MonoObj ty1 obj1) (MonoObj ty2 obj2)
  = uncurry MonoObj $ applyMono' (ty1, obj1) (ty2, obj2)

-- | Apply an HsObj to a sequence of other HsObjs. Return a result in the
-- PythonM. Raise a nice Python TypeError if there's a type error.

apply :: HsObj -> [HsObj] -> PythonM HsObj
apply fn_orig args_orig = do
  (fn, args, resultType) <- promoteErr $ alignTypes (fn_orig, args_orig)
  case fn of
    PolyObj {} -> tryMakeMonomorphic $
        (fn {polyObjFinalType  =resultType,
             polyObjAppliedTo  =polyObjAppliedTo fn ++ args})
    MonoObj _ _ -> do
      args'  <- mapM makeMonomorphicUnsafe args
      return $ foldl applyMono fn args'

-- | Given an HsObj whose type is monomorphic (but which may itself be
-- implemented, cheatingly, as a polymorphic obj), turn it into a real
-- monomorphic HsObj if it wasn't one already; otherwise return Nothing.

makeMonomorphic :: HsObj -> PythonM (Maybe HsObj)
makeMonomorphic m@(MonoObj _ _) = return $ return m
makeMonomorphic obj@(PolyObj {})
  | isMonoType (polyObjFinalType obj)
  = do appliedToMono <- mapM makeMonomorphicUnsafe (polyObjAppliedTo obj)
       coreMono      <- resolvePolyObjCoreToType (polyObjCore obj) (polyObjCoreType obj)
       return . Just $ foldl applyMono coreMono appliedToMono
makeMonomorphic _ = return Nothing

-- | Like makeMonomorphic, but barfs if it cannot make the HsObj monomorphic.

makeMonomorphicUnsafe obj = do
  res <- makeMonomorphic obj
  case res of
    Just res' -> return res'
    Nothing   -> pyTypeErr' (
      "makeMonomorphicUnsafe:not monomorphic: " ++ (T.unpack $ debugDumpHsObj obj))

-- | Like makeMonomorphic, but returns the original Obj unchanged if
-- it can't make it monomorphic.

tryMakeMonomorphic :: HsObj -> PythonM HsObj
tryMakeMonomorphic o = fromMaybe o <$> makeMonomorphic o

-- | Given an HsObj which we hope represents an IO action, return an IO
-- action that does the represented action and returns another HsObj
-- with the return value. If presented with an HsObj which doesn't
-- represent an IO action, we produce a nice error message.

doIO :: HsObj -> Either ErrMsg (IO HsObj)
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
