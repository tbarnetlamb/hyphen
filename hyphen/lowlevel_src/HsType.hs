{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HsType where

--import Debug.Trace
import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.DeepSeq
import Data.Typeable    (TypeRep)
import Data.Hashable
import Data.Text        (Text)
import Data.Map.Strict  (Map)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.Traversable
import qualified Data.Typeable

import HyphenBase
import HyphenKinds
import HyphenTyCon

----

newtype Var    = Var    {getVar    :: Text} deriving (Eq, Ord, Show)

instance NFData (Var) where
  rnf (Var v) = rnf v

instance Hashable (Var) where
  hash (Var v) = hash v

----

data HsType
  = HsType {
    typeHash     :: {-# UNPACK #-} !Int,
    typeHead     :: Either TyCon (Var, Kind),
    typeTail     :: [HsType],
    typeKind     :: Kind,
    typeFreeVars :: Map Var Kind,
    typeName     :: Text
    } deriving (Ord, Show)

instance Hashable (HsType) where
  hash HsType {typeHash=th} = th

bracketedTypeName :: HsType -> Text
bracketedTypeName hst | never_bracket (typeHead hst)  = typeName hst
                      | null (typeTail hst)           = typeName hst
                      | otherwise                     = bracket $ typeName hst
  where never_bracket (Left tyc) = isListTyCon tyc || isTupTyCon tyc
        never_bracket (Right _)  = False

bracket :: Text -> Text
bracket t = T.concat [T.pack "(",  t, T.pack ")"]

instance Eq (HsType) where
  a == b = hash a == hash b

mapVars :: (Text -> Text) -> HsType -> HsType
mapVars fn = process
  where process hst = let rest = (map process $ typeTail hst) in case typeHead hst of
          Right (Var v, k)  -> mkHsType (Right (Var (fn v), k)) rest
          tycon             -> mkHsType tycon                   rest
          

mkHsType :: Either TyCon (Var, Kind) -> [HsType] -> HsType
mkHsType head = either (error . T.unpack . getErrMsg) id . mkHsTypeSafe head

mkHsTypeSafe :: Either TyCon (Var, Kind) -> [HsType] -> Either ErrMsg HsType
mkHsTypeSafe head tail = let
  thash = case head of (Left  c) -> hash (c, tail)
                       (Right v) ->  hash (("tyvar", v), tail)
      
  fvMaps = headVars head : zipWith tailVars [1..] tail
  headVars (Right (v,k)) = Map.fromList [(v, Right (k,"in the head of the new type"))]
  headVars _              = Map.empty
  tailVars i hst = let f k = Right (k, "in argument " ++ show i ++ "of the new type")
                   in f <$> typeFreeVars hst
  fvMap  = foldl (Map.unionWithKey combine) Map.empty fvMaps
  combine _ a@(Left _) _  = a
  combine _ _ a@(Left _)   = a
  combine v a@(Right (k1, r1)) (Right (k2, r2)) 
    | k1 == k2   = a
    | otherwise  = Left (
      "Error in attempting to construct type " ++ T.unpack name ++ "; type variable " ++ 
      T.unpack (getVar v) ++ " has kind " ++ kindString k1 ++ 
      " " ++ r1 ++ ", but has kind " ++ kindString k2 ++ " " ++ r2 ++ ".")

  (errMsgIntro, headKind) = case head of
    (Left c)           -> ("Type constructor " ++ T.unpack (tyConName c), tyConKind c)
    (Right (Var v, k)) -> ("Type variable " ++ T.unpack v,                k          )

  resultKind = Kind . drop (length tail) . kindArgKinds $ headKind

  kindArityCheck :: Either ErrMsg ()
  kindArityCheck = let 
    arity = (length (kindArgKinds headKind)) :: Int
    in when (arity < length tail) $ report (
      errMsgIntro ++ " applied to too many type variables. It has kind " ++
      kindString headKind ++ ", so should be applied to at most " ++ show arity ++ 
      "arguments; instead, it is applied to the " ++ show (length tail :: Int) ++ 
      " arguments " ++ (unwords $ map (T.unpack . typeName) tail) ++ ".")

  argKindCheck :: Int -> Kind -> Kind -> Either ErrMsg ()
  argKindCheck i kExpected kActual 
    = unless (kExpected == kActual) $ report (
      errMsgIntro ++ " has kind " ++ kindString headKind ++ ", so its parameter number "
      ++ show i ++ " should have kind " ++ kindString kExpected ++ " but the actual "
      ++ "parameter " ++ T.unpack (typeName (tail !! (i - 1))) ++ " has kind " 
      ++ kindString kActual)
  
  name  | either isTupTyCon (const False) head  = bracket ( 
               T.intercalate (T.pack ", ") (map typeName tail))
        | either isListTyCon (const False) head = T.concat [
               T.pack "[", T.intercalate (T.pack ", ") (map typeName tail), T.pack "]"]
        | head == Left fnTyCon = T.concat $ case tail of
                  [frty, toty] -> 
                    if typeHead frty == Left fnTyCon
                    then [bracketedTypeName frty, T.pack " -> ", typeName toty]
                    else [typeName frty,          T.pack " -> ", typeName toty]
        | otherwise        = let headName (Left  c)          = tyConFullName c
                                 headName (Right (Var v, k)) = v
                             in T.unwords (headName head:map bracketedTypeName tail)

  in do kindArityCheck
        sequence $ zipWith3 argKindCheck [1..] (kindArgKinds headKind) (map typeKind tail)
        fvs <- Data.Traversable.mapM (either report (return . fst)) fvMap
        return $ HsType thash (force head) tail resultKind fvs name
  
isMonoType :: HsType -> Bool
isMonoType = Map.null . typeFreeVars

fnHsType a b        = mkHsType (Left fnTyCon           ) [a, b]

breakFnType :: HsType -> Either ErrMsg (HsType, HsType)
breakFnType (HsType {typeHead=Left tc, typeTail=[fr, to]}) 
  | tc == fnTyCon = return (fr, to)
breakFnType ty = report $ "Trying to apply object of type '" ++ T.unpack (typeName ty)
                 ++ "', but this is not a function type."

breakFnTypeUnsafe :: HsType -> (HsType, HsType)
breakFnTypeUnsafe (HsType {typeHead=Left tc, typeTail=[fr, to]}) 
  | tc == fnTyCon = (fr, to)
breakFnTypeUnsafe ty = error $ "breakFnTypeUnsafe:'" ++ T.unpack (typeName ty)
                       ++ "' is not a function type."

transformType :: Map Var HsType -> HsType -> HsType
transformType dict = go
  where go HsType {typeHead=Left con, typeTail=tail} 
          = mkHsType (Left con) (map go tail)
        go HsType {typeHead=Right (var, kind), typeTail=tail} = case Map.lookup var dict of
          Nothing                  -> mkHsType (Right (var, kind)) (map go tail)
          Just HsType {typeHead=head', typeTail=tail'}
                                   -> mkHsType head'       (tail' ++ map go tail)

transformTypeAllowedCheck :: Map Var HsType -> HsType -> Either ErrMsg ()
transformTypeAllowedCheck substs original = do
  let usableKinds = typeFreeVars original
      unusable    = map fst . Map.toList $ substs `Map.difference` usableKinds
  mapM_ (\ (Var v) -> report $ "A substitution is given for the variable  " ++ T.unpack v 
                      ++ "but that variable isn't used in the type into which we're "
                      ++ "making the substitution") unusable
  let checkKindCompatibility :: Var -> Kind -> HsType -> Either ErrMsg ()
      checkKindCompatibility (Var v) k t = 
        unless (k == typeKind t) ( 
          report $ "The substitution offered for the variable " ++ T.unpack v ++
          "ought to have kind " ++ kindString k ++ " but you provided something " ++
          "of kind " ++ kindString (typeKind t) ++ ", viz " ++ T.unpack (typeName t))
  sequence_ . map snd . Map.toList $
    Map.intersectionWithKey checkKindCompatibility usableKinds substs

peelArgTypes :: HsType -> Int -> ([HsType], HsType)
peelArgTypes ty max_peel 
  | max_peel == 0   = ([], ty)
  | otherwise       = if typeHead ty /= Left fnTyCon then ([], ty) else 
                        let [arg1, rest] = typeTail ty
                            (args', ret) = peelArgTypes rest (max_peel-1)
                        in  (arg1:args', ret)

hsTypeFromSimpleTypeRep :: TypeRep -> HsType
hsTypeFromSimpleTypeRep tr = let
  (con, args) = Data.Typeable.splitTyConApp tr
  args'       = map hsTypeFromSimpleTypeRep args
  con'        = tyConFromTypeableTyCon (simplKnd $ length args) con
  in mkHsType (Left con') args'

hsTypeRepr :: HsType -> Text
hsTypeRepr hst 
  = let tailRep = map hsTypeRepr $ typeTail hst
    in case typeHead hst of
      Right (var, _) -> T.concat $ [
        T.pack "hyphen.HsType(", T.intercalate (T.pack ", ") (
           [T.concat[T.pack "\"", getVar var, T.pack "\""]] ++ tailRep ++ 
           if typeKind hst /= Kind [] then 
             [T.concat [T.pack "kind=\"", T.pack . kindString $ typeKind hst, T.pack "\""]]
           else []), T.pack ")"]
      Left   tyc     -> T.concat $ [
        tyConRepr tyc, T.pack "(", T.intercalate (T.pack ", ") tailRep, T.pack ")"]
                           