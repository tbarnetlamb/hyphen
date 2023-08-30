{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HsType where

--import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.RWS
import Control.DeepSeq
import Data.List
import Data.Typeable                  (TypeRep)
import Data.Hashable
import Data.Text                      (Text)
import Data.Map.Strict                (Map)
import Data.HashMap.Strict            (HashMap)
import qualified Data.Text            as T
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Traversable
import qualified Data.Typeable

import HyphenBase
import HyphenKinds
import HyphenTyCon

-- | This module provides a Haskell representation of Haskell
-- types. It is similar to the Data.Typeable built-in module, but it
-- differs in that it can represent polymorphic types (that is, types
-- with type variables in them), and that the type constructors within
-- the types are encoded using our HyphenTyCon.TyCon type rather than
-- Data.Typeable.TyCon: ours contains a bit more information (to wit,
-- the kind and some information on how the type constructor can be
-- 'found' in importable modules; see comments in HyphenTyCon for
-- details).
--
-- Although (as mentioned above) we can represent polymorphic types,
-- every type we represent has a fixed kind: we don't handle
-- Kind-polymorphic beasts like @(a Text)@ where @a@ is a type
-- variable. (This could have kind @*@, if @a@ has Kind @* -> *@, but
-- could also have Kind @* -> *@, if @a@ had kind @* -> * -> *@, and
-- so on...)

----

-- | Simple wrapper type to represent a Variable. Just contains a Text
-- giving the name of the variable.

newtype Var    = Var    {getVar    :: Text} deriving (Eq, Ord, Show)

instance NFData (Var) where
  rnf (Var v) = rnf v

instance Hashable (Var) where
  hashWithSalt salt (Var v) = hashWithSalt salt v

----

-- | Our representation for a Haskell type. The only 'true' fields
-- here are typeHead and typeTail; all the other fields are computed
-- from them and only present in the constructor for efficiency (so
-- that, once computed, we remember their values). The head of a type
-- is either a Type constructor, or a Variable with a kind. (Types
-- like @((a :: * -> * -> *) Text Text)@ are legal in Haskell, but
-- very rare). Note that in this example the Kind of the Variable must
-- be fixed because we do not support Kind polymorphism of any nature:
-- see second paragraph of comment at the top of the file.) The tail
-- of the type is the set of other types to which the constructor has
-- been applied.
--
-- Note that not all combinations of typeHead and typeTail are
-- allowed: there might be a kind mismatch.
--
-- The smart constructors mkHsTypeSafe and mkHsType are the preferred
-- way of building HsTypes from the 'true' fields.
--
-- typeHash is a hash or fingerprint for the type, stored once for
-- fast access. typeKind is the Kind of the type as a whole: this can
-- be figured out by taking the kind of the head and imagining that
-- @N@ arguments have already been provided to it, where @N@ is the
-- number of elemetns of the typeTail. typeFreeVars are all the free
-- type variables in the type, together with the kinds they must have
-- for the expression to make sense; this can be gleaned by scanning
-- recursively for variables. typeName is the name of the type in
-- standard Haskell notation.

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
  hashWithSalt salt (HsType {typeHead=head, typeTail=tail} )
    = case head of (Left  c) -> hashWithSalt salt (c, tail)
                   (Right v) -> hashWithSalt salt (("tyvar", v), tail)

bracketedTypeName :: HsType -> Text
bracketedTypeName hst | never_bracket (typeHead hst)  = typeName hst
                      | null (typeTail hst)           = typeName hst
                      | otherwise                     = bracket $ typeName hst
  where never_bracket (Left tyc) = isListTyCon tyc || isTupTyCon tyc
        never_bracket (Right _)  = False

-- | Convenience function: put brackets round a string

bracket :: Text -> Text
bracket t = T.concat [T.pack "(",  t, T.pack ")"]

instance Eq (HsType) where
  a == b = hash a == hash b

-- | map the Variable names used in an HsType; useful for renaming
-- variables.

mapVars :: (Text -> Text) -> HsType -> HsType
mapVars fn = process
  where process hst = let rest = (map process $ typeTail hst) in case typeHead hst of
          Right (Var v, k)  -> mkHsType (Right (Var (fn v), k)) rest
          tycon             -> mkHsType tycon                   rest


-- | Secondary way of making an HsType from the 'true' fields, the
-- head and the tail. As mentioned in the docstring for HsType itself,
-- not all combinations of a head and a tail will be legal (some fail
-- to Kind-check); in case of failure, this function gives an IO
-- exception.

mkHsType :: Either TyCon (Var, Kind) -> [HsType] -> HsType
mkHsType head = either (error . T.unpack . getErrMsg) id . mkHsTypeSafe head

-- | Main way of making an HsType from the 'true' fields, the head and
-- the tail. As mentioned in the docstring for HsType itself, not all
-- combinations of a head and a tail will be legal (some fail to
-- Kind-check); in case of failure, this function gives a nice,
-- monadic error message.

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

-- | Is this type monomorphic (free of type variables)?

isMonoType :: HsType -> Bool
isMonoType = Map.null . typeFreeVars

-- | Given HsTypes representing types @A@ and @B@, construct the type
-- of functions taking @A@ and returning @B@.

fnHsType a b        = mkHsType (Left fnTyCon           ) [a, b]

-- | Given an HsType which we hope desribes the type of functions from
-- some type @A@ to some type @B@, return a pair containing HsTypes
-- representing @A@ and @B@ respectively, or else a friendly error
-- message saying why it couldn't be done.

breakFnType :: HsType -> Either ErrMsg (HsType, HsType)
breakFnType (HsType {typeHead=Left tc, typeTail=[fr, to]})
  | tc == fnTyCon = return (fr, to)
breakFnType ty = report $ "Trying to apply object of type '" ++ T.unpack (typeName ty)
                 ++ "', but this is not a function type."

-- | Given an HsType, think of it as a function type A_1 -> A_2 ->
-- .. -> A_n -> R, and return a list of HsTypes representing the
-- argument types A_1, ..., A_n and an HsType representing the result
-- type R; this is always possible because we may take the list A_i as
-- empty (although we always make it as long as possible).

breakFnTypeRec :: HsType -> ([HsType], HsType)
breakFnTypeRec hst = go hst []
  where go hst sofar = case breakFnType hst of
          Left  _         -> (reverse sofar, hst)
          Right (fr, to)  -> go to (fr:sofar)

-- | Given an HsType which we hope desribes the type of functions from
-- some type @A@ to some type @B@, return a pair containing HsTypes
-- representing @A@ and @B@ respectively, or else panic.

breakFnTypeUnsafe :: HsType -> (HsType, HsType)
breakFnTypeUnsafe (HsType {typeHead=Left tc, typeTail=[fr, to]})
  | tc == fnTyCon = (fr, to)
breakFnTypeUnsafe ty = error $ "breakFnTypeUnsafe:'" ++ T.unpack (typeName ty)
                       ++ "' is not a function type."

-- | Find and replace for types. Given a map of variables to types,
-- and an HsType X, replace the variables with the chosen new types in
-- X. If you try to use transformType to replace a variable with a
-- type whose kind doesn't match the variable's kind, then
-- transformType will panic as HsType's constructor complains. If you
-- want to check to make sure this will not happen, see
-- transformTypeAllowedCheck below.

transformType :: Map Var HsType -> HsType -> HsType
transformType dict = go
  where go HsType {typeHead=Left con, typeTail=tail}
          = mkHsType (Left con) (map go tail)
        go HsType {typeHead=Right (var, kind), typeTail=tail} = case Map.lookup var dict of
          Nothing                  -> mkHsType (Right (var, kind)) (map go tail)
          Just HsType {typeHead=head', typeTail=tail'}
                                   -> mkHsType head'       (tail' ++ map go tail)

-- | If you try to use transformType to replace a variable with a type
-- whose kind doesn't match the variable's kind, then transformType
-- will panic as HsType's constructor complains. If you want to check
-- to make sure this will not happen, use this
-- transformTypeAllowedCheck function, which either returns a friendly
-- error message explaining why the requested transformType is bogus,
-- or () if it's safe to call transformType.

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

-- | Given an HsType, think of it as a function type A_1 -> A_2 ->
-- .. -> A_n -> R, and return a list of HsTypes representing the
-- argument types A_1, ..., A_n and an HsType representing the result
-- type R; this is always possible because we may take the list A_i as
-- empty. We take n to be the provided integer max_peel if possible;
-- if it's not possible, we make it as large as we can.

peelArgTypes :: HsType -> Int -> ([HsType], HsType)
peelArgTypes ty max_peel
  | max_peel == 0   = ([], ty)
  | otherwise       = if typeHead ty /= Left fnTyCon then ([], ty) else
                        let [arg1, rest] = typeTail ty
                            (args', ret) = peelArgTypes rest (max_peel-1)
                        in  (arg1:args', ret)

-- | Create an HsType from a TypeRep, in a way which only works in the
-- special case that all the Type Constructors used in the TypeRep are
-- fully saturated. (That is, every argument passed to a type
-- constructor has kind *, and the final result has kind *.) Do not
-- call outside this case!

hsTypeFromSimpleTypeRep :: TypeRep -> HsType
hsTypeFromSimpleTypeRep tr = let
  (con, args) = Data.Typeable.splitTyConApp tr
  args'       = map hsTypeFromSimpleTypeRep args
  con'        = tyConFromTypeableTyCon (simplKnd $ length args) con
  in mkHsType (Left con') args'

-- | Get a string representation of the HsType that is suitable to use
-- as its Python @repr@ (that is a string that, if executed in
-- python+hyphen with appropriate imports, will evaluate to a python
-- object which represents the HsType in question). Note that this
-- depends on the high-level haskell/python bridge, so there's a bit
-- of something like leaky abstraction here...

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
        tyConRepr' True tyc, T.pack "(", T.intercalate (T.pack ", ") tailRep, T.pack ")"]

-- | A very technical but very important role in hyphen is played by
-- *type forcers*, which exist only for *monomorphic* types. The idea
-- of a type forcer is that it's a string that we can pass to the
-- haskell compiler which will evaluate to the identity function on a
-- particular type in question. The point of this is that we can apply
-- such a thing to an polymorphic expression to produce a monomorphic
-- realization of (the object encoded by) that expression.
--
-- In simple cases, this is just a matter of writing @(id :: <type> ->
-- <type>)@. Not all cases are this simple, however, because sometimes
-- we just can't name the type in question (so we can't write
-- something that would go in the spot @<type>@ there). This is
-- because it's possible for a Haskell module locally define a type
-- constructor X and to export an entity whose type involves a X
-- without exporting X. (This is considered by some to be a wart.) In
-- our world, we see this in that not every @TyCLocation@ is built
-- using @InExplicitModuleNamed@. So in more general cases, we end up
-- doing something like the following. Pretend that [] is never
-- exported. Then we can force something to have type [Int] by passing
-- it through:
--
-- @
--   (((const $ id) :: (x a -> a) -> x Int -> x Int) head)
-- @
--
-- The basic idea here is that since head has type @[u] -> u@, to
-- applying the @(const $ id)@ to head forces the @x@ in the
-- explicitly given type signature to mean @[]@, so the @id@ we get is
-- actually @id : [Int] -> [Int])@. Tada!
--
-- The rest of the code here is just a matter of doing this in
-- general. We write a function from HsTypes to Texts that builds the
-- name of the type which we want to force to (@x Int@) assuming that
-- certain type variables (@x@ in this case) have been forced to be
-- synonyms for certain type constructors. This code is in a WriterT
-- monad transformer, and as it goes along it writes out pairs like
-- @('head', 'x a')@ which means that @x@ will be forced be a synonym
-- for the type constructor we want if we ensure that @x a@ matches
-- the type of @head@. As we build this, we need a good supply of type
-- variables, so we also have a state monad transformer to keep track
-- of the next fresh variable. We acutally (for human convenience) use
-- two stocks of variables: 'x variables' (which are variables that
-- will eventually be set to type constructors we want to use) and 'a
-- variables' which are just padding.

makeTypeForcer :: HsType -> Text
makeTypeForcer hst = let
  (expr, _, constraints) = runRWS (makeTypeForcerM hst) () (0, 0)
  nConstr   = length constraints
  core      = bracket . T.concat $ (
    (replicate nConstr $ T.pack "Prelude.const Prelude.$ ") ++ [T.pack "Prelude.id"])
  typedCore = bracket . T.unwords $ (
    [core, T.pack "::"] ++ intersperse (T.pack "->") (map snd constraints ++ [expr, expr]))
  in bracket . T.unwords $ typedCore : map fst constraints

type TypeForcerM = RWS () [(Text, Text)] (Int, Int)

-- | Make a fresh 'x variable'; see above for what this means

makeXVar :: TypeForcerM Text
makeXVar = do (i0, i1) <- get
              put (i0, i1+1)
              return $ T.concat [T.pack "x", T.pack $ show i1]

-- | Make a fresh 'a variable'; see above for what this means

makeAVar :: TypeForcerM Text
makeAVar = do (i0, i1) <- get
              put (i0+1, i1)
              return $ T.concat [T.pack "a", T.pack $ show i0]

-- | Emit a constraint (like @('head', 'x a')@ in the example above)

emitConstraint :: (Text, Text) -> TypeForcerM ()
emitConstraint pair = tell [pair]

typeHeadForcerRenames :: HashMap (Text, Text) Text
typeHeadForcerRenames = HashMap.fromList $ map ((T.pack *** T.pack) *** T.pack) $ [
#if __GLASGOW_HASKELL__ >= 808
  (("GHC.Integer.Type", "Integer"), ("GHC.Integer.Integer")),
  (("Data.ByteString.Internal.Type", "ByteString"), ("Data.ByteString.Internal.ByteString"))
#endif
  ]

-- | Make a forcer for a type constructor

makeTypeHeadForcer :: HsType -> TypeForcerM Text
makeTypeHeadForcer hst = case typeHead hst of
  Right (Var v, k)
    -> error "makeTypeForcer: expected monomorphic type"
  Left tyc@(TyCon {tyConName      = oname,
                   tyConLocation = (InExplicitModuleNamed mname)})
    -> if isTupTyCon tyc || isListTyCon tyc || tyc == fnTyCon
         then return oname
         else case HashMap.lookup (mname, oname) typeHeadForcerRenames of
           Nothing     -> return $ T.concat [mname, T.pack ".", oname]
           Just answer -> return answer
  Left (TyCon {tyConLocation = (ImplicitlyVia tycLocObj False tycLocPath)})
    -> do var <- makeXVar
          pp  <- processPath var tycLocPath
          emitConstraint (tycLocObj, pp)
          return var
  Left (TyCon {tyConLocation = (ImplicitlyVia tycLocTyp True  tycLocPath)})
    -> do var <- makeXVar
          pp  <- processPath var tycLocPath
          emitConstraint (T.concat [T.pack "(undefined :: ", tycLocTyp, T.pack ")"], pp)
          return var

makeTypeForcerM :: HsType -> TypeForcerM Text
makeTypeForcerM hst =
  do headImg <- makeTypeHeadForcer hst
     tailImg <- mapM makeTypeForcerM $ typeTail hst
     return . bracket . T.unwords $ headImg : tailImg

processPath :: Text -> [Int] -> TypeForcerM Text
processPath v [i] = do
  tail <- sequence $ genericReplicate i makeAVar
  return . bracket . T.unwords $ v : tail
processPath v (i:is@(_:_)) = do
  hVar <- makeAVar
  mid  <- processPath v is
  tail <- sequence $ genericReplicate i makeAVar
  return . bracket . T.unwords $ hVar : mid : tail
