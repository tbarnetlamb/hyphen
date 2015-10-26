{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module HyphenUnify (unify) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Monoid
import Data.Set                       (Set)
import Data.Map.Strict                (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set

import HyphenKinds
import HyphenTyCon
import HsType

-- | The main purpose of this file is to define a function for
-- unifying HsTypes. We describe what we mean by this. Supose we are
-- given a list of HsTypes. We imagine that their free variables are
-- drawn from a 'common namespace': that is, if we see an @a@ in one
-- and and @a@ in another, we think of them as the *same* @a@. (We can
-- always reduce to this situation by appropriate renaming.) Each of
-- these types is (except in degenerate cases) polymorphic. It's a
-- fact that if there's any type which is simultaneously an instance
-- of all these polymorphic types, there's a single 'most general'
-- polymorphic type T such that all other types which are
-- simultaneously instances of all our original polymorphic types will
-- in fact be instances of T. Our goal is to determine whether T
-- exists (i.e. whether any type which is simultaneously an instance
-- of all the original given types) and if so, to determine T and also
-- (incidentally) a map from the type variables used in the original
-- expression to the types that end up substituted for them in T.

-- This is done as the function Unify below. We build up various bits
-- of plumbing to allows to define it.

-- The ListWithFinial a b type is like [a]except that the empty list
-- has an instance of b attached.

data ListWithFinial a b = (:-:) !a !(ListWithFinial a b) | Final !b

-- Convert list to ListWithFinial

addFinal :: [a] -> b -> ListWithFinial a b
addFinal []     b = Final b
addFinal (a:as) b = a :-: addFinal as b

reverseListWithFinial :: ListWithFinial a b -> (b, [a])
reverseListWithFinial l = rev l []
  where rev (Final b)  as = (b, as)
        rev (a :-: as) x  = rev as (a:x)

-- Our HsType representation is useful for many purposes; human
-- programmers tend to think of Haskell types as heads (usually type
-- constructors) applied to other types or to type variables (or a
-- mixture). This is certainly the interface we want to expose to
-- Python. BUT in it's technically (quite literally) completely
-- backward. The point is that type application is right associative
-- (so @Either a b@ is interpreted as @((Either a) b)@, so the 'parse
-- tree' of the type actually has as it's top node a binary node with
-- children @Either a@ and @b@. In other words, formally types want to
-- be read from right to left, not left to right, as HsType does (by
-- singling out the head for special treatment and by using a
-- list---which in Haskell is an intrinsically left-to-right data
-- structure---for the tail). So it's useful to have a reversed HsType
-- type, to which HsTypes can be freely round-tripped. We define this
-- now.

type RevHsType = ListWithFinial HsType (Either TyCon (Var, Kind))
reversifyHsType   :: HsType -> RevHsType
reversifyHsType hst = addFinal (reverse $ typeTail hst) (typeHead hst)
unreversifyHsType :: RevHsType -> HsType
unreversifyHsType = uncurry mkHsType . reverseListWithFinial

-- | What is the common value of all the things in the list (or mzero
-- if they don't agree)?

commonValue :: (Eq a, MonadPlus m) => [a] -> m a
commonValue []     = mzero
commonValue (x:xs) = if (all (==x) xs) then return x else mzero

----

--During the process of unification, we store a particular quantum of
--information for each variable which has been declared equal to some
--compound expression. In particular, we store the compound expression
--to which it is equivalent, and the set of free variables that are
--present in its *maximal* expansion (that is, its expansion when all
--variables it it that are themselves equivalent to compound
--expressions are re-expanded recursively)
data PerVarState = PerVarState {pvsEquivTo :: RevHsType, pvsUltimateFVs :: Set Var}
type UnifyM = StateT (Map Var PerVarState) Maybe

-- What are the explicit variables in a Reversed HsType?

explicitVars :: RevHsType -> Set Var
explicitVars (h :-: t)              = Set.union (Map.keysSet $ typeFreeVars h) (explicitVars t)
explicitVars (Final (Left _))       = Set.empty
explicitVars (Final (Right (v, k))) = Set.singleton v

-- Operation in the UnifyM monad that handles declaring that a
-- variable is equivalent to some type.

declareEquivalent :: Var -> RevHsType -> UnifyM ()
declareEquivalent v rhst = do
  assocs <- get
  let explicit = explicitVars rhst
      implicit = [maybe (Set.empty) pvsUltimateFVs (Map.lookup v assocs)
                 | v <- Set.toAscList explicit]
      total    = mconcat (explicit : implicit)
  guard  $ v `Set.notMember` total -- this is the 'occurs check'
  modify $ Map.insert v (PerVarState rhst total)
  modify $ fmap (\ pvs -> let oldFVs = pvsUltimateFVs pvs
                          in if v `Set.member` oldFVs then
                               pvs {pvsUltimateFVs = oldFVs `Set.union` total} else pvs)

unifyMImpl :: [RevHsType] -> UnifyM RevHsType
unifyMImpl []  = error "unifyM:empty list"
unifyMImpl [x] = return x
unifyMImpl rhts = do
  assocs <- get
  let expandFinalIfPoss :: RevHsType -> RevHsType
      expandFinalIfPoss orig@(Final (Right (v, _)))
        = maybe orig pvsEquivTo (Map.lookup v assocs)
      expandFinalIfPoss orig = orig
      rhts'   = map expandFinalIfPoss rhts
      finals  = [f   | Final f      <- rhts' ]
      nonFin  = [nf  | nf@(_ :-: _) <- rhts' ]
      ctrs    = [ctr | Left ctr     <- finals]
      vars    = [v   | Right v      <- finals]
  case nonFin of
    [] ->
      if null ctrs then
        do let ((v1, k1):vRest) = vars
               rhst = Final (Right (v1, k1))
           sequence_ [declareEquivalent v rhst | (v, _) <- vRest, v1 /= v]
           return rhst
      else
        do ctr <- commonValue ctrs
           let rhst = Final (Left ctr)
           sequence_ [declareEquivalent v rhst | (v, _) <- vars]
           return rhst
    nfs@(nf:_) ->
      do lift . guard $ null ctrs
         sequence_ [declareEquivalent v nf        | (v, _) <- vars]
         let (heads, tails) = ([reversifyHsType a | a:-:_ <- nfs], [b | _:-:b <- nfs])
         head' <- unifyMImpl heads
         tail' <- unifyMImpl tails
         return $ unreversifyHsType head' :-: tail'

-- 'Find and replace' in types; unlike transformType, this is
-- 'recursive'; if the variables in the map are used in the
-- replacement HsTypes, they get replaced again...

transformTypeRec :: Map Var HsType -> HsType -> HsType
transformTypeRec dict = go
  where go HsType {typeHead=Left con, typeTail=tail}
          = mkHsType (Left con) (map go tail)
        go HsType {typeHead=Right (var, kind), typeTail=tail} = case Map.lookup var dict of
          Nothing                  -> mkHsType (Right (var, kind)) (map go tail)
          Just HsType {typeHead=head', typeTail=tail'}
                                   -> go $ mkHsType head' (tail' ++ tail)

unify  :: [HsType] -> Maybe (HsType, Map Var HsType)
unify hst =
  do (res_r, subs_r) <- flip runStateT Map.empty . unifyMImpl . map reversifyHsType $ hst
     let res   = unreversifyHsType res_r
         subs  = fmap (unreversifyHsType . pvsEquivTo) subs_r
         trans = transformTypeRec subs :: HsType -> HsType
     return (trans res, fmap trans subs)
