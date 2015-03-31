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

data ListWithFinial a b = (:-:) !a !(ListWithFinial a b) | Final !b

addFinal :: [a] -> b -> ListWithFinial a b
addFinal []     b = Final b
addFinal (a:as) b = a :-: addFinal as b

reverseListWithFinial :: ListWithFinial a b -> (b, [a])
reverseListWithFinial l = rev l []
  where rev (Final b)  as = (b, as)
        rev (a :-: as) x  = rev as (a:x)

type RevHsType = ListWithFinial HsType (Either TyCon (Var, Kind))
reversifyHsType   :: HsType -> RevHsType
reversifyHsType hst = addFinal (reverse $ typeTail hst) (typeHead hst)
unreversifyHsType :: RevHsType -> HsType
unreversifyHsType = uncurry mkHsType . reverseListWithFinial

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

explicitVars :: RevHsType -> Set Var
explicitVars (h :-: t)              = Set.union (Map.keysSet $ typeFreeVars h) (explicitVars t)
explicitVars (Final (Left _))       = Set.empty
explicitVars (Final (Right (v, k))) = Set.singleton v
                                     
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

