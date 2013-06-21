{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.InPlace.RoundedBasis
    Description :  domain bases with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Domain bases with outwards and inwards rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent RefinementOrder. 
-}
module Numeric.AERN.RefinementOrder.InPlace.RoundedBasis 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.RefinementOrder.PartialComparison
import Numeric.AERN.RefinementOrder.Arbitrary
import Numeric.AERN.RefinementOrder.RoundedBasis

import Numeric.AERN.Basics.Laws.OperationRelation
import Numeric.AERN.Basics.Laws.RoundedOperation

import Numeric.AERN.Basics.Laws.RoundedOpInPlace

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

infixr 2 <\/>?=, >\/<?=, <⊔>?=, >⊔<?= 

{-|
    A type with outward-rounding lattice operations.
-}
class (RoundedBasisEffort t, CanBeMutable t) =>
    RoundedBasisInPlace t
    where
    partialJoinOutInPlaceEff :: OpPartialMutable2Eff (PartialJoinEffortIndicator t) t s 
    partialJoinInInPlaceEff :: OpPartialMutable2Eff (PartialJoinEffortIndicator t) t s 

-- | Partial outward rounded in-place join with default effort
partialJoinOutInPlace :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
partialJoinOutInPlace = 
    partialMutable2EffToPartialMutable2 partialJoinOutInPlaceEff partialJoinDefaultEffort 
-- | Partial outward rounded in-place join with default effort
(<\/>?=) :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
(<\/>?=) = partialJoinOutInPlace
-- | Partial outward rounded in-place join with default effort
(<⊔>?=) :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
(<⊔>?=) = partialJoinOutInPlace

-- | Partial inward rounded in-place join with default effort
partialJoinInInPlace :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
partialJoinInInPlace = 
    partialMutable2EffToPartialMutable2 partialJoinInInPlaceEff partialJoinDefaultEffort
-- | Partial inward rounded in-place join with default effort
(>\/<?=) :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
(>\/<?=) = partialJoinInInPlace
-- | Partial inward rounded in-place join with default effort
(>⊔<?=) :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
(>⊔<?=) = partialJoinInInPlace

partialJoinOutInPlaceEffFromPure :: 
    (CanBeMutable t, RoundedBasis t) => 
    OpPartialMutable2Eff (PartialJoinEffortIndicator t) t s  
partialJoinOutInPlaceEffFromPure = pureToPartial2Eff partialJoinOutEff 

partialJoinOutEffFromInPlace ::
 (CanBeMutable t, RoundedBasisInPlace t) =>
 (PartialJoinEffortIndicator t) -> t -> t -> Maybe t
partialJoinOutEffFromInPlace = mutablePartial2EffToPure partialJoinOutInPlaceEff 

partialJoinInInPlaceEffFromPure :: 
    (CanBeMutable t, RoundedBasis t) => 
    OpPartialMutable2Eff (PartialJoinEffortIndicator t) t s  
partialJoinInInPlaceEffFromPure = pureToPartial2Eff partialJoinInEff 

partialJoinInEffFromInPlace ::
 (CanBeMutable t, RoundedBasisInPlace t) =>
 (PartialJoinEffortIndicator t) -> t -> t -> Maybe t
partialJoinInEffFromInPlace = mutablePartial2EffToPure partialJoinInInPlaceEff 

propInOutRoundedBasisJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RoundedBasisInPlace t, 
     RoundedBasis t, 
     CanBeMutable t) =>
    t -> 
    (PartialJoinEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propInOutRoundedBasisJoinInPlaceConsistentWithPure 
    _ (partialJoinEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPurePartial2 (pLeqEff effortComp) 
        (partialJoinOutInPlaceEff partialJoinEffort)  
        (partialJoinInInPlaceEff partialJoinEffort)
        (partialJoinOutEff partialJoinEffort) 
        (partialJoinInEff partialJoinEffort) 
        e1 e2  

testsOuterInnerRoundedBasisInPlace :: 
    (PartialComparison t,
     RoundedBasisInPlace t, 
     RoundedBasis t, 
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (PartialJoinEffortIndicator t), Show (PartialJoinEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsOuterInnerRoundedBasisInPlace (name, sample) =
    testGroup (name ++ " partial join rounded in-place") $
        [
         testProperty "partial join in-place=pure"
             (propInOutRoundedBasisJoinInPlaceConsistentWithPure sample)
        ]
