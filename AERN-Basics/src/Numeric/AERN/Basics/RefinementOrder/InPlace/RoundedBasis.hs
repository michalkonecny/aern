{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.InPlace.RoundedBasis
    Description :  domain bases with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Domain bases with outwards and inwards rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.InPlace.RoundedBasis 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.PartialComparison
import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.RoundedBasis

import Numeric.AERN.Basics.Laws.OperationRelation
import Numeric.AERN.Basics.Laws.RoundedOperation

import Numeric.AERN.Basics.Laws.RoundedOpInPlace

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)


{-|
    A type with outward-rounding lattice operations.
-}
class (OuterRoundedBasisEffort t, CanBeMutable t) =>
    OuterRoundedBasisInPlace t
    where
    partialJoinOutInPlaceEff :: OpPartialMutable2Eff (PartialJoinOutEffortIndicator t) t s 

partialJoinOutInPlaceEffFromPure :: 
    (CanBeMutable t, OuterRoundedBasis t) => 
    OpPartialMutable2Eff (PartialJoinOutEffortIndicator t) t s  
partialJoinOutInPlaceEffFromPure = pureToPartial2Eff partialJoinOutEff 

partialJoinOutEffFromInPlace ::
 (CanBeMutable t, OuterRoundedBasisInPlace t) =>
 (PartialJoinOutEffortIndicator t) -> t -> t -> Maybe t
partialJoinOutEffFromInPlace = mutablePartial2EffToPure partialJoinOutInPlaceEff 

{-|
    A type with inward-rounding lattice operations.
-}
class (InnerRoundedBasisEffort t, CanBeMutable t) =>
    InnerRoundedBasisInPlace t
    where
    partialJoinInInPlaceEff :: OpPartialMutable2Eff (PartialJoinInEffortIndicator t) t s 

class (OuterRoundedBasisInPlace t, InnerRoundedBasisInPlace t) => RoundedBasisInPlace t 

partialJoinInInPlaceEffFromPure :: 
    (CanBeMutable t, InnerRoundedBasis t) => 
    OpPartialMutable2Eff (PartialJoinInEffortIndicator t) t s  
partialJoinInInPlaceEffFromPure = pureToPartial2Eff partialJoinInEff 

partialJoinInEffFromInPlace ::
 (CanBeMutable t, InnerRoundedBasisInPlace t) =>
 (PartialJoinInEffortIndicator t) -> t -> t -> Maybe t
partialJoinInEffFromInPlace = mutablePartial2EffToPure partialJoinInInPlaceEff 

propOuterInnerRoundedBasisJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     OuterRoundedBasisInPlace t, InnerRoundedBasisInPlace t, 
     RoundedBasis t, 
     CanBeMutable t) =>
    t -> 
    (PartialJoinOutEffortIndicator t, PartialJoinInEffortIndicator t, 
     PartialCompareEffortIndicator t) -> 
     UniformlyOrderedPair t -> Bool
propOuterInnerRoundedBasisJoinInPlaceConsistentWithPure 
    _ (partialjoinOutEffort, partialjoinInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPurePartial2 (pLeqEff effortComp) 
        (partialJoinOutInPlaceEff partialjoinOutEffort)  
        (partialJoinInInPlaceEff partialjoinInEffort)
        (partialJoinOutEff partialjoinOutEffort) 
        (partialJoinInEff partialjoinInEffort) 
        e1 e2  

testsOuterInnerRoundedBasisInPlace :: 
    (PartialComparison t,
     OuterRoundedBasisInPlace t, InnerRoundedBasisInPlace t, 
     RoundedBasis t, 
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (PartialJoinOutEffortIndicator t), Show (PartialJoinOutEffortIndicator t), 
     Arbitrary (PartialJoinInEffortIndicator t), Show (PartialJoinInEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsOuterInnerRoundedBasisInPlace (name, sample) =
    testGroup (name ++ " partial join rounded in-place") $
        [
         testProperty "partial join in-place=pure"
             (propOuterInnerRoundedBasisJoinInPlaceConsistentWithPure sample)
        ]
