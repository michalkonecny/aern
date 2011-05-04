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
    
    This module is hidden and reexported via its parent RefinementOrder. 
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
    partialJoinOutInPlaceEff :: OpMutableMaybeMutable2Eff (PartialJoinOutEffortIndicator t) t s 

partialJoinOutInPlaceEffFromPure :: 
    (CanBeMutable t, OuterRoundedBasis t) => 
    OpMutable2Eff (PartialJoinOutEffortIndicator t) t s  
partialJoinOutInPlaceEffFromPure = pureToMutable2Eff partialJoinOutEff 

partialJoinOutEffFromInPlace ::
 (CanBeMutable t, OuterRoundedBasisInPlace t) =>
 (PartialJoinOutEffortIndicator t) -> t -> t -> Maybe t
partialJoinOutEffFromInPlace = mutable2EffToPure partialJoinOutInPlaceEff 

{-|
    A type with inward-rounding lattice operations.
-}
class (InnerRoundedBasisEffort t, CanBeMutable t) =>
    InnerRoundedBasisInPlace t
    where
    partialJoinInInPlaceEff :: OpMutableMaybeMutable2Eff (PartialJoinInEffortIndicator t) t s 

class (OuterRoundedBasisInPlace t, InnerRoundedBasisInPlace t) => RoundedBasisInPlace t 

partialJoinInInPlaceEffFromPure :: 
    (CanBeMutable t, InnerRoundedBasis t) => 
    OpMutable2Eff (PartialJoinInEffortIndicator t) t s  
partialJoinInInPlaceEffFromPure = pureToMutable2Eff partialJoinInEff 

partialJoinInEffFromInPlace ::
 (CanBeMutable t, InnerRoundedBasisInPlace t) =>
 (PartialJoinInEffortIndicator t) -> t -> t -> Maybe t
partialJoinInEffFromInPlace = mutable2EffToPure partialJoinInInPlaceEff 

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
    _ (joinOutEffort, joinInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (partialJoinOutInPlaceEff joinOutEffort)  
        (partialJoinInInPlaceEff joinInEffort)
        (partialJoinOutEff joinOutEffort) 
        (partialJoinInEff joinInEffort) 
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
    testGroup (name ++ " (join,meet) rounded in-place") $
        [
         testProperty "partial join in-place=pure"
             (propOuterInnerRoundedBasisJoinInPlaceConsistentWithPure sample)
        ]
