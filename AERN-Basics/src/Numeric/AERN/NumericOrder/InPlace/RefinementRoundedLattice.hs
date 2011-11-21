{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.InPlace.RefinementRoundedLattice
    Description :  numeric-order lattices with refinement-rounded in-place operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric-order lattices with refinement-rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent NumericOrder. 
-}
module Numeric.AERN.NumericOrder.InPlace.RefinementRoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.NumericOrder.Arbitrary
import Numeric.AERN.NumericOrder.PartialComparison 
import Numeric.AERN.NumericOrder.Extrema
import Numeric.AERN.NumericOrder.RefinementRoundedLattice

import Numeric.AERN.Basics.Laws.RoundedOpInPlace

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with refinement-outer-rounding numerical-order-lattice operations.
-}
class (OuterRoundedLatticeEffort t, CanBeMutable t) => 
    OuterRoundedLatticeInPlace t 
    where
    maxOutInPlaceEff :: OpMutable2Eff (MinmaxOuterEffortIndicator t) t s 
    minOutInPlaceEff :: OpMutable2Eff (MinmaxOuterEffortIndicator t) t s

maxOutInPlaceEffFromPure,
 minOutInPlaceEffFromPure ::
    (CanBeMutable t, OuterRoundedLattice t) => 
    OpMutable2Eff (MinmaxOuterEffortIndicator t) t s  
maxOutInPlaceEffFromPure = pureToMutable2Eff maxOutEff
minOutInPlaceEffFromPure = pureToMutable2Eff minOutEff

maxOutEffFromInPlace,
 minOutEffFromInPlace ::
 (CanBeMutable t, OuterRoundedLatticeInPlace t) =>
 (MinmaxOuterEffortIndicator t) -> t -> t -> t
maxOutEffFromInPlace = mutable2EffToPure maxOutInPlaceEff 
minOutEffFromInPlace = mutable2EffToPure minOutInPlaceEff 

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class (InnerRoundedLatticeEffort t, CanBeMutable t) => 
    InnerRoundedLatticeInPlace t 
    where
    maxInInPlaceEff :: OpMutable2Eff (MinmaxInnerEffortIndicator t) t s 
    minInInPlaceEff :: OpMutable2Eff (MinmaxInnerEffortIndicator t) t s
    
maxInInPlaceEffFromPure,
 minInInPlaceEffFromPure ::
    (CanBeMutable t, InnerRoundedLattice t) => 
    OpMutable2Eff (MinmaxInnerEffortIndicator t) t s  
maxInInPlaceEffFromPure = pureToMutable2Eff maxInEff
minInInPlaceEffFromPure = pureToMutable2Eff minInEff

maxInEffFromInPlace,
 minInEffFromInPlace ::
 (CanBeMutable t, InnerRoundedLatticeInPlace t) =>
 (MinmaxInnerEffortIndicator t) -> t -> t -> t
maxInEffFromInPlace = mutable2EffToPure maxInInPlaceEff 
minInEffFromInPlace = mutable2EffToPure minInInPlaceEff 

class (OuterRoundedLatticeInPlace t, InnerRoundedLatticeInPlace t) => 
    RefinementRoundedLatticeInPlace t

propRefinementRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RefinementRoundedLatticeInPlace t, RefinementRoundedLattice t, 
     CanBeMutable t) => 
    t -> 
    (MinmaxOuterEffortIndicator t, MinmaxInnerEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinInPlaceConsistentWithPure sample (minmaxOutEffort, minmaxInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (maxOutInPlaceEff minmaxOutEffort)  
        (maxInInPlaceEff minmaxInEffort)
        (maxOutEff minmaxOutEffort) 
        (maxInEff minmaxInEffort) 
        e1 e2  

propRefinementRoundedLatticeMeetInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RefinementRoundedLatticeInPlace t, RefinementRoundedLattice t, 
     CanBeMutable t) => 
    t -> 
    (MinmaxOuterEffortIndicator t, MinmaxInnerEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetInPlaceConsistentWithPure sample (minmaxOutEffort, minmaxInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (minOutInPlaceEff minmaxOutEffort)  
        (minInInPlaceEff minmaxInEffort)
        (minOutEff minmaxOutEffort) 
        (minInEff minmaxInEffort) 
        e1 e2  

testsRefinementRoundedLatticeInPlace :: 
    (PartialComparison t,
     RefinementRoundedLatticeInPlace t, RefinementRoundedLattice t, 
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsRefinementRoundedLatticeInPlace (name, sample) =
    testGroup (name ++ " (min,max) refinement-rounded in-place") $
        [
         testProperty "join in-place=pure"
             (propRefinementRoundedLatticeJoinInPlaceConsistentWithPure sample)
        ,
         testProperty "meet in-place=pure"
             (propRefinementRoundedLatticeMeetInPlaceConsistentWithPure sample)
        ]

