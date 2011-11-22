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
class (RefinementRoundedLatticeEffort t, CanBeMutable t) => 
    RefinementRoundedLatticeInPlace t 
    where
    maxInInPlaceEff :: OpMutable2Eff (MinmaxInOutEffortIndicator t) t s 
    maxOutInPlaceEff :: OpMutable2Eff (MinmaxInOutEffortIndicator t) t s 
    minInInPlaceEff :: OpMutable2Eff (MinmaxInOutEffortIndicator t) t s
    minOutInPlaceEff :: OpMutable2Eff (MinmaxInOutEffortIndicator t) t s

maxOutInPlaceEffFromPure,
 minOutInPlaceEffFromPure ::
    (CanBeMutable t, RefinementRoundedLattice t) => 
    OpMutable2Eff (MinmaxInOutEffortIndicator t) t s  
maxOutInPlaceEffFromPure = pureToMutable2Eff maxOutEff
minOutInPlaceEffFromPure = pureToMutable2Eff minOutEff

maxOutEffFromInPlace,
 minOutEffFromInPlace ::
 (CanBeMutable t, RefinementRoundedLatticeInPlace t) =>
 (MinmaxInOutEffortIndicator t) -> t -> t -> t
maxOutEffFromInPlace = mutable2EffToPure maxOutInPlaceEff 
minOutEffFromInPlace = mutable2EffToPure minOutInPlaceEff 

    
maxInInPlaceEffFromPure,
 minInInPlaceEffFromPure ::
    (CanBeMutable t, RefinementRoundedLattice t) => 
    OpMutable2Eff (MinmaxInOutEffortIndicator t) t s  
maxInInPlaceEffFromPure = pureToMutable2Eff maxInEff
minInInPlaceEffFromPure = pureToMutable2Eff minInEff

maxInEffFromInPlace,
 minInEffFromInPlace ::
 (CanBeMutable t, RefinementRoundedLatticeInPlace t) =>
 (MinmaxInOutEffortIndicator t) -> t -> t -> t
maxInEffFromInPlace = mutable2EffToPure maxInInPlaceEff 
minInEffFromInPlace = mutable2EffToPure minInInPlaceEff 

propRefinementRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, 
     RefinementRoundedLatticeInPlace t, RefinementRoundedLattice t, 
     CanBeMutable t) => 
    t -> 
    (MinmaxInOutEffortIndicator t, MinmaxInOutEffortIndicator t, PartialCompareEffortIndicator t) -> 
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
    (MinmaxInOutEffortIndicator t, MinmaxInOutEffortIndicator t, PartialCompareEffortIndicator t) -> 
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
     Arbitrary (MinmaxInOutEffortIndicator t), Show (MinmaxInOutEffortIndicator t), 
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

