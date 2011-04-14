{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.InPlace.RefinementRoundedLattice
    Description :  numeric-order lattices with refinement-rounded in-place operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric-order lattices with refinement-rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.InPlace.RefinementRoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.NumericOrder.PartialComparison 
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice

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
    maxOuterInPlaceEff :: OpMutable2Eff (MinmaxOuterEffortIndicator t) t s 
    minOuterInPlaceEff :: OpMutable2Eff (MinmaxOuterEffortIndicator t) t s

maxOuterInPlaceEffFromPure,
 minOuterInPlaceEffFromPure ::
    (CanBeMutable t, OuterRoundedLattice t) => 
    OpMutable2Eff (MinmaxOuterEffortIndicator t) t s  
maxOuterInPlaceEffFromPure = pureToMutable2Eff maxOuterEff
minOuterInPlaceEffFromPure = pureToMutable2Eff minOuterEff

maxOuterEffFromInPlace,
 minOuterEffFromInPlace ::
 (CanBeMutable t, OuterRoundedLatticeInPlace t) =>
 (MinmaxOuterEffortIndicator t) -> t -> t -> t
maxOuterEffFromInPlace = mutable2EffToPure $ maxOuterInPlaceEff 
minOuterEffFromInPlace = mutable2EffToPure $ minOuterInPlaceEff 

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class (InnerRoundedLatticeEffort t, CanBeMutable t) => 
    InnerRoundedLatticeInPlace t 
    where
    maxInnerInPlaceEff :: OpMutable2Eff (MinmaxInnerEffortIndicator t) t s 
    minInnerInPlaceEff :: OpMutable2Eff (MinmaxInnerEffortIndicator t) t s
    
maxInnerInPlaceEffFromPure,
 minInnerInPlaceEffFromPure ::
    (CanBeMutable t, InnerRoundedLattice t) => 
    OpMutable2Eff (MinmaxInnerEffortIndicator t) t s  
maxInnerInPlaceEffFromPure = pureToMutable2Eff maxInnerEff
minInnerInPlaceEffFromPure = pureToMutable2Eff minInnerEff

maxInnerEffFromInPlace,
 minInnerEffFromInPlace ::
 (CanBeMutable t, InnerRoundedLatticeInPlace t) =>
 (MinmaxInnerEffortIndicator t) -> t -> t -> t
maxInnerEffFromInPlace = mutable2EffToPure $ maxInnerInPlaceEff 
minInnerEffFromInPlace = mutable2EffToPure $ minInnerInPlaceEff 

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
        (maxOuterInPlaceEff minmaxOutEffort)  
        (maxInnerInPlaceEff minmaxInEffort)
        (maxOuterEff minmaxOutEffort) 
        (maxInnerEff minmaxInEffort) 
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
        (minOuterInPlaceEff minmaxOutEffort)  
        (minInnerInPlaceEff minmaxInEffort)
        (minOuterEff minmaxOutEffort) 
        (minInnerEff minmaxInEffort) 
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

