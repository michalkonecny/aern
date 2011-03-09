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
class (OuterRoundedLattice t, CanBeMutable t) => OuterRoundedLatticeInPlace t where
    maxOuterInPlaceEff :: t -> OpMutable2Eff (MinmaxOuterEffortIndicator t) t s 
    minOuterInPlaceEff :: t -> OpMutable2Eff (MinmaxOuterEffortIndicator t) t s
    maxOuterInPlaceEff sample = pureToMutable2Eff sample maxOuterEff
    minOuterInPlaceEff sample = pureToMutable2Eff sample minOuterEff

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class (InnerRoundedLattice t, CanBeMutable t) => InnerRoundedLatticeInPlace t where
    maxInnerInPlaceEff :: t -> OpMutable2Eff (MinmaxInnerEffortIndicator t) t s 
    minInnerInPlaceEff :: t -> OpMutable2Eff (MinmaxInnerEffortIndicator t) t s
    maxInnerInPlaceEff sample = pureToMutable2Eff sample maxInnerEff
    minInnerInPlaceEff sample = pureToMutable2Eff sample minInnerEff

class (OuterRoundedLatticeInPlace t, InnerRoundedLatticeInPlace t) => 
    RefinementRoundedLatticeInPlace t

propRefinementRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, RefinementRoundedLatticeInPlace t, CanBeMutable t) => 
    t -> 
    (MinmaxOuterEffortIndicator t, MinmaxInnerEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinInPlaceConsistentWithPure sample (minmaxOutEffort, minmaxInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (maxOuterInPlaceEff sample minmaxOutEffort)  
        (maxInnerInPlaceEff sample minmaxInEffort)
        (maxOuterEff minmaxOutEffort) 
        (maxInnerEff minmaxInEffort) 
        e1 e2  

propRefinementRoundedLatticeMeetInPlaceConsistentWithPure ::
    (PartialComparison t, RefinementRoundedLatticeInPlace t, CanBeMutable t) => 
    t -> 
    (MinmaxOuterEffortIndicator t, MinmaxInnerEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetInPlaceConsistentWithPure sample (minmaxOutEffort, minmaxInEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (minOuterInPlaceEff sample minmaxOutEffort)  
        (minInnerInPlaceEff sample minmaxInEffort)
        (minOuterEff minmaxOutEffort) 
        (minInnerEff minmaxInEffort) 
        e1 e2  

testsRefinementRoundedLatticeInPlace :: 
    (PartialComparison t,
     RefinementRoundedLatticeInPlace t,  
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRefinementRoundedLatticeInPlace (name, sample) maybeIllegalArg =
    testGroup (name ++ " (min,max) refinement-rounded in-place") $
        [
         testProperty "join in-place=pure"
             (propRefinementRoundedLatticeJoinInPlaceConsistentWithPure sample)
        ,
         testProperty "meet in-place=pure"
             (propRefinementRoundedLatticeMeetInPlaceConsistentWithPure sample)
        ]

