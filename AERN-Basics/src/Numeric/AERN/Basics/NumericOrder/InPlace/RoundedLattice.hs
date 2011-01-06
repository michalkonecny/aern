{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.InPlace.RoundedLattice
    Description :  lattices with directed-rounded in-place operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with directed-rounded in-place operations.
    
    This module is hidden and reexported via its grand-parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.InPlace.RoundedLattice 
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
import Numeric.AERN.Basics.NumericOrder.RoundedLattice

import Numeric.AERN.Basics.Laws.RoundedOpInPlace

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with directed-rounding lattice operations.
-}
class (RoundedLattice t, CanBeMutable t) => RoundedLatticeInPlace t where
    maxUpInPlaceEff :: t -> OpMutable2Eff (MinmaxEffortIndicator t) t s
    maxDnInPlaceEff :: t -> OpMutable2Eff (MinmaxEffortIndicator t) t s
    minUpInPlaceEff :: t -> OpMutable2Eff (MinmaxEffortIndicator t) t s
    minDnInPlaceEff :: t -> OpMutable2Eff (MinmaxEffortIndicator t) t s
    maxUpInPlaceEff sample = pureToMutable2Eff sample maxUpEff 
    maxDnInPlaceEff sample = pureToMutable2Eff sample maxDnEff 
    minUpInPlaceEff sample = pureToMutable2Eff sample minUpEff 
    minDnInPlaceEff sample = pureToMutable2Eff sample minDnEff 

propRoundedLatticeJoinInPlaceConsistentWithPure ::
    (PartialComparison t, RoundedLatticeInPlace t, CanBeMutable t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinInPlaceConsistentWithPure sample (minmaxEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (maxDnInPlaceEff sample minmaxEffort)  
        (maxUpInPlaceEff sample minmaxEffort)
        (maxDnEff minmaxEffort) 
        (maxUpEff minmaxEffort) 
        e1 e2  

propRoundedLatticeMeetInPlaceConsistentWithPure ::
    (PartialComparison t, RoundedLatticeInPlace t, CanBeMutable t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetInPlaceConsistentWithPure sample (minmaxEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) =
    inPlaceConsistentWithPure2 (pLeqEff effortComp) 
        (minDnInPlaceEff sample minmaxEffort)  
        (minUpInPlaceEff sample minmaxEffort)
        (minDnEff minmaxEffort) 
        (minUpEff minmaxEffort) 
        e1 e2  

testsRoundedLatticeInPlace :: 
    (PartialComparison t,
     RoundedLatticeInPlace t,  
     CanBeMutable t,
     Arbitrary t, Show t, 
     Arbitrary (MinmaxEffortIndicator t), Show (MinmaxEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRoundedLatticeInPlace (name, sample) maybeIllegalArg =
    testGroup (name ++ " (min,max) rounded in-place") $
        [
         testProperty "join in-place=pure"
             (propRoundedLatticeJoinInPlaceConsistentWithPure sample)
        ,
         testProperty "meet in-place=pure"
             (propRoundedLatticeMeetInPlaceConsistentWithPure sample)
        ]

