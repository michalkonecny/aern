{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.RoundedLattice
    Description :  lattices with directed-rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with directed-rounded operations.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.NumericOrder.RoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.NumericOrder.Arbitrary 
import Numeric.AERN.NumericOrder.PartialComparison 
import Numeric.AERN.NumericOrder.Extrema

import Numeric.AERN.Basics.Laws.PartialRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with directed-rounding lattice operations.
-}
class (RoundedLatticeEffort t) => RoundedLattice t where
    maxUpEff :: MinmaxEffortIndicator t -> t -> t -> t
    maxDnEff :: MinmaxEffortIndicator t -> t -> t -> t
    minUpEff :: MinmaxEffortIndicator t -> t -> t -> t
    minDnEff :: MinmaxEffortIndicator t -> t -> t -> t

class RoundedLatticeEffort t where
    type MinmaxEffortIndicator t
    minmaxDefaultEffort :: t -> MinmaxEffortIndicator t

propRoundedLatticeComparisonCompatible :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible _ (minmaxEffort, effortComp) 
        (UniformlyOrderedPair (e1,e2)) =
    (downRoundedJoinOfOrderedPair (pLeqEff effortComp) (minDnEff minmaxEffort) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (pLeqEff effortComp) (maxUpEff minmaxEffort) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (minmaxEffort, effortComp) 
        (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (pLeqEff effortComp) (maxUpEff minmaxEffort) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (minmaxEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (pLeqEff effortComp) (minDnEff minmaxEffort) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    t -> Bool
propRoundedLatticeJoinIdempotent _ (minmaxEffort, effortComp) = 
    roundedIdempotent (pLeqEff effortComp) (maxUpEff minmaxEffort) (maxDnEff minmaxEffort)

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (minmaxEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (maxUpEff minmaxEffort) (maxDnEff minmaxEffort) e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (minmaxEffort, effortComp)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (maxUpEff minmaxEffort) (maxDnEff minmaxEffort) e1 e2 e3

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedSingleton t -> 
    Bool
propRoundedLatticeMeetIdempotent _ (minmaxEffort, effortComp) 
        (UniformlyOrderedSingleton e) = 
    roundedIdempotent (pLeqEff effortComp) (minUpEff minmaxEffort) (minDnEff minmaxEffort) e

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (minmaxEffort, effortComp)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (minUpEff minmaxEffort) (minDnEff minmaxEffort) e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (minmaxEffort, effortComp)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (minUpEff minmaxEffort) (minDnEff minmaxEffort) e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular _ (minmaxEffort, effortComp)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (pLeqEff effortComp) 
        (maxUpEff minmaxEffort) (minUpEff minmaxEffort) 
        (maxDnEff minmaxEffort) (minDnEff minmaxEffort) 
        e1 e2 e3

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (MinmaxEffortIndicator t, PartialCompareEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (minmaxEffort, effortComp)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive (pLeqEff effortComp) 
        (maxUpEff minmaxEffort) (minUpEff minmaxEffort) 
        (maxDnEff minmaxEffort) (minDnEff minmaxEffort) 
        e1 e2 e3)
    && 
    (roundedLeftDistributive (pLeqEff effortComp) 
        (minUpEff minmaxEffort) (maxUpEff minmaxEffort) 
        (minDnEff minmaxEffort) (maxDnEff minmaxEffort) 
        e1 e2 e3)

testsRoundedLatticeDistributive :: 
    (PartialComparison t,
     RoundedLattice t,
     Arbitrary t, Show t, HasLegalValues t, 
     Arbitrary (MinmaxEffortIndicator t), Show (MinmaxEffortIndicator t), 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t
     ) => 
    (String, t) -> Test
testsRoundedLatticeDistributive (name, sample) =
    testGroup (name ++ " (min,max) rounded") $
        [
         testProperty "Comparison compatible" (propRoundedLatticeComparisonCompatible sample)
        ,
         testProperty "join above" (propRoundedLatticeJoinAboveBoth sample)
        ,
         testProperty "meet below" (propRoundedLatticeMeetBelowBoth sample)
        ,
         testProperty "join idempotent" (propRoundedLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propRoundedLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propRoundedLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRoundedLatticeMeetAssocative sample)
        ,
         testProperty "distributive" (propRoundedLatticeDistributive sample)
        ]
