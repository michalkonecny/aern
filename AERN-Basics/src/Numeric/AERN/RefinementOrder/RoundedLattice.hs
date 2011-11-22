{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.RoundedLattice
    Description :  lattices with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with outwards and inwards rounded operations.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.RefinementOrder.RoundedLattice 
where

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.RefinementOrder.Arbitrary
import Numeric.AERN.RefinementOrder.PartialComparison

import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with outward-rounding lattice operations.
-}
class RoundedLatticeEffort t where
    type JoinMeetEffortIndicator t
    joinmeetDefaultEffort :: t -> JoinMeetEffortIndicator t

class (RoundedLatticeEffort t) => RoundedLattice t where
    joinInEff :: JoinMeetEffortIndicator t -> t -> t -> t
    joinOutEff :: JoinMeetEffortIndicator t -> t -> t -> t
    meetInEff :: JoinMeetEffortIndicator t -> t -> t -> t
    meetOutEff :: JoinMeetEffortIndicator t -> t -> t -> t

-- properties of RoundedLattice

propRoundedLatticeComparisonCompatible :: 
    (PartialComparison t, RoundedLattice t) => 
    t ->
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible _ (effortComp, effortInOut) 
        (UniformlyOrderedPair (e1,e2)) =
    (downRoundedJoinOfOrderedPair (pLeqEff effortComp) (joinOutEff effortInOut) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (pLeqEff effortComp) (meetInEff effortInOut) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (pLeqEff effortComp) (joinInEff effortInOut) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (pLeqEff effortComp) (meetOutEff effortInOut) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> 
    Bool
propRoundedLatticeJoinIdempotent _ (effortComp, effortInOut) 
        (UniformlyOrderedSingleton e) = 
    roundedIdempotent (pLeqEff effortComp) (joinInEff effortInOut) (joinOutEff effortInOut) e

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (joinInEff effortInOut) (joinOutEff effortInOut) e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (joinInEff effortInOut) (joinOutEff effortInOut) e1 e2 e3

propRoundedLatticeJoinMonotone ::
    (Eq t, RoundedLattice t, PartialComparison t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    LEPair t -> 
    LEPair t ->
    Bool
propRoundedLatticeJoinMonotone _ (effortComp, effortInOut)
        (LEPair (e1Lower,e1)) 
        (LEPair (e2Lower,e2)) =
    case pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
   where
    rLower = joinOutEff effortInOut e1Lower e2Lower 
    r = joinInEff effortInOut e1 e2 

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> 
    Bool
propRoundedLatticeMeetIdempotent _ (effortComp, effortInOut) 
        (UniformlyOrderedSingleton e) = 
    roundedIdempotent (pLeqEff effortComp) (meetInEff effortInOut) (meetOutEff effortInOut) e

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (meetInEff effortInOut) (meetOutEff effortInOut) e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (meetInEff effortInOut) (meetOutEff effortInOut) e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (pLeqEff effortComp) (meetInEff effortInOut) (joinInEff effortInOut) (meetOutEff effortInOut) (joinOutEff effortInOut) e1 e2 e3

propRoundedLatticeMeetMonotone ::
    (Eq t, RoundedLattice t, PartialComparison t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    LEPair t -> 
    LEPair t ->
    Bool
propRoundedLatticeMeetMonotone _ (effortComp, effortInOut)
        (LEPair (e1Lower,e1)) 
        (LEPair (e2Lower,e2)) =
    case pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
   where
    rLower = meetOutEff effortInOut e1Lower e2Lower 
    r = meetInEff effortInOut e1 e2 

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (pLeqEff effortComp) (meetInEff effortInOut) (joinInEff effortInOut) (meetOutEff effortInOut) (joinOutEff effortInOut) e1 e2 e3)
    && 
    (roundedLeftDistributive  (pLeqEff effortComp) (joinInEff effortInOut) (meetInEff effortInOut) (joinOutEff effortInOut) (meetOutEff effortInOut) e1 e2 e3)


testsRoundedLatticeDistributive :: 
    (PartialComparison t,
     RoundedLattice t,
     Arbitrary t, Show t, HasLegalValues t,
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     Arbitrary (JoinMeetEffortIndicator t), Show (JoinMeetEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t) => 
    (String, t) -> Test
testsRoundedLatticeDistributive (name, sample) =
    testGroup (name ++ " (⊓,⊔) rounded") $
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
         testProperty "join monotone" (propRoundedLatticeJoinMonotone sample)
        ,
         testProperty "meet idempotent" (propRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRoundedLatticeMeetAssocative sample)
        ,
         testProperty "meet monotone" (propRoundedLatticeMeetMonotone sample)
        ,
         testProperty "distributive" (propRoundedLatticeDistributive sample)
        ]
    
-- mutable versions (TODO)    