{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.RoundedLattice
    Description :  lattices with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with outwards and inwards rounded operations.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.RoundedLattice 
where

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.PartialComparison

import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedLattice t where
    type JoinMeetOutEffortIndicator t
    joinmeetOutDefaultEffort :: t -> JoinMeetOutEffortIndicator t

    joinOutEff :: JoinMeetOutEffortIndicator t -> t -> t -> t
    meetOutEff :: JoinMeetOutEffortIndicator t -> t -> t -> t


{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedLattice t where
    type JoinMeetInEffortIndicator t
    joinmeetInDefaultEffort :: t -> JoinMeetInEffortIndicator t
    
    joinInEff :: JoinMeetInEffortIndicator t -> t -> t -> t
    meetInEff :: JoinMeetInEffortIndicator t -> t -> t -> t


class (InnerRoundedLattice t, OuterRoundedLattice t) => RoundedLattice t

-- properties of RoundedLattice

propRoundedLatticeComparisonCompatible :: 
    (PartialComparison t, RoundedLattice t) => 
    t ->
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible _ (effortComp, effortIn, effortOut) 
        (UniformlyOrderedPair (e1,e2)) =
    (downRoundedJoinOfOrderedPair (pLeqEff effortComp) (joinOutEff effortOut) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (pLeqEff effortComp) (meetInEff effortIn) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (effortComp, effortIn)
        (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (pLeqEff effortComp) (meetInEff effortIn) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (effortComp, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (pLeqEff effortComp) (joinOutEff effortOut) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    t -> Bool
propRoundedLatticeJoinIdempotent _ (effortComp, effortIn, effortOut) = 
    roundedIdempotent (pLeqEff effortComp) (meetInEff effortIn) (meetOutEff effortOut)

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (meetInEff effortIn) (meetOutEff effortOut) e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (meetInEff effortIn) (meetOutEff effortOut) e1 e2 e3

propRoundedLatticeJoinMonotone ::
    (Eq t, RoundedLattice t, PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetOutEffortIndicator t, 
     JoinMeetInEffortIndicator t) -> 
    LEPair t -> 
    LEPair t ->
    Bool
propRoundedLatticeJoinMonotone _ (effortComp, effortOut, effortIn)
        (LEPair (e1Lower,e1)) 
        (LEPair (e2Lower,e2)) =
    case pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
   where
    rLower = joinOutEff effortOut e1Lower e2Lower 
    r = joinInEff effortIn e1 e2 

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    t -> Bool
propRoundedLatticeMeetIdempotent _ (effortComp, effortIn, effortOut) = 
    roundedIdempotent (pLeqEff effortComp) (joinInEff effortIn) (joinOutEff effortOut)

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (pLeqEff effortComp) (joinInEff effortIn) (joinOutEff effortOut) e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (pLeqEff effortComp) (joinInEff effortIn) (joinOutEff effortOut) e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (pLeqEff effortComp) (meetInEff effortIn) (joinInEff effortIn) (meetOutEff effortOut) (joinOutEff effortOut) e1 e2 e3

propRoundedLatticeMeetMonotone ::
    (Eq t, RoundedLattice t, PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetOutEffortIndicator t, 
     JoinMeetInEffortIndicator t) -> 
    LEPair t -> 
    LEPair t ->
    Bool
propRoundedLatticeMeetMonotone _ (effortComp, effortOut, effortIn)
        (LEPair (e1Lower,e1)) 
        (LEPair (e2Lower,e2)) =
    case pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
   where
    rLower = meetOutEff effortOut e1Lower e2Lower 
    r = meetInEff effortIn e1 e2 

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (pLeqEff effortComp) (meetInEff effortIn) (joinInEff effortIn) (meetOutEff effortOut) (joinOutEff effortOut) e1 e2 e3)
    && 
    (roundedLeftDistributive  (pLeqEff effortComp) (meetInEff effortIn) (joinInEff effortIn) (meetOutEff effortOut) (joinOutEff effortOut) e1 e2 e3)


testsRoundedLatticeDistributive :: 
    (PartialComparison t,
     RoundedLattice t,
     Arbitrary t, Show t, 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     Arbitrary (JoinMeetOutEffortIndicator t), Show (JoinMeetOutEffortIndicator t), 
     Arbitrary (JoinMeetInEffortIndicator t), Show (JoinMeetInEffortIndicator t), 
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