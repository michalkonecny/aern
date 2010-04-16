{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

infixr 3 <|/\>, <⊓>, >|/\<, >⊓< 
infixr 2 <|\/>, <⊔>, >|\/<, >⊔< 

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedLattice t where
    type JoinMeetOutEffortIndicator t
    joinmeetOutDefaultEffort :: t -> JoinMeetOutEffortIndicator t

    joinOutEff :: JoinMeetOutEffortIndicator t -> t -> t -> t
    meetOutEff :: JoinMeetOutEffortIndicator t -> t -> t -> t

    (<|\/>) :: (?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => t -> t -> t
    (<|/\>) :: (?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => t -> t -> t
    
    (<|\/>) = joinOutEff ?joinmeetOutEffort 
    (<|/\>) = meetOutEff ?joinmeetOutEffort 

{-| convenience Unicode notation for '<|\/>' -}
(<⊔>) :: 
    (OuterRoundedLattice t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    t -> t -> t
(<⊔>) = (<|\/>)
{-| convenience Unicode notation for '<|/\>' -}
(<⊓>) :: 
    (OuterRoundedLattice t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    t -> t -> t
(<⊓>) = (<|/\>)


{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedLattice t where
    type JoinMeetInEffortIndicator t
    joinmeetInDefaultEffort :: t -> JoinMeetInEffortIndicator t
    
    joinInEff :: JoinMeetInEffortIndicator t -> t -> t -> t
    meetInEff :: JoinMeetInEffortIndicator t -> t -> t -> t

    (>|\/<) :: (?joinmeetInEffort :: JoinMeetInEffortIndicator t) => t -> t -> t
    (>|/\<) :: (?joinmeetInEffort :: JoinMeetInEffortIndicator t) => t -> t -> t
    
    (>|\/<) = joinInEff ?joinmeetInEffort 
    (>|/\<) = meetInEff ?joinmeetInEffort 

{-| convenience Unicode notation for '>|\/<' -}
(>⊔<) :: 
    (InnerRoundedLattice t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    t -> t -> t
(>⊔<) = (>|\/<)
{-| convenience Unicode notation for '>|/\<' -}
(>⊓<) :: 
    (InnerRoundedLattice t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    t -> t -> t
(>⊓<) = (>|/\<)

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
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    (downRoundedJoinOfOrderedPair (|<=?) (<⊓>) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (|<=?) (>⊔<) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth _ (effortComp, effortIn)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp;
        ?joinmeetInEffort = effortIn  in 
    joinAboveOperands (|<=?) (>⊔<) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth _ (effortComp, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp;
        ?joinmeetOutEffort = effortOut  in 
    meetBelowOperands (|<=?) (<⊓>) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    t -> Bool
propRoundedLatticeJoinIdempotent _ (effortComp, effortIn, effortOut) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedIdempotent (|<=?) (>⊔<) (<⊔>)

propRoundedLatticeJoinCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedCommutative (|<=?) (>⊔<) (<⊔>) e1 e2

propRoundedLatticeJoinAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedAssociative (|<=?) (>⊔<) (<⊔>) e1 e2 e3

propRoundedLatticeMeetIdempotent :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    t -> Bool
propRoundedLatticeMeetIdempotent _ (effortComp, effortIn, effortOut) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedIdempotent (|<=?) (>⊓<) (<⊓>)

propRoundedLatticeMeetCommutative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedCommutative (|<=?) (>⊓<) (<⊓>) e1 e2

propRoundedLatticeMeetAssocative :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedAssociative (|<=?) (>⊓<) (<⊓>) e1 e2 e3

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
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    roundedModular (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3

propRoundedLatticeDistributive :: 
    (PartialComparison t, RoundedLattice t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     JoinMeetInEffortIndicator t, 
     JoinMeetOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp; 
        ?joinmeetInEffort = effortIn; 
        ?joinmeetOutEffort = effortOut in 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)
    && 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)


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
    
-- mutable versions (TODO)    