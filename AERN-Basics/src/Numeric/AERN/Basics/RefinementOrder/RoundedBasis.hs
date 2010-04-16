{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.RoundedBasis
    Description :  domain bases with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Domain bases with outwards and inwards rounded operations.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.RoundedBasis 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.PartialComparison
import Numeric.AERN.Basics.RefinementOrder.Arbitrary

import Numeric.AERN.Basics.Laws.OperationRelation
import Numeric.AERN.Basics.Laws.RoundedOperation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

infixr 2 <|\/>?, <⊔>?, >|\/<?, >⊔<?

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedBasis t where
    type PartialJoinOutEffortIndicator t
    partialJoinOutEff :: PartialJoinOutEffortIndicator t -> t -> t -> Maybe t
    partialJoinOutDefaultEffort :: t -> PartialJoinOutEffortIndicator t

    (<|\/>?) ::
        (?partialJoinOutEffort :: PartialJoinOutEffortIndicator t) => 
        t -> t -> Maybe t
    
    (<|\/>?) = partialJoinOutEff ?partialJoinOutEffort 

{-| convenience Unicode notation for '<|\/>?' -}
(<⊔>?) :: 
    (OuterRoundedBasis t, ?partialJoinOutEffort :: PartialJoinOutEffortIndicator t) => 
    t -> t -> Maybe t
(<⊔>?) = (<|\/>?)

-- properties of OuterRoundedBasis
propOuterRoundedBasisComparisonCompatible :: 
    (PartialComparison t, OuterRoundedBasis t) => 
    t -> 
    (PartialCompareEffortIndicator t, PartialJoinOutEffortIndicator t) ->
    t -> t -> Bool
propOuterRoundedBasisComparisonCompatible _ (effortComp, effortJoin) =
    let ?pCompareEffort = effortComp; ?partialJoinOutEffort = effortJoin in 
        downRoundedPartialJoinOfOrderedPair (|<=?) (<|\/>?)

{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedBasis t where
    type PartialJoinInEffortIndicator t
    partialJoinInEff :: PartialJoinInEffortIndicator t -> t -> t -> Maybe t
    partialJoinInDefaultEffort :: t -> PartialJoinInEffortIndicator t

    (>|\/<?) ::
        (?partialJoinInEffort :: PartialJoinInEffortIndicator t) => 
        t -> t -> Maybe t
    
    (>|\/<?) = partialJoinInEff ?partialJoinInEffort 

{-| convenience Unicode notation for '>|\/<?' -}
(>⊔<?) :: 
    (InnerRoundedBasis t, ?partialJoinInEffort :: PartialJoinInEffortIndicator t) => 
    t -> t -> Maybe t
(>⊔<?) = (>|\/<?)

-- properties of InnerRoundedBasis:
propInnerRoundedBasisJoinAboveBoth :: 
    (PartialComparison t, InnerRoundedBasis t) => 
    t -> 
    (PartialCompareEffortIndicator t, PartialJoinInEffortIndicator t) ->
    t -> t -> Bool
propInnerRoundedBasisJoinAboveBoth _ (effortComp, effortJoin) = 
    let ?pCompareEffort = effortComp; ?partialJoinInEffort = effortJoin in 
        partialJoinAboveOperands (|<=?) (>|\/<?)

class (OuterRoundedBasis t, InnerRoundedBasis t) => RoundedBasis t

-- properties of RoundedBasis:
propRoundedBasisJoinIdempotent :: 
    (PartialComparison t, RoundedBasis t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     PartialJoinInEffortIndicator t, 
     PartialJoinOutEffortIndicator t) ->
    t -> Bool
propRoundedBasisJoinIdempotent _ (effortComp, effortJoinIn, effortJoinOut) = 
    let ?pCompareEffort = effortComp 
        ?partialJoinInEffort = effortJoinIn
        ?partialJoinOutEffort = effortJoinOut in 
    partialRoundedIdempotent (|<=?) (>|\/<?) (<|\/>?)

propRoundedBasisJoinCommutative :: 
    (PartialComparison t, RoundedBasis t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     PartialJoinInEffortIndicator t, 
     PartialJoinOutEffortIndicator t) ->
    UniformlyOrderedPair t -> Bool
propRoundedBasisJoinCommutative _ (effortComp, effortJoinIn, effortJoinOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp 
        ?partialJoinInEffort = effortJoinIn
        ?partialJoinOutEffort = effortJoinOut in 
    partialRoundedCommutative (|<=?) (>|\/<?) (<|\/>?) e1 e2

propRoundedBasisJoinAssociative :: 
    (PartialComparison t, RoundedBasis t) => 
    t -> 
    (PartialCompareEffortIndicator t, 
     PartialJoinInEffortIndicator t, 
     PartialJoinOutEffortIndicator t) ->
    UniformlyOrderedTriple t -> Bool
propRoundedBasisJoinAssociative _ (effortComp, effortJoinIn, effortJoinOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp 
        ?partialJoinInEffort = effortJoinIn
        ?partialJoinOutEffort = effortJoinOut in 
    partialRoundedAssociative (|<=?) (>|\/<?) (<|\/>?) e1 e2 e3


testsRoundedBasis ::
    (PartialComparison t,
     RoundedBasis t,
     Arbitrary t, Show t, 
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t), 
     Arbitrary (PartialJoinOutEffortIndicator t), Show (PartialJoinOutEffortIndicator t), 
     Arbitrary (PartialJoinInEffortIndicator t), Show (PartialJoinInEffortIndicator t), 
     ArbitraryOrderedTuple t,
     Eq t) => 
    (String, t) -> Test
testsRoundedBasis (name, sample) =
    testGroup (name ++ " (<⊔>?, >⊔<?)") $
        [
         testProperty "rounded join comparison compatible"  (propOuterRoundedBasisComparisonCompatible sample),
         testProperty "rounded join above both"  (propInnerRoundedBasisJoinAboveBoth sample),
         testProperty "rounded join idempotent" (propRoundedBasisJoinIdempotent sample),
         testProperty "rounded join commutative" (propRoundedBasisJoinCommutative sample),
         testProperty "rounded join associative" (propRoundedBasisJoinAssociative sample)
        ]

-- mutable versions (TODO)    
