{-# LANGUAGE TypeFamilies #-}
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
import Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison
import Numeric.AERN.Basics.RefinementOrder.Arbitrary

import Numeric.AERN.Basics.Laws.OperationRelation
import Numeric.AERN.Basics.Laws.RoundedOperation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedBasis t where
    type PartialJoinOutEffortIndicator t
    partialJoinOut :: PartialJoinOutEffortIndicator t -> t -> t -> Maybe t
    partialJoinOutDefaultEffort :: t -> PartialJoinOutEffortIndicator t

    (<|\/>?) :: t -> t -> Maybe t
    
    a <|\/>? b = partialJoinOut (partialJoinOutDefaultEffort a) a b 

{-| convenience Unicode notation for '<|\/>?' -}
(<⊔>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t
(<⊔>?) = (<|\/>?)

-- properties of OuterRoundedBasis
propOuterRoundedBasisComparisonCompatible :: 
    (SemidecidableComparison t, OuterRoundedBasis t) => 
    t -> t -> t -> Bool
propOuterRoundedBasisComparisonCompatible _ = 
    downRoundedPartialJoinOfOrderedPair (|<=?) (<|\/>?)

{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedBasis t where
    type PartialJoinInEffortIndicator t
    partialJoinIn :: PartialJoinInEffortIndicator t -> t -> t -> Maybe t
    partialJoinInDefaultEffort :: t -> PartialJoinInEffortIndicator t

    (>|\/<?) :: t -> t -> Maybe t
    
    a >|\/<? b = partialJoinIn (partialJoinInDefaultEffort a) a b 

{-| convenience Unicode notation for '>|\/<?' -}
(>⊔<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>⊔<?) = (>|\/<?)

-- properties of InnerRoundedBasis:
propInnerRoundedBasisJoinAboveBoth :: 
    (SemidecidableComparison t, InnerRoundedBasis t) => 
    t -> t -> t -> Bool
propInnerRoundedBasisJoinAboveBoth _ = 
    partialJoinAboveOperands (|<=?) (>|\/<?)

class (OuterRoundedBasis t, InnerRoundedBasis t) => RoundedBasis t

-- properties of RoundedBasis:
propRoundedBasisJoinIdempotent :: 
    (SemidecidableComparison t, RoundedBasis t) => 
    t -> t -> Bool
propRoundedBasisJoinIdempotent _ = 
    partialRoundedIdempotent (|<=?) (>|\/<?) (<|\/>?)

propRoundedBasisJoinCommutative :: 
    (SemidecidableComparison t, RoundedBasis t) => 
    t -> UniformlyOrderedPair t -> Bool
propRoundedBasisJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    partialRoundedCommutative (|<=?) (>|\/<?) (<|\/>?) e1 e2

propRoundedBasisJoinAssociative :: 
    (SemidecidableComparison t, RoundedBasis t) => 
    t -> UniformlyOrderedTriple t -> Bool
propRoundedBasisJoinAssociative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialRoundedAssociative (|<=?) (>|\/<?) (<|\/>?) e1 e2 e3


testsRoundedBasis ::
    (SemidecidableComparison t,
     RoundedBasis t,
     Arbitrary t, 
     ArbitraryOrderedTuple t,
     Eq t, 
     Show t) => 
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
