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

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedBasis t where
    partialJoinOut :: [EffortIndicator] -> t -> t -> Maybe t
    partialJoinOutDefaultEffort :: t -> [EffortIndicator]

    (<|\/>?) :: t -> t -> Maybe t
    
    a <|\/>? b = partialJoinOut (partialJoinOutDefaultEffort a) a b 

{-| convenience Unicode notation for '<|\/>?' -}
(<⊔>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t
(<⊔>?) = (<|\/>?)

-- properties of OuterRoundedBasis
propOuterRoundedBasisComparisonCompatible :: (SemidecidableComparison t, OuterRoundedBasis t) => t -> t -> Bool
propOuterRoundedBasisComparisonCompatible = downRoundedPartialJoinOfOrderedPair (|<=?) (<|\/>?)

{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedBasis t where
    partialJoinIn :: [EffortIndicator] -> t -> t -> Maybe t
    partialJoinInDefaultEffort :: t -> [EffortIndicator]

    (>|\/<?) :: t -> t -> Maybe t
    
    a >|\/<? b = partialJoinIn (partialJoinInDefaultEffort a) a b 

{-| convenience Unicode notation for '>|\/<?' -}
(>⊔<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>⊔<?) = (>|\/<?)

-- properties of InnerRoundedBasis:
propInnerRoundedBasisJoinAboveBoth :: (SemidecidableComparison t, InnerRoundedBasis t) => t -> t -> Bool
propInnerRoundedBasisJoinAboveBoth = partialJoinAboveOperands (|<=?) (>|\/<?)

class (OuterRoundedBasis t, InnerRoundedBasis t) => RoundedBasis t

-- properties of RoundedBasis:
propRoundedBasisJoinIdempotent :: (SemidecidableComparison t, RoundedBasis t) => t -> Bool
propRoundedBasisJoinIdempotent = partialRoundedIdempotent (|<=?) (>|\/<?) (<|\/>?)

propRoundedBasisJoinCommutative :: (SemidecidableComparison t, RoundedBasis t) => UniformlyOrderedPair t -> Bool
propRoundedBasisJoinCommutative (UniformlyOrderedPair (e1,e2)) = 
    partialRoundedCommutative (|<=?) (>|\/<?) (<|\/>?) e1 e2

propRoundedBasisJoinAssocative :: (SemidecidableComparison t, RoundedBasis t) => UniformlyOrderedTriple t -> Bool
propRoundedBasisJoinAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialRoundedAssociative (|<=?) (>|\/<?) (<|\/>?) e1 e2 e3


-- mutable versions (TODO)    
