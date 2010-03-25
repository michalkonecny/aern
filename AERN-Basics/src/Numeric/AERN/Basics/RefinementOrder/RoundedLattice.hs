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
import Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison

import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedLattice t where
    joinOut :: [EffortIndicator] -> t -> t -> t
    meetOut :: [EffortIndicator] -> t -> t -> t
    joinmeetOutDefaultEffort :: t -> [EffortIndicator]

    (<|\/>) :: t -> t -> t
    (<|/\>) :: t -> t -> t
    
    a <|\/> b = joinOut (joinmeetOutDefaultEffort a) a b 
    a <|/\> b = meetOut (joinmeetOutDefaultEffort a) a b 

{-| convenience Unicode notation for '<|\/>' -}
(<⊔>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊔>) = (<|\/>)
{-| convenience Unicode notation for '<|/\>' -}
(<⊓>) :: (OuterRoundedLattice t) => t -> t -> t
(<⊓>) = (<|/\>)


{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedLattice t where
    joinIn :: [EffortIndicator] -> t -> t -> t
    meetIn :: [EffortIndicator] -> t -> t -> t
    joinmeetInDefaultEffort :: t -> [EffortIndicator]

    (>|\/<) :: t -> t -> t
    (>|/\<) :: t -> t -> t
    
    a >|\/< b = joinIn (joinmeetInDefaultEffort a) a b 
    a >|/\< b = meetIn (joinmeetInDefaultEffort a) a b 

{-| convenience Unicode notation for '>|\/<' -}
(>⊔<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊔<) = (>|\/<)
{-| convenience Unicode notation for '>|/\<' -}
(>⊓<) :: (InnerRoundedLattice t) => t -> t -> t
(>⊓<) = (>|/\<)

class (InnerRoundedLattice t, OuterRoundedLattice t) => RoundedLattice t

-- properties of RoundedLattice

propRoundedLatticeIllegalArgException :: (RoundedLattice t) => t -> t -> Bool
propRoundedLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] | op <- [(>⊔<), (<⊔>), (>⊓<), (<⊓>)]] 

propRoundedLatticeComparisonCompatible :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible (UniformlyOrderedPair (e1,e2)) = 
    (downRoundedJoinOfOrderedPair (|<=?) (<⊓>) e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (|<=?) (>⊔<) e1 e2)

propRoundedLatticeJoinAboveBoth :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (|<=?) (>⊔<) e1 e2

propRoundedLatticeMeetBelowBoth :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (|<=?) (<⊓>) e1 e2

propRoundedLatticeJoinIdempotent :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    t -> Bool
propRoundedLatticeJoinIdempotent = roundedIdempotent (|<=?) (>⊔<) (<⊔>)

propRoundedLatticeJoinCommutative :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) (>⊔<) (<⊔>) e1 e2

propRoundedLatticeJoinAssocative :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) (>⊔<) (<⊔>) e1 e2 e3

propRoundedLatticeMeetIdempotent :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    t -> Bool
propRoundedLatticeMeetIdempotent = 
    roundedIdempotent (|<=?) (>⊓<) (<⊓>)

propRoundedLatticeMeetCommutative :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (|<=?) (>⊓<) (<⊓>) e1 e2

propRoundedLatticeMeetAssocative :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (|<=?) (>⊓<) (<⊓>) e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3

propRoundedLatticeDistributive :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)
    && 
    (roundedLeftDistributive  (|<=?) (>⊔<) (>⊓<) (<⊔>) (<⊓>) e1 e2 e3)


    
-- mutable versions (TODO)    