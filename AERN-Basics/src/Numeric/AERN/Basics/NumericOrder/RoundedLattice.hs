{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.RoundedLattice
    Description :  lattices with directed-rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices with directed-rounded operations.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.RoundedLattice 
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Comparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary 
import Numeric.AERN.Basics.NumericOrder.SemidecidableComparison 

import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

{-|
    A type with directed-rounding lattice operations.
-}
class RoundedLattice t where
    maxUpEff :: [EffortIndicator] -> t -> t -> t
    maxDnEff :: [EffortIndicator] -> t -> t -> t
    minUpEff :: [EffortIndicator] -> t -> t -> t
    minDnEff :: [EffortIndicator] -> t -> t -> t
    minmaxDefaultEffort :: t -> [EffortIndicator]

    maxUp :: t -> t -> t
    maxDn :: t -> t -> t
    minUp :: t -> t -> t
    minDn :: t -> t -> t
    
    maxUp a b = maxUpEff (minmaxDefaultEffort a) a b 
    maxDn a b = maxDnEff (minmaxDefaultEffort a) a b 
    minUp a b = minUpEff (minmaxDefaultEffort a) a b 
    minDn a b = minDnEff (minmaxDefaultEffort a) a b 


propRoundedLatticeIllegalArgException :: (RoundedLattice t) => t -> t -> Bool
propRoundedLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] | op <- [maxUp, maxDn, minUp, minDn]] 

propRoundedLatticeComparisonCompatible :: 
    (SemidecidableComparison t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticeComparisonCompatible (UniformlyOrderedPair (e1,e2)) = 
    (downRoundedJoinOfOrderedPair (<=?) minDn e1 e2)
    && 
    (upRoundedMeetOfOrderedPair (<=?) maxUp e1 e2)

propRoundedLatticeJoinAboveBoth :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (<=?) maxUp e1 e2

propRoundedLatticeMeetBelowBoth :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (<=?) minDn e1 e2

propRoundedLatticeJoinIdempotent :: (SemidecidableComparison t, RoundedLattice t) => t -> Bool
propRoundedLatticeJoinIdempotent = roundedIdempotent (<=?) maxUp maxDn

propRoundedLatticeJoinCommutative :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (<=?) maxUp maxDn e1 e2

propRoundedLatticeJoinAssocative :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (<=?) maxUp maxDn e1 e2 e3

propRoundedLatticeMeetIdempotent :: (SemidecidableComparison t, RoundedLattice t) => t -> Bool
propRoundedLatticeMeetIdempotent = 
    roundedIdempotent (<=?) minUp minDn

propRoundedLatticeMeetCommutative :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (<=?) minUp minDn e1 e2

propRoundedLatticeMeetAssocative :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (<=?) minUp minDn e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (<=?) maxUp minUp maxDn minDn e1 e2 e3

propRoundedLatticeDistributive :: (SemidecidableComparison t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (<=?) maxUp minUp maxDn minDn e1 e2 e3)
    && 
    (roundedLeftDistributive  (<=?) maxUp minUp maxDn minDn e1 e2 e3)

-- further properties of RoundedLattice (TODO)
    
{-|
    A type with directed-rounding lattice operations
    that also supported in-place.
-}
class (RoundedLattice t, CanBeMutable t) => RoundedLatticeMutable t where
    {-| maxUpMutable e a b c means a := maxUp e b c; a can be the same as b and/or c -}
    maxUpMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| maxDnMutable e a b c means a := maxDn e b c; a can be the same as b and/or c -}
    maxDnMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minUpMutable e a b c means a := minUp e b c; a can be the same as b and/or c -}
    minUpMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minDnMutable e a b c means a := minDn e b c; a can be the same as b and/or c -}
    minDnMutable :: [EffortIndicator] -> Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
    