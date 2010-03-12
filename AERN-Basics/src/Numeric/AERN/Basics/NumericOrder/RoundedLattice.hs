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
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Poset 
import Numeric.AERN.Basics.NumericOrder.SemidecidablePoset 

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

propRoundedLatticePosetCompatible :: 
    (SemidecidablePoset t, RoundedLattice t) => 
    UniformlyOrderedPair t -> Bool
propRoundedLatticePosetCompatible (UniformlyOrderedPair (e1,e2)) = 
    (roundedJoinOfOrderedPair (completeWith True (<=?)) minUp e1 e2)
    && 
    (roundedMeetOfOrderedPair (completeWith True (<=?)) maxDn e1 e2)

propRoundedLatticeJoinAboveBoth :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinAboveBoth (UniformlyOrderedPair (e1,e2)) = 
    joinAboveOperands (completeWith True (<=?)) maxUp e1 e2

propRoundedLatticeMeetBelowBoth :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetBelowBoth (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (completeWith True (<=?)) minDn e1 e2

propRoundedLatticeJoinIdempotent :: (SemidecidablePoset t, RoundedLattice t) => t -> Bool
propRoundedLatticeJoinIdempotent = roundedIdempotent (completeWith True (<=?)) maxUp maxDn

propRoundedLatticeJoinCommutative :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeJoinCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (completeWith True (<=?)) maxUp maxDn e1 e2

propRoundedLatticeJoinAssocative :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeJoinAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (completeWith True (<=?)) maxUp maxDn e1 e2 e3

propRoundedLatticeMeetIdempotent :: (SemidecidablePoset t, RoundedLattice t) => t -> Bool
propRoundedLatticeMeetIdempotent = 
    roundedIdempotent (completeWith True (<=?)) minUp minDn

propRoundedLatticeMeetCommutative :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedPair t -> Bool
propRoundedLatticeMeetCommutative (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (completeWith True (<=?)) minUp minDn e1 e2

propRoundedLatticeMeetAssocative :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeMeetAssocative (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (completeWith True (<=?)) minUp minDn e1 e2 e3

{- optional properties: -}
propRoundedLatticeModular :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeModular (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (completeWith True (<=?)) maxUp minUp maxDn minDn e1 e2 e3

propRoundedLatticeDistributive :: (SemidecidablePoset t, RoundedLattice t) => UniformlyOrderedTriple t -> Bool
propRoundedLatticeDistributive (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedLeftDistributive  (completeWith True (<=?)) maxUp minUp maxDn minDn e1 e2 e3)
    && 
    (roundedLeftDistributive  (completeWith True (<=?)) maxUp minUp maxDn minDn e1 e2 e3)

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
    