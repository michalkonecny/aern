{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Lattice
    Description :  lattices using numeric order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices using numeric order notation.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.Lattice where

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.PartialOrdering
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.NumericOrder.SemidecidableComparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

import qualified Prelude 
import Prelude hiding (min, max, EQ, LT, GT, (<), (<=), (>=), (>))

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class Lattice t where
    min :: t -> t -> t
    max :: t -> t -> t

propLatticeIllegalArgException :: (Lattice t) => t -> t -> Bool
propLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException 
                [min d illegalArg, min illegalArg d, max d illegalArg, max illegalArg d] 

propLatticeComparisonCompatible :: 
    (Eq t, SemidecidableComparison t, Lattice t) => 
    UniformlyOrderedPair t -> Bool
propLatticeComparisonCompatible (UniformlyOrderedPair (e1,e2)) =
    ((joinOfOrderedPair (==) (<=?) max e1 e2)
    && 
    (meetOfOrderedPair (==) (<=?) min e1 e2))

propLatticeJoinAboveBoth :: 
    (Eq t, SemidecidableComparison t, Lattice t) => 
    UniformlyOrderedPair t -> Bool
propLatticeJoinAboveBoth (UniformlyOrderedPair (e1,e2)) =
    (joinAboveOperands (<=?) max e1 e2)

propLatticeMeetBelowBoth :: (Eq t, SemidecidableComparison t, Lattice t) => UniformlyOrderedPair t -> Bool
propLatticeMeetBelowBoth (UniformlyOrderedPair (e1,e2)) = meetBelowOperands (<=?) min e1 e2

propLatticeJoinIdempotent :: (Eq t, Lattice t) => t -> Bool
propLatticeJoinIdempotent = idempotent (==) max

propLatticeJoinCommutative :: (Eq t, Lattice t) => UniformlyOrderedPair t -> Bool
propLatticeJoinCommutative (UniformlyOrderedPair (e1,e2)) = commutative (==) max e1 e2

propLatticeJoinAssocative :: (Eq t, Lattice t) => UniformlyOrderedTriple t -> Bool
propLatticeJoinAssocative (UniformlyOrderedTriple (e1,e2,e3)) = associative (==) max e1 e2 e3

propLatticeMeetIdempotent :: (Eq t, Lattice t) => t -> Bool
propLatticeMeetIdempotent = idempotent (==) min

propLatticeMeetCommutative :: (Eq t, Lattice t) => UniformlyOrderedPair t -> Bool
propLatticeMeetCommutative (UniformlyOrderedPair (e1,e2)) = commutative (==) min e1 e2

propLatticeMeetAssocative :: (Eq t, Lattice t) => UniformlyOrderedTriple t -> Bool
propLatticeMeetAssocative (UniformlyOrderedTriple (e1,e2,e3)) = associative (==) min e1 e2 e3

{- optional properties: -}
propLatticeModular :: (Eq t, Lattice t) => UniformlyOrderedTriple t -> Bool
propLatticeModular (UniformlyOrderedTriple (e1,e2,e3)) = modular (==) max min e1 e2 e3

propLatticeDistributive :: (Eq t, Lattice t) => UniformlyOrderedTriple t -> Bool
propLatticeDistributive (UniformlyOrderedTriple (e1,e2,e3)) = 
        (leftDistributive (==) max min e1 e2 e3)
        && 
        (leftDistributive (==) min max e1 e2 e3)



{-|
    A lattice that supports in-place operations.
-}
class (Lattice t, CanBeMutable t) => LatticeMutable t where
    {-| maxMutable a b c means a := b `max` c; a can be the same as b and/or c -}
    maxMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minMutable a b c means a := b `min` c; a can be the same as b and/or c -}
    minMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
        