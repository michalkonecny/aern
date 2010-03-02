{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Lattice
    Description :  lattices using numeric order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.Lattice where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.NumericOrder.Poset
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import qualified Prelude 
import Prelude hiding (min, max, EQ, LT, GT, (<), (<=), (>=), (>))

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class (Eq t) => Lattice t where
    min :: t -> t -> t
    max :: t -> t -> t

propLatticePosetCompatible :: (Poset t, Lattice t) => t -> t -> Bool
propLatticePosetCompatible = joinMeetOfOrderedPair (<=) min max 

propLatticeJoinAboveBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeJoinAboveBoth = joinAboveOperands (<=) max 


propLatticeMeetBelowBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeMeetBelowBoth = meetBelowOperands (<=) min

propLatticeJoinIdempotent :: (Lattice t) => t -> Bool
propLatticeJoinIdempotent = idempotent max

propLatticeJoinCommutative :: (Lattice t) => t -> t -> Bool
propLatticeJoinCommutative = commutative max

propLatticeJoinAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeJoinAssocative = associative max

propLatticeMeetIdempotent :: (Lattice t) => t -> Bool
propLatticeMeetIdempotent = idempotent min

propLatticeMeetCommutative :: (Lattice t) => t -> t -> Bool
propLatticeMeetCommutative = commutative min

propLatticeMeetAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeMeetAssocative = associative min

{- optional properties: -}
propLatticeModular :: (Lattice t) => t -> t -> t -> Bool
propLatticeModular = modular max min

propLatticeDistributive :: (Lattice t) => t -> t -> t -> Bool
propLatticeDistributive e1 e2 e3 = 
        (leftDistributive max min e1 e2 e3)
        && 
        (leftDistributive min max e1 e2 e3)

{-|
    A lattice that supports in-place operations.
-}
class (Lattice t, CanBeMutable t) => LatticeMutable t where
    {-| maxMutable a b c means a := b `max` c; a can be the same as b and/or c -}
    maxMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minMutable a b c means a := b `min` c; a can be the same as b and/or c -}
    minMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
        