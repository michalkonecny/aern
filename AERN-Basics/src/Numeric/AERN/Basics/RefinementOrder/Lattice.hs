{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Lattice
    Description :  lattices using refinement order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.Lattice where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.RefinementOrder.Poset
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import qualified Prelude 
import Prelude hiding (min, max, EQ, LT, GT)

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class (Eq t) => Lattice t where
    (|/\) :: t -> t -> t
    (|\/) :: t -> t -> t

(⊓) :: (Lattice t) => t -> t -> t
(⊓) = (|/\)

(⊔) :: (Lattice t) => t -> t -> t
(⊔) = (|\/)

propLatticePosetCompatible :: (Poset t, Lattice t) => t -> t -> Bool
propLatticePosetCompatible = joinMeetOfOrderedPair (|<=) (|/\) (|\/) 

propLatticeJoinAboveBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeJoinAboveBoth = joinAboveOperands (|<=) (|\/)


propLatticeMeetBelowBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeMeetBelowBoth = meetBelowOperands (|<=) (|/\)

propLatticeJoinIdempotent :: (Lattice t) => t -> Bool
propLatticeJoinIdempotent = idempotent (|\/)

propLatticeJoinCommutative :: (Lattice t) => t -> t -> Bool
propLatticeJoinCommutative = commutative (|\/)

propLatticeJoinAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeJoinAssocative = associative (|\/)

propLatticeMeetIdempotent :: (Lattice t) => t -> Bool
propLatticeMeetIdempotent = idempotent (|/\)

propLatticeMeetCommutative :: (Lattice t) => t -> t -> Bool
propLatticeMeetCommutative = commutative (|/\)

propLatticeMeetAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeMeetAssocative = associative (|/\)

{- optional properties: -}
propLatticeModular :: (Lattice t) => t -> t -> t -> Bool
propLatticeModular = modular (|\/) (|/\)

propLatticeDistributive :: (Lattice t) => t -> t -> t -> Bool
propLatticeDistributive e1 e2 e3 = 
        (leftDistributive (|\/) (|/\) e1 e2 e3)
        && 
        (leftDistributive (|/\) (|\/) e1 e2 e3)

{-|
    A lattice that supports in-place operations.
-}
class (Lattice t, CanBeMutable t) => LatticeMutable t where
    {-| joinMutable a b c means a := b |\/ c; a can be the same as b and/or c -}
    joinMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| meetMutable a b c means a := b |/\ c; a can be the same as b and/or c -}
    meetMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
