{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Lattice
    Description :  lattices using refinement order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices using refinement order notation.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.Lattice where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.RefinementOrder.Comparison
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import qualified Prelude 
import Prelude hiding (Eq, (==), min, max, EQ, LT, GT)

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class Lattice t where
    (|/\) :: t -> t -> t
    (|\/) :: t -> t -> t

(⊓) :: (Lattice t) => t -> t -> t
(⊓) = (|/\)

(⊔) :: (Lattice t) => t -> t -> t
(⊔) = (|\/)

propLatticeComparisonCompatible :: (Comparison t, Lattice t) => t -> t -> Bool
propLatticeComparisonCompatible e1 e2 = 
    (joinOfOrderedPair (|==) (|<=) (|\/) e1 e2) 
    && 
    (meetOfOrderedPair (|==) (|<=) (|/\) e1 e2) 

propLatticeJoinAboveBoth :: (Comparison t, Lattice t) => t -> t -> Bool
propLatticeJoinAboveBoth = joinAboveOperands (|<=) (|\/)


propLatticeMeetBelowBoth :: (Comparison t, Lattice t) => t -> t -> Bool
propLatticeMeetBelowBoth = meetBelowOperands (|<=) (|/\)

propLatticeJoinIdempotent :: (Comparison t, Lattice t) => t -> Bool
propLatticeJoinIdempotent = idempotent (|==) (|\/)

propLatticeJoinCommutative :: (Comparison t, Lattice t) => t -> t -> Bool
propLatticeJoinCommutative = commutative (|==) (|\/)

propLatticeJoinAssocative :: (Comparison t, Lattice t) => t -> t -> t -> Bool
propLatticeJoinAssocative = associative (|==) (|\/)

propLatticeMeetIdempotent :: (Comparison t, Lattice t) => t -> Bool
propLatticeMeetIdempotent = idempotent (|==) (|/\)

propLatticeMeetCommutative :: (Comparison t, Lattice t) => t -> t -> Bool
propLatticeMeetCommutative = commutative (|==) (|/\)

propLatticeMeetAssocative :: (Comparison t, Lattice t) => t -> t -> t -> Bool
propLatticeMeetAssocative = associative (|==) (|/\)

{- optional properties: -}
propLatticeModular :: (Comparison t, Lattice t) => t -> t -> t -> Bool
propLatticeModular = modular (|==) (|\/) (|/\)

propLatticeDistributive :: (Comparison t, Lattice t) => t -> t -> t -> Bool
propLatticeDistributive e1 e2 e3 = 
        (leftDistributive (|==) (|\/) (|/\) e1 e2 e3)
        && 
        (leftDistributive (|==) (|/\) (|\/) e1 e2 e3)

