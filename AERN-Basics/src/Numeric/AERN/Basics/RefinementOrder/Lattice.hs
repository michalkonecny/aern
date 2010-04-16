{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

import qualified Prelude 
import Prelude hiding (min, max, EQ, LT, GT)

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.PartialComparison

import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

infixr 3 |/\, ⊓
infixr 2 |\/, ⊔

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

propLatticeComparisonCompatible :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propLatticeComparisonCompatible _ effort (UniformlyOrderedPair (e1,e2)) =
    let ?pCompareEffort = effort in 
        (joinOfOrderedPair (==) (|<=?) (|\/) e1 e2) 
        && 
        (meetOfOrderedPair (==) (|<=?) (|/\) e1 e2) 

propLatticeJoinAboveBoth :: 
    (PartialComparison t, Lattice t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propLatticeJoinAboveBoth _ effort (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effort in 
        joinAboveOperands (|<=?) (|\/) e1 e2


propLatticeMeetBelowBoth :: 
    (PartialComparison t, Lattice t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propLatticeMeetBelowBoth _ effort (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effort in 
        meetBelowOperands (|<=?) (|/\) e1 e2

propLatticeJoinIdempotent :: 
    (Eq t, Lattice t) => 
    t -> t -> Bool
propLatticeJoinIdempotent _ = idempotent (==) (|\/)

propLatticeJoinCommutative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    commutative (==) (|\/) e1 e2

propLatticeJoinAssocative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeJoinAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    associative (==) (|\/) e1 e2 e3

propLatticeMeetIdempotent :: 
    (Eq t, Lattice t) => 
    t -> t -> Bool
propLatticeMeetIdempotent _ = idempotent (==) (|/\)

propLatticeMeetCommutative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeMeetCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    commutative (==) (|/\) e1 e2

propLatticeMeetAssocative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeMeetAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    associative (==) (|/\) e1 e2 e3

{- optional properties: -}
propLatticeModular :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeModular _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    modular (==) (|\/) (|/\) e1 e2 e3

propLatticeDistributive :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeDistributive _ (UniformlyOrderedTriple (e1,e2,e3)) = 
        (leftDistributive (==) (|\/) (|/\) e1 e2 e3)
        && 
        (leftDistributive (==) (|/\) (|\/) e1 e2 e3)

testsLatticeDistributive ::
    (PartialComparison t,
     Lattice t,
     Arbitrary t, Show t,
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t),
     ArbitraryOrderedTuple t,
     Eq t) => 
    (String, t) -> Test
testsLatticeDistributive (name, sample) =
    testGroup (name ++ " (min,max)") $
        [
         testProperty "Comparison compatible" (propLatticeComparisonCompatible sample)
        ,
         testProperty "join above" (propLatticeJoinAboveBoth sample)
        ,
         testProperty "meet below" (propLatticeMeetBelowBoth sample)
        ,
         testProperty "join idempotent" (propLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propLatticeMeetAssocative sample)
        ,
         testProperty "distributive" (propLatticeDistributive sample)
        ]
