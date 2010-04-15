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

import Numeric.AERN.Basics.NumericOrder.PartialComparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

import qualified Prelude 
import Prelude hiding (min, max, EQ, LT, GT, (<), (<=), (>=), (>))

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)


{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class Lattice t where
    min :: t -> t -> t
    max :: t -> t -> t

instance Lattice Int where
    min = Prelude.min
    max = Prelude.max

instance Lattice () where
    min _ _ = () 
    max _ _ = ()

propLatticeIllegalArgException :: (Lattice t) => t -> t -> Bool
propLatticeIllegalArgException illegalArg d =
    and $ map raisesAERNException 
                [min d illegalArg, min illegalArg d, max d illegalArg, max illegalArg d] 

propLatticeComparisonCompatible :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeComparisonCompatible _ (UniformlyOrderedPair (e1,e2)) =
    ((joinOfOrderedPair (==) (<=?) max e1 e2)
    && 
    (meetOfOrderedPair (==) (<=?) min e1 e2))

propLatticeJoinAboveBoth :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeJoinAboveBoth _ (UniformlyOrderedPair (e1,e2)) =
    (joinAboveOperands (<=?) max e1 e2)

propLatticeMeetBelowBoth :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeMeetBelowBoth _ (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (<=?) min e1 e2

propLatticeJoinIdempotent :: (Eq t, Lattice t) => t -> t -> Bool
propLatticeJoinIdempotent _ = idempotent (==) max

propLatticeJoinCommutative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    commutative (==) max e1 e2

propLatticeJoinAssocative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeJoinAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    associative (==) max e1 e2 e3

propLatticeMeetIdempotent :: (Eq t, Lattice t) => t -> t -> Bool
propLatticeMeetIdempotent _ = idempotent (==) min

propLatticeMeetCommutative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedPair t -> Bool
propLatticeMeetCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    commutative (==) min e1 e2

propLatticeMeetAssocative :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeMeetAssocative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    associative (==) min e1 e2 e3

{- optional properties: -}
propLatticeModular :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeModular _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    modular (==) max min e1 e2 e3

propLatticeDistributive :: 
    (Eq t, Lattice t) => 
    t -> UniformlyOrderedTriple t -> Bool
propLatticeDistributive _ (UniformlyOrderedTriple (e1,e2,e3)) = 
        (leftDistributive (==) max min e1 e2 e3)
        && 
        (leftDistributive (==) min max e1 e2 e3)

testsLatticeDistributive ::
    (PartialComparison t,
     Lattice t,
     Arbitrary t, 
     ArbitraryOrderedTuple t,
     Eq t, 
     Show t) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsLatticeDistributive (name, sample) maybeIllegalArg =
    testGroup (name ++ " (min,max)") $
        (case maybeIllegalArg of 
            Nothing -> []
            Just (illegalArgName, illegalArg) -> 
                [testProperty (illegalArgName ++ " exception") 
                              (propLatticeIllegalArgException illegalArg)]) 
        ++
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

--
--{-|
--    A lattice that supports in-place operations.
---}
--class (Lattice t, CanBeMutable t) => LatticeMutable t where
--    {-| maxMutable a b c means a := b `max` c; a can be the same as b and/or c -}
--    maxMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--    {-| minMutable a b c means a := b `min` c; a can be the same as b and/or c -}
--    minMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--
--    -- TODO: add default implementations using read/write
        