{-# LANGUAGE FlexibleContexts #-}
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

module Numeric.AERN.Basics.NumericOrder.Lattice 
(
    Lattice(..),
    testsLattice, testsLatticeDistributive, testsLatticeDistributiveMonotone
)
where

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.PartialOrdering
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.NumericOrder.PartialComparison
import Numeric.AERN.Basics.NumericOrder.Arbitrary
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

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
    t -> (PartialCompareEffortIndicator t) -> UniformlyOrderedPair t -> Bool
propLatticeComparisonCompatible _ effort (UniformlyOrderedPair (e1,e2)) =
    (joinOfOrderedPair (==) (pLeqEff effort) max e1 e2)
    && 
    (meetOfOrderedPair (==) (pLeqEff effort) min e1 e2)

propLatticeJoinAboveBoth :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> (PartialCompareEffortIndicator t) -> UniformlyOrderedPair t -> Bool
propLatticeJoinAboveBoth _ effort (UniformlyOrderedPair (e1,e2)) =
    (joinAboveOperands (pLeqEff effort) max e1 e2)

propLatticeMeetBelowBoth :: 
    (Eq t, PartialComparison t, Lattice t) => 
    t -> (PartialCompareEffortIndicator t) -> UniformlyOrderedPair t -> Bool
propLatticeMeetBelowBoth _ effort (UniformlyOrderedPair (e1,e2)) = 
    meetBelowOperands (pLeqEff effort) min e1 e2

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

propLatticeJoinMonotone ::
    (Eq t, Lattice t, RefOrd.PartialComparison t) => 
    t -> 
    RefOrd.PartialCompareEffortIndicator t -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propLatticeJoinMonotone _ effortComp
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = max e1Lower e2Lower 
    r = max e1 e2 
    
propLatticeMeetMonotone ::
    (Eq t, Lattice t, RefOrd.PartialComparison t) => 
    t -> 
    RefOrd.PartialCompareEffortIndicator t -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propLatticeMeetMonotone _ effortComp
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = min e1Lower e2Lower 
    r = min e1 e2 
    

mkTestGroupLattice name = testGroup (name ++ " (min,max)")

testsLattice ::
    (PartialComparison t,
     Lattice t,
     Arbitrary t, Show t, 
     ArbitraryOrderedTuple t,
     Eq t,
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t) 
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsLattice (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsLatticeL sample maybeIllegalArg)

testsLatticeDistributive ::
    (PartialComparison t,
     Lattice t,
     Arbitrary t, Show t, 
     ArbitraryOrderedTuple t,
     Eq t,
     Arbitrary (PartialCompareEffortIndicator t), Show (PartialCompareEffortIndicator t) 
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsLatticeDistributive (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsLatticeDistributiveL sample maybeIllegalArg)

testsLatticeDistributiveMonotone ::
    (PartialComparison t,
     Lattice t,
     Arbitrary t, Show t, 
     ArbitraryOrderedTuple t,
     Arbitrary (PartialCompareEffortIndicator t), 
     Show (PartialCompareEffortIndicator t), 
     RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), 
     Show (RefOrd.PartialCompareEffortIndicator t), 
     Eq t
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsLatticeDistributiveMonotone (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsLatticeDistributiveMonotoneL sample maybeIllegalArg)

testsLatticeL sample maybeIllegalArg =
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
        ]    

testsLatticeDistributiveL sample maybeIllegalArg =
    (testsLatticeL sample maybeIllegalArg) ++
        [
         testProperty "distributive" (propLatticeDistributive sample)
        ]

testsLatticeDistributiveMonotoneL sample maybeIllegalArg =
    (testsLatticeDistributiveL sample maybeIllegalArg) ++
        [
         testProperty "join monotone" (propLatticeJoinMonotone sample)
        ,
         testProperty "meet monotone" (propLatticeMeetMonotone sample)
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
        