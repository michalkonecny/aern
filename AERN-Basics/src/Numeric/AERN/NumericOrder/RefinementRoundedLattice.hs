{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.RefinementRoundedLattice
    Description :  lattices over numerical order but with refinement order rounding  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices over numerical order but with refinement order rounding.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.NumericOrder.RefinementRoundedLattice 
(
    RefinementRoundedLattice(..),
    RefinementRoundedLatticeEffort(..),
    testsRefinementRoundedLattice, 
    testsRefinementRoundedLatticeDistributive,
    testsRefinementRoundedLatticeDistributiveMonotone
)
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception 

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.NumericOrder.Arbitrary 
import Numeric.AERN.NumericOrder.PartialComparison 
import Numeric.AERN.NumericOrder.Extrema

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Laws.PartialRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with refinement rounding numerical order lattice operations.
-}
class (RefinementRoundedLatticeEffort t) => RefinementRoundedLattice t where
    maxInEff :: MinmaxInOutEffortIndicator t -> t -> t -> t
    maxOutEff :: MinmaxInOutEffortIndicator t -> t -> t -> t
    minInEff :: MinmaxInOutEffortIndicator t -> t -> t -> t
    minOutEff :: MinmaxInOutEffortIndicator t -> t -> t -> t

class RefinementRoundedLatticeEffort t where
    type MinmaxInOutEffortIndicator t
    minmaxInOutDefaultEffort :: t -> MinmaxInOutEffortIndicator t

propRefinementRoundedLatticeJoinIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> Bool
propRefinementRoundedLatticeJoinIdempotent _ (effortComp, effortInOut) 
        (UniformlyOrderedSingleton e) =
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (maxInEff effortInOut) (maxOutEff effortInOut) e

propRefinementRoundedLatticeJoinCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinCommutative _ (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (maxInEff effortInOut) (maxOutEff effortInOut) e1 e2

propRefinementRoundedLatticeJoinAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeJoinAssocative _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (RefOrd.pLeqEff effortComp) 
        (maxInEff effortInOut) (maxOutEff effortInOut) e1 e2 e3

propRefinementRoundedLatticeMeetIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> Bool
propRefinementRoundedLatticeMeetIdempotent _ (effortComp, effortInOut) 
        (UniformlyOrderedSingleton e) = 
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (minInEff effortInOut) (minOutEff effortInOut) e

propRefinementRoundedLatticeMeetCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetCommutative _  (effortComp, effortInOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (minInEff effortInOut) (minOutEff effortInOut) e1 e2

propRefinementRoundedLatticeMeetAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeMeetAssocative _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative  (RefOrd.pLeqEff effortComp) 
        (minInEff effortInOut) (minOutEff effortInOut) e1 e2 e3

{- optional properties: -}
propRefinementRoundedLatticeModular :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeModular _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInEff effortInOut) (minInEff effortInOut)
        (maxOutEff effortInOut) (minOutEff effortInOut)
        e1 e2 e3

propRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t,
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeDistributive _ (effortComp, effortInOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInEff effortInOut) (minInEff effortInOut)
        (maxOutEff effortInOut) (minOutEff effortInOut)
        e1 e2 e3)
    && 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (minInEff effortInOut) (maxInEff effortInOut)
        (minOutEff effortInOut) (maxOutEff effortInOut)
        e1 e2 e3)
    
propRefinementRoundedLatticeJoinMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t, 
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propRefinementRoundedLatticeJoinMonotone _ (effortComp, effortInOut)
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = maxOutEff effortInOut e1Lower e2Lower 
    r = maxInEff effortInOut e1 e2 
    
propRefinementRoundedLatticeMeetMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t, 
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInOutEffortIndicator t) -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propRefinementRoundedLatticeMeetMonotone _ (effortComp, effortInOut)
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = minOutEff effortInOut e1Lower e2Lower 
    r = minInEff effortInOut e1 e2 
    
mkTestGroupLattice name = testGroup (name ++ " (min,max) treated as refinement rounded")
    
testsRefinementRoundedLattice :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, HasLegalValues t,
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInOutEffortIndicator t), Show (MinmaxInOutEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> Test
testsRefinementRoundedLattice (name, sample) =
    mkTestGroupLattice name (testsRefinementRoundedLatticeL sample)

testsRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, HasLegalValues t, 
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInOutEffortIndicator t), Show (MinmaxInOutEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> Test
testsRefinementRoundedLatticeDistributive (name, sample) =
    mkTestGroupLattice name (testsRefinementRoundedLatticeDistributiveL sample)

testsRefinementRoundedLatticeDistributiveMonotone :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, HasLegalValues t, 
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInOutEffortIndicator t), Show (MinmaxInOutEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> Test
testsRefinementRoundedLatticeDistributiveMonotone (name, sample) =
    mkTestGroupLattice name (testsRefinementRoundedLatticeDistributiveMonotoneL sample)

testsRefinementRoundedLatticeL sample =    
        [
         testProperty "join idempotent" (propRefinementRoundedLatticeJoinIdempotent sample)
        ,
         testProperty "join commutative" (propRefinementRoundedLatticeJoinCommutative sample)
        ,
         testProperty "join associative" (propRefinementRoundedLatticeJoinAssocative sample)
        ,
         testProperty "meet idempotent" (propRefinementRoundedLatticeMeetIdempotent sample)
        ,
         testProperty "meet commutative" (propRefinementRoundedLatticeMeetCommutative sample)
        ,
         testProperty "meet associative" (propRefinementRoundedLatticeMeetAssocative sample)
        ]
        
testsRefinementRoundedLatticeDistributiveL sample =
    testsRefinementRoundedLatticeL sample ++
        [    
         testProperty "distributive" (propRefinementRoundedLatticeDistributive sample)
        ]
        
testsRefinementRoundedLatticeDistributiveMonotoneL sample =
    testsRefinementRoundedLatticeDistributiveL sample ++
        [    
         testProperty "join monotone" (propRefinementRoundedLatticeJoinMonotone sample)
        ,
         testProperty "meet monotone" (propRefinementRoundedLatticeMeetMonotone sample)
        ]
    