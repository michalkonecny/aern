{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice
    Description :  lattices over numerical order but with refinement order rounding  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Lattices over numerical order but with refinement order rounding.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.RefinementRoundedLattice 
(
    OuterRoundedLattice(..),
    OuterRoundedLatticeEffort(..),
    InnerRoundedLattice(..),
    InnerRoundedLatticeEffort(..),
    RefinementRoundedLattice(..),
    testsRefinementRoundedLattice, 
    testsRefinementRoundedLatticeDistributive,
    testsRefinementRoundedLatticeDistributiveMonotone
)
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception 

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Arbitrary 
import Numeric.AERN.Basics.NumericOrder.PartialComparison 
import Numeric.AERN.Basics.NumericOrder.Extrema

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Laws.PartialRelation
import Numeric.AERN.Basics.Laws.RoundedOperation
import Numeric.AERN.Basics.Laws.OperationRelation

import Numeric.AERN.Misc.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with refinement-outer-rounding numerical-order-lattice operations.
-}
class (OuterRoundedLatticeEffort t) => OuterRoundedLattice t where
    maxOutEff :: MinmaxOuterEffortIndicator t -> t -> t -> t
    minOutEff :: MinmaxOuterEffortIndicator t -> t -> t -> t

class OuterRoundedLatticeEffort t where
    type MinmaxOuterEffortIndicator t
    minmaxOutDefaultEffort :: t -> MinmaxOuterEffortIndicator t

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class (InnerRoundedLatticeEffort t) => InnerRoundedLattice t where
    maxInEff :: MinmaxInnerEffortIndicator t -> t -> t -> t
    minInEff :: MinmaxInnerEffortIndicator t -> t -> t -> t

class InnerRoundedLatticeEffort t where
    type MinmaxInnerEffortIndicator t
    minmaxInDefaultEffort :: t -> MinmaxInnerEffortIndicator t

class (OuterRoundedLattice t, InnerRoundedLattice t) => RefinementRoundedLattice t

propRefinementRoundedLatticeJoinIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> Bool
propRefinementRoundedLatticeJoinIdempotent _ (effortComp, effortIn, effortOut) 
        (UniformlyOrderedSingleton e) =
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (maxInEff effortIn) (maxOutEff effortOut) e

propRefinementRoundedLatticeJoinCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (maxInEff effortIn) (maxOutEff effortOut) e1 e2

propRefinementRoundedLatticeJoinAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeJoinAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (RefOrd.pLeqEff effortComp) 
        (maxInEff effortIn) (maxOutEff effortOut) e1 e2 e3

propRefinementRoundedLatticeMeetIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> Bool
propRefinementRoundedLatticeMeetIdempotent _ (effortComp, effortIn, effortOut) 
        (UniformlyOrderedSingleton e) = 
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (minInEff effortIn) (minOutEff effortOut) e

propRefinementRoundedLatticeMeetCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetCommutative _  (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (minInEff effortIn) (minOutEff effortOut) e1 e2

propRefinementRoundedLatticeMeetAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeMeetAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative  (RefOrd.pLeqEff effortComp) 
        (minInEff effortIn) (minOutEff effortOut) e1 e2 e3

{- optional properties: -}
propRefinementRoundedLatticeModular :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t, Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeModular _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInEff effortIn) (minInEff effortIn)
        (maxOutEff effortOut) (minOutEff effortOut)
        e1 e2 e3

propRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t,
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeDistributive _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInEff effortIn) (minInEff effortIn)
        (maxOutEff effortOut) (minOutEff effortOut)
        e1 e2 e3)
    && 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (minInEff effortIn) (maxInEff effortIn)
        (minOutEff effortOut) (maxOutEff effortOut)
        e1 e2 e3)
    
propRefinementRoundedLatticeJoinMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t, 
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propRefinementRoundedLatticeJoinMonotone _ (effortComp, effortIn, effortOut)
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = maxOutEff effortOut e1Lower e2Lower 
    r = maxInEff effortIn e1 e2 
    
propRefinementRoundedLatticeMeetMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t, 
     Show t, HasLegalValues t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    RefOrd.LEPair t -> 
    RefOrd.LEPair t ->
    Bool
propRefinementRoundedLatticeMeetMonotone _ (effortComp, effortIn, effortOut)
        (RefOrd.LEPair (e1Lower,e1)) 
        (RefOrd.LEPair (e2Lower,e2)) =
    case RefOrd.pLeqEff effortComp rLower r of
        Just b -> b
        Nothing -> True
    where
    rLower = minOutEff effortOut e1Lower e2Lower 
    r = minInEff effortIn e1 e2 
    
mkTestGroupLattice name = testGroup (name ++ " (min,max) treated as refinement rounded")
    
testsRefinementRoundedLattice :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, HasLegalValues t,
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
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
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
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
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
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
    