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
    maxOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t
    minOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t

class OuterRoundedLatticeEffort t where
    type MinmaxOuterEffortIndicator t
    minmaxOuterDefaultEffort :: t -> MinmaxOuterEffortIndicator t

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class (InnerRoundedLatticeEffort t) => InnerRoundedLattice t where
    maxInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t
    minInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t

class InnerRoundedLatticeEffort t where
    type MinmaxInnerEffortIndicator t
    minmaxInnerDefaultEffort :: t -> MinmaxInnerEffortIndicator t

class (OuterRoundedLattice t, InnerRoundedLattice t) => RefinementRoundedLattice t

propRefinementRoundedLatticeIllegalArgException :: 
    (RefinementRoundedLattice t) => 
    t -> 
    (MinmaxInnerEffortIndicator t, MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeIllegalArgException illegalArg (effortIn, effortOut) d =
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] 
                          | op <- [maxInnerEff effortIn, maxOuterEff effortOut, 
                                   minInnerEff effortIn, minOuterEff effortOut]] 

propRefinementRoundedLatticeJoinIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeJoinIdempotent _ (effortComp, effortIn, effortOut) =
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (maxInnerEff effortIn) (maxOuterEff effortOut)

propRefinementRoundedLatticeJoinCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (maxInnerEff effortIn) (maxOuterEff effortOut) e1 e2

propRefinementRoundedLatticeJoinAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeJoinAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative (RefOrd.pLeqEff effortComp) 
        (maxInnerEff effortIn) (maxOuterEff effortOut) e1 e2 e3

propRefinementRoundedLatticeMeetIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeMeetIdempotent _ (effortComp, effortIn, effortOut) = 
    roundedIdempotent (RefOrd.pLeqEff effortComp) 
        (minInnerEff effortIn) (minOuterEff effortOut)

propRefinementRoundedLatticeMeetCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetCommutative _  (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    roundedCommutative (RefOrd.pLeqEff effortComp) 
        (minInnerEff effortIn) (minOuterEff effortOut) e1 e2

propRefinementRoundedLatticeMeetAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeMeetAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedAssociative  (RefOrd.pLeqEff effortComp) 
        (minInnerEff effortIn) (minOuterEff effortOut) e1 e2 e3

{- optional properties: -}
propRefinementRoundedLatticeModular :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeModular _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInnerEff effortIn) (minInnerEff effortIn)
        (maxOuterEff effortOut) (minOuterEff effortOut)
        e1 e2 e3

propRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeDistributive _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (maxInnerEff effortIn) (minInnerEff effortIn)
        (maxOuterEff effortOut) (minOuterEff effortOut)
        e1 e2 e3)
    && 
    (roundedModular (RefOrd.pLeqEff effortComp) 
        (minInnerEff effortIn) (maxInnerEff effortIn)
        (minOuterEff effortOut) (maxOuterEff effortOut)
        e1 e2 e3)
    
propRefinementRoundedLatticeJoinMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t) => 
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
    rLower = maxOuterEff effortOut e1Lower e2Lower 
    r = maxInnerEff effortIn e1 e2 
    
propRefinementRoundedLatticeMeetMonotone ::
    (Eq t, RefinementRoundedLattice t, RefOrd.PartialComparison t) => 
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
    rLower = minOuterEff effortOut e1Lower e2Lower 
    r = minInnerEff effortIn e1 e2 
    
mkTestGroupLattice name = testGroup (name ++ " (min,max) treated as refinement rounded")
    
testsRefinementRoundedLattice :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, 
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRefinementRoundedLattice (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsRefinementRoundedLatticeL sample maybeIllegalArg)

testsRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, 
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRefinementRoundedLatticeDistributive (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsRefinementRoundedLatticeDistributiveL sample maybeIllegalArg)

testsRefinementRoundedLatticeDistributiveMonotone :: 
    (RefOrd.PartialComparison t,
     RefOrd.ArbitraryOrderedTuple t,
     HasExtrema t,
     RefinementRoundedLattice t,
     ArbitraryOrderedTuple t,
     Arbitrary t, Show t, 
     Arbitrary (RefOrd.PartialCompareEffortIndicator t), Show (RefOrd.PartialCompareEffortIndicator t), 
     Arbitrary (MinmaxInnerEffortIndicator t), Show (MinmaxInnerEffortIndicator t), 
     Arbitrary (MinmaxOuterEffortIndicator t), Show (MinmaxOuterEffortIndicator t), 
     Eq t 
     ) => 
    (String, t) -> (Maybe (String, t)) -> Test
testsRefinementRoundedLatticeDistributiveMonotone (name, sample) maybeIllegalArg =
    mkTestGroupLattice name (testsRefinementRoundedLatticeDistributiveMonotoneL sample maybeIllegalArg)

testsRefinementRoundedLatticeL sample maybeIllegalArg =    
        (case maybeIllegalArg of 
            Nothing -> []
            Just (illegalArgName, illegalArg) -> 
                [testProperty (illegalArgName ++ " exception") 
                              (propRefinementRoundedLatticeIllegalArgException illegalArg)]) 
        ++
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
        
testsRefinementRoundedLatticeDistributiveL sample maybeIllegalArg =
    testsRefinementRoundedLatticeL sample maybeIllegalArg ++
        [    
         testProperty "distributive" (propRefinementRoundedLatticeDistributive sample)
        ]
        
testsRefinementRoundedLatticeDistributiveMonotoneL sample maybeIllegalArg =
    testsRefinementRoundedLatticeDistributiveL sample maybeIllegalArg ++
        [    
         testProperty "join monotone" (propRefinementRoundedLatticeJoinMonotone sample)
        ,
         testProperty "meet monotone" (propRefinementRoundedLatticeMeetMonotone sample)
        ]
    