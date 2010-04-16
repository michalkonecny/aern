{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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
where

import Prelude hiding ((<=))

import Numeric.AERN.Basics.Exception 

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Arbitrary 
import Numeric.AERN.Basics.NumericOrder.PartialComparison 
import Numeric.AERN.Basics.NumericOrder.Extrema

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder ((|<=?))

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
class OuterRoundedLattice t where
    type MinmaxOuterEffortIndicator t
    minmaxOuterDefaultEffort :: t -> MinmaxOuterEffortIndicator t

    maxOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t
    minOuterEff :: MinmaxOuterEffortIndicator t -> t -> t -> t

    maxOuter :: 
        (?minmaxOuterEffort :: MinmaxOuterEffortIndicator t) => 
        t -> t -> t
    minOuter ::
        (?minmaxOuterEffort :: MinmaxOuterEffortIndicator t) => 
        t -> t -> t
    
    maxOuter = maxOuterEff ?minmaxOuterEffort 
    minOuter = minOuterEff ?minmaxOuterEffort 

{-|
    A type with refinement-inner-rounding numerical-order-lattice operations.
-}
class InnerRoundedLattice t where
    type MinmaxInnerEffortIndicator t
    minmaxInnerDefaultEffort :: t -> MinmaxInnerEffortIndicator t

    maxInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t
    minInnerEff :: MinmaxInnerEffortIndicator t -> t -> t -> t

    maxInner :: 
        (?minmaxInnerEffort :: MinmaxInnerEffortIndicator t) => 
        t -> t -> t
    minInner ::
        (?minmaxInnerEffort :: MinmaxInnerEffortIndicator t) => 
        t -> t -> t
    
    maxInner = maxInnerEff ?minmaxInnerEffort 
    minInner = minInnerEff ?minmaxInnerEffort 


class (OuterRoundedLattice t, InnerRoundedLattice t) => RefinementRoundedLattice t

propRefinementRoundedLatticeIllegalArgException :: 
    (RefinementRoundedLattice t) => 
    t -> 
    (MinmaxInnerEffortIndicator t, MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeIllegalArgException illegalArg (effortIn, effortOut) d =
    let ?minmaxInnerEffort = effortIn; ?minmaxOuterEffort = effortOut in
    and $ map raisesAERNException $ 
                concat [[op d illegalArg, op illegalArg d] 
                          | op <- [maxInner, maxOuter, minInner, minOuter]] 

propRefinementRoundedLatticeJoinIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeJoinIdempotent _ (effortComp, effortIn, effortOut) =
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedIdempotent (|<=?) maxInner maxOuter

propRefinementRoundedLatticeJoinCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeJoinCommutative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedCommutative (|<=?) maxInner maxOuter e1 e2

propRefinementRoundedLatticeJoinAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeJoinAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedAssociative (|<=?) maxInner maxOuter e1 e2 e3

propRefinementRoundedLatticeMeetIdempotent :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    t -> Bool
propRefinementRoundedLatticeMeetIdempotent _ (effortComp, effortIn, effortOut) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedIdempotent (|<=?) minInner minOuter

propRefinementRoundedLatticeMeetCommutative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedPair t -> Bool
propRefinementRoundedLatticeMeetCommutative _  (effortComp, effortIn, effortOut)
        (UniformlyOrderedPair (e1,e2)) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedCommutative (|<=?) minInner minOuter e1 e2

propRefinementRoundedLatticeMeetAssocative :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeMeetAssocative _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedAssociative (|<=?) minInner minOuter e1 e2 e3

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
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    roundedModular (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3

propRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t, RefinementRoundedLattice t) => 
    t -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     MinmaxInnerEffortIndicator t, 
     MinmaxOuterEffortIndicator t) -> 
    UniformlyOrderedTriple t -> Bool
propRefinementRoundedLatticeDistributive _ (effortComp, effortIn, effortOut)
        (UniformlyOrderedTriple (e1,e2,e3)) = 
    let ?pCompareEffort = effortComp
        ?minmaxInnerEffort = effortIn
        ?minmaxOuterEffort = effortOut in 
    (roundedLeftDistributive  (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3)
    && 
    (roundedLeftDistributive  (|<=?) maxInner minInner maxOuter minOuter e1 e2 e3)
    
testsRefinementRoundedLatticeDistributive :: 
    (RefOrd.PartialComparison t,
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
    testGroup (name ++ " (min,max) treated as refinement rounded") $
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
        ,
         testProperty "distributive" (propRefinementRoundedLatticeDistributive sample)
        ]
    