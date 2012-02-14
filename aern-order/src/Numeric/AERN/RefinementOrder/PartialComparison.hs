{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.ApproxOrder
    Description :  Comparisons with semidecidable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Comparisons with semidecidable order.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.RefinementOrder.PartialComparison 
where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.RefinementOrder.Extrema
import Numeric.AERN.RefinementOrder.Arbitrary

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Laws.PartialRelation

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A type with semi-decidable equality and partial order
-}
class
    (EffortIndicator (PartialCompareEffortIndicator t))
    => 
    PartialComparison t 
    where
    type PartialCompareEffortIndicator t
    pCompareEff :: PartialCompareEffortIndicator t -> t -> t -> Maybe PartialOrdering
    pCompareInFullEff :: PartialCompareEffortIndicator t -> t -> t -> PartialOrderingPartialInfo
    pCompareInFullEff eff a b = partialOrdering2PartialInfo $ pCompareEff eff a b 
    pCompareDefaultEffort :: t -> PartialCompareEffortIndicator t
    
    -- | Partial equality
    pEqualEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    -- | Partial `is comparable to`.
    pComparableEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    -- | Partial `is not comparable to`.
    pIncomparableEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pLessEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pLeqEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pGeqEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    pGreaterEff :: (PartialCompareEffortIndicator t) -> t -> t -> Maybe Bool
    
    -- defaults for all convenience operations:
    pEqualEff effort a b =
        pOrdInfEQ $ pCompareInFullEff effort a b
    pLessEff effort a b = 
        pOrdInfLT $ pCompareInFullEff effort a b
    pGreaterEff effort a b = 
        pOrdInfGT $ pCompareInFullEff effort a b
    pLeqEff effort a b =
        pOrdInfLEQ $ pCompareInFullEff effort a b
    pGeqEff effort a b =
        pOrdInfGEQ $ pCompareInFullEff effort a b
    pComparableEff effort a b = 
        fmap not $ pOrdInfNC $ pCompareInFullEff effort a b
    pIncomparableEff effort a b =
        pOrdInfNC $ pCompareInFullEff effort a b


propPartialComparisonReflexiveEQ :: 
    (PartialComparison t) => 
    t -> 
    (PartialCompareEffortIndicator t) -> 
    (UniformlyOrderedSingleton t) -> 
    Bool
propPartialComparisonReflexiveEQ _ 
        effort 
        (UniformlyOrderedSingleton e) 
    = 
    case pCompareEff effort e e of Just EQ -> True; Nothing -> True; _ -> False 

propPartialComparisonAntiSymmetric :: 
    (PartialComparison t) => 
    t -> 
    UniformlyOrderedPair t -> 
    (PartialCompareEffortIndicator t) -> 
    Bool
propPartialComparisonAntiSymmetric _ 
        (UniformlyOrderedPair (e1, e2)) 
        effort 
    =
    case (pCompareEff effort e2 e1, pCompareEff effort e1 e2) of
        (Just b1, Just b2) -> b1 == partialOrderingTranspose b2
        _ -> True 

propPartialComparisonTransitiveEQ :: 
    (PartialComparison t) => 
    t -> 
    UniformlyOrderedTriple t -> 
    (PartialCompareEffortIndicator t) -> 
    Bool
propPartialComparisonTransitiveEQ _ 
        (UniformlyOrderedTriple (e1,e2,e3)) 
        effort 
    = 
    partialTransitive (pEqualEff effort) e1 e2 e3

propPartialComparisonTransitiveLT :: 
    (PartialComparison t) => 
    t -> 
    UniformlyOrderedTriple t -> 
    (PartialCompareEffortIndicator t) -> 
    Bool
propPartialComparisonTransitiveLT _ 
        (UniformlyOrderedTriple (e1,e2,e3)) 
        effort 
    = 
    partialTransitive (pLessEff effort) e1 e2 e3

propPartialComparisonTransitiveLE :: 
    (PartialComparison t) => 
    t -> 
    UniformlyOrderedTriple t -> 
    (PartialCompareEffortIndicator t) -> 
    Bool
propPartialComparisonTransitiveLE _ 
        (UniformlyOrderedTriple (e1,e2,e3)) 
        effort
    = 
    partialTransitive (pLeqEff effort) e1 e2 e3

propExtremaInPartialComparison :: 
    (PartialComparison t, HasExtrema t) => 
    t -> 
    (UniformlyOrderedSingleton t) -> 
    (PartialCompareEffortIndicator t) -> 
    Bool
propExtremaInPartialComparison _ 
        (UniformlyOrderedSingleton e) 
        effort 
    = 
    partialOrderExtrema (pLeqEff effort) (bottom e) (top e) e

testsPartialComparison :: 
    (PartialComparison t,
     HasExtrema t,
     ArbitraryOrderedTuple t, Show t) 
    => 
    (String, t) -> 
    (Area t) ->
    Test
testsPartialComparison (name, sample) area =
    testGroup (name ++ " (⊑?)")
        [
         testProperty "anti symmetric" (area, propPartialComparisonAntiSymmetric sample)
        ,
         testProperty "transitive EQ" (area, propPartialComparisonTransitiveEQ sample)
        ,
         testProperty "transitive LE" (area, propPartialComparisonTransitiveLE sample)
        ,
         testProperty "transitive LT" (area, propPartialComparisonTransitiveLT sample)
        ,
         testProperty "extrema" (area, propExtremaInPartialComparison sample)
        ]
